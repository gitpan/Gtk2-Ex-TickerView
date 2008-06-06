# Copyright 2007, 2008 Kevin Ryde

# This file is part of Gtk2-Ex-TickerView.
#
# Gtk2-Ex-TickerView is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 3, or (at your option) any later
# version.
#
# Gtk2-Ex-TickerView is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
# or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
#
# You should have received a copy of the GNU General Public License along
# with Gtk2-Ex-TickerView.  If not, see <http://www.gnu.org/licenses/>.

package Gtk2::Ex::TickerView;
use strict;
use warnings;
use Carp;
use Glib;
# 1.180 for Gtk2::CellLayout interface and for working $region->get_clipbox
use Gtk2 1.180;
use List::Util qw(min max);
use POSIX qw(DBL_MAX);
use Gtk2::Ex::CellLayout::Base 2;  # version 2 for Gtk2::Buildable
use base 'Gtk2::Ex::CellLayout::Base';

our $VERSION = 4;

# set this to 1 for some diagnostic prints, or 2 for even more prints
use constant DEBUG => 0;

use constant {
  DEFAULT_FRAME_RATE => 4,     # times per second
  DEFAULT_SPEED      => 30,    # pixels per second
};

# not wrapped as of gtk2-perl version 1.181
use constant GTK_PRIORITY_REDRAW => (Glib::G_PRIORITY_HIGH_IDLE + 20);

use Glib::Object::Subclass
  Gtk2::DrawingArea::,
  interfaces => [ 'Gtk2::CellLayout', 'Gtk2::Buildable' ],
  signals => { expose_event            => \&_do_expose_event,
               no_expose_event         => \&_do_no_expose_event,
               size_request            => \&_do_size_request,
               button_press_event      => \&_do_button_press_event,
               motion_notify_event     => \&_do_motion_notify_event,
               button_release_event    => \&_do_button_release_event,
               visibility_notify_event => \&_do_visibility_notify_event,
               direction_changed       => \&_do_direction_changed,
               map                     => \&_do_map,
               unmap                   => \&_do_unmap,
               unrealize               => \&_do_unrealize,
             },
  properties => [Glib::ParamSpec->object
                 ('model',
                  'model',
                  'TreeModel giving the items to display.',
                  'Gtk2::TreeModel',
                  Glib::G_PARAM_READWRITE),

                 Glib::ParamSpec->boolean
                 ('run',
                  'run',
                  'Whether to run the ticker, ie. scroll across.',
                  1, # default yes
                  Glib::G_PARAM_READWRITE),

                 Glib::ParamSpec->double
                 ('speed',
                  'speed',
                  'Speed to move the items across, in pixels per second.',
                  0, DBL_MAX,
                  DEFAULT_SPEED,
                  Glib::G_PARAM_READWRITE),

                 Glib::ParamSpec->double
                 ('frame-rate',
                  'frame-rate',
                  'How many times per second to move for scrolling.',
                  0, DBL_MAX,
                  DEFAULT_FRAME_RATE,
                  Glib::G_PARAM_READWRITE),

                 Glib::ParamSpec->boolean
                 ('fixed-height-mode',
                  'fixed-height-mode',
                  'Assume all cells have the same desired height.',
                  0, # default no
                  Glib::G_PARAM_READWRITE),
                 ];

#------------------------------------------------------------------------------
# generic helpers

# Return a procedure to be called $func->($index,$width), designed to
# protect against every $index having a zero $width.
#
# It returns true until it sees $index==0 and then $index==0 a second time,
# with every call (every $index value) having $width==0.  The idea is that
# the ticker has looped around all the way from zero back to zero and seen
# every $width equal to zero then it should bail out.
#
# Any non-zero $width seen makes the returned procedure always return true.
# It might be just a single index position out of thousands, but that's
# enough.
#
sub _make_all_zeros_proc {
  my $seen_nonzero = 0;
  my $count_index_zero = 0;
  return sub {
    my ($index, $width) = @_;
    if ($width != 0) { $seen_nonzero = 1; }
    if ($index == 0) { $count_index_zero++; }
    return (! $seen_nonzero) && ($count_index_zero >= 2);
  }
}

# return a new Gtk2::Gdk::Rectangle which is the intersection of $rect with
# $region, or return undef if no intersection at all
sub _rect_intersect_region {
  my ($rect, $region) = @_;
  if ($region->rect_in ($rect) eq 'out') { return undef; }

  $region = $region->copy;
  $region->intersect (Gtk2::Gdk::Region->rectangle ($rect));
  return $region->get_clipbox;
}

# do a window clear on the given $region
sub _window_clear_region {
  my ($win, $region) = @_;
  foreach my $rect ($region->get_rectangles) {
    $win->clear_area ($rect->x, $rect->y, $rect->width, $rect->height);
  }
}

#------------------------------------------------------------------------------

# 'size_request' class closure
sub _do_size_request {
  my ($self, $req) = @_;
  if (DEBUG) { print "size_request $self\n"; }

  my $want_height = 0;
  if (my $model = $self->{'model'}) {
    if (my @cells = $self->GET_CELLS) {
      my $iter = $model->get_iter_first;
      if (DEBUG) { if (! $iter) { print "  model is empty\n"; } }
      while ($iter) {
        $self->_set_cell_data ($iter);
        foreach my $cell (@cells) {
          my (undef, undef, undef, $height) = $cell->get_size ($self, undef);
          $want_height = max ($want_height, $height);
        }
        if ($self->{'fixed_height_mode'}) {
          if (DEBUG) { print "  one row only for fixed-height-mode\n"; }
          last;
        }
        $iter = $model->iter_next ($iter);
      }
    }
  }
  if (DEBUG) { print "  decide height $want_height\n"; }

  $req->width (0);
  $req->height ($want_height);
}

#------------------------------------------------------------------------------
# scroll timer

sub _do_timer {
  my ($ref_weak_self) = @_;

  # shouldn't see an undef in our weak ref here, because the timer should be
  # stopped already by _do_unrealize in the course of widget destruction,
  # but if for some reason that hasn't happened then stop the timer now
  my $self = $$ref_weak_self or return 0; # stop timer

  # during a drag the timer still runs but we suppress motion
  if (! $self->is_drag_active) {
    my $step = $self->{'speed'} / $self->{'frame_rate'};
    $self->scroll_pixels ($step);
    if (DEBUG >= 2) { print "scroll $step, to ", $self->{'want_x'}, "\n"; }
  }
  return 1;  # continue timer
}

sub _stop_timer {
  my ($self) = @_;
  if (my $id = delete $self->{'timer_id'}) {
    if (DEBUG) { print "$self stop timer $id\n"; }
    Glib::Source->remove ($id);
  }
}

# start or stop the scroll timer according to the various settings
sub _update_timer {
  my ($self) = @_;

  my $want_timer = $self->{'run'}
    && $self->mapped
    && $self->{'visibility_state'} ne 'fully-obscured'
    && $self->{'cellinfo_list'}
    && @{$self->{'cellinfo_list'}}
    && $self->{'model'}
    && $self->{'model'}->get_iter_first;

  if (DEBUG) {
    print "$self run=", $self->{'run'},
      " mapped=",     $self->mapped ? 1 : 0,
      " visibility=", $self->{'visibility_state'},
      " model=",      $self->get('model') || '[none]',
      " nonempty=",   ! defined $self->{'model'} ? '(n/a)'
                      : $self->{'model'}->get_iter_first ? 'yes' : 'no',
      " --> want ", ($want_timer ? 'yes' : 'no'), "\n";
  }

  if ($want_timer) {
    $self->{'timer_id'} ||= do {
      my $period = 1000.0 / $self->get('frame-rate');
      my $weak_self = $self;
      Scalar::Util::weaken ($weak_self);
      if (DEBUG) { print "$self start timer\n"; }
      Glib::Timeout->add ($period, \&_do_timer, \$weak_self);
    };
  } else {
    _stop_timer ($self);
  }
}

#------------------------------------------------------------------------------

sub _do_expose_event {
  my ($self, $event) = @_;
  if (DEBUG >= 2) { print "$self expose ",$event->count,"\n"; }

  $self->{'scroll_deferred'} = 0;
  if ($self->{'scroll_in_progress'}) {
    # An Expose after our XCopyArea means we may have copied an invalidated
    # region.  An expose and a copy "crossing" like this is unlikely, so
    # just do a full redraw to ensure it's right.
    #
    # A GraphicsExpose from our XCopyArea would be normal and we could just
    # fill in the uncopied portions, but how to distinguish that?  Gdk seems
    # to lump both exposes together, or makes you block in
    # $win->get_graphics_expose (which seems imperfect too actually).
    # Currently a copy is only done when "unobscured", so there shouldn't be
    # a GraphicsExpose unless an obscuring window moved over just moments
    # before.
    #
    $self->{'scroll_in_progress'} = 0;
    $self->queue_draw;

  } else {
    my $region = $event->region;
    my $win = $self->window;
    if (defined $self->{'drawn_x'}
        && POSIX::floor ($self->{'drawn_x'})
           != POSIX::floor ($self->{'want_x'})) {
      # our desired position moved by a scroll (one not yet applied with a
      # copy or whatever), so clear and draw everything
      my ($win_width, $win_height) = $win->get_size;
      my $rect = Gtk2::Gdk::Rectangle->new (0, 0, $win_width, $win_height);
      if ($region->rect_in ($rect) ne 'in') {
        # the event specified region doesn't already cover whole window
        $region = Gtk2::Gdk::Region->rectangle ($rect);
        # $win->clear;
      }
    }
    _draw_region ($self, $region);
  }
  return 0; # propagate event
}

# return a new iter which is the last child under the given $iter
# $iter can be undef to get the last top-level
# if there's no children under $iter the return is undef
sub _model_iter_last_child {
  my ($model, $iter) = @_;
  my $nchildren = $model->iter_n_children ($iter);
  #  if ($nchildren == 0) { return undef; }
  return $model->iter_nth_child ($iter, $nchildren - 1);
}

# return a new iter which is the row preceding the given $iter, and at the
# same depth as $iter
# if $iter is the first element at its depth then the return is undef
sub _model_iter_prev {
  my ($model, $iter) = @_;
  my $path = $model->get_path ($iter);
  if ($path->prev) {
    return $model->get_iter ($path);
  } else {
    return _model_iter_last_child ($model, $model->iter_parent ($iter));
  }
}

sub _draw_region {
  my ($self, $region) = @_;

  my $model = $self->{'model'};
  if (! $model) {
    if (DEBUG) { print "$self no model to draw\n"; }
  CLEAR_REGION:
    _window_clear_region ($self->window, $region);
    return;
  }

  my $cellinfo_list = $self->{'cellinfo_list'};
  if (! @$cellinfo_list) {
    if (DEBUG) { print "$self no cell renderers to draw with\n"; }
    goto CLEAR_REGION;
  }
  my $x = POSIX::floor ($self->{'want_x'});

  # order the cells per their "pack_start" or "pack_end"
  $cellinfo_list = [ grep ({$_->{'pack'} eq 'start'} @$cellinfo_list),
                     reverse grep {$_->{'pack'} eq 'end'} @$cellinfo_list ];

  my $index = $self->{'want_index'};
  if ($index != $self->{'drawn_index'}) {
    if (DEBUG) { print "$self full region for different index: ",
                   " want $index drawn ", $self->{'drawn_index'}, "\n"; }
    my $win = $self->window;
    my ($win_width, $win_height) = $win->get_size;
    $region = Gtk2::Gdk::Region->rectangle
      (Gtk2::Gdk::Rectangle->new (0, 0, $win_width, $win_height));
  }

  # If a backwards scroll has moved the starting offset into the window,
  # ie. x>0, then decrement $index enough to be x<=0.
  #
  if ($x > 0) {
    if (DEBUG) { print "$self back up for negative scroll\n"; }

    my $all_zeros = _make_all_zeros_proc();
    do {
      $index--;
      if ($index < 0) {
        $index = max (0, $model->iter_n_children (undef) - 1);
      }
      my $iter = $model->iter_nth_child (undef, $index);
      if (! $iter) {
        # perhaps index left dodgy by new model
        $index = 0;
        $iter = $model->get_iter_first;
        if (! $iter) {
          if (DEBUG) { print "$self model has no rows\n"; }
          goto CLEAR_REGION;
        }
      }

      $self->_set_cell_data ($iter);
      my $total_width = 0;
      foreach my $cellinfo (@$cellinfo_list) {
        my $cell = $cellinfo->{'cell'};
        if (! $cell->get('visible')) { next; }
        my (undef, undef, $width, undef) = $cell->get_size ($self, undef);
        $total_width += $width;
      }
      if ($all_zeros->($index, $total_width)) {
        if (DEBUG) { print "$self all cell widths on all rows are zero\n"; }
        $self->{'want_x'} = $self->{'drawn_x'} = 0;
        goto CLEAR_REGION;
      }

      $x -= $total_width;
      $self->{'drawn_x'} = ($self->{'want_x'} -= $total_width);
    } while ($x > 0);

    $self->{'want_index'} = $index;
    $self->{'drawn_index'} = $index;
  }

  $self->{'drawn_index'} = $index;
  $self->{'drawn_x'} = $self->{'want_x'};

  my $iter = $model->iter_nth_child (undef, $index);
  if (! $iter) {
    # new model might have fewer rows, and perhaps no rows at all
    if (DEBUG) { print "$self nothing at index $index\n"; }
    $index = 0;
    $iter = $model->get_iter_first;
    if (! $iter) {
      if (DEBUG) { print "  model has no rows\n"; }
      goto CLEAR_REGION;
    }
  }

  # fresh check because working forwards now (could cross index==0 a second
  # time without seeing everything)
  my $all_zeros = _make_all_zeros_proc();

  my $ltor   = $self->get_direction eq 'ltr';
  my $win    = $self->window;
  my ($win_width, $win_height) = $win->get_size;

  # in this loop $index is maintained for two reasons,
  #   - to increment the stored $self->{'want_index'} while $x<=0 to step
  #     along for a positive scroll (ie. the whole of the current $index is
  #     off the left of the window)
  #   - to let &$all_zeros() notice when we've traversed the whole model
  #     without seeing a non-zero cell width
  # but $iter is stepped with $model->iter_next in case the model can "go to
  # next" more efficiently than doing a get_nth_child every time
  #
  while ($x < $win_width) {
    if ($x <= 0) {
      if (DEBUG >= 2) {
        print "$self advance to idx=$index/x=$x for positive scroll\n"; }
      $self->{'want_index'} = $self->{'drawn_index'} = $index;
      # preserve fraction part
      $self->{'want_x'} = $self->{'drawn_x'}
        = $x + $self->{'want_x'} - POSIX::floor ($self->{'want_x'});
    }
    my $total_width = 0;

    $self->_set_cell_data ($iter);
    foreach my $cellinfo (@$cellinfo_list) {
      my $cell = $cellinfo->{'cell'};
      if (! $cell->get('visible')) { next; }

      my ($x_offset, $y_offset, $width, $height)
        = $cell->get_size ($self, undef);

      if ($x + $width > 0) { # only once inside the window
        my $rect = $ltor
          ? Gtk2::Gdk::Rectangle->new ($x, 0, $width, $win_height)
            : Gtk2::Gdk::Rectangle->new ($win_width - 1 - $x - $width, 0,
                                         $width, $win_height);
        if (my $exposerect = _rect_intersect_region ($rect, $region)) {
          $win->clear_area ($exposerect->x, $exposerect->y,
                            $exposerect->width, $exposerect->height);
          $cell->render ($win, $self, $rect, $rect, $exposerect, []);
        }
      }
      $x += $width;
      $total_width += $width;
    }
    if ($all_zeros->($index, $total_width)) {
      if (DEBUG) { print "$self all cell widths on all rows are zero\n"; }
      $self->{'want_x'} = 0;
      goto CLEAR_REGION;
    }

    $index++;
    $iter = $model->iter_next ($iter);
    if (! $iter) {
      $index = 0;
      $iter = $model->get_iter_first;
      if (! $iter) {
        if (DEBUG) { print "$self  model has no rows\n"; }
        goto CLEAR_REGION;
      }
    }
  }
}

sub _scroll_idle_handler {
  my ($ref_weak_self) = @_;
  my $self = $$ref_weak_self;
  if (! $self) {
    # weak ref turned to undef -- we ought to have removed this handler in
    # FINALIZE_INSTANCE, so probably this shouldn't be reached
    if (DEBUG) {
      print "TickerView: oops, scroll idle called after weakened to undef\n"; }
    return 0;  # remove idle
  }
  if (DEBUG >= 2) { print "scroll idle\n"; }
  $self->{'scroll_idle_id'} = 0;  # once only, always removed

  if ($self->{'scroll_in_progress'}) {
    $self->{'scroll_deferred'} = 1;
    return 0;  # remove idle
  }
  if ($self->{'want_index'} != $self->{'drawn_index'}
      || $self->{'visibility_state'} ne 'unobscured'
      || ! defined $self->{'drawn_x'}) {
    $self->queue_draw;
    return 0;  # remove idle
  }

  my $win = $self->window;
  if (! $win) {
    # not realized, so nothing to draw
    return 0;  # remove idle
  }

  my $want_x  = POSIX::floor ($self->{'want_x'});
  my $drawn_x = POSIX::floor ($self->{'drawn_x'});
  if ($want_x == $drawn_x) {
    # already drawn in the right spot
    return 0;  # remove idle
  }

  my ($win_width, $win_height) = $win->get_size;
  my $redraw_x = 0;
  my $redraw_width = $win_width;

  my $step = $drawn_x - $want_x;
  if (abs ($step) >= $win_width) {
    # nothing in the window to be retained, use full redraw
    $self->queue_draw;
    return 0;  # remove idle
  }

  $self->{'drawn_x'} = $self->{'want_x'};

  # x measured off the right edge for right-to-left
  if ($self->get_direction eq 'rtl') {
    $want_x  = $win_width - 1 - $want_x;
    $drawn_x = $win_width - 1 - $drawn_x;
  }
  my $gc = ($self->{'copy_gc'} ||= do {
    Gtk2::GC->get ($win->get_depth, $win->get_colormap,
                   { graphics_exposures => 1 }) });
  
  if ($step < 0) {
    # moving to the right, gap at start
    $step = -$step;
    $win->draw_drawable ($gc,
                         $win, 0,0,  # src
                         $step,0,    # dst
                         $win_width-$step, $win_height);
    $redraw_x = 0;
  } elsif ($step > 0) {
    # moving to the left, gap at end
    $win->draw_drawable ($gc,
                         $win, $step,0,  # src
                         0,0,            # dst
                         $win_width-$step, $win_height);
    $redraw_x = $win_width - $step;
  }
  $redraw_width = $step;
  $self->{'scroll_in_progress'} = 1;

  if (DEBUG >= 2) { print "  draw $redraw_x width $redraw_width\n"; }
  # $win->clear;
  _draw_region ($self,
                Gtk2::Gdk::Region->rectangle
                (Gtk2::Gdk::Rectangle->new
                 ($redraw_x, 0, $redraw_width, $win_height)));

  return 0;  # remove idle
}

sub _ensure_scroll_idle {
  my ($self) = @_;
  if ($self->{'scroll_in_progress'}) {
    $self->{'scroll_deferred'} = 1;
    return;
  }
  $self->{'scroll_idle_id'} ||= do {
    my $weak_self = $self;
    Scalar::Util::weaken ($weak_self);
    Glib::Idle->add (\&_scroll_idle_handler, \$weak_self,
                     GTK_PRIORITY_REDRAW+1); # just below redraw
  };
}

sub _do_no_expose_event {
  my ($self, $event) = @_;
  if (DEBUG >= 2) { print "$self no expose",
                      ($self->{'scroll_deferred'} ? " pending scroll" : ""),
                      "\n"; }
  $self->{'scroll_in_progress'} = 0;
  if ($self->{'scroll_deferred'}) {
    $self->{'scroll_deferred'} = 0;
    _ensure_scroll_idle ($self);
  }
}

sub _scroll_to_pos {
  my ($self, $x, $index) = @_;
  if (DEBUG >= 2) { print "scroll to offset $x index $index inprog ",
                      $self->{'scroll_in_progress'}||0, "\n"; }

  my $prev_index = $self->{'want_index'};
  $self->{'want_index'} = $index;
  $self->{'want_x'} = $x;
  _ensure_scroll_idle ($self);
}

sub scroll_pixels {
  my ($self, $pixels) = @_;
  _scroll_to_pos ($self, $self->{'want_x'} - $pixels, $self->{'want_index'});
}

sub scroll_to_start {
  my ($self) = @_;
  _scroll_to_pos ($self, 0, 0);
}

sub is_drag_active {
  my ($self) = @_;
  return defined $self->{'drag_x'};
}

# 'visibility_notify_event' class closure
sub _do_visibility_notify_event {
  my ($self, $event) = @_;
  if (DEBUG) { print "$self visibility ",$event->state,"\n"; }
  $self->{'visibility_state'} = $event->state;
  _update_timer ($self);
  return $self->signal_chain_from_overridden ($event);
}

# 'button_press_event' class closure
sub _do_button_press_event {
  my ($self, $event) = @_;
  if ($event->button == 1) {
    $self->{'drag_x'} = $event->x;
  }
  return $self->signal_chain_from_overridden ($event);
}

sub _motion_notify_scroll {
  my ($self, $event) = @_;
  if ($self->is_drag_active) {
    my $x = $event->x;
    my $step = $self->{'drag_x'} - $x;
    if ($self->get_direction eq 'rtl') { $step = -$step; }
    $self->scroll_pixels ($step);
    $self->{'drag_x'} = $x;
  }
}

# 'motion_notify_event' class closure
sub _do_motion_notify_event {
  my ($self, $event) = @_;
  _motion_notify_scroll ($self, $event);
  return $self->signal_chain_from_overridden ($event);
}

# 'button_release_event' class closure
sub _do_button_release_event {
  my ($self, $event) = @_;
  if ($event->button == 1) {
    # final dragged position from X,Y in this release event
    _motion_notify_scroll ($self, $event);
    delete $self->{'drag_x'};
  }
  return $self->signal_chain_from_overridden ($event);
}

# 'map' class closure
sub _do_map {
  my ($self) = @_;
  $self->signal_chain_from_overridden;
  _update_timer ($self);
}

# 'direction_changed' class closure.

# There's no chain_from_overridden here because the GtkWidget code (as of
# gtk 2.12) in gtk_widget_direction_changed() does a queue_resize, which we
# don't need or want.
sub _do_direction_changed {
  my ($self, $prev_dir) = @_;
  $self->queue_draw;
}

sub _do_row_changed {
  my ($model, $path, $iter, $ref_weak_self) = @_;
  my $self = $$ref_weak_self or return;
  $self->queue_draw;
}

sub _do_row_inserted {
  my ($model, $ins_path, $ins_iter, $ref_weak_self) = @_;
  my $self = $$ref_weak_self or return;

  # if inserted before current then advance
  my ($ins_index) = $ins_path->get_indices;
  if ($ins_index <= $self->{'want_index'}) {
    $self->{'want_index'}++;
    if (DEBUG) { print "row_inserted at or before, move index to ",
                   $self->{'want_index'},"\n"; }
  }

  my $newly_nonempty = ! $model->iter_nth_child (undef, 1);
  if ($newly_nonempty) {
    _update_timer ($self);
  }
  if ($newly_nonempty || ! $self->{'fixed_height_mode'}) {
    # become non-empty, or any new row when every row checked for size
    $self->queue_resize;
  }
  $self->queue_draw;
}

sub _do_row_deleted {
  my ($model, $del_path, $ref_weak_self) = @_;
  my $self = $$ref_weak_self or return;
  if (DEBUG) { print "row_deleted, current index ",$self->{'want_index'},"\n";}

  # if deleted before current then decrement
  my ($del_index) = $del_path->get_indices;
  if ($del_index < $self->{'want_index'}) {
    $self->{'want_index'}--;
    if (DEBUG) { print " delete $del_index is before, move to ",
                   $self->{'want_index'},"\n"; }
  }

  my $empty = ! $model->get_iter_first;
  if ($empty) {
    _update_timer ($self);
  }
  if ($empty || ! $self->{'fixed_height_mode'}) {
    # become empty, or any delete when every row checked for size
    $self->queue_resize;
  }
  $self->queue_draw;
}

sub _do_rows_reordered {
  my ($model, $reordered_path, $reordered_iter, $aref, $ref_weak_self) = @_;
  my $self = $$ref_weak_self or return;

  # follow start to new index
  $self->{'want_index'} = ($aref->[$self->{'want_index'}] || 0);
  # pessimistic assumption that rest may be different and hence need redraw
  $self->queue_draw;
}

sub SET_PROPERTY {
  my ($self, $pspec, $newval) = @_;
  my $pname = $pspec->get_name;
  my $oldval = $self->{$pname};
  $self->{$pname} = $newval;  # per default GET_PROPERTY
  if (DEBUG) { print "$self set $pname  ",
                 defined $newval ? $newval : '[undef]', "\n"; }

  if ($pname eq 'model' && ($oldval||0) != ($newval||0)) {
    my $model = $newval;

    $self->{'model_ids'} = $model && do {
      my $weak_self = $self;
      Scalar::Util::weaken ($weak_self);
      my $ref_weak_self = \$weak_self;

      require Glib::Ex::SignalIds;
      Glib::Ex::SignalIds->new
          ($model,
           $model->signal_connect(row_changed    => \&_do_row_changed,
                                  $ref_weak_self),
           $model->signal_connect(row_inserted   => \&_do_row_inserted,
                                  $ref_weak_self),
           $model->signal_connect(row_deleted    => \&_do_row_deleted,
                                  $ref_weak_self),
           $model->signal_connect(rows_reordered => \&_do_rows_reordered,
                                  $ref_weak_self))
        };

    $self->queue_resize;
  }

  if ($pname eq 'fixed_height_mode' && $oldval && ! $newval) {
    # Resize when turning fixed height mode "off" so as to have a look at
    # all the rows beyond the first.
    #
    # Don't resize when turning fixed height mode "on"; the size we have is
    # based on all rows and assuming the first row is truely representative
    # then its size is the same as what we've got already.
    #
    $self->queue_resize;
  }

  if ($pname eq 'model' || $pname eq 'run' || $pname eq 'frame_rate') {
    if ($pname eq 'frame_rate') {
      _stop_timer ($self);  # ready for new period
    }
    _update_timer ($self);
  }
  $self->queue_draw;
}

sub INIT_INSTANCE {
  my ($self) = @_;
  $self->set_double_buffered (0);
  $self->{'want_index'}  = 0;
  $self->{'want_x'}      = 0;
  $self->{'drawn_index'} = -1;
  $self->{'visibility_state'}  = 'initial';
  $self->{'run'}               = 1; # default yes
  $self->{'frame_rate'}        = DEFAULT_FRAME_RATE;
  $self->{'speed'}             = DEFAULT_SPEED;
  $self->{'fixed_height_mode'} = 0; # default no
  
  $self->add_events (['visibility-notify-mask',
                      'button-press-mask',
                      'button-motion-mask',
                      'button-release-mask']);
  _update_timer ($self);
}

# 'unmap' class closure.
sub _do_unmap {
  my ($self) = @_;
  if (DEBUG) { print "$self unmap\n"; }
  # chain before _update_timer(), so the GtkWidget code clears the mapped flag
  $self->signal_chain_from_overridden;
  _update_timer ($self);
}

# 'unrealize' class closure
# When removed from a container we only get unrealize, not unmap then
# unrealize, hence an _update_timer check here as well as in _do_unmap().
sub _do_unrealize {
  my ($self) = @_;
  # might need a different depth gc on new window
  if (my $gc = delete $self->{'copy_gc'}) {
    if (DEBUG) { print "$self unrealize release $gc\n"; }
    Gtk2::GC->release ($gc);
  }
  # chain before _update_timer(), so the GtkWidget code clears the mapped flag
  $self->signal_chain_from_overridden;
  _update_timer ($self);
}

sub FINALIZE_INSTANCE {
  my ($self) = @_;
  if (my $id = delete $self->{'scroll_idle_id'}) {
    Glib::Source->remove ($id);
  }
}

1;
__END__

=head1 NAME

Gtk2::Ex::TickerView -- scrolling ticker display widget

=head1 SYNOPSIS

 use Gtk2::Ex::TickerView;
 my $ticker = Gtk2::Ex::TickerView->new (model => $model,
                                         ...);
 my $renderer = Gtk2::CellRendererText->new;
 $ticker->pack_start ($renderer, 0);
 $ticker->set_attributes ($renderer, text => 0);  # column

=head1 WIDGET HIERARCHY

C<Gtk2::Ex::TickerView> is a subclass of C<Gtk2::DrawingArea>, but that
might change so it's recommended you rely only on C<Gtk2::Widget>.

    Gtk2::Widget
      Gtk2::DrawingArea
        Gtk2::Ex::TickerView

The interfaces implemented are:

    Gtk2::Buildable
    Gtk2::CellLayout

=head1 DESCRIPTION

A C<Gtk2::Ex::TickerView> widget displays items from a C<Gtk2::TreeModel>
scrolling horizontally across the window, like a news bar or stock ticker.

    +----------------------------------------------------------+
    | st item  * The second item  * The third item   * The fou |
    +----------------------------------------------------------+
        <---- scrolling

Items are drawn using one or more C<Gtk2::CellRenderer> objects set into the
TickerView as per the C<Gtk2::CellLayout> interface.  For scrolling text for
example you can use C<Gtk2::CellRendererText>.

If two or more renderers are set then they're drawn one after the other for
each item, ie. row of the model.  For example you could have a
C<Gtk2::CellRendererPixbuf> to draw an icon then a C<Gtk2::CellRendererText>
to draw some text and they scroll across together.  The icon could use the
model's data, or be just a fixed image to go before every item.

The display and scrolling direction follow the text C<set_direction> (see
L<Gtk2::Widget>).  For C<ltr> mode item 0 starts at the left of the window
and items scroll to the left.  For C<rtl> item 0 starts at the right of the
window and items scroll to the right.

    +----------------------------------------------------------+
    | m five  * item four  * item three  * item two  * item on |
    +----------------------------------------------------------+
                        right to left mode, scrolling ----->

Any text or drawing direction within each cell is a matter for the
renderers.  For example in C<Gtk2::CellRendererText> Pango recognises
right-to-left scripts such as Arabic based on the characters and shouldn't
need any special setups.

Currently only a list style model is expected, meaning only a single level,
and only that topmost level of the model is drawn.  So for example a
C<Gtk2::ListStore> suits.  Perhaps in the future something will be done to
descend into and draw subrows too.

The whole Gtk model/view/layout/renderer/attributes as used here is
ridiculously complicated.  Its power comes when showing a big updating list
or wanting customized drawing, but the amount of code to get something on
the screen is not nice.  Have a look at "Tree and List Widget Overview" in
the Gtk reference manual if you haven't already.  Then F<examples/simple.pl>
in the TickerView sources is more or less the minimum to actually display
something.

=head1 FUNCTIONS

=over 4

=item C<< Gtk2::Ex::TickerView->new (key => value, ...) >>

Create and return a new C<Gtk2::Ex::TickerView> widget.  Optional key/value
pairs set initial properties as per C<< Glib::Object->new >> (see
L<Glib::Object>).

=item C<< $ticker->scroll_pixels ($n) >>

Scroll the ticker contents across by C<$n> pixels.  Postive C<$n> moves in
the normal scrolled direction or a negative value goes backwards.

C<$n> doesn't have to be an integer, the display position is maintained as a
floating point value, allowing fractional amounts to accumulate until a
whole pixel step is reached.

=item C<< $ticker->scroll_to_start () >>

Scroll the ticker contents back to the start, ie. to show the first row in
the model at the left edge of the display (or right for C<rtl>).

=back

=head1 OBJECT PROPERTIES

=over 4

=item C<model> (object implementing C<Gtk2::TreeModel>, default undef)

This is any C<Glib::Object> implementing the C<Gtk2::TreeModel> interface,
for example a C<Gtk2::ListStore>.  It supplies the data to be displayed.
Until this is set the ticker is blank.

=item C<run> (boolean, default true)

Whether to run the ticker, ie. to scroll it across the screen under a timer.
If false then the ticker just draws the items at its current position,
without moving (except by the programatic scroll functions above, or user
dragging with mouse button 1).

=item C<speed> (floating point pixels per second, default 25)

The speed the items scroll across, in pixels per second.

=item C<frame-rate> (floating point frames per second, default 4)

The number of times each second the ticker moves and redraws.  (Each move is
C<speed> divided by C<frame-rate> many pixels.)

=item C<fixed-height-mode> (boolean, default false)

If true then assume all rows in the model have the same height.  This means
the ticker can ask its renderers about just one row from the model, instead
of going through all of them.  If the model is big this is much faster.

=back

The text direction giving the display order and scrolling is not a property
but instead accessed with the usual widget C<get_direction> and
C<set_direction> methods (see L<Gtk2::Widget>).

The C<visible> property in each cell renderer is recognised and a renderer
that's not visible is skipped and takes no space.  Each C<visible> can be
set globally in the renderer to suppress it entirely, or controlled with the
attributes mechanism or data setup func to suppress it just for selected
items from the model.

=head1 BUILDABLE

C<Gtk2::Ex::TickerView> implements the C<Gtk2::Buildable> interface,
allowing C<Gtk2::Builder> to construct a TickerView.  The class name is
C<Gtk2__Ex__TickerView> and renderers and attributes are added as children
per C<Gtk2::CellLayout>.  Here's a sample, or see F<examples/builder.pl> in
the TickerView sources for a complete program,

    <object class="Gtk2__Ex__TickerView" id="myticker">
      <property name="model">myliststore</property>
      <child>
        <object class="GtkCellRendererText" id="myrenderer">
          <property name="xpad">10</property>
        </object>
        <attributes>
          <attribute name="text">0</attribute>
        </attributes>
      </child>
    </object>

=head1 OTHER NOTES

Mouse button 1 is setup for the user to drag the display back and forwards.
This is good to go back and see something that's just moved off the edge, or
to skip past boring bits.  Perhaps in the future the button used will be
customizable.

Some care is taken drawing for scrolling.  If unobscured then scrolling uses
"CopyArea" and a draw of just the balance at the end.  To avoid hammering
the X server any further scroll waits until hearing from the server that the
last completed (a NoExpose event normally).  If partly obscured then alas
plain full redraws must be used, though working progressively by cell to
reduce flashing.  Avoiding hammering is left up to C<queue_draw> in that
case.  The effect of all this is probably only noticeable if your
C<frame-rate> is a bit too high or the server is lagged by other operations.

Scroll moves are always spun through an idle handler too, at roughly
C<GDK_PRIORITY_REDRAW> priority since they represent redraws, the aim being
to collapse multiple MotionNotify events from a user drag, or multiple
programmatic C<scroll_pixels> calls from slack application code.

The Gtk reference documentation for C<GtkCellLayout> doesn't describe
exactly how C<pack_start> and C<pack_end> order cells, but it's the same as
C<GtkBox> and a description can be found there.  Basically each cell is
noted as "start" or "end", the starts are drawn from the left, the ends are
drawn from the right.  In a TickerView the ends immediately follow the
starts, there's no gap in between, unlike say in a C<Gtk2::HBox>.  See
F<examples/order.pl> for a demonstration.

When the model has no rows the TickerView has a desired height of zero.
This is a pest if you want a visible but blank area when there's nothing to
display.  But there's no way the TickerView can work out a height when it's
got no data at all to set the renderers.  You might like to try calculating
a fixed size from a dummy model and use C<set_size_request> to force the
height, or alternately have a "no data" row in the model instead of letting
it go empty, or even switch to a dummy model with a "no data" row when the
real one is empty.

=head1 SEE ALSO

L<Gtk2::CellLayout>, L<Gtk2::TreeModel>, L<Gtk2::CellRenderer>,
L<Gtk2::Ex::CellLayout::Base>

=head1 HOME PAGE

L<http://www.geocities.com/user42_kevin/gtk2-ex-tickerview/index.html>

=head1 LICENSE

Copyright 2007, 2008 Kevin Ryde

Gtk2-Ex-TickerView is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any later
version.

Gtk2-Ex-TickerView is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
more details.

You should have received a copy of the GNU General Public License along with
Gtk2-Ex-TickerView.  If not, see L<http://www.gnu.org/licenses/>.

=cut
