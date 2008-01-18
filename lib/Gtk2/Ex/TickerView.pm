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
# with this program.  If not, see <http://www.gnu.org/licenses/>.

package Gtk2::Ex::TickerView;
use strict;
use warnings;
use Carp;
use Glib;
use Gtk2;
use List::Util qw(min max);
use POSIX qw(FLT_MAX);

our $VERSION = 1;

use constant {
  DEFAULT_FRAME_RATE => 4,     # times per second
  DEFAULT_SPEED      => 30,    # pixels per second
};

# not wrapped in gtk2-perl version 1.161
use constant GTK_PRIORITY_REDRAW => (Glib::G_PRIORITY_HIGH_IDLE + 20);

# set this to 1 for some diagnostic prints, or 2 for even more prints
use constant DEBUG => 0;

use Glib::Object::Subclass
  Gtk2::DrawingArea::,
  signals => { expose_event            => \&do_expose_event,
               no_expose_event         => \&do_no_expose_event,
               size_request            => \&do_size_request,
               button_press_event      => \&do_button_press_event,
               motion_notify_event     => \&do_motion_notify_event,
               button_release_event    => \&do_button_release_event,
               visibility_notify_event => \&do_visibility_notify_event,
               direction_changed       => \&do_direction_changed,
               map                     => \&do_map,
               unmap                   => \&do_unmap,
               unrealize               => \&do_unrealize,
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

                 Glib::ParamSpec->float
                 ('speed',
                  'speed',
                  'Speed to move the items across, in pixels per second.',
                  0, FLT_MAX,
                  DEFAULT_SPEED,
                  Glib::G_PARAM_READWRITE),

                 Glib::ParamSpec->float
                 ('frame-rate',
                  'frame-rate',
                  'How many times per second to move for scrolling.',
                  0, FLT_MAX,
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

# return a procedure to be called &$func($index,$width), it returns true
# until/unless it sees that all $index values have $width==0
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

  # get_clipbox is buggy in Gtk2-Perl 1.161
  my ($ret, @rects) = $region->get_rectangles;
  foreach (@rects) { $ret = $ret->union ($_); }
  return $ret;
}

#------------------------------------------------------------------------------
# CellLayout-compatible interface

sub _find_cellinfo {
  my ($self, $cell) = @_;
  $self->{'cellinfo_list'} ||= [];
  return List::Util::first {$_->{'cell'} == $cell} @{$self->{'cellinfo_list'}};
}

sub _get_cellinfo {
  my ($self, $cell) = @_;
  return (_find_cellinfo ($self, $cell)
          || croak "no such CellRenderer packed into this TickerView");
}

sub _cellinfo_setup {
  my ($self, $cellinfo, $model, $iter) = @_;
  my $cell = $cellinfo->{'cell'};

  if (my $func = $cellinfo->{'data_func'}) {
    &$func ($self, $cell, $model, $iter, $cellinfo->{'data_func_data'});
  } else {
    my $ahash = $cellinfo->{'attributes'} || {};
    if (%$ahash) {
      $cell->set (map {($_, $model->get_value ($iter, $ahash->{$_}))}
                  keys %$ahash);
    }
  }
  return $cell;
}

# gtk_cell_layout_add_attribute
sub add_attribute {
  my ($self, $cell, $attribute, $column) = @_;
  my $cellinfo = _get_cellinfo ($self, $cell);
  $cellinfo->{'attributes'}->{$attribute} = $column;
  $self->queue_resize;
  $self->queue_draw;
}

# gtk_cell_layout_set_attributes
sub set_attributes {
  my ($self, $cell, %ahash) = @_;
  my $cellinfoinfo = _get_cellinfo ($self, $cell);
  $cellinfoinfo->{'attributes'} = \%ahash;
  $self->queue_resize;
  $self->queue_draw;
}

# gtk_cell_layout_set_cell_data_func
sub set_cell_data_func {
  my ($self, $cell, $func, $funcdata) = @_;
  my $cellinfo = _get_cellinfo ($self, $cell);
  $cellinfo->{'data_func'} = $func;
  $cellinfo->{'data_func_data'} = $funcdata;
  $self->queue_resize;
  $self->queue_draw;
}

# gtk_cell_layout_get_cells
sub get_cells {
  my ($self) = @_;
  return map {$_->{'cell'}} @{$self->{'cellinfo_list'}};
}

# gtk_cell_layout_clear
sub clear {
  my ($self) = @_;
  $self->{'cellinfo_list'} = [];
  _update_timer ($self);
  $self->queue_resize;
  $self->queue_draw;
}

# gtk_cell_layout_clear_attributes
sub clear_attributes {
  my ($self, $cell) = @_;
  my $cellinfo =  _get_cellinfo ($self, $cell);
  $cellinfo->{'attributes'} = {};
  $self->queue_resize;
  $self->queue_draw;
}

# gtk_cell_layout_pack_start
sub pack_start {
  my ($self, $cell, $expand) = @_;
  if (_find_cellinfo ($self, $cell)) {
    croak "this CellRenderer already packed into this TickerView";
  }
  push @{$self->{'cellinfo_list'}}, { cell => $cell };
  _update_timer ($self);
  $self->queue_resize;
  $self->queue_draw;
}

# gtk_cell_layout_pack_end
sub pack_end {
  my ($self, $cell, $expand) = @_;
  if (_find_cellinfo ($self, $cell)) {
    croak "this CellRenderer already packed into this TickerView";
  }
  unshift @{$self->{'cell_list'}}, { cell => $cell };
  _update_timer ($self);
  $self->queue_resize;
  $self->queue_draw;
}

# gtk_cell_layout_reorder
sub reorder {
  my ($self, $cell, $position) = @_;
  my $cellinfo_list = $self->{'cellinfo_list'};
  foreach my $i (0 .. $#$cellinfo_list) {
    if ($cellinfo_list->[$i]->{'cell'} == $cell) {
      if ($i == $position) {
        # already in the right position, do nothing
        return;
      }      
      my $cellinfo = splice @$cellinfo_list, $i, 1;
      splice @$cellinfo_list, $position, 1, $cellinfo;
      $self->queue_draw;
      return;
    }
  }
  croak "cell renderer not in this layout";
}

#------------------------------------------------------------------------------

# 'size_request' class closure
sub do_size_request {
  my ($self, $req) = @_;
  if (DEBUG) { print $self->get_name, " size_request\n"; }

  my $want_height = 0;
  if (my $model = $self->{'model'}) {
    my $cellinfo_list = $self->{'cellinfo_list'};
    if (@$cellinfo_list) {
      my $rows = $model->iter_n_children (undef);
      if ($self->{'fixed-height-mode'}) {
        $rows = min ($rows, 1); # look just at first element
      }
      foreach my $index (0 .. $rows-1) {  # possibly no rows at all
        my $iter = $model->iter_nth_child (undef, $index);
        foreach my $cellinfo (@$cellinfo_list) {
          my $cell = _cellinfo_setup ($self, $cellinfo, $model, $iter);
          my ($x_offset, $y_offset, $width, $height)
            = $cell->get_size ($self, undef);
          $want_height = max ($want_height, $height);
        }
      }
    }
  }
  if (DEBUG) { print "  decide $want_height\n"; }

  $req->width (0);
  $req->height ($want_height);
}

#------------------------------------------------------------------------------
# scroll timer

sub do_timer {
  my ($ref_self) = @_;
  my $self = $$ref_self;
  if (! $self) { return 0; }  # if weakened gone then stop timer

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
    if (DEBUG) { print $self->get_name," stop timer $id\n"; }
    Glib::Source->remove ($id);
  }
}

# start or stop the scroll timer according to the various settings
sub _update_timer {
  my ($self) = @_;

  my $want_timer = $self->{'run'}
    && $self->mapped
    && $self->{'visibility_state'} ne 'fully-obscured'
    && @{$self->{'cellinfo_list'}}
    && $self->{'model'}
    && $self->{'model'}->iter_n_children (undef);

  if (DEBUG) {
    print $self->get_name, " run ", $self->{'run'},
      " mapped=",     $self->mapped ? 1 : 0,
      " visibility=", $self->{'visibility_state'},
      " model=",      $self->get('model') || '(none)',
      " rows=",       $self->get('model')
                      ? $self->get('model')->iter_n_children(undef) : 'n/a',
      " --> want ", ($want_timer ? 'yes' : 'no'), "\n";
  }

  if ($want_timer) {
    $self->{'timer_id'} ||= do {
      my $period = 1000.0 / $self->get('frame-rate');
      my $weak_self = $self;
      Scalar::Util::weaken ($weak_self);
      if (DEBUG) { print $self->get_name," start timer\n"; }
      Glib::Timeout->add ($period,\&do_timer,\$weak_self);
    };
  } else {
    _stop_timer ($self);
  }
}

#------------------------------------------------------------------------------

sub do_expose_event {
  my ($self, $event) = @_;
  if (DEBUG >= 2) { print $self->get_name, " expose ",$event->count,"\n"; }

  $self->{'scroll_deferred'} = 0;
  if ($self->{'scroll_in_progress'}) {
    # An Expose after our XCopyArea means we may have copied an invalidated
    # region, redraw everything.
    #
    # A GraphicsExpose from our XCopyArea would be normal and we could just
    # fill in the uncopied portions, but how to distinguish that?  Gdk seems
    # to lump both exposes together.  Currently copy is only done when
    # "unobscured", so for now there shouldn't be a GraphicsExpose unless an
    # obscuring window moved over it just moments before.
    #
    $self->{'scroll_in_progress'} = 0;
    $self->queue_draw;

  } else {
    my $region = $event->region;
    if (defined $self->{'drawn_x'}
        && POSIX::floor ($self->{'drawn_x'}) 
           != POSIX::floor ($self->{'want_x'})) {
      # our desired position has moved by a scroll, so clear and draw everything
      my $win = $self->window;
      my ($win_width, $win_height) = $win->get_size;
      my $rect = Gtk2::Gdk::Rectangle->new (0, 0, $win_width, $win_height);
      if ($region->rect_in ($rect) ne 'in') {
        # the event specified region doesn't already cover whole window
        $win->clear_area (0, 0, $win_width, $win_height);
        $region = Gtk2::Gdk::Region->rectangle ($rect);
      }
    }
    _draw_region ($self, $region);
  }
  return 0; # propagate event
}

sub _draw_region {
  my ($self, $region) = @_;

  my $model = $self->{'model'};
  if (! $model) {
    if (DEBUG) { print $self->get_name," no model to draw\n"; }
    return;
  }
  my $rows = $model->iter_n_children (undef);
  if ($rows == 0) {
    if (DEBUG) { print $self->get_name," model has no rows\n"; }
    return;
  }
  my $cellinfo_list = $self->{'cellinfo_list'};
  if (! @$cellinfo_list) {
    if (DEBUG) { print $self->get_name," no cell renderers to draw with\n"; }
    return;
  }

  my $ltor   = $self->get_direction eq 'ltr';
  my $win    = $self->window;
  my ($win_width, $win_height) = $win->get_size;
  my $x      = POSIX::floor ($self->{'want_x'});

  my $index = $self->{'want_index'};
  # new model might have fewer rows
  if ($index >= $rows) { $index = $self->{'want_index'} = $rows-1; }

  if ($index != $self->{'drawn_index'}) {
    $region = Gtk2::Gdk::Region->rectangle
      (Gtk2::Gdk::Rectangle->new (0, 0, $win_width, $win_height));
  }
  $self->{'drawn_index'} = $index;
  $self->{'drawn_x'} = $self->{'want_x'};

  # If a backwards scroll has moved the starting offset into the window,
  # ie. x>0, then decrement $index enough to be x<=0.
  #
  my $all_zeros = _make_all_zeros_proc();
  while ($x > 0) {
    $index--;
    if ($index < 0) { $index = $rows-1; }
    my $total_width = 0;
    my $iter = $model->iter_nth_child (undef, $index);

    foreach my $cellinfo (@$cellinfo_list) {
      my $cell = _cellinfo_setup ($self, $cellinfo, $model, $iter);
      if (! $cell->get('visible')) { next; }
      my ($x_offset, $y_offset, $width, $height)
        = $cell->get_size ($self, undef);
      $total_width += $width;
    }
    if (&$all_zeros ($index, $total_width)) {
      if (DEBUG) { print $self->get_name," all cell widths on all rows are zero\n"; }
      $self->{'want_x'} = 0;
      return;
    }

    $x -= $total_width;
    $self->{'want_x'} -= $total_width;
    $self->{'want_index'} = $index;
    $self->{'drawn_x'} = $self->{'want_x'};
    $self->{'drawn_index'} = $index;
  }

  # fresh check because working forwards now (could cross index==0 a second
  # time without seeing everything)
  $all_zeros = _make_all_zeros_proc();

  while ($x < $win_width) {
    if ($x <= 0) {
      $self->{'want_index'} = $index;
      $self->{'drawn_index'} = $index;
      $self->{'want_x'} = $self->{'want_x'} - POSIX::floor ($self->{'want_x'})
                            + $x;
      $self->{'drawn_x'} = $self->{'want_x'};
    }
    my $total_width = 0;
    my $iter = $model->iter_nth_child (undef, $index);

    foreach my $cellinfo (@$cellinfo_list) {
      my $cell = _cellinfo_setup ($self, $cellinfo, $model, $iter);
      if (! $cell->get('visible')) { next; }

      my ($x_offset, $y_offset, $width, $height)
        = $cell->get_size ($self, undef);

      if ($x + $width > 0) { # only once inside the window
        my $rect = $ltor
          ? Gtk2::Gdk::Rectangle->new ($x, 0,
                                       $width, $win_height)
          : Gtk2::Gdk::Rectangle->new ($win_width - 1 - $x - $width, 0,
                                       $width, $win_height);
        if (my $exposerect = _rect_intersect_region ($rect, $region)) {
          $cell->render ($win, $self, $rect, $rect, $exposerect, []);
        }
      }
      $x += $width;
      $total_width += $width;
    }
    if (&$all_zeros ($index, $total_width)) {
      if (DEBUG) { print $self->get_name," all cell widths on all rows are zero\n"; }
      $self->{'want_x'} = 0;
      last;  # avoid infinite loop!
    }
    $index++;
    if ($index >= $rows) { $index = 0; }
  }
}

sub _scroll_idle_handler {
  my ($ref_self) = @_;
  my $self = $$ref_self;
  if (! $self) {
    # weak ref turned to undef -- we ought to have removed this handler in
    # FINALIZE_INSTANCE, so probably this shouldn't be reached
    if (DEBUG) { print "oops, scroll idle called after weakened to undef\n"; }
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
  $win->clear_area ($redraw_x, 0, $redraw_width, $win_height);
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

sub do_no_expose_event {
  my ($self, $event) = @_;
  if (DEBUG >= 2) { print $self->get_name," no expose",
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
sub do_visibility_notify_event {
  my ($self, $event) = @_;
  if (DEBUG) { print $self->get_name," visibility ",$event->state,"\n"; }
  $self->{'visibility_state'} = $event->state;
  _update_timer ($self);
  return $self->signal_chain_from_overridden ($event);
}

# 'button_press_event' class closure
sub do_button_press_event {
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
sub do_motion_notify_event {
  my ($self, $event) = @_;
  _motion_notify_scroll ($self, $event);
  return $self->signal_chain_from_overridden ($event);
}

# 'button_release_event' class closure
sub do_button_release_event {
  my ($self, $event) = @_;
  if ($event->button == 1) {
    # final dragged position from X,Y in this release event
    _motion_notify_scroll ($self, $event);
    delete $self->{'drag_x'};
  }
  return $self->signal_chain_from_overridden ($event);
}

# 'map' class closure
sub do_map {
  my ($self) = @_;
  $self->signal_chain_from_overridden;
  _update_timer ($self);
}

# 'direction_changed' class closure.

# There's no chain_from_overridden here because the GtkWidget code (as of
# gtk 2.12) in gtk_widget_direction_changed() does a queue_resize, which we
# don't need or want.
sub do_direction_changed {
  my ($self, $prev_dir) = @_;
  $self->queue_draw;
}

sub do_row_changed {
  my ($model, $path, $iter, $ref_self) = @_;
  my $self = $$ref_self;
  $self->queue_draw;
}

sub do_row_inserted {
  my ($model, $ins_path, $ins_iter, $ref_self) = @_;
  my $self = $$ref_self;
  my $rows = $model->iter_n_children (undef);

  # if inserted before current then advance
  my ($ins_index) = $ins_path->get_indices;
  if ($ins_index <= $self->{'want_index'}) {
    $self->{'want_index'}++;
    if ($self->{'want_index'} > $rows) {
      $self->{'want_index'} = 0;
    }
    if (DEBUG) { print "row_inserted at or before, move index to ",
                   $self->{'want_index'},"\n"; }
  }

  if ($rows == 1) {
    # become non-empty
    _update_timer ($self);
  }

  if ($rows == 1 || ! $self->{'fixed-height-mode'}) {
    # become non-empty, or any new row when every row checked for size
    $self->queue_resize;
  }
  $self->queue_draw;
}

sub do_row_deleted {
  my ($model, $del_path, $ref_self) = @_;
  my $self = $$ref_self;
  if (DEBUG) { print "row_deleted, current index ",$self->{'want_index'},"\n";}
  my $rows = $model->iter_n_children (undef);

  # if deleted before current then decrement
  my ($del_index) = $del_path->get_indices;
  if ($del_index < $self->{'want_index'}) {
    if (--$self->{'want_index'} < 0) {
      $self->goto_index (max (0, $rows - 1));
    }
    if (DEBUG) { print " delete $del_index is before, move to ",
                   $self->{'want_index'},"\n"; }
  }

  if ($rows == 0) {
    # become empty
    _update_timer ($self);
  }

  if ($rows == 0 || ! $self->{'fixed-height-mode'}) {
    # become empty, or any delete when every row checked for size
    $self->queue_resize;
  }
  $self->queue_draw;
}

sub do_rows_reordered {
  my ($model, $reordered_path, $reordered_iter, $aref, $ref_self) = @_;
  my $self = $$ref_self;

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
  if (DEBUG) { print $self->get_name," set $pname  $newval\n"; }

  if ($pname eq 'model' && ($oldval||0) != ($newval||0)) {
    my $old_model = $oldval;
    foreach my $id (@{$self->{'model_ids'}}) {
      $old_model->signal_handler_disconnect ($id);
    }
    my $model = $newval;
    my $weak_self = $self;
    Scalar::Util::weaken ($weak_self);
    my $ref_self = \$weak_self;
    $self->{'model_ids'} =
      [$model->signal_connect('row-changed',   \&do_row_changed,   $ref_self),
       $model->signal_connect('row-inserted',  \&do_row_inserted,  $ref_self),
       $model->signal_connect('row-deleted',   \&do_row_deleted,   $ref_self),
       $model->signal_connect('rows-reordered',\&do_rows_reordered,$ref_self)
      ];
  }
  if (($pname eq 'model' || $pname eq 'fixed_height_mode')
      && ($oldval||0) != ($newval||0)) {
    $self->queue_resize;
  }
  if ($pname eq 'model' || $pname eq 'run' || $pname eq 'frame_rate') {
    if ($pname eq 'frame_rate') { _stop_timer ($self); }  # new period
    _update_timer ($self);
  }
  $self->queue_draw;
}

sub INIT_INSTANCE {
  my ($self) = @_;

  $self->{'want_index'}  = 0;
  $self->{'want_x'}      = 0;
  $self->{'drawn_index'} = -1;
  $self->{'visibility_state'} = 'initial';
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
sub do_unmap {
  my ($self) = @_;
  if (DEBUG) { print $self->get_name," unmap\n"; }
  # chain before _update_timer(), so the GtkWidget code clears the mapped flag
  $self->signal_chain_from_overridden;
  _update_timer ($self);
}

# 'unrealize' class closure
# When removed from a container we only get unrealize, not unmap then
# unrealize, hence an _update_timer check here as well as in do_unmap().
sub do_unrealize {
  my ($self) = @_;
  # might need a different depth gc on new window
  if (my $gc = delete $self->{'copy_gc'}) {
    if (DEBUG) { print $self->get_name," unrealize release $gc\n"; }
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
  if (my $model_ids = delete $self->{'model_ids'}) {
    foreach my $id (@$model_ids) {
      if (DEBUG) { print $self->get_name," model disconnect $id\n"; }
      $self->{'model'}->signal_handler_disconnect ($id);
    }
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
might change so it's recommended you only rely on C<Gtk2::Widget>.

    Gtk2::Widget
      Gtk2::DrawingArea
        Gtk2::Ex::TickerView

The interfaces implemented are:

    Gtk2::CellLayout

Or rather the set of methods in L<Gtk2::CellLayout> are implemented, since
as of Gtk2-Perl version 1.161 it's not yet an actual C<GInterface>.

=head1 DESCRIPTION

A C<Gtk2::Ex::TickerView> widget displays items from a C<Gtk2::TreeModel>
scrolling horizontally across the window, like a news bar or stock ticker.

    +----------------------------------------------------------+
    | st item  * The second item  * The third item   * The fou |
    +----------------------------------------------------------+
        <---- scrolling

Items are drawn using one or more C<Gtk2::CellRenderer> objects set into the
view as per the C<Gtk2::CellLayout> interface.  For scrolling text you would
use C<Gtk2::CellRendererText>.

If two or more renderers are set then they're drawn one after the other for
each item (ie. row of the model).  For example you could have a
C<Gtk2::CellRendererPixbuf> to draw an icon then a C<Gtk2::CellRendererText>
to draw some text and they scroll across together.  (The icon could use the
model's data, or be just a fixed blob to go before every item.)

The display and scrolling direction follow the widget text direction (the
C<set_direction> method, per L<Gtk2::Widget>).  For C<ltr> mode item 0
starts at the left of the window and items scroll to the left.  For C<rtl>
item 0 starts at the right of the window and items scroll to the right.

    +----------------------------------------------------------+
    | m five  * item four  * item three  * item two  * item on |
    +----------------------------------------------------------+
                        right to left mode, scrolling ----->

Any text or drawing direction within the cell renderers is a matter for
them.  For C<Gtk2::CellRendererText> Pango recognises right-to-left scripts
such as Arabic based on the utf-8 characters and shouldn't need any special
setups.

Currently only a list style model is expected and only the topmost level of
the model is drawn, so for instance a C<Gtk2::ListStore> suits.  Perhaps in
the future something will be done to descend into and draw child rows too.

The whole Gtk model/view/layout/renderer/attributes stuff as provided here
is ridiculously complicated.  Its power comes when showing a big updating
list or wanting customized drawing, but the amount of code to get started
with something on the screen is not nice.  Have a look at "Tree and List
Widget Overview" in the Gtk reference manual if you haven't already, then
F<examples/simple.pl> in the C<Gtk2::Ex::TickerView> sources is more or less
the minimum to actually display something.

=head1 FUNCTIONS

=over 4

=item C<< Gtk2::Ex::TickerView->new (key => value, ...) >>

Create and return a C<Gtk2::Ex::TickerView> widget.  Optional key/value
pairs can be given to set initial properties as per
C<< Glib::Object->new >>.

=item C<< $ticker->scroll_pixels ($n) >>

Scroll the ticker contents across by C<$n> pixels.  Postive C<$n> moves in
the normal scrolled direction or a negative value goes backwards.  C<$n>
doesn't have to be an integer, the display position is maintained as a
floating point value.

=item C<< $ticker->scroll_to_start () >>

Scroll the ticker contents back to the start, ie. the first row in the
model.

=back

=head1 PROPERTIES

=over 4

=item C<model> (object implementing C<Gtk2::TreeModel>, default undef)

This is any object implementing the C<Gtk2::TreeModel> interface, for
example a C<Gtk2::ListStore>.  It supplies the data to be displayed.  Until
this is set the ticker is blank.

=item C<run> (boolean, default 1)

Whether to run the ticker, ie. to scroll it across the screen under a timer.
If false then the ticker just draws the items at its current position,
without moving (except by the programatic scroll calls above, or user
dragging with mouse button 1).

=item C<speed> (floating point pixels per second, default 25)

The speed the items scroll across, in pixels per second.

=item C<frame-rate> (floating point frames per second, default 4)

The number of times each second the ticker moves and redraws.  (Each move is
C<speed> divided by C<frame-rate> many pixels.)

=item C<fixed-height-mode> (boolean, default false)

If true then assume all rows in the model have the same height.  This means
the ticker can ask its renderers about just one row from the model, instead
of going through all of them.  If the model is big this makes size
negotiation with the ticker's container parent much faster.

=back

The text direction which gives the display order and scrolling is not a
property but instead accessed with the usual widget C<get_direction> and
C<set_direction> methods (see L<Gtk2::Widget>).

The C<visible> property in the cell renderer(s) is recognised and a renderer
that's not visible is skipped and takes no space.  Each C<visible> can be
set globally in the renderer to suppress it entirely, or controlled with the
attributes mechanism (or data setup function) to suppress just selected
items from the model.

=head1 OTHER NOTES

Mouse button 1 is setup for the user to drag the display back and forwards.
This is good to see something that's just gone off the edge, or to skip past
boring bits.  Perhaps in the future the button used will be customizable.

Some scrolling optimizations are attempted.  In the current implementation
when the window is unobscured a scroll is done by a "CopyArea" and a draw of
the balance at the end.  To avoid hammering the server any further copying
is deferred until hearing back from the server that the last completed (a
NoExpose event normally).  All scroll steps are also spun through an idle
handler (at roughly C<GDK_PRIORITY_REDRAW> priority), with the aim of
collapsing say multiple MotionNotify events from a user drag.  The effect of
all this is probably only noticable if your C<frame-rate> is a bit too high
or the server is lagged by other operations.

=head1 SEE ALSO

L<Gtk2::CellLayout>, L<Gtk2::TreeModel>, L<Gtk2::CellRenderer>
