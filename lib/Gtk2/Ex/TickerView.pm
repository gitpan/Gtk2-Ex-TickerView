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
use List::Util qw(min max);
use POSIX qw(DBL_MAX);
use Time::HiRes;

use Glib;
# 1.180 for Gtk2::CellLayout interface and for working $region->get_clipbox
use Gtk2 1.180;

use Gtk2::Ex::SyncCall;
use Gtk2::Ex::CellLayout::Base 2;  # version 2 for Gtk2::Buildable
use base 'Gtk2::Ex::CellLayout::Base';

our $VERSION = 6;

# set this to 1 for some diagnostic prints, or 2 for even more prints
use constant DEBUG => 0;


use constant {
  DEFAULT_FRAME_RATE => 4,     # times per second
  DEFAULT_SPEED      => 30,    # pixels per second
};

use Glib::Object::Subclass
  Gtk2::DrawingArea::,
  interfaces => [ 'Gtk2::CellLayout', 'Gtk2::Buildable' ],
  signals => { expose_event            => \&_do_expose_event,
               size_request            => \&_do_size_request,
               size_allocate           => \&_do_size_allocate,
               button_press_event      => \&_do_button_press_event,
               motion_notify_event     => \&_do_motion_notify_event,
               button_release_event    => \&_do_button_release_event,
               visibility_notify_event => \&_do_visibility_notify_event,
               direction_changed       => \&_do_direction_changed,
               map                     => \&_do_map,
               unmap                   => \&_do_unmap,
               unrealize               => \&_do_unrealize,
               notify                  => \&_do_notify,
               state_changed           => \&_do_state_changed,
               style_set               => \&_do_style_set,
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

# The private fields are:
#
# pixmap
#     a Gtk2::Gdk::Pixmap established in _pixmap(), or undef until then
#
# draw_x
#     The $x position within 'pixmap' to draw at x=0 in the widget window,
#     or undef when a full redraw of the pixmap is needed.
#
# row_widths
#     Hash of { $index => $width } which is the width of each row (the total
#     width of all the renderers).
#
#     This is a hash instead of an array with the idea that maybe only some
#     of the most recently used row sizes will be kept when the model is
#     very big.  But there's no code to turfing out aging sizes yet, and
#     doing so could be tricky if there's a lot of zero-width rows that
#     ought to be retained to be skipped when working out sizes and
#     positions.
#
# drawn
#     Hash of { $index => $x } where $x is the position in 'pixmap' where
#     row $index has been drawn, or the leftmost of that row if it's been
#     drawn more than once.
#
#     This is a hash instead of an array since generally only a small set of
#     index positions in a big model will be on-screen at any given time.
#
# want_x, want_index
#     want_index is the desired row number (counting from 0) to be shown at
#     the start of the ticker window.  want_x is an x position where the
#     left edge of the want_index row should start.  This is zero or
#     negative, negative being when the want_index item is partly off the
#     left edge.
#
#     want_x becomes a bigger negative than the want_index row width during
#     scrolling.  _forward_scroll() looks for that and increments want_index
#     to move up.
#
#     want_x can go positive when scrolled backwards.  _back_scroll()
#     decrements want_index to work back to what should then become the new
#     left edge item.
#
# pixmap_x, pixmap_index
#     The x/index which is the start of the pixmap.  Will have pixmap_x <= 0
#     similar to want_x.  At a redraw pixmap_x,pixmap_index are set to
#     want_x,want_index.
#
# pixmap_end_x, pixmap_end_index
#     The x/index just after the last drawn part of the pixmap.  Or
#     pixmap_end_x is the pixmap width when full.
#
#     The pixmap starts with only content that there's a window width worth
#     starting at the draw_x position.  As that draw_x position increases by
#     scrolling more is drawn at pixmap_end_x by _extend_pixmap().
#
#     The "undrawn" area from pixmap_end_x onwards is always cleared to the
#     background colour by _redraw_pixmap(), so _extend_pixmap can just
#     draw.  Although some of that area might never be used, the idea is to
#     do a single big XDrawRectangle instead of several small ones.
#
# visibility_state
#     A GdkVisibilityState enum string, or 'initial' initially, maintained
#     from 'visiblity-notify-event's.  When 'fully-obscured' the scroll
#     timer is suppressed.
#
# drag_x
#     The last x position of the mouse in widget coordinates during a drag,
#     or undef when not in a drag.
#
# In RtoL mode all the x positions are measured from the right edge of the
# window or pixmap.  Only the expose and the cell renderer drawing must
# mirror those RtoL logical position into LtoR screen coordinates.
#

sub INIT_INSTANCE {
  my ($self) = @_;
  $self->set_double_buffered (0);
  $self->{'want_index'}  = 0;
  $self->{'want_x'}      = 0;
  $self->{'row_widths'} = {};
  $self->{'drawn'} = {};
  $self->{'visibility_state'}  = 'initial';
  $self->{'run'}               = 1; # default yes
  $self->{'frame_rate'}        = DEFAULT_FRAME_RATE;
  $self->{'speed'}             = DEFAULT_SPEED;
  $self->{'fixed_height_mode'} = 0; # default no
  
  $self->add_events (['visibility-notify-mask',
                      'button-press-mask',
                      'button-motion-mask',
                      'button-release-mask']);
}

sub FINALIZE_INSTANCE {
  my ($self) = @_;
  _stop_timer ($self);
}

sub SET_PROPERTY {
  my ($self, $pspec, $newval) = @_;
  my $pname = $pspec->get_name;
  my $oldval = $self->{$pname};
  $self->{$pname} = $newval;  # per default GET_PROPERTY
  if (DEBUG) { print "$self set $pname  ",
                 defined $newval ? $newval : '[undef]', "\n"; }

  if ($pname eq 'model') {
    if (($oldval||0) == ($newval||0)) { return; }
    my $model = $newval;

    $self->{'model_ids'} = $model && do {
      my $weak_self = $self;
      Scalar::Util::weaken ($weak_self);
      my $ref_weak_self = \$weak_self;

      require Glib::Ex::SignalIds;
      Glib::Ex::SignalIds->new
          ($model,
           $model->signal_connect (row_changed    => \&_do_row_changed,
                                   $ref_weak_self),
           $model->signal_connect (row_inserted   => \&_do_row_inserted,
                                   $ref_weak_self),
           $model->signal_connect (row_deleted    => \&_do_row_deleted,
                                   $ref_weak_self),
           $model->signal_connect (rows_reordered => \&_do_rows_reordered,
                                   $ref_weak_self))
        };

    %{$self->{'row_widths'}} = ();
    $self->queue_resize;
  }

  if ($pname eq 'fixed_height_mode' && $oldval && ! $newval) {
    # Resize when turning fixed height mode "off" so as to have a look at
    # all the rows beyond the first.
    #
    # Don't resize when turning fixed height mode "on" since the size we
    # have is based on all rows and if the first row is truely
    # representative then its size is the same as what we've got already.
    #
    $self->queue_resize;
  }

  if ($pname eq 'model' || $pname eq 'run' || $pname eq 'frame_rate') {
    if ($pname eq 'frame_rate') {
      _stop_timer ($self);  # ready for new period
    }
    _update_timer ($self);
  }
}


#------------------------------------------------------------------------------

# 'size_request' class closure
sub _do_size_request {
  my ($self, $req) = @_;
  if (DEBUG) { print "TickerView size_request\n"; }

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

# 'size_allocate' class closure
#
# This is also reached for cell renderer and attribute changes through
# $self->queue_resize in CellLayout::Base.
#
# The _drag_scroll updates to keep the contents at the same position under
# the mouse during a drag.  Unfortunately some flashing occurs because the
# server moves the contents with the window then we redraw back to an
# un-moved position.  Hopefully a window move during a drag is fairly
# unusual, so it's good enough for now.
#
# For a move without a resize the pixmap at its current size could be
# retained.  Probably moves alone won't occur often enough to make that
# worth worrying about.
#
sub _do_size_allocate {
  my ($self, $alloc) = @_;
  if (DEBUG) { print "TickerView size_allocate\n"; }
  $self->signal_chain_from_overridden ($alloc);

  if ($self->is_drag_active) {
    my ($x, $y) = $self->get_pointer;
    _drag_scroll ($self, $x);
  }
  $self->{'draw_x'} = undef; # force redraw
  $self->{'pixmap'} = undef; # new size
  $self->queue_draw;
}


#------------------------------------------------------------------------------
# drawing, incl programmatic scrolls

sub _do_expose_event {
  my ($self, $event) = @_;
  if (DEBUG >= 2) { print "TickerView expose ",$event->count,"\n"; }

  my $x = $self->{'draw_x'};
  if (! defined $x) {
    _redraw_pixmap ($self);
    $x = $self->{'draw_x'};
  }

  my $pixmap = $self->{'pixmap'};
  if ($self->get_direction eq 'rtl') {
    my ($pix_width, undef) = $pixmap->get_size;
    $x = $pix_width - 1 - $self->allocation->width - $x;
  }

  my $gc = $self->get_style->black_gc;  # any old gc for an XCopyArea
  my $win = $self->window;
  my ($win_width, $win_height) = $win->get_size;
  my $clip_region = $event->region;

  $gc->set_clip_region ($clip_region);
  $win->draw_drawable ($gc, $pixmap,
                       $x,0,  # src
                       0,0,   # dst
                       $win_width, $win_height);
  $gc->set_clip_region (undef);
  return 0; # propagate event
}

sub _pixmap {
  my ($self) = @_;
  return ($self->{'pixmap'} ||= do {
    my $alloc = $self->allocation;
    my $win_width = $alloc->width;
    my $win_height = $alloc->height;
    my $screen_width = $self->get_screen->get_width;
    my $pix_width = max ($win_width * 2, int ($screen_width / 2));
    if (DEBUG) { print "  create pixmap ${pix_width}x${win_height}\n"; }
    Gtk2::Gdk::Pixmap->new ($self->window, $pix_width, $win_height, -1);
  });
}

sub _redraw_pixmap {
  my ($self) = @_;
  if (DEBUG) { print "  _redraw_pixmap for ",
                 $self->{'want_index'},",",$self->{'want_x'},"\n"; }

  my $pixmap = _pixmap($self);
  my ($pix_width, $pix_height) = $pixmap->get_size;
  my $gc = $self->get_style->bg_gc ($self->state);
  $pixmap->draw_rectangle ($gc, 1, 0,0, $pix_width,$pix_height);
  %{$self->{'drawn'}} = ();

  _normalize_want ($self);
  my $want_index = $self->{'want_index'};
  my $want_x = $self->{'want_x'};
  $self->{'pixmap_index'} = $want_index;
  $self->{'pixmap_x'} = $want_x;
  $self->{'pixmap_end_index'} = $want_index;
  $self->{'pixmap_end_x'} = $want_x;
  $self->{'draw_x'} = 0;

  _extend_pixmap ($self, $self->allocation->width);
}

sub _extend_pixmap {
  my ($self, $target_x) = @_;
  if (DEBUG >= 2) { print "  _extend_pixmap to $target_x\n"; }

  my $x = $self->{'pixmap_end_x'};
  if ($x >= $target_x) { return; } # if target already covered

  my $pixmap = _pixmap($self);
  my ($pix_width, $pix_height) = $pixmap->get_size;

  my $model = $self->{'model'};
  if (! $model) {
    if (DEBUG) { print "$self no model to draw\n"; }
  EMPTY:
    $self->{'pixmap_end_x'} = $pix_width;
    return;
  }

  my $cellinfo_list = $self->{'cellinfo_list'};
  if (! @$cellinfo_list) {
    if (DEBUG) { print "$self no cell renderers to draw with\n"; }
    goto EMPTY;
  }
  # order the cells per their "pack_start" or "pack_end"
  $cellinfo_list = [ grep ({$_->{'pack'} eq 'start'} @$cellinfo_list),
                     reverse grep {$_->{'pack'} eq 'end'} @$cellinfo_list ];

  my $all_zeros = _make_all_zeros_proc();
  my $ltor = ($self->get_direction eq 'ltr');
  my $row_widths = $self->{'row_widths'};
  my $drawn = $self->{'drawn'};

  my $index = $self->{'pixmap_end_index'};
  my $iter = $model->iter_nth_child (undef, $index);

  for (;;) {
    if (! $iter) {
      $index = 0;
      $iter = $model->get_iter_first;
      if (! $iter) {
        if (DEBUG) { print "  model has no rows\n"; }
        $x = $pix_width;
        last;
      }
    }

    $self->_set_cell_data ($iter);

    $drawn->{$index} ||= $x;
    my $row_width = 0;
    foreach my $cellinfo (@$cellinfo_list) {
      my $cell = $cellinfo->{'cell'};
      if (! $cell->get('visible')) { next; }

      my ($x_offset, $y_offset, $width, $height)
        = $cell->get_size ($self, undef);

      my $rect = Gtk2::Gdk::Rectangle->new
        ($ltor ? $x : $pix_width - 1 - $x - $width,
         0,
         $width, $pix_height);
      $cell->render ($pixmap, $self, $rect, $rect, $rect, []);
      $x += $width;
      $row_width += $width;
    }
    $row_widths->{$index} = $row_width;
    if (DEBUG >= 2) { print "  pixmap $index at ",$x-$row_width,
                        " width $row_width\n"; }

    if ($all_zeros->($index, $row_width)) {
      if (DEBUG) { print "$self all cell widths on all rows are zero\n"; }
      $self->{'want_x'} = 0;
      $x = $pix_width;
      last;
    }

    $index++;
    if ($x >= $target_x) { last; }  # stop when target covered
    $iter = $model->iter_next ($iter);
  }

  $self->{'pixmap_end_x'} = min ($x, $pix_width);
  $self->{'pixmap_end_index'} = $index;
  if (DEBUG >= 2) { print "    extended to $index,$x\n"; }
}

sub _move_pixmap {
  my ($self) = @_;
  if (DEBUG >= 2) { print "_move_pixmap ",
                      $self->{'want_index'},",",$self->{'want_x'},"\n"; }
  if (! $self->{'model'}) { return; }

  if (! defined $self->{'draw_x'}) { # need redraw flag
  REDRAW:
    if (DEBUG >= 2) { print "  redraw\n"; }
    _redraw_pixmap ($self);

  EXPOSE:
    if ($self->drawable) { # visible (ie. 'show'n) and mapped
      $self->queue_draw;
      $self->window->process_updates (1);
    }
    return;
  }

  _normalize_want ($self);
  my $want_index = $self->{'want_index'};
  my $want_index_drawn = $self->{'drawn'}->{$want_index};
  if (! defined $want_index_drawn) {
    goto REDRAW;
  }
  if (DEBUG >= 2) { print "  index $want_index at $want_index_drawn\n"; }

  my $want_x = POSIX::floor ($self->{'want_x'});
  my $draw_x = $want_index_drawn - $want_x;
  if ($draw_x == $self->{'draw_x'}) {
    return;  # not moved at all
  }
  if ($draw_x < 0) {
    # pixmap starts too far into first cell for desired draw position
    goto REDRAW;
  }
  if (DEBUG >= 2) { print "  draw_x want $draw_x\n"; }
  $self->{'draw_x'} = $draw_x;

  my $want_pixmap_end_x = $draw_x + $self->allocation->width;
  if (DEBUG >= 2) { print "  pixmap_end_x got ", $self->{'pixmap_end_x'},
                      " want ", $want_pixmap_end_x, "\n"; }
  if ($want_pixmap_end_x <= $self->{'pixmap_end_x'}) {
    # draw_x fits within currently drawn pixmap contents
    goto EXPOSE;
  }

  my $pixmap = _pixmap($self);
  my ($pixmap_width, $pixmap_height) = $pixmap->get_size;

  if ($want_pixmap_end_x > $pixmap_width) {
    # not enough room in pixmap to extend
    goto REDRAW;
  }

  _extend_pixmap ($self, $want_pixmap_end_x);
  goto EXPOSE;
}

sub _normalize_want {
  my ($self) = @_;
  if ($self->{'want_x'} > 0) {
    goto &_back_scroll;
  } else {
    goto &_forward_scroll;
  }
}

# Here want_x <= 0 and we're looking to see if the want_index row is
# entirely off-screen, ie. if want_x + row_width (of that row) would still
# be want_x <= 0.
#
# This is tested on every move/redraw and most of the time the answer is no,
# we're not ready to step want_index, so the test of $x and cached row_width
# tries to get out with minimum work.
#
sub _forward_scroll {
  my ($self) = @_;

  my $x = $self->{'want_x'};
  my $index = $self->{'want_index'};
  my $row_width = _row_width ($self, $index);
  if ($x == 0 || $x + $row_width > 0) { return; }

  if (DEBUG >= 2) { print "  scroll forward from $x,$index\n"; }
  my $model = $self->{'model'} or return;
  my $all_zeros = _make_all_zeros_proc();
  my $iter = $model->iter_nth_child (undef, $index);

  for (;;) {
    $x += $row_width;
    $index++;

    if (DEBUG >= 2) { print "    row width $row_width to $x,$index \n"; }
    if ($iter) { # undef first time through on empty model
      $iter = $model->iter_next ($iter);
    }
    if (! $iter) {
      $index = 0;
      $iter = $model->get_iter_first;
      if (! $iter) {
        if (DEBUG) { print "  model has no rows\n"; }
        $x = 0;
        last;
      }
    }

    $row_width = _row_width ($self, $index, $iter);
    if ($all_zeros->($index, $row_width)) {
      if (DEBUG) { print "    all zeros\n"; }
      $self->{'want_x'} = 0;
      return;
    }
    if ($x + $row_width > 0) { last; }
  }
  
  $self->{'want_x'} = $x;
  $self->{'want_index'} = $index;
}

# If a backwards scroll has moved the starting offset into the window,
# ie. x>0, then decrement $index enough to be x<=0.
#
sub _back_scroll {
  my ($self) = @_;

  my $x = $self->{'want_x'};
  # if ($x <= 0) { return; }
  if (DEBUG) { print "_back_scroll from $x\n"; }

  my $model = $self->{'model'} or return;
  my $index = $self->{'want_index'};
  my $all_zeros = _make_all_zeros_proc();

  do {
    $index--;
    if ($index < 0) {
      $index = max (0, $model->iter_n_children(undef) - 1);
    }
    my $row_width = _row_width ($self, $index);
    if ($all_zeros->($index, $row_width)) {
      if (DEBUG) { print "$self all cell widths on all rows are zero\n"; }
      $x = 0;
      last;
    }
    $x -= $row_width;
  } while ($x > 0);
  
  if (DEBUG) { print "  to $x,$index\n"; }
  $self->{'want_x'} = $x;
  $self->{'want_index'} = $index;
}

# return the width in pixels of row $index
# $iter is an iterator for $index, or undef to get one
sub _row_width {
  my ($self, $index, $iter) = @_;

  my $row_widths = $self->{'row_widths'};
  my $row_width = $row_widths->{$index};
  if (! defined $row_width) {
    if (DEBUG) { print "  _row_width on $index ",
                   defined $iter ? $iter : 'undef', "\n"; }
    if (! defined $iter) {
      my $model = $self->{'model'};
      $iter = $model && $model->iter_nth_child (undef, $index);
      if (! defined $iter) {
        if (DEBUG) { print "  _row_width index $index out of range\n"; }
        return 0;
      }
    }
    $self->_set_cell_data ($iter);

    $row_width = 0;
    foreach my $cellinfo (@{$self->{'cellinfo_list'}}) {
      my $cell = $cellinfo->{'cell'};
      if (! $cell->get('visible')) { next; }
      my (undef, undef, $width, undef) = $cell->get_size ($self, undef);
      $row_width += $width;
    }
    $row_widths->{$index} = $row_width;
    if (DEBUG) { print "  calc row width $index is $row_width\n"; }
  }
  return $row_width;
}

sub scroll_to_start {
  my ($self) = @_;
  _scroll_to_pos ($self, 0, 0);
}

sub scroll_pixels {
  my ($self, $pixels) = @_;
  _scroll_to_pos ($self, $self->{'want_x'} - $pixels, $self->{'want_index'});
}

sub _scroll_to_pos {
  my ($self, $x, $index) = @_;
  if (DEBUG >= 2) { print "scroll_to_pos offset $x index $index\n"; }

  my $prev_index = $self->{'want_index'};
  $self->{'want_index'} = $index;
  $self->{'want_x'} = $x;

  $self->{'sync_call'} ||= do {
    my $weak_self = $self;
    Scalar::Util::weaken ($weak_self);
    Gtk2::Ex::SyncCall->sync ($self, \&_sync_call_handler, \$weak_self);
  };
}
sub _sync_call_handler {
  my ($ref_weak_self) = @_;
  my $self = $$ref_weak_self or return;
  if (DEBUG >= 2) { print "TickerView sync call\n"; }

  $self->{'sync_call'} = undef;
  _move_pixmap ($self);
}


#------------------------------------------------------------------------------
# drawing style changes

# 'direction_changed' class closure
# There's no chain_from_overridden here because the GtkWidget code (as of
# gtk 2.12) in gtk_widget_direction_changed() does a queue_resize, which we
# don't need or want.
sub _do_direction_changed {
  my ($self, $prev_dir) = @_;
  $self->{'draw_x'} = undef;
  $self->queue_draw;
}

# 'notify' class closure
sub _do_notify {
  my ($self, $pspec) = @_;
  my $pname = $pspec->get_name;
  if (DEBUG) { print "TickerView notify '$pname'\n"; }

  if ($pname eq 'sensitive') {
    # gtk_widget_set_sensitive has already done $self->queue_draw, so just
    # need to invalidate pixmap contents here
    $self->{'draw_x'} = undef;
  }
  $self->signal_chain_from_overridden ($pspec);
}

# 'state_changed' class closure
sub _do_state_changed {
  my ($self, $state) = @_;
  if (DEBUG) { print "TickerView state changed '$state'\n"; }
  $self->{'draw_x'} = undef;
  $self->signal_chain_from_overridden ($state);
}

# 'style_set' class closure
sub _do_style_set {
  my ($self, $prev_style) = @_;
  if (DEBUG) { print "TickerView style-set"; }
  $self->{'draw_x'} = undef;
  $self->signal_chain_from_overridden ($prev_style);
}


#------------------------------------------------------------------------------
# scroll timer

# not wrapped as of gtk2-perl version 1.183
use constant GDK_PRIORITY_REDRAW => (Glib::G_PRIORITY_HIGH_IDLE + 20);

# TIMER_PRIORITY is below the drawing done at GDK_PRIORITY_EVENTS under the
# SyncCall so that the current position can go out before making a move to a
# new position.
#
# And try TIMER_PRIORITY below GDK_PRIORITY_REDRAW too, to hopefully
# cooperate with redrawing of other widgets, letting them go out before
# moving the ticker.
#
use constant TIMER_PRIORITY => (GDK_PRIORITY_REDRAW + 10);

# gettime() returns a floating point count of seconds since some fixed but
# unspecified origin time
#
# clock_gettime() croaks if there's no such library func, in which case fall
# back on the hi-res time() for gettime
#
# Maybe it'd be worth checking clock_getres() to see it's a decent
# resolution.  It's conceivable some old implementations might do
# CLOCK_REALTIME just from the CLK_TCK times() counter, giving only 10
# millisecond resolution.  That'd be enough for a modest 10 or 20
# frames/sec, but if attempting say 100 frames on a fast computer for ultra
# smoothness then higher resolution would be needed.
#
sub gettime {
  return Time::HiRes::clock_gettime (Time::HiRes::CLOCK_REALTIME());
}
if (! eval { gettime(); 1 }) {
  if (DEBUG) { print "TickerView fallback to Time::HiRes::time()\n"; }
  no warnings;
  *gettime = \&Time::HiRes::time;
}

# start or stop the scroll timer according to the various settings
sub _update_timer {
  my ($self) = @_;

  my $want_timer = $self->{'run'}
    && $self->mapped
    && $self->{'visibility_state'} ne 'fully-obscured'
    && $self->{'cellinfo_list'}
    && @{$self->{'cellinfo_list'}}
    && $self->{'frame_rate'} > 0
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
      my $period = POSIX::ceil (1000.0 / $self->{'frame_rate'});
      $self->{'period_seconds'} = $period / 1000.0;
      my $weak_self = $self;
      Scalar::Util::weaken ($weak_self);
      if (DEBUG) { print "$self start timer, $period ms\n"; }
      $self->{'prev_time'} = gettime();
      Glib::Timeout->add ($period, \&_do_timer, \$weak_self, TIMER_PRIORITY);
    };
  } else {
    goto &_stop_timer;
  }
}

sub _do_timer {
  my ($ref_weak_self) = @_;
  # shouldn't see an undef in $$ref_weak_self because the timer should be
  # stopped already by _do_unrealize in the course of widget destruction,
  # but if for some reason that hasn't happened then stop it now
  my $self = $$ref_weak_self or return 0; # stop timer

  # during a drag the timer still runs but suppress motion
  if ($self->{'drag_x'}) { return 1; } # continue timer

  my $t = gettime();
  my $delta = $t - $self->{'prev_time'};
  $self->{'prev_time'} = $t;

  # Watch out for the clock going backwards, don't want to scroll back.
  # Watch out for jumping wildly forwards due to the process blocked for a
  # while, don't want to churn through some huge pixel count forwards.
  $delta = min (10, max (0, $delta));

  my $step = $self->{'speed'} * $delta;
  $self->scroll_pixels ($step);
  if (DEBUG >= 2) { print "_do_timer scroll $step to ",
                      $self->{'want_x'}, "\n"; }
  return 1;  # continue timer
}

sub _stop_timer {
  my ($self) = @_;
  if (my $id = delete $self->{'timer_id'}) {
    if (DEBUG) { print "$self stop timer $id\n"; }
    Glib::Source->remove ($id);
  }
}

# 'map' class closure
# (asking the widget to map itself, not the map-event back from the server)
sub _do_map {
  my ($self) = @_;
  # chain before _update_timer(), so the GtkWidget code sets the mapped flag
  $self->signal_chain_from_overridden;
  _update_timer ($self);
}

# 'unmap' class closure.
# (asking the widget to unmap itself, not unmap-event back from the server)
sub _do_unmap {
  my ($self) = @_;
  if (DEBUG) { print "TickerView unmap\n"; }
  # chain before _update_timer(), so the GtkWidget code clears the mapped flag
  $self->signal_chain_from_overridden;
  _update_timer ($self);
}

# 'unrealize' class closure
# (asking the widget to unrealize itself)
#
# When a ticker is removed from a container only the unrealize is called,
# not unmap then unrealize, hence an _update_timer() check here as well as
# in _do_unmap().
#
sub _do_unrealize {
  my ($self) = @_;
  # chain before _update_timer(), so the GtkWidget code clears the mapped flag
  $self->signal_chain_from_overridden;

  $self->{'draw_x'} = undef;
  $self->{'pixmap'} = undef; # different depth when realized next time
  _update_timer ($self);
}

# 'visibility_notify_event' class closure
sub _do_visibility_notify_event {
  my ($self, $event) = @_;
  if (DEBUG) { print "$self visibility ",$event->state,"\n"; }
  $self->{'visibility_state'} = $event->state;
  _update_timer ($self);
  return $self->signal_chain_from_overridden ($event);
}


#------------------------------------------------------------------------------
# dragging
#
# The mouse position for dragging is maintained in widget coordinates, which
# is the easiest way tot keep the contents anchored to the mouse if the
# window moves.  (There's no motion_notify for a window move, must recheck
# the position in _do_size_allocate() above.)

sub is_drag_active {
  my ($self) = @_;
  return (defined $self->{'drag_x'});
}

# 'button_press_event' class closure
sub _do_button_press_event {
  my ($self, $event) = @_;
  if (DEBUG >= 2) { print "TickerView button_press ",$event->button,"\n"; }
  if ($event->button == 1) {
    $self->{'drag_x'} = $event->x;
  }
  return $self->signal_chain_from_overridden ($event);
}

# 'motion_notify_event' class closure
#
# is_hint() supports 'pointer-motion-hint-mask' perhaps set by the
# application or some add-on feature.  Dragging only runs from a mouse
# button so it's enough to use get_pointer() rather than
# $display->get_state(), for now.
#
sub _do_motion_notify_event {
  my ($self, $event) = @_;
  if (DEBUG >= 2) { print "TickerView motion_notify ",$event->x,
                      $event->is_hint ? ' hint' : '', "\n"; }

  if (defined $self->{'drag_x'}) {
    my $x;
    if ($event->is_hint) {
      ($x, undef) = $self->get_pointer;
    } else {
      $x = $event->x;
    }
    _drag_scroll ($self, $x);
  }
  return $self->signal_chain_from_overridden ($event);
}

sub _drag_scroll {
  my ($self, $x) = @_;

  my $step = $self->{'drag_x'} - $x;
  $self->{'drag_x'} = $x;

  if ($self->get_direction eq 'rtl') { $step = -$step; }
  $self->scroll_pixels ($step);
}

# 'button_release_event' class closure
sub _do_button_release_event {
  my ($self, $event) = @_;
  if (DEBUG >= 2) { print "TickerView button_release ",$event->button,"\n"; }

  if (defined $self->{'drag_x'} && $event->button == 1) {
    # final dragged position from this release event
    _drag_scroll ($self, $event->x);
    delete $self->{'drag_x'};
  }
  return $self->signal_chain_from_overridden ($event);
}


#------------------------------------------------------------------------------
# renderer changes

sub _cellinfo_list_changed {
  my ($self) = @_;
  %{$self->{'row_widths'}} = ();
  $self->{'draw_x'} = undef;
  _update_timer ($self);  # newly empty or non-empty cellinfo list
  $self->SUPER::_cellinfo_list_changed;
}

sub _cellinfo_attributes_changed {
  my ($self) = @_;
  %{$self->{'row_widths'}} = ();
  $self->{'draw_x'} = undef;
  $self->SUPER::_cellinfo_attributes_changed;
}


#------------------------------------------------------------------------------
# model changes
#
# All sorts of updates to the pixmap would be possible instead of redrawing,
# it's just a question of whether it's worth the trouble.
#
# The simplest optimization could be to recognise when an
# insert/delete/reorder only affects rows outside the drawn pixmap and thus
# needing nothing except transforming the various index numbers recorded.
#

sub _do_row_changed {
  my ($model, $path, $iter, $ref_weak_self) = @_;
  if (DEBUG) { print "TickerView row_changed path=",$path->to_string,"\n"; }
  my $self = $$ref_weak_self or return;
  if ($path->get_depth != 1) { return; }  # a sub-row
  my ($index) = $path->get_indices;

  # recalculate width
  delete $self->{'row_widths'}->{$index};

  # redraw if changed row displayed in pixmap
  if (defined $self->{'drawn'}->{$index}) {
    $self->{'draw_x'} = undef;
    $self->queue_draw;
  }
}

sub _do_row_inserted {
  my ($model, $ins_path, $ins_iter, $ref_weak_self) = @_;
  my $self = $$ref_weak_self or return;
  if ($ins_path->get_depth != 1) { return; }  # a sub-row

  # if inserted before current then advance
  my ($ins_index) = $ins_path->get_indices;
  if ($ins_index <= $self->{'want_index'}) {
    $self->{'want_index'}++;
    if (DEBUG) { print "row_inserted at or before, move index to ",
                   $self->{'want_index'},"\n"; }
  }

  # if now one row then this insert made it non-empty
  my $newly_nonempty = ! $model->iter_nth_child (undef, 1);
  if ($newly_nonempty) {
    _update_timer ($self);
  }
  if ($newly_nonempty || ! $self->{'fixed_height_mode'}) {
    # become non-empty, or any new row when every row checked for size
    $self->queue_resize;
  }

  %{$self->{'row_widths'}} = ();
  $self->{'draw_x'} = undef;
  $self->queue_draw;
}

sub _do_row_deleted {
  my ($model, $del_path, $ref_weak_self) = @_;
  my $self = $$ref_weak_self or return;
  if (DEBUG) { print "row_deleted, current index ",$self->{'want_index'},"\n";}
  if ($del_path->get_depth != 1) { return; }  # a sub-row

  %{$self->{'row_widths'}} = ();

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
    # newly empty, or any delete when every row checked for size
    $self->queue_resize;
  }

  %{$self->{'row_widths'}} = ();
  $self->{'draw_x'} = undef;
  $self->queue_draw;
}

sub _do_rows_reordered {
  my ($model, $reordered_path, $reordered_iter, $aref, $ref_weak_self) = @_;
  my $self = $$ref_weak_self or return;
  if ($reordered_path->get_depth != 0) { return; }  # a sub-row

  # follow start to new index
  my $new_want_index = $aref->[$self->{'want_index'}];
  # check for undef in case want_index is outside current size, and thus
  # outside @$aref
  if (defined $new_want_index) {
    $self->{'want_index'} = $new_want_index;
  }

  %{$self->{'row_widths'}} = ();
  $self->{'draw_x'} = undef;
  $self->queue_draw;
}


#------------------------------------------------------------------------------
# generic helpers

# _make_all_zeros_proc() returns a procedure to be called
# $func->($index,$width) designed to protect against every $index having a
# zero $width.
#
# $func returns true until it sees an $index==0 and then $index==0 a second
# time, with every call having $width==0.  The idea is that if the drawing,
# scrolling, etc, loop has gone all the way around from zero back to zero
# and all the $width's are equal to zero, then it should bail out.
#
# Any non-zero $width seen makes the returned procedure always return true.
# It might be only a single index position out of thousands, but that's
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
 $ticker->set_attributes ($renderer, text => 0); # column

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

The display and scrolling direction follow C<set_direction> (see
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
the normal scrolled direction or negative goes backwards.

C<$n> doesn't have to be an integer, the display position is maintained as a
floating point value so fractional amounts can accumulate until a whole
pixel step is reached.

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

The number of times each second the ticker moves and redraws.  Each move is
C<speed> divided by C<frame-rate> many pixels.

The current current code uses the Glib main loop timer so the frame rate is
turned into an integer number of milliseconds for actual use.  (And a
minimum of 1 millisecond, meaning frame rates more than 1000 are treated as
1000.  Of course such a rate is pointlessly high, and almost certainly
unattainable.)

=item C<fixed-height-mode> (boolean, default false)

If true then assume all rows in the model have the same height.  This means
the ticker can ask its renderers about just one row from the model, instead
of going through all of them.  If the model is big this is much faster.

=back

The direction for display order and scrolling is not a property but instead
accessed with the usual widget C<get_direction> and C<set_direction> methods
(see L<Gtk2::Widget>).

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

The Gtk reference documentation for C<GtkCellLayout> doesn't really describe
how C<pack_start> and C<pack_end> order cells, but it's the same as
C<GtkBox> and a description can be found there.  Basically each cell is
noted as "start" or "end", with starts drawn from the left and ends from the
right (vice versa in RtoL mode).  In a TickerView the ends immediately
follow the starts, there's no gap in between, unlike say in a C<Gtk2::HBox>.
(Which means the "expand" parameter is ignored currently.)  See
F<examples/order.pl> for a demonstration.

When the model has no rows the TickerView's desired height in
C<size_request> is zero.  This is no good if you want a visible but blank
area when there's nothing to display.  But there's no way TickerView can
work out a height when it's got no data at all to set into the renderers.
You can try calculating a fixed height from a sample model and
C<set_size_request> to force that, or alternately have a "no data" row
displaying in the model instead of letting it go empty, or even switch to a
dummy model with a "no data" row when the real one is empty.

=head2 Drawing

Cells are drawn into an off-screen pixmap which is copied to the window at
successively advancing X positions as the ticker scrolls across.  The aim is
to run the model fetching and cell rendering just once for each row as it
appears on screen.

Scroll drawing goes through a SyncCall (see L<Gtk2::Ex::SyncCall>) so that
after drawing one frame the next won't go out until hearing back from the
server that it's finished drawing the previous.  This ensures a high frame
rate won't flood the server with more drawing than it can keep up with.

Scroll steps under the timer are calculated on elapsed time
(C<clock_gettime> realtime when available, or HiRes system time otherwise,
see L<Time::HiRes>).  This means the apparent motion is still per the
requested C<speed> property even if the C<frame-rate> is not being achieved
(either on the client side or due to the server).

=head1 SEE ALSO

L<Gtk2::CellLayout>, L<Gtk2::TreeModel>, L<Gtk2::CellRenderer>,
L<Gtk2::Ex::CellLayout::Base>

=head1 HOME PAGE

L<http://www.geocities.com/user42_kevin/gtk2-ex-tickerview/index.html>

=head1 COPYRIGHT

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
