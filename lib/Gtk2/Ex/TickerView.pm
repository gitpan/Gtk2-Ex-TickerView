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
use Gtk2 '1.180'; # version 1.180 for Gtk2::CellLayout as an interface

use Gtk2::Ex::SyncCall;
use Gtk2::Ex::CellLayout::Base 2;  # version 2 for Gtk2::Buildable
use base 'Gtk2::Ex::CellLayout::Base';

our $VERSION = 11;

# not wrapped until Gtk2-Perl 1.200
use constant GDK_PRIORITY_REDRAW => (Glib::G_PRIORITY_HIGH_IDLE + 20);


# set this to 1 for some diagnostic prints, or 2 for even more prints
use constant DEBUG => 0;

use constant {
  DEFAULT_FRAME_RATE => 4,     # times per second
  DEFAULT_SPEED      => 30,    # pixels per second
};

use Glib::Object::Subclass
  Gtk2::DrawingArea::,
  interfaces =>
  [ 'Gtk2::CellLayout',
    # Gtk2::Buildable is new in Gtk 2.12, omit if not available
    Gtk2::Widget->isa('Gtk2::Buildable') ? ('Gtk2::Buildable') : ()
  ],

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

                 Glib::ParamSpec->enum
                 ('orientation',
                  'orientation',
                  'Blurb ...',
                  'Gtk2::Orientation',
                  'horizontal',
                  Glib::G_PARAM_READWRITE),

                 Glib::ParamSpec->enum
                 ('renderers-orientation',
                  'renderers-orientation',
                  'Blurb ...',
                  'Gtk2::Orientation',
                  'horizontal',
                  Glib::G_PARAM_READWRITE),
                 ];

# The private per-object fields are:
#
# pixmap [Gtk2::Gdk::Pixmap]
#     established in _pixmap(), or undef until then
#
# draw_x
#     An x position in 'pixmap' which is the part of the pixmap to be draw
#     at x=0 in the widget window.  This is always an integer (unlike want_x
#     below which is kept as a float).  'draw_x' is undef when a full redraw
#     of the pixmap is needed.
#
# row_widths
#     Hash of { $index => $width }.  $index is an integer row number.
#     $width is the total width of all the renderers' drawing of that row.
#
#     This is a hash instead of an array with the idea that only some of the
#     most recently used row sizes will be kept when the model is very big.
#     But there's no code to turf out aging sizes yet, and doing so could be
#     tricky if there's a lot of zero-width rows that ought to be retained
#     to be skipped when working out sizes and positions.  The idea will
#     probably be on a pixbuf full redraw to save only the widths which were
#     used in that draw.  Lots of sizes could accumulate in between for
#     normalize or whatnot, but they'd only be temporary.
#
# drawn
#     Hash of { $index => $x }.  $index is an integer row number.  $x is the
#     position in 'pixmap' where row $index has been drawn, or the leftmost
#     occurrance of that row if it appears more than once in pixmap.
#
#     This is a hash instead of an array since generally only a small set of
#     index positions from a big model will be on-screen at any given time.
#
# want_x, want_index
#     want_index is the desired row number (counting from 0) to be shown at
#     the start of the ticker window.  want_x is an x position where the
#     left edge of the want_index row should start.  want_x is zero or
#     negative.  Negative means the want_index item is partly off the left
#     edge.
#
#     Scrolling will soon make want_x a larger negative than the width of
#     the want_index row.  _normalize_want() (using _normalize()) looks for
#     that and moves up by incrementing want_index and adding the skipped
#     row width to want_x.  It can go across multiple rows that way if
#     necessary.
#
#     Scrolling backwards can make want_x go positive.  _normalize_want()
#     and _normalize() again adjust, this time by decrementing want_index to
#     a preceding row and subtracting that width from want_x, working back
#     to find what row should be at the left edge or just off the left edge.
#
# pixmap_x, pixmap_index
#     The x/index which is the start of the pixmap.  Will have pixmap_x <= 0
#     like want_x.  At a full redraw pixmap_x,pixmap_index are set to
#     want_x,want_index, meaning the row part wanted at the left edge of the
#     window is the left edge of the pixmap.
#
# pixmap_end_x, pixmap_end_index
#     The x/index just after the last drawn part of the pixmap.  Or
#     pixmap_end_x is the pixmap width when full.
#
#     The pixmap starts with only as much content as needed for the draw_x
#     position and the window width.  As the draw_x position increases by
#     scrolling _extend_pixmap() draws more content at pixmap_end_x.
#
#     The "undrawn" area from pixmap_end_x onwards is always cleared to the
#     background colour, in _redraw_pixmap(), so _extend_pixmap() can just
#     draw.  Some of that area might never be used but the idea is to do a
#     single big XDrawRectangle instead of several small ones.
#
# visibility_state
#     A Gtk2::Gdk::VisibilityState enum string value, or 'initial'
#     initially.  This is maintained from 'visiblity-notify-event's.  If
#     'fully-obscured' the scroll timer is stopped.
#
# drag_x, drag_y
#     The last x,y position of the mouse, in root window coordinates, during
#     a drag.  Or drag_x is undef or not existing in the hash at all when
#     not dragging.  The timer is stopped while drag_x is set.
#
# model_empty [boolean]
#     True when we believe $self->{'model'} is empty.  Initialized in
#     SET_PROPERTY when the model is first set, then kept up to date in
#     _row_inserted() and _do_row_deleted().
#
#     The aim of this is to give _do_row_inserted() a way to be sure of when
#     the model transitions from empty to non-empty.  This provokes a resize
#     and a possible timer start.
#
#     Testing for model length == 1 in _do_row_inserted() would be very
#     nearly enough, but not perfect.  If an earlier connected row-inserted
#     handler inserts yet another row in response to this first insertion
#     then by the time _do_row_inserted() runs it sees length==2.  This
#     would be pretty unusual, and probably needs 'iters-persist' on the
#     model for the first iter to remain valid across the extra model
#     change, but it's not completely outrageous.
#
# In RtoL mode all the x positions are measured from the right edge of the
# window or pixmap instead.  Only the expose and the cell renderer drawing
# must mirror those RtoL logical positions into LtoR screen coordinates.
#
# In vertical mode the "x" values are in fact y positions and the "width"s
# are in fact heights.  Stand by for a search and replace to something
# neutral like "p" and "size" or whatever :-).
#

sub INIT_INSTANCE {
  my ($self) = @_;

  # the offscreen 'pixmap' already works as a form of double buffering, no
  # need for DBE
  $self->set_double_buffered (0);

  $self->{'want_index'} = 0;
  $self->{'want_x'}     = 0;
  $self->{'row_widths'} = {};
  $self->{'drawn'}      = {};
  $self->{'visibility_state'} = 'initial';
  $self->{'run'}              = 1; # default yes
  $self->{'frame_rate'}       = DEFAULT_FRAME_RATE;
  $self->{'speed'}            = DEFAULT_SPEED;
  $self->{'orientation'}      = 'horizontal';
  
  $self->add_events (['visibility-notify-mask',
                      'button-press-mask',
                      'button-motion-mask',
                      'button-release-mask']);
}

sub SET_PROPERTY {
  my ($self, $pspec, $newval) = @_;
  my $pname = $pspec->get_name;
  my $oldval = $self->{$pname};
  $self->{$pname} = $newval;  # per default GET_PROPERTY
  if (DEBUG) { print "$self set $pname  ",
                 defined $newval ? $newval : '[undef]', "\n"; }

  if ($pname eq 'model') {
    if (($oldval||0) == ($newval||0)) {
      # no change, avoid queue_resize
      return;
    }
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

    $self->{'model_empty'} = ! ($model && $model->get_iter_first);
    %{$self->{'row_widths'}} = ();
    $self->queue_resize;
  }

  # 'fixed_height_mode' turned off "off" provokes a resize, so as to look at
  # all the rows, not just the first.  But don't resize when turning
  # fixed_height_mode "on" since the size we have is based on all rows and
  # if the first row is truely representative then its size is the same as
  # what we've got already.
  #
  if (($pname eq 'orientation' && $oldval ne $newval)
      || ($pname eq 'fixed_height_mode' && $oldval && ! $newval)) {
    $self->queue_resize;
  }

  if ($pname eq 'model' || $pname eq 'run' || $pname eq 'frame_rate') {
    if ($pname eq 'frame_rate') {
      delete $self->{'timer'};  # discard ready for new period
    }
    _update_timer ($self);
  }
}


#------------------------------------------------------------------------------

# 'size_request' class closure
sub _do_size_request {
  my ($self, $req) = @_;
  if (DEBUG) { print "TickerView size_request\n"; }

  $req->width (0);
  $req->height (0);

  my $model = $self->{'model'} || return;  # no size if no model
  my @cells = $self->GET_CELLS;
  @cells || return;  # no size if no cells

  my $horizontal = ($self->{'orientation'} eq 'horizontal');
  my $sizefield = ($horizontal ? 3 : 2); # height for horiz, width for vert

  my $want_size = 0;
  for (my $iter = $model->get_iter_first;
       $iter;
       $iter = $model->iter_next ($iter)) {
    $self->_set_cell_data ($iter);
    foreach my $cell (@cells) {
      $want_size = max ($want_size,
                        ($cell->get_size($self,undef))[$sizefield]);
    }
    if ($self->{'fixed_height_mode'}) {
      if (DEBUG) { print "  one row only for fixed-height-mode\n"; }
      last;
    }
  }

  if ($horizontal) {
    $req->height ($want_size);
  } else {
    $req->width ($want_size);
  }
  if (DEBUG) { print "  decide size ",$req->width,"x",$req->height,"\n"; }
}

# 'size_allocate' class closure
#
# This is also reached for cell renderer and attribute changes through
# $self->queue_resize in CellLayout::Base.
#
# For a move without a resize the pixmap at its current size could be
# retained.  Probably moves alone won't occur often enough to make that
# worth worrying about.
#
sub _do_size_allocate {
  my ($self, $alloc) = @_;
  if (DEBUG) { print "TickerView size_allocate\n"; }
  $self->signal_chain_from_overridden ($alloc);

  $self->{'draw_x'} = undef; # force redraw
  $self->{'pixmap'} = undef; # new size
  $self->queue_draw;
}


#------------------------------------------------------------------------------
# drawing, including for exposes and for scrolls

# 'expose-event' class closure
# The stuff to draw is in the offscreen 'pixmap', all that's needed here is
# to block copy to the window.
#
# Expose is run for scroll moves (through queue_draw and immediate
# process_updates) as well the usual window visiblity etc changes.  The
# effect of that is other code hanging on expose-event gets a chance to run
# when the contents are redrawn.
#
sub _do_expose_event {
  my ($self, $event) = @_;
  if (DEBUG >= 2) { print "TickerView expose ",$event->count,"\n"; }

  my $x = $self->{'draw_x'};
  if (! defined $x) {
    if (DEBUG >= 2) { print "  redraw for expose\n"; }
    _redraw_pixmap ($self);
    $x = $self->{'draw_x'};
  }

  my $pixmap = $self->{'pixmap'};
  my $horizontal = ($self->{'orientation'} eq 'horizontal');
  if ($self->get_direction eq 'rtl') {
    my ($pix_width, $pix_height) = $pixmap->get_size;
    my $alloc = $self->allocation;
    if ($horizontal) {
      $x = $pix_width - 1 - $alloc->width - $x;
    } else {
      $x = $pix_height - 1 - $alloc->height - $x;
    }
  }

  my $gc = $self->get_style->black_gc;  # any gc for an XCopyArea
  my $win = $self->window;
  my ($win_width, $win_height) = $win->get_size;
  my $clip_region = $event->region;

  $gc->set_clip_region ($clip_region);
  $win->draw_drawable ($gc, $pixmap,
                       ($horizontal ? ($x,0) : (0,$x)), # src
                       0,0,   # dst
                       $win_width, $win_height);
  $gc->set_clip_region (undef);
  return 0; # propagate event
}

# Return 'pixmap', creating it if it doesn't already exist.
# The height is the same as the window height.
# The width is twice the window width, or half the screen width, whichever
# is bigger.
#
# The pixmap is designed to avoid drawing the same row repeatedly as it
# scrolls across.  The wider the pixmap the less often a full redraw will be
# needed.  The width used is therefore a compromise between the memory taken
# by a wide pixmap, versus redraws caused by a narrow pixmap.
#
# Twice the window width gives a reasonable amount of hidden pixmap
# buffering off the window ends.  However if the window is unusually narrow
# it could be much less than a typical row, so impose a minimum of half the
# screen width.
#
# (Maybe a maximum of say twice the screen width could be imposed too, so
# that a hugely wide window doesn't result in a massive pixmap.  But for a
# pixmap smaller than the window we'd have to notice what portion of the
# window is on-screen.  Probably that's much more trouble than it's worth.
# If you ask for a stupidly wide window then expect to have your pixmap
# memory used up. :-)
#
sub _pixmap {
  my ($self) = @_;
  return ($self->{'pixmap'} ||= do {
    my $alloc = $self->allocation;
    my $pix_width  = $alloc->width;
    my $pix_height = $alloc->height;
    if ($self->{'orientation'} eq 'horizontal') {
      my $screen_width = $self->get_screen->get_width;
      $pix_width = max ($pix_width * 2,
                        int ($screen_width / 2));
    } else {
      my $screen_height = $self->get_screen->get_height;
      $pix_height = max ($pix_height * 2,
                         int ($screen_height / 2));
    }
    if (DEBUG) { print "  create pixmap ${pix_width}x${pix_height}\n"; }
    Gtk2::Gdk::Pixmap->new ($self->window, $pix_width, $pix_height, -1);
  });
}

# a full redraw of 'pixmap' contents for want_x/want_index
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
  my $want_x = $self->{'want_x'};
  my $want_index = $self->{'want_index'};

  $self->{'pixmap_index'} = $want_index;
  $self->{'pixmap_x'} = $want_x;
  $self->{'pixmap_end_index'} = $want_index;
  $self->{'pixmap_end_x'} = $want_x;
  $self->{'draw_x'} = 0;

  my $alloc = $self->allocation;
  _extend_pixmap ($self, ($self->{'orientation'} eq 'horizontal'
                          ? $alloc->width : $alloc->height));
}

# draw more at 'pixmap_end_x' to ensure it's not less than $target_x
sub _extend_pixmap {
  my ($self, $target_x) = @_;
  if (DEBUG >= 2) { print "  _extend_pixmap to $target_x\n"; }

  my $x = $self->{'pixmap_end_x'};
  if ($x >= $target_x) { return; } # if target already covered

  my $pixmap = _pixmap($self);
  my ($pix_width, $pix_height) = $pixmap->get_size;
  my $horizontal = ($self->{'orientation'} eq 'horizontal');
  my $pix_size = ($horizontal ? $pix_width : $pix_height);

  my $model = $self->{'model'};
  if (! $model) {
    if (DEBUG) { print "    no model set\n"; }
  EMPTY:
    $self->{'pixmap_end_x'} = $pix_size;
    return;
  }

  my $cellinfo_list = $self->{'cellinfo_list'};
  if (! @$cellinfo_list) {
    if (DEBUG) { print "    no cell renderers to draw with\n"; }
    goto EMPTY;
  }
  # order the cells per their "pack_start" or "pack_end"
  $cellinfo_list = [ grep ({$_->{'pack'} eq 'start'}      @$cellinfo_list),
                     reverse grep {$_->{'pack'} eq 'end'} @$cellinfo_list ];

  my $all_zeros = _make_all_zeros_proc();
  my $ltor = ($self->get_direction eq 'ltr');
  my $row_widths = $self->{'row_widths'};
  my $drawn = $self->{'drawn'};

  my $index = $self->{'pixmap_end_index'};
  my $iter = $model->iter_nth_child (undef, $index);

  for (;;) {
    if (! $iter) {
      # initial $index was past the end, or stepped iter_next() past the
      # end, either way wrap around
      $index = 0;
      $iter = $model->get_iter_first;
      if (! $iter) {
        if (DEBUG) { print "    model has no rows\n"; }
        $x = $pix_size;
        last;
      }
    }

    $self->_set_cell_data ($iter);

    if (! exists $drawn->{$index}) { $drawn->{$index} = $x; }
    my $row_size = 0;
    foreach my $cellinfo (@$cellinfo_list) {
      my $cell = $cellinfo->{'cell'};
      if (! $cell->get('visible')) { next; }

      my (undef, undef, $width, $height) = $cell->get_size ($self, undef);

      my $rect;
      if ($horizontal) {
        $rect = Gtk2::Gdk::Rectangle->new
          ($ltor ? $x : $pix_width - 1 - $x - $width,  0,
           $width,  $pix_height);
        $x += $width;
        $row_size += $width;
      } else {
        $rect = Gtk2::Gdk::Rectangle->new
          (0,  $ltor ? $x : $pix_height - 1 - $x - $height,
           $pix_width,  $height);
        $x += $height;
        $row_size += $height;
      }
      $cell->render ($pixmap, $self, $rect, $rect, $rect, []);
    }
    $row_widths->{$index} = $row_size;
    if (DEBUG >= 2) { print "  pixmap $index at ",$x-$row_size,
                        " width $row_size\n"; }

    if ($all_zeros->($index, $row_size)) {
      if (DEBUG) { print "    all cell widths on all rows are zero\n"; }
      $self->{'want_x'} = 0;
      $x = $pix_size;
      last;
    }

    $index++;
    if ($x >= $target_x) { last; }  # stop when target covered
    $iter = $model->iter_next ($iter);
  }

  $self->{'pixmap_end_x'} = min ($x, $pix_size);
  $self->{'pixmap_end_index'} = $index;
  if (DEBUG >= 2) { print "    extended to $index,$x\n"; }
}

# move the displayed pixmap draw_x position to match want_x (for a scroll),
# and do expose drawing immediately
sub _move_pixmap {
  my ($self) = @_;
  if (DEBUG >= 2) { print "_move_pixmap ",
                      $self->{'want_index'},",",$self->{'want_x'},"\n"; }
  if (! $self->{'model'}) { return; }

  if (! defined $self->{'draw_x'}) { # flag for needing redraw
    if (DEBUG >= 2) { print "  redraw from flag\n"; }
  REDRAW:
    _redraw_pixmap ($self);

  EXPOSE:
    if ($self->drawable) { # visible (meaning 'show'n) plus mapped
      $self->queue_draw;
      $self->window->process_updates (1);
    }
    return;
  }

  _normalize_want ($self);
  my $want_index = $self->{'want_index'};
  my $want_index_drawn = $self->{'drawn'}->{$want_index};
  if (! defined $want_index_drawn) {
    # A scroll has moved to a row not currently drawn in the pixmap at all.
    # Maybe some of the pixmap can be re-used, but for now just redraw.
    goto REDRAW;
  }
  if (DEBUG >= 2) { print "  index $want_index at $want_index_drawn\n"; }

  my $want_x = POSIX::floor ($self->{'want_x'});
  my $draw_x = $want_index_drawn - $want_x;
  if ($draw_x == $self->{'draw_x'}) {
    return;  # not moved at all
  }
  if ($draw_x < 0) {
    # Pixmap starts too far into first cell for desired draw position.
    # Maybe some of the pixmap can be shifted up and re-used, but for now
    # just redraw.  (This case is usually a user drag backwards.)
    goto REDRAW;
  }
  if (DEBUG >= 2) { print "  draw_x want $draw_x\n"; }
  $self->{'draw_x'} = $draw_x;

  my $alloc = $self->allocation;
  my $horizontal = ($self->{'orientation'} eq 'horizontal');
  my $want_pixmap_end_x = $draw_x
    + ($horizontal ? $alloc->width : $alloc->height);
  if (DEBUG >= 2) { print "  pixmap_end_x got ", $self->{'pixmap_end_x'},
                      " want ", $want_pixmap_end_x, "\n"; }
  if ($want_pixmap_end_x <= $self->{'pixmap_end_x'}) {
    # draw_x fits within currently drawn pixmap contents
    goto EXPOSE;
  }

  my $pixmap = _pixmap($self);
  my ($pixmap_width, $pixmap_height) = $pixmap->get_size;
  my $pixmap_size = ($horizontal ? $pixmap_width : $pixmap_height);

  if ($want_pixmap_end_x > $pixmap_size) {
    # Not enough room in pixmap to extend.
    # ENHANCE-ME: Move the existing contents down -- this is the common case
    # of scrolling reaching the end of the pixmap.
    goto REDRAW;
  }

  _extend_pixmap ($self, $want_pixmap_end_x);
  goto EXPOSE;
}

# Here if all rows are zero width, ie. _normalize() returns $x==undef, then
# go to $x==0 which slips through subsequent _normalize() with no action.
# The $index position is retained, since it can be nice to stay at the
# previous position if later that row (or nearby rows) become non-zero
# width.
#
sub _normalize_want {
  my ($self) = @_;
  if (my ($x,$index) = _normalize ($self,
                                   $self->{'want_x'}, $self->{'want_index'})) {
    $self->{'want_x'} = ($x || 0);
    $self->{'want_index'} = $index;
  } else {
    $self->{'want_x'} = 0;
  }
}

# Normalize $x,$index so that $x<=0 and $x+$row_width >= 0.
# The return is two new values ($x,$index).
# If $x==0 then $x,$index are returned unchanged.
# If there's no model, or the model is empty, the return is empty ().
# If all rows are zero width the return is $x==undef and $index unchanged.
#
sub _normalize {
  my ($self, $x, $index) = @_;

  my $model = $self->{'model'} || return;
  my $all_zeros = _make_all_zeros_proc();
  my $len = $model->iter_n_children(undef) || return;  # if model empty

  if ($x < 0) {
    # Here we're looking to see if the want_index row is entirely
    # off-screen, ie. if want_x + row_width (of that row) would still be
    # want_x <= 0.
    #
    # If _row_width() gives us a $iter, because it used it to get a row
    # width, then we keep it going for further rows.  If _row_width()
    # operates out of its cache then there's no iter.
    #
    if (DEBUG >= 2) { print "  forward from $x,$index\n"; }
    my $iter;

    for (;;) {
      my $row_width = _row_width ($self, $index, $iter);
      if ($x + $row_width > 0) {
        last;
      }
      if ($all_zeros->($index, $row_width)) {
        if (DEBUG) { print "$self all cell widths on all rows are zero\n"; }
        return (undef, $_[2]);  # with original $index
      }
      $x += $row_width;
      $index++;
      if ($index >= $len) {
        $index = 0;
        $iter = undef;
      } else {
        if ($iter) {
          $iter = $model->iter_next ($iter);
        }
      }
    }

  } else {
    # Here we're trying to bring $x back to <= 0, usually because a backward
    # scroll has pushed our want_x position to the right and we have to see
    # what the preceding row is and want position to draw it.
    #
    # Because there's no "iter_prev" there's no use of iters here, it ends
    # up a new iter_nth_child in _row_width() for every row not already
    # cached.  For a user scroll back just a short distance the previous row
    # is probably already cached (and even probably in the pixmap).
    #
    if (DEBUG) { print "  backward from $x,$index\n"; }

    while ($x > 0) {
      $index--;
      if ($index < 0) {
        $index = max (0, $len-1);
      }
      my $row_width = _row_width ($self, $index);
      if ($all_zeros->($index, $row_width)) {
        if (DEBUG) { print "$self all cell widths on all rows are zero\n"; }
        return (undef, $_[2]);  # with original $index
      }
      $x -= $row_width;
    }
  }
  if (DEBUG >= 2) { print "  now at $x,$index\n"; }
  return ($x, $index);
}

# Return the width in pixels of row $index.
# $iter is an iterator for $index, or undef to make one here if necessary.
# If an iterator is made then it's stored back to $_[2], as call-by-reference.
#
sub _row_width {
  my ($self, $index, $iter) = @_;

  my $row_widths = $self->{'row_widths'};
  my $row_width = $row_widths->{$index};
  if (! defined $row_width) {
    if (DEBUG) { print "  _row_width on $index ",
                   defined $iter ? $iter : 'undef', "\n"; }
    if (! defined $iter) {
      my $model = $self->{'model'};
      $iter = $_[2] = $model && $model->iter_nth_child (undef, $index);
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
  my $self = $$ref_weak_self || return;
  if (DEBUG >= 2) { print "TickerView sync call\n"; }

  $self->{'sync_call'} = undef;
  _move_pixmap ($self);
}


#------------------------------------------------------------------------------
# drawing style changes

# 'direction_changed' class closure
sub _do_direction_changed {
  my ($self, $prev_dir) = @_;
  $self->{'draw_x'} = undef;
  $self->queue_draw;

  # As of Gtk 2.12 the GtkWidget code in gtk_widget_direction_changed() does
  # a queue_resize, which we don't need or want.  But a direction change
  # should be infrequent and better make sure anything GtkWidget might do in
  # the future gets run.
  $self->signal_chain_from_overridden ($prev_dir);
}

# 'notify' class closure
# SET_PROPERTY() is called only for our own class properties, this default
# handler sees changes made to those defined by other classes
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
  $self->{'draw_x'} = undef; # full redraw
  $self->signal_chain_from_overridden ($state);
}

# 'style_set' class closure
sub _do_style_set {
  my ($self, $prev_style) = @_;
  if (DEBUG) { print "TickerView style-set"; }
  $self->{'draw_x'} = undef; # full redraw
  $self->signal_chain_from_overridden ($prev_style);
}


#------------------------------------------------------------------------------
# scroll timer
#
# _update_timer() starts or stops the timer according to the numerous
# conditions set out in that func.  In general the idea is not to run the
# timer when there's nothing to see/move: eg. the window is not visible, or
# there's nothing in the model+renderers.  Care must be taken that class
# closures etc call _update_timer() when any of the conditions may have
# changed.
#
# The timer action itself is pretty simple, it just calls the public
# $self->scroll_pixels() to make the move (by an amount based on elapsed
# real-time, per "OTHER NOTES" in the pod below).  The hairy stuff in
# scroll_pixels() collapsing multiple motions and drawing works as well from
# the timer as it does from application uses of that func.  In fact that
# collapsing exists mainly to help a timer running a touch too fast for the
# server's drawing.
#

# _TIMER_PRIORITY is below the drawing done at GDK_PRIORITY_EVENTS under the
# SyncCall, so that drawing of the the current position can go out before
# making a scroll to a new position.
#
# And try _TIMER_PRIORITY below GDK_PRIORITY_REDRAW too, to hopefully
# cooperate with redrawing of other widgets, letting their drawing go out
# before scrolling the ticker.
#
use constant _TIMER_PRIORITY => (GDK_PRIORITY_REDRAW + 10);

# _gettime() returns a floating point count of seconds since some fixed but
# unspecified origin time.
#
# clock_gettime(CLOCK_REALTIME) is preferred.  clock_gettime() always
# exists, but it croaks if there's no such C library func.  In that case
# fall back on the hires time(), which is whatever best thing Time::HiRes
# can do, probably gettimeofday() normally.
#
# Maybe it'd be worth checking clock_getres() to see it's a decent
# resolution.  It's conceivable some old implementations might do
# CLOCK_REALTIME just from the CLK_TCK times() counter, giving only 10
# millisecond resolution.  That's enough for a modest 10 or 20 frames/sec,
# but if attempting say 100 frames on a fast computer for ultra smoothness
# then higher resolution would be needed.
#
sub _gettime {
  return Time::HiRes::clock_gettime (Time::HiRes::CLOCK_REALTIME());
}
if (! eval { _gettime(); 1 }) {
  if (DEBUG) { print "TickerView fallback to Time::HiRes::time()\n"; }
  no warnings;
  *_gettime = \&Time::HiRes::time;
}

# start or stop the scroll timer according to the various settings
sub _update_timer {
  my ($self) = @_;

  my $want_timer = $self->{'run'}
    && ! $self->{'paused_count'}
    && $self->mapped
    && $self->{'visibility_state'} ne 'fully-obscured'
    && $self->{'cellinfo_list'}
    && @{$self->{'cellinfo_list'}}  # renderer list not empty
    && $self->{'frame_rate'} > 0
    && ! defined $self->{'drag_x'}  # not in a drag
    && $self->{'model'}
    && ! $self->{'model_empty'};

  if (DEBUG) {
    print "$self run=", $self->{'run'},
      " paused=",      ($self->{'paused_count'}||'no'),
      " mapped=",      $self->mapped ? 1 : 0,
      " visibility=",  $self->{'visibility_state'},
      " model=",       $self->{'model'} || '[none]',
      " model_empty=", $self->{'model_empty'} || 'no',
      " iter_nonempty=", ! defined $self->{'model'} ? '(n/a)'
                      : $self->{'model'}->get_iter_first ? 'yes' : 'no',
      " --> want ", ($want_timer ? 'yes' : 'no'), "\n";
  }

  $self->{'timer'} = $want_timer &&
    ($self->{'timer'} || do {
      my $period = POSIX::ceil (1000.0 / $self->{'frame_rate'});

      my $weak_self = $self;
      Scalar::Util::weaken ($weak_self);
      if (DEBUG) { print "$self start timer, $period ms\n"; }
      $self->{'prev_time'} = _gettime();

      require Glib::Ex::SourceIds;
      Glib::Ex::SourceIds->new
          (Glib::Timeout->add ($period, \&_do_timer, \$weak_self,
                               _TIMER_PRIORITY));
    });
}

sub _do_timer {
  my ($ref_weak_self) = @_;
  # shouldn't see an undef in $$ref_weak_self because the timer should be
  # stopped already by _do_unrealize in the course of widget destruction,
  # but if for some reason that hasn't happened then stop it now
  my $self = $$ref_weak_self || return 0; # stop timer

  my $t = _gettime();
  my $delta = $t - $self->{'prev_time'};
  $self->{'prev_time'} = $t;

  # Watch out for the clock going backwards, don't want to scroll back.
  # Watch out for jumping wildly forwards too due to the process blocked for
  # a while, don't want to churn through some massive pixel count forwards.
  $delta = min (10, max (0, $delta));

  my $step = $self->{'speed'} * $delta;
  $self->scroll_pixels ($step);
  if (DEBUG >= 2) { print "_do_timer scroll $delta seconds, $step pixels, to ",
                      $self->{'want_x'}, "\n"; }
  return 1;  # continue timer
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

  $self->{'draw_x'} = undef; # full redrawn if realized again later
  $self->{'pixmap'} = undef; # possible different depth if realized again later
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
# The basic operation here is pretty simple, it's just a matter of calling
# the public $self->scroll_pixels() with each mouse move amount reported by
# motion-notify.  The hairy stuff in scroll_pixels() to collapse moving and
# drawing works as well for our use of that func as it does for application
# uses.
#
# If someone does a grab_pointer, either within the program or another
# client, then we'll no longer get motion notifies.  Should timer based
# scrolling resume immediately, or only on button release?  If the new grab
# is some unrelated action taking over then immediately might be best.  But
# only on button release may be more consistent.  The latter is done for
# now.
#
#
# If the window is moved during the drag, either repositioned by application
# code, or repositioned by the window manager etc, then it's possible to
# either
#
# 1. Let the displayed contents stay with the left edge of the window.
# 2. Let the displayed contents stay with the mouse, so it's like the
#    window move reveals a different portion.
#
# Neither is too difficult, but 1 is adopted since in 2 there's a bit of
# flashing when the server copies the contents with the move and they then
# have to be redrawn.  (Redrawn under size-allocate, since there's only
# configure-notify for a window move, no mouse motion-notify event.)
#
# Perhaps the double-buffering extension could help with the flashing, but
# it'd have to be applied to the parent window, and could only work when the
# move originates client-side, not say from the window manager.  For now 1
# is easier, and window moves during a drag should be fairly unusual anyway.
#
# To implement 1 the mouse position for dragging is maintained in root
# window coordinates.  This means it's independent of the ticker window
# position.  On that basis we don't need to pay any attention to the window
# position, simply apply root window based mouse motion to scroll_pixels().
# Both x and y are maintained so that you can actually change the
# "orientation" property in the middle of a drag and still get the right
# result!
#

sub is_drag_active {
  my ($self) = @_;
  return (defined $self->{'drag_x'});
}

# 'button_press_event' class closure, getting a Gtk2::Gdk::Event::Button
sub _do_button_press_event {
  my ($self, $event) = @_;
  if (DEBUG >= 2) { print "TickerView button_press ",$event->button,"\n"; }
  if ($event->button == 1) {
    ($self->{'drag_x'}, $self->{'drag_y'}) = $event->root_coords;
    _update_timer ($self); # stop timer
  }
  return $self->signal_chain_from_overridden ($event);
}

# 'motion_notify_event' class closure, getting a Gtk2::Gdk::Event::Motion
#
# Use of is_hint() supports 'pointer-motion-hint-mask' perhaps set by the
# application or some add-on feature.  Dragging only runs from a mouse
# button so for now it's enough to use get_pointer() rather than
# $display->get_state().
#
sub _do_motion_notify_event {
  my ($self, $event) = @_;
  if (DEBUG >= 2) { print "TickerView motion_notify ",$event->x,
                      $event->is_hint ? ' hint' : '', "\n"; }

  if (defined $self->{'drag_x'}) { # ignore motion/drags of other buttons
    _drag_scroll ($self, $event);
  }
  return $self->signal_chain_from_overridden ($event);
}

# 'button_release_event' class closure
#
sub _do_button_release_event {
  my ($self, $event) = @_;
  if (DEBUG >= 2) { print "TickerView button_release ",$event->button,"\n"; }

  if (defined $self->{'drag_x'} && $event->button == 1) {
    _drag_scroll ($self, $event); # final dragged position from this event
    delete $self->{'drag_x'};
    delete $self->{'drag_y'};
    _update_timer ($self); # restart timer
  }
  return $self->signal_chain_from_overridden ($event);
}

sub _drag_scroll {
  my ($self, $event) = @_;

  my ($x, $y) = ($event->can('is_hint') && $event->is_hint
                 ? $self->get_root_window->get_pointer
                 : $event->root_coords);

  # step is simply how much the new position has moved from the old one
  my $step = ($self->{'orientation'} eq 'horizontal'
              ? $self->{'drag_x'} - $x
              : $self->{'drag_y'} - $y);
  $self->{'drag_x'} = $x;
  $self->{'drag_y'} = $y;

  if ($self->get_direction eq 'rtl') { $step = -$step; }
  $self->scroll_pixels ($step);
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
# The main optimization attempted here is to do nothing when changed rows
# are off screen, with the aim of only doing work when the visible part
# changes.
#
# In practice you have to be in fixed-height-mode for off-screen updates to
# do nothing since in the default "all rows sized" mode any change or delete
# has to re-examine all rows, and also currently an insert always provokes a
# full recheck (though better would be possible).
#

# 'row-changed' on the model
sub _do_row_changed {
  my ($model, $chg_path, $chg_iter, $ref_weak_self) = @_;
  if (DEBUG) { print "TickerView row_changed path=",
                 $chg_path->to_string,"\n"; }
  my $self = $$ref_weak_self || return;
  $chg_path->get_depth == 1 || return;  # only top rows
  my ($index) = $chg_path->get_indices;

  # recalculate width
  delete $self->{'row_widths'}->{$index};

  # In fixed-height-mode don't think it's necessary to re-check the size,
  # even when the change is to the representative row 0; the only sizing is
  # on going from empty to non-empty.  That's fairly harsh, but believe it's
  # how GtkTreeView interprets fixed-height-mode there, and it has the happy
  # effect of not provoking repeated rechecks if row 0 changes a lot.
  #
  # In non fixed-height-mode however every row change potentially increases
  # or decreases the height.
  #
  if (! $self->{'fixed_height_mode'}) {
    $self->queue_resize;
  }

  # if changed row is on screen then redraw
  if (defined $self->{'drawn'}->{$index}) {
    $self->{'draw_x'} = undef;
    $self->queue_draw;
  }
}

# 'row-inserted' on the model
sub _do_row_inserted {
  my ($model, $ins_path, $ins_iter, $ref_weak_self) = @_;
  my $self = $$ref_weak_self || return;
  $ins_path->get_depth == 1 || return;  # only top rows
  my ($ins_index) = $ins_path->get_indices;

  # called as $new_index = $remap->($old_index)
  my $remap = sub { $_[0] >= $ins_index ? $_[0] + 1 : $_[0] };

  # want_index and row_widths move
  $self->{'want_index'} = $remap->($self->{'want_index'});
  _hash_keys_remap ($self->{'row_widths'}, $remap);

  my $drawn = $self->{'drawn'};
  if (defined $drawn->{$ins_index}) {
    # $ins_index is on screen, must redraw
    $self->{'draw_x'} = undef;
    $self->queue_draw;
  } else {
    # inserted off screen, just update indexes
    _hash_keys_remap ($drawn, $remap);
  }

  if ($self->{'model_empty'}) {
    # empty -> non-empty restarts timer if stopped due to empty
    _update_timer ($self);
  }
  if ($self->{'model_empty'} || ! $self->{'fixed_height_mode'}) {
    # empty -> non-empty changes size from zero to something;
    # and any new row insertion resizes when non fixed-height-mode
    # ENHANCE-ME: could just see if this new row bigger than already calculated
    $self->queue_resize;
  }
  $self->{'model_empty'} = 0;
}

# 'row-deleted' on the model
sub _do_row_deleted {
  my ($model, $del_path, $ref_weak_self) = @_;
  my $self = $$ref_weak_self || return;
  if (DEBUG) { print "row-deleted, current index ",$self->{'want_index'},"\n";}
  $del_path->get_depth == 1 || return;  # only top rows
  my ($del_index) = $del_path->get_indices;

  # called as $new_index = $remap->($old_index)
  my $remap = sub { $_[0] > $del_index ? $_[0] - 1 : $_[0] };

  # want_index and row_widths move down.
  # If want_index itself is deleted then leave it unchanged to show from the
  # next following, and if it was the last row then it'll wrap around in the
  # draw.
  #
  $self->{'want_index'} = $remap->($self->{'want_index'});
  my $row_widths = $self->{'row_widths'};
  delete $row_widths->{$del_index};
  _hash_keys_remap ($row_widths, $remap);

  my $model_empty = $self->{'model_empty'} = ! $model->get_iter_first;
  if ($model_empty) {
    # becoming empty, stop timer while empty
    _update_timer ($self);
  }
  if ($model_empty || ! $self->{'fixed_height_mode'}) {
    # becoming empty will become zero size;
    # or if ever row checked then any delete affects height
    $self->queue_resize;
  }

  my $drawn = $self->{'drawn'};
  if (defined $drawn->{$del_index}) {
    # deleted row was on screen, must redraw
    $self->{'draw_x'} = undef;
    $self->queue_draw;
  } else {
    # deleted off screen, just update indexes
    _hash_keys_remap ($drawn, $remap);
  }
}

# 'rows-reordered' signal on the model
sub _do_rows_reordered {
  my ($model, $reordered_path, $reordered_iter, $aref, $ref_weak_self) = @_;
  my $self = $$ref_weak_self || return;
  if (DEBUG) { print "_do_rows_reordered\n"; }
  $reordered_path->get_depth == 0 || return;   # top rows only

  # $oldpos == $aref->[$newpos], ie. aref says where the row used to be.
  # $remap{$oldpos} == $newpos, ie. where the old has been sent
  # Building a hash might be a bit unnecessary if not much to remap, but
  # it's less code than a linear search or similar.
  #
  my %remap;
  @remap{@$aref} = (0 .. $#$aref);
  if (DEBUG) { require Data::Dumper;
               print " remap ",Data::Dumper::Dumper(\%remap); }
  # called as $new_index = $remap->($old_index)
  my $remap = sub { $remap{$_[0]} };

  # want_index and row_widths permute
  $self->{'want_index'} = $remap->($self->{'want_index'});
  _hash_keys_remap ($self->{'row_widths'}, $remap);

  my $drawn = $self->{'drawn'};
  my $len = scalar @$aref;
  if (! _all_map_same (sub { ($_[0] - $remap->($_[0]) + $len) % $len },
                       keys %$drawn)) {
    # the drawn rows have shuffled about, redraw
    $self->{'draw_x'} = undef;
    $self->queue_draw;
  } else {
    # the drawn rows are all still next to each other, modulo the model length
    # so can just follow the reordering
    _hash_keys_remap ($drawn, $remap);
  }
}

# _all_map_same ($func, $x, $y, ...) returns true if calls $func->($x),
# $func->($y), etc all return the same number.
sub _all_map_same {
  my $func = shift;
  if (@_) {
    my $want = $func->(shift @_);
    foreach (@_) {
      if ($func->($_) != $want) { return 0; }
    }
  }
  return 1;
}

# modify the keys in %$href by $newkey=$func->($oldkey)
# the value which had been associated with $oldkey moves to $newkey
#
sub _hash_keys_remap {
  my ($href, $func) = @_;
  %$href = map { ($func->($_), $href->{$_}) } keys %$href;
}


#------------------------------------------------------------------------------
# generic helpers

# _make_all_zeros_proc() returns a procedure to be called
# $func->($index,$width) designed to protect against every $index having a
# zero $width.
#
# $func returns true until it sees an $index==0 and then a second $index==0,
# with all calls having $width==0.  The idea is that if the drawing,
# scrolling or whatever loop has gone from $index zero all the way up and
# around back to $index zero again, and all the $width's seen are zero, then
# it should bail out.
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


#------------------------------------------------------------------------------
# other method funcs

sub get_path_at_pos {
  my ($self, $x, $y) = @_;
  if (DEBUG) { print "get_path_at_pos($x,$y)\n"; }

  # Go from the want_x/want_index desired position, even if the drawing
  # isn't yet actually displaying that.  This makes most sense after a
  # programmatic scroll, and if it's a user button press then the display
  # will only be a moment away from showing that want_x/want_index position.
  #
  my $index = $self->{'want_index'};
  $x -= $self->{'want_x'};
  if (DEBUG) { print "  adj for want_x=",$self->{'want_x'},", to x=$x\n"; }

  ($x, $index) = _normalize ($self, -$x, $index);
  if (DEBUG) { print "  got ", (defined $x ? $x : 'undef'), ",$index\n"; }
  if (defined $x) {
    return Gtk2::TreePath->new_from_indices ($index);
  } else {
    return undef;
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
might change so it's recommended you only rely on C<Gtk2::Widget>.

    Gtk2::Widget
      Gtk2::DrawingArea
        Gtk2::Ex::TickerView

The interfaces implemented are:

    Gtk2::Buildable (Gtk 2.12 and up)
    Gtk2::CellLayout

=head1 DESCRIPTION

A C<Gtk2::Ex::TickerView> widget displays items from a C<Gtk2::TreeModel>
scrolling across the window, like a news bar or stock ticker.

    +----------------------------------------------------------+
    | st item  * The second item  * The third item   * The fou |
    +----------------------------------------------------------+
        <---- scrolling

Or in C<vertical> orientation it scrolls upwards.

    +-------+
    | Two   |  ^
    | Three |  |
    | Four  |  | scrolling
    | Five  |  |
    | Six   |  |
    | ...   |
    +-------+

Items are drawn with one or more C<Gtk2::CellRenderer> objects set into the
TickerView as per the CellLayout interface (see L<Gtk2::CellLayout>).  For
example for scrolling text you can use C<Gtk2::CellRendererText> as a
renderer.

If two or more renderers are set then they're drawn one after the other for
each item, ie. row of the model.  For example you could have a
C<Gtk2::CellRendererPixbuf> to draw an icon then a C<Gtk2::CellRendererText>
to draw some text and they scroll across together (or upwards on top of each
other when vertical).  The icon could use the row data, or just be a fixed
image to go before every item.

     +-----------------------------------------------+
     |    +--------++--------++--------++--------+   |
     | ...| Pixbuf || Text   || Pixbuf || Text   |...|
     |    | row 35 || row 35 || row 36 || row 36 |   |
     |    +--------++--------++--------++--------+   |
     +-----------------------------------------------+

The display and scrolling direction follow the left-to-right or
right-to-left of C<set_direction> (see L<Gtk2::Widget>).  For C<ltr> mode
item 0 starts at the left of the window and items scroll to the left.  For
C<rtl> item 0 starts at the right of the window and items scroll to the
right.

    +----------------------------------------------------------+
    | m five  * item four  * item three  * item two  * item on |
    +----------------------------------------------------------+
                        rtl mode, scrolling ----->

In vertical mode C<ltr> scrolls upwards and C<rtl> scrolls downwards.  This
doesn't make as much sense as it does horizontally.  (Perhaps it should
change, though if you have to set vertical orientation it's not too terrible
that C<set_direction> is the slightly unusual case of a downwards scroll.)

Within each renderer cell any text or drawing direction is a matter for that
renderer.  For example in C<Gtk2::CellRendererText> Pango recognises
right-to-left scripts such as Arabic based on the characters and shouldn't
need any special setups.  (But if you want to rotate 90 degrees for
something vertical it might be much trickier -- just setting text "gravity"
doesn't work.)

Currently only a list style model is expected, meaning only a single level,
and only that topmost level of the model is drawn.  For example a
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

The display position is maintained as a floating point value so fractional
C<$n> amounts accumulate until a whole pixel step is reached.

=item C<< $ticker->scroll_to_start () >>

Scroll the ticker contents back to the start, ie. to show the first row of
the model at the left end of the window (or upper end for vertical, or right
end for C<rtl>, or bottom end for vertical plus C<rtl>!).

=item C<< $path = $ticker->get_path_at_pos ($x, $y) >>

Return a C<Gtk2::TreePath> which is the model row displayed at C<$x>,C<$y>,
or return C<undef> if there's nothing displayed there.  There can be nothing
if no C<model> is set, or it has no rows, or all rows are zero width or not
visible (the renderer C<visible> property).

C<$x> can be outside the window, in which case the item which would be shown
at that point is still returned.  C<$y> is currently ignored, since all
items simply use the full window height.  Perhaps in the future a C<$y>
outside the window height will cause an C<undef> return.

=back

=head1 OBJECT PROPERTIES

=over 4

=item C<model> (object implementing C<Gtk2::TreeModel>, default undef)

This is any C<Glib::Object> implementing the C<Gtk2::TreeModel> interface,
for example a C<Gtk2::ListStore>.  It supplies the data to be displayed.
Until this is set the ticker is blank.

=item C<run> (boolean, default true)

Whether to run the ticker, ie. to scroll it across under a timer.  If false
then the ticker just draws the items at its current position without moving
(except by the programatic scroll functions above, or user dragging with
mouse button 1).

=item C<speed> (floating point pixels per second, default 25)

The speed the items scroll across, in pixels per second.

=item C<frame-rate> (floating point frames per second, default 4)

The number of times each second the ticker moves and redraws.  Each move
will be C<speed> divided by C<frame-rate> many pixels.

The current current code uses the Glib main loop timer so the frame rate is
becomes integer milliseconds for actual use.  A minimum 1 millisecond is
imposed, meaning frame rates more than 1000 are treated as 1000.  Of course
1000 frames a second is pointlessly high.

=item C<orientation> (C<Gtk2::Orientation> enum, default C<"horizontal">)

If set to C<"vertical"> the ticker items are drawn vertically from the top
of the window downwards and scroll up the screen.  Or with C<set_direction>
of C<rtl> mode the direction reverses so they're drawn from the bottom of
the window upwards and scroll down the screen.  (The name C<rtl> doesn't
make a great deal of sense in vertical mode.  Something to reverse the
direction is certainly desired, but perhaps it shouldn't be the LtoR/RtoL
setting ...)

=item C<fixed-height-mode> (boolean, default false)

If true then assume all rows in the model have the same height and it
doesn't change.  This allows the ticker to get its desired height by asking
the renderers about just one row of the model, instead of going through them
all and resizing on every insert, delete or change.  If the model is big
this is a significant speedup.

If you force a height with C<set_size_request> in the usual widget fashion
then you should turn on C<fixed-height-mode> too because under
C<set_size_request> the sizing mechanism ends up running the widget size
code even though it then overrides the result.

=back

The C<visible> property in each cell renderer is recognised and a renderer
that's not visible is skipped and takes no space.  C<visible> can be set
globally in the renderer to suppress it entirely, or controlled with the
attributes mechanism or data setup function to suppress it just for selected
rows from the model.

(Suppressing lots of rows using C<visible> might be a bit slow since
TickerView basically must go each to see its state.  A
C<Gtk2::TreeModelFilter> may be a better way to pick out a small number of
desired rows from a very big model.)

=head1 BUILDABLE

C<Gtk2::Ex::TickerView> implements the C<Gtk2::Buildable> interface of Gtk
2.12 and up, allowing C<Gtk2::Builder> to construct a TickerView.  The class
name is C<Gtk2__Ex__TickerView> and renderers and attributes are added as
children per C<Gtk2::CellLayout>.  Here's a sample, or see
F<examples/builder.pl> in the TickerView sources for a complete program,

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

But see L<Gtk2::Ex::CellLayout::Base/BUILDABLE INTERFACE> for caveats about
widget superclass tags (like the "accessibility" settings) which end up
unavailable.

=head1 OTHER NOTES

Mouse button 1 is setup for the user to drag the display back and forwards.
This is good to go back and see something that's just moved off the edge, or
to skip past boring bits.  Perhaps in the future the button used will be
customizable.

The Gtk reference documentation for C<GtkCellLayout> doesn't really describe
how C<pack_start> and C<pack_end> order the cells, but it's the same as
C<GtkBox> and a description can be found there.  Basically each cell is
noted as "start" or "end", with "starts" drawn from the left and "ends" from
the right (vice versa in RtoL mode).  In a TickerView the ends immediately
follow the starts, there's no gap in between, unlike say in a C<Gtk2::HBox>.
(Which means the "expand" parameter is ignored currently.)  See
F<examples/order.pl> in the sources for a demonstration.

When the model has no rows the TickerView's desired height from
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
appears on screen.  (Rows wider than the pixmap will get multiple draws.)

The drawing for scroll movement goes through a SyncCall (see
L<Gtk2::Ex::SyncCall>) so that after drawing one frame the next won't go out
until hearing back from the server that it's finished the previous.  This
ensures a high frame rate doesn't flood the server with more drawing than it
can keep up with.

Scroll movement amounts are calculated from elapsed time using
C<clock_gettime()> real time when available, or high-res system time
otherwise (see C<Time::HiRes>).  This means the C<speed> setting is followed
even if the requested C<frame-rate> is not being achieved.  Slow frame rates
can occur on the client side if the main loop is busy doing other things
(including temporarily blocked completely), or can be on the X server side
if it's busy with other drawing etc.

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
