# Test::Weaken::Gtk2 -- some helpers for Test::Weaken with Gtk2

# Copyright 2008, 2009 Kevin Ryde

# Test::Weaken::Gtk2 is shared by several distributions.
#
# Test::Weaken::Gtk2 is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 3, or (at your option) any later
# version.
#
# Test::Weaken::Gtk2 is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
# or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
#
# You should have received a copy of the GNU General Public License along
# with this file.  If not, see <http://www.gnu.org/licenses/>.


package Test::Weaken::Gtk2;
use 5.008;
use strict;
use warnings;

use base 'Exporter';
our @EXPORT_OK = qw(contents_gtk_container
                    destructor_gtk_destroy
                    ignore_GdkDisplay);
our %EXPORT_TAGS = (all => \@EXPORT_OK);

use constant DEBUG => 0;


# $ref is expected to be either a Gtk2::Object, or an array whose first
# element is a Gtk2::Object.  Call that object's ->destroy()to ask it to
# destruct.  Generally this is only needed on Gtk2::Window and its
# subclasses.
# 
sub destructor_destroy {
  my ($ref) = @_;
  if (ref $ref eq 'ARRAY') {
    $ref = $ref->[0];
  }
  $ref->destroy;

  # iterate to make Widget Cursor go unbusy
  require MyTestHelpers;
  MyTestHelpers::main_iterations();
}

# If $ref is a Gtk2::Container object then return its container children,
# otherwise return an empty list.
# 
sub contents_container {
  my ($ref) = @_;
  require Scalar::Util;
  return unless Scalar::Util::blessed($ref);
  return unless $ref->isa('Gtk2::Container');

  if (DEBUG) { Test::More::diag ("contents ",ref $ref); }
  return $ref->get_children;
}

sub ignore_GdkDisplay {
  my ($ref) = @_;
  require Scalar::Util;
  return (Scalar::Util::blessed($ref)
          && $ref->isa('Gtk2::Gdk::Display'));
}

1;
__END__
