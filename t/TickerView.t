#!/usr/bin/perl

# Copyright 2007, 2008 Kevin Ryde

# This file is part of Gtk2-Ex-TickerView.
#
# Gtk2-Ex-TickerView is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as published
# by the Free Software Foundation; either version 3, or (at your option) any
# later version.
#
# Gtk2-Ex-TickerView is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
# Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with Gtk2-Ex-TickerView.  If not, see <http://www.gnu.org/licenses/>.

use strict;
use warnings;
use Gtk2::Ex::TickerView;
use Test::More tests => 36;

# return true if there's any signal handlers connected to $obj
sub any_signal_connections {
  my ($obj) = @_;
  my @connected = grep {$obj->signal_handler_is_connected ($_)} (0 .. 500);
  if (@connected) {
    diag "$obj signal handlers connected: ",join(' ',@connected),"\n";
    return 1;
  }
  return 0;
}


ok ($Gtk2::Ex::TickerView::VERSION >= 8);
ok (Gtk2::Ex::TickerView->VERSION  >= 8);

{
  my $all_zeros = Gtk2::Ex::TickerView::_make_all_zeros_proc();
  ok (! &$all_zeros(0,0));
  ok (  &$all_zeros(0,0));
}
{
  my $all_zeros = Gtk2::Ex::TickerView::_make_all_zeros_proc();
  ok (! &$all_zeros(0,0));
  ok (! &$all_zeros(1,0));
  ok (! &$all_zeros(2,0));
  ok (  &$all_zeros(0,0));
}
{
  my $all_zeros = Gtk2::Ex::TickerView::_make_all_zeros_proc();
  ok (! &$all_zeros(1,0));
  ok (! &$all_zeros(2,0));
  ok (! &$all_zeros(0,0));
  ok (! &$all_zeros(1,0));
  ok (! &$all_zeros(2,0));
  ok (  &$all_zeros(0,0));
}
{
  my $all_zeros = Gtk2::Ex::TickerView::_make_all_zeros_proc();
  ok (! &$all_zeros(0,1));
  ok (! &$all_zeros(0,0));
  ok (! &$all_zeros(0,0));
}
{
  my $all_zeros = Gtk2::Ex::TickerView::_make_all_zeros_proc();
  ok (! &$all_zeros(0,0));
  ok (! &$all_zeros(1,0));
  ok (! &$all_zeros(0,1));
}

{
  my $ticker = Gtk2::Ex::TickerView->new;
  require Gtk2;
  my $r1 = Gtk2::CellRendererText->new;
  my $r2 = Gtk2::CellRendererText->new;
  $ticker->pack_start ($r1, 0);
  $ticker->pack_start ($r2, 0);

  $ticker->reorder ($r1, 0);
  is_deeply ([$ticker->get_cells], [$r1, $r2]);

  $ticker->reorder ($r1, 1);
  is_deeply ([$ticker->get_cells], [$r2, $r1]);

  $ticker->reorder ($r1, 0);
  is_deeply ([$ticker->get_cells], [$r1, $r2]);
}

{
  my $ticker = Gtk2::Ex::TickerView->new;
  my $r1 = Gtk2::CellRendererText->new;
  my $r2 = Gtk2::CellRendererText->new;
  my $r3 = Gtk2::CellRendererText->new;
  $ticker->pack_start ($r1, 0);
  $ticker->pack_start ($r2, 0);
  $ticker->pack_start ($r3, 0);

  $ticker->reorder ($r1, 0);
  is_deeply ([$ticker->get_cells], [$r1, $r2, $r3]);

  $ticker->reorder ($r1, 1);
  is_deeply ([$ticker->get_cells], [$r2, $r1, $r3]);

  $ticker->reorder ($r3, 0);
  is_deeply ([$ticker->get_cells], [$r3, $r2, $r1]);

  $ticker->reorder ($r3, 2);
  is_deeply ([$ticker->get_cells], [$r2, $r1, $r3]);
}

SKIP: {
  if (! Gtk2->init_check) { skip 'due to no DISPLAY available', 9; }

  {
    my $m1 = Gtk2::ListStore->new ('Glib::String');
    my $m2 = Gtk2::ListStore->new ('Glib::String');
    my $ticker = Gtk2::Ex::TickerView->new (model => $m1);
    require Scalar::Util;
    Scalar::Util::weaken ($m1);
    $ticker->set(model => $m2);
    is ($m1, undef, "shouldn't keep a reference to previous model");
  }

  {
    my $ticker = Gtk2::Ex::TickerView->new;
    Scalar::Util::weaken ($ticker);
    is ($ticker, undef, 'garbage collected when weakened - empty');
  }
  {
    my $store = Gtk2::ListStore->new ('Glib::String');
    my $ticker = Gtk2::Ex::TickerView->new (model => $store);
    Scalar::Util::weaken ($ticker);
    is ($ticker, undef, 'garbage collected when weakened - with model');
  }
  {
    my $store = Gtk2::ListStore->new ('Glib::String');
    my $ticker = Gtk2::Ex::TickerView->new (model => $store);
    $ticker->set (model => undef);
    my $get_model = $ticker->get('model');
    is ($get_model, undef, 'unset model from ticker');
  }
  {
    my $store = Gtk2::ListStore->new ('Glib::String');
    my $ticker = Gtk2::Ex::TickerView->new (model => $store);
    $ticker->set (model => undef);
    ok (! any_signal_connections ($store),
        'no signal handlers left on model when unset');
  }
  {
    my $ticker = Gtk2::Ex::TickerView->new;
    my $path = $ticker->get_path_at_pos (0, 0);
    if ($path) { $path = $path->to_string; }
    is ($path, undef, "get_path_at_pos when nothing");

    my $store = Gtk2::ListStore->new ('Glib::String');
    $ticker->set (model => $store);
    $path = $ticker->get_path_at_pos (0, 0);
    if ($path) { $path = $path->to_string; }
    is ($path, undef, "get_path_at_pos when empty and unrealized");

    $ticker->set (model => undef);
    $store->insert_with_values (0, 0=>'foo');
    $ticker->set (model => $store);
    $path = $ticker->get_path_at_pos (0, 0);
    if ($path) { $path = $path->to_string; }
    is ($path, '0', "get_path_at_pos when non-empty and unrealized");

    $ticker->set (model => undef);
    $store->remove ($store->iter_nth_child(undef,0));
    $ticker->set (model => $store);
    my $toplevel = Gtk2::Window->new ('toplevel');
    $toplevel->add ($ticker);
    $toplevel->show_all;
    $path = $ticker->get_path_at_pos (0, 0);
    if ($path) { $path = $path->to_string; }
    is ($path, undef, "get_path_at_pos when empty and realized");
  }
}

exit 0;
