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


use Test::More tests => 31;

use Gtk2;
use Gtk2::Ex::TickerView;
use Scalar::Util;

ok ($Gtk2::Ex::TickerView::VERSION >= 1);
ok (Gtk2::Ex::TickerView->VERSION >= 1);

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
  if (! Gtk2->init_check) { skip 'due to no DISPLAY available', 4; }

  {
    my $m1 = Gtk2::ListStore->new ('Glib::String');
    my $m2 = Gtk2::ListStore->new ('Glib::String');
    my $ticker = Gtk2::Ex::TickerView->new (model => $m1);
    Scalar::Util::weaken ($m1);
    $ticker->set(model => $m2);
    is ('not defined',
        defined $m1 ? 'defined' : 'not defined',
        "shouldn't keep a reference to previous model");
  }

  {
    my $ticker = Gtk2::Ex::TickerView->new;
    Scalar::Util::weaken ($ticker);
    is ('not defined',
        defined $ticker ? 'defined' : 'not defined',
        'garbage collected when weakened - empty');
  }
  {
    my $store = Gtk2::ListStore->new ('Glib::String');
    my $ticker = Gtk2::Ex::TickerView->new (model => $store);
    Scalar::Util::weaken ($ticker);
    is ('not defined',
        defined $ticker ? 'defined' : 'not defined',
        'garbage collected when weakened - with model');
  }
  {
    my $store = Gtk2::ListStore->new ('Glib::String');
    my $ticker = Gtk2::Ex::TickerView->new (model => $store);
    $ticker->set (model => undef);
    my $get_model = $ticker->get('model');
    is ('not defined',
        defined $get_model ? 'defined' : 'not defined',
        'unset model from ticker');
  }
};

exit 0;
