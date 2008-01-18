# Gtk2::Ex::TickerView widget tests.

# Copyright 2007, 2008 Kevin Ryde

# This file is part of Gtk2::Ex::TickerView.
#
# Gtk2::Ex::TickerView is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as published
# by the Free Software Foundation; either version 3, or (at your option) any
# later version.
#
# Gtk2::Ex::TickerView is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
# Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with Gtk2::Ex::TickerView.  If not, see <http://www.gnu.org/licenses/>.


use Test::More tests => 22;

use Gtk2;
use Gtk2::Ex::TickerView;
use Scalar::Util;

ok ($Gtk2::Ex::TickerView::VERSION >= 1);

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

SKIP: {
  if (! Gtk2->init_check) { skip 'due to no DISPLAY available', 3; }

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
};

exit 0;
