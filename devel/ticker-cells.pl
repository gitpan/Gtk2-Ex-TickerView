#!/usr/bin/perl

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


# This is a development program for exercising various ticker features.
# It's pretty rough.

use strict;
use warnings;
use Gtk2 '-init';
use Gtk2::Ex::TickerView;

my $toplevel = Gtk2::Window->new('toplevel');
$toplevel->set_default_size (500, -1);
$toplevel->signal_connect (destroy => sub { Gtk2->main_quit;
                                            return 1; # no propagate
                                          });

my $hbox = Gtk2::HBox->new (0, 0);
$toplevel->add ($hbox);

my $left_vbox = Gtk2::VBox->new (0, 0);
$hbox->pack_start ($left_vbox, 0,0,0);

my $right_vbox = Gtk2::VBox->new (0, 0);
$hbox->pack_start ($right_vbox, 1,1,0);


my $model = Gtk2::ListStore->new ('Glib::String');
foreach my $str ('yy', 'zz-bb', '<b>xx</b>', 'fjdks', '32492', "abc\ndef") {
  my $iter = $model->append;
  $model->set_value ($iter, 0, $str);
}

my $renderer = Gtk2::CellRendererText->new;
# $renderer->set (width=>0);

if (0) {
  {
    my $ticker = Gtk2::Ex::TickerView->new (model => $model, 
                                            renderer => $renderer,
                                            attributes => {text => 0}
                                           );
    my $menu = $ticker->menu;
    $ticker->signal_connect (destroy => sub {
                               print "ticker destroy signal\n";
                             });
    $menu->signal_connect (destroy => sub {
                             print "menu destroy signal\n";
                           });
  }
  Gtk2->main;
}
  
my $ticker = Gtk2::Ex::TickerView->new (model => $model, 
                                        # frame_rate => 0.5,
                                        # renderer => $renderer,
                                        # attributes => {text => 0},
                                        run => 0,
                                       );
$ticker->pack_start ($renderer, 1);
$ticker->set_attributes ($renderer, text => 0);
print "ticker flags: ", $ticker->flags,"\n";

$ticker->signal_connect (direction_changed => sub {
                           print "ticker direction changed\n";
                         });

if (1) {
  my $cell = Gtk2::CellRendererText->new;
  $ticker->pack_start ($cell, 1);
  $ticker->set_attributes ($cell, markup => 0);
}
if (0) {
  my $cell = Gtk2::CellRendererText->new;
  $cell->set (text => 'VIS');
  $ticker->pack_start ($cell, 1);
  $ticker->set_cell_data_func
    ($cell, sub {
       my ($ticker, $cell, $model, $iter, $userdata) = @_;
       my $len = length ($model->get ($iter, 0));
       $cell->set('visible', $len != 5);
     });
}
$ticker->signal_connect (destroy => sub {
                           print "ticker destroy signal\n";
                         });
$right_vbox->pack_start ($ticker, 0,0,0);

if (0) {
  my $menu = $ticker->menu;
  $menu->signal_connect (destroy => sub {
                           print "menu destroy signal\n";
                         });
  require Gtk2::Ex::CheckMenuItem::Property;
  my $item = Gtk2::Ex::CheckMenuItem::Property->new_with_label ('Foo');
  $item->show;
  $menu->append ($item);
}

{
  my $button = Gtk2::Button->new_with_label ('Redraw');
  $button->signal_connect (clicked => sub { $ticker->queue_draw; });
  $left_vbox->pack_start ($button, 0, 0, 0);
}
{
  my $button = Gtk2::CheckButton->new_with_label ('Run');
  $button->set_active ($ticker->get('run'));
  $button->signal_connect (toggled => sub {
                             $ticker->set(run => $button->get_active);
                           });
  $left_vbox->pack_start ($button, 0, 0, 0);
}
{
  my $button = Gtk2::CheckButton->new_with_label ('Show');
  $button->set_active (1);
  $button->signal_connect (toggled => sub {
                             $ticker->set(visible => $button->get_active);
                           });
  $left_vbox->pack_start ($button, 0, 0, 0);
}
{
  my $button = Gtk2::CheckButton->new_with_label ('Sens');
  $button->set_active (1);
  $button->signal_connect (toggled => sub {
                             $ticker->set_sensitive ($button->get_active);
                           });
  $left_vbox->pack_start ($button, 0, 0, 0);
}
{
  my $direction_model = Gtk2::ListStore->new ('Glib::String',
                                              'Gtk2::TextDirection');
  $direction_model->set ($direction_model->append, 0, 'LtoR', 1, 'ltr');
  $direction_model->set ($direction_model->append, 0, 'RtoL', 1, 'rtl');

  my $combobox = Gtk2::ComboBox->new_with_model ($direction_model);
  my $renderer = Gtk2::CellRendererText->new;
  $combobox->pack_start ($renderer, 1);
  $combobox->set_attributes ($renderer, 'text', 0);
  $combobox->set_active (0);
  $combobox->signal_connect ('changed', sub {
                               my $iter = $combobox->get_active_iter;
                               my $dir = $direction_model->get_value ($iter,1);
                               $ticker->set_direction ($dir);
                             });
  $left_vbox->pack_start ($combobox, 0,0,0);
}
{
  my $button = Gtk2::Button->new_with_label ('Goto Start');
  $button->signal_connect (clicked => sub { $ticker->scroll_to_start; });
  $left_vbox->pack_start ($button, 0, 0, 0);
}
{
  my $button = Gtk2::Button->new_with_label ('Unparent');
  $button->signal_connect (clicked => sub { $right_vbox->remove ($ticker); });
  $left_vbox->pack_start ($button, 0, 0, 0);
}
{
  my $button = Gtk2::CheckButton->new_with_label ('DebugUps');
  $button->set_tooltip_markup ("Set Gtk2::Gdk::Window->set_debug_updates to flash invalidated regions");
  $button->set_active (0);
  $button->signal_connect (toggled => sub {
                             Gtk2::Gdk::Window->set_debug_updates
                                 ($button->get_active);
                           });
  $left_vbox->pack_start ($button, 0, 0, 0);
}
{
  my $button = Gtk2::Button->new_with_label ('Reorder');
  $button->signal_connect (clicked => sub {
                             my $rows = $model->iter_n_children (undef);
                             $model->reorder (reverse 0 .. $rows-1);
                           });
  $left_vbox->pack_start ($button, 0, 0, 0);
}
{
  my $button = Gtk2::Button->new_with_label ('Delete First');
  $button->signal_connect (clicked => sub {
                             $model->remove ($model->get_iter_first);
                           });
  $left_vbox->pack_start ($button, 0, 0, 0);
}
{
  my $button = Gtk2::Button->new_with_label ('Delete Last');
  $button->signal_connect (clicked => sub {
                             my $rows = $model->iter_n_children (undef);
                             $model->remove ($model->get_iter_from_string
                                             ($rows-1));
                           });
  $left_vbox->pack_start ($button, 0, 0, 0);
}
my $insert_count = 1;
{
  my $button = Gtk2::Button->new_with_label ('Insert First');
  $button->signal_connect (clicked => sub {
                             $model->insert_with_values (0, 0,
                                                         "x$insert_count");
                             $insert_count++;
                           });
  $left_vbox->pack_start ($button, 0, 0, 0);
}
{
  my $button = Gtk2::Button->new_with_label ('Insert Last');
  $button->signal_connect (clicked => sub {
                             my $rows = $model->iter_n_children (undef);
                             $model->insert_with_values ($rows, 0,
                                                         "x$insert_count");
                             $insert_count++;
                           });
  $left_vbox->pack_start ($button, 0, 0, 0);
}
{
  my $button = Gtk2::Button->new_with_label ('Quit');
  $button->signal_connect (clicked => sub { $toplevel->destroy; });
  $left_vbox->pack_start ($button, 0, 0, 0);
}



{
  my $treeview = Gtk2::TreeView->new_with_model ($model);
  $treeview->set (reorderable => 1);
  $right_vbox->pack_start ($treeview, 1,1,0);

  my $column = Gtk2::TreeViewColumn->new_with_attributes
    ("Item", $renderer, text => 0);
  $column->set (resizable => 1);
  $treeview->append_column ($column);
}

$toplevel->show_all;
Gtk2->main;
exit 0;
