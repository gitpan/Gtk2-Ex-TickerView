#!/usr/bin/perl

# 0-Test-YAML-Meta.t -- run Test::YAML::Meta if available

# Copyright 2009 Kevin Ryde

# 0-Test-YAML-Meta.t is shared by several distributions.
#
# 0-Test-YAML-Meta.t is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 3, or (at your option) any later
# version.
#
# 0-Test-YAML-Meta.t is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
# or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
#
# You should have received a copy of the GNU General Public License along
# with this file.  If not, see <http://www.gnu.org/licenses/>.

use 5.000;
use strict;
use warnings;
use FindBin;
use File::Spec;
use Test::More;


my $meta_filename = File::Spec->catfile
  ($FindBin::Bin, File::Spec->updir, 'META.yml');
unless (-e $meta_filename) {
  plan skip_all => "$meta_filename doesn't exist -- assume this is a working directory not a dist";
}

# Test::YAML::Meta version 0.13 for fix "optional_features" as a map
#
eval 'use Test::YAML::Meta 0.13; 1'
  or plan skip_all => "due to Test::YAML::Meta 0.13 not available -- $@";

Test::YAML::Meta::meta_yaml_ok();
exit 0;
