# The ACRU Model Menu Generator Scripts

## Context
[Dr. Stefan Kienzle](http://people.uleth.ca/~stefan.kienzle/) created many fortran scripts to automate calibration on the [ACRU - Agricultural Catchments Research Unit](http://unfccc.int/adaptation/nairobi_work_programme/knowledge_resources_and_publications/items/5299.php) model using Fortran 77 language and compiled with Compaq Visual Fortran Version 6.6 compiler.

## Legacy

The purpose of revising the fortran scripts is to upgrade the scripts to Fortran 90 and compiled using the SilverFrost Plato Fortran 95 compiler. I have also implemented a logfile system to ensure accuracy when dealing with changing calibration values for each ACRU runs. For my project, I will only be using three scripts:

- Fortran script that will select a specific range of Hydrological response unite (HRUs).
- Fortran script that will convert the MENU file into an appropriate  for distributed mode
- Fortran script that will copy calibration values from a tab-delimited file that contains specific ACRU variables.

# License

The revised scripts are licensed under a GNU General Public License v2.0

Copyright (C) 2013-2015 Charmaine Michelle T. Bonifacio

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program; if not, write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
