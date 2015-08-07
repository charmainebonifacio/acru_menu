# The ACRU Parameterization Menu Generator

This program will only automatically adjust 22 variables in the MENU Parameter File using an updated parameter text file. This program will work with a distributed mode or point mode MENU Parameter File. This program also utilizes an implemented a logfile system to ensure accuracy when dealing with the automatic adjustment of values.

The variables that will be updated after each run are as follows:
 - SAUEF
 - TMAXLR
 - TMINLR
 - ALBEDO
 - DEPAHO
 - DEPBHO
 - WP1
 - WP2
 - FC1
 - FC2
 - PO1
 - PO2
 - ABRESP
 - BFRESP
 - CAY
 - ELAIM
 - ROOTA
 - QFRESP
 - COFRU
 - SMDDEP
 - COIAM
 - ICC

 This script will automatically adjust the parameters for calibrating a catchments within the ACRU model. These variables belong to six different information blocks in the Menu Parameter File:
 - Locational and Catchment Unit Information : SAUEF
 - Reference Potential Evaporation Unit Information : TMAXLR, TMINLR, ALBEDO
 - Catchment Soils Information : DEPAHO, DEPBHO, WP1, WP2, FC1, FC2, PO1, PO2, ABRESP, BFRESP
 - Catchment Land Cover Information : CAY, ELAIM, ROOTA
 - Streamflow Simulation Control Variables : QFRESP, COFRU, SMDDEP, COIAM
 - Snow Option Variables : ICC

## Context

[Dr. Stefan Kienzle](http://people.uleth.ca/~stefan.kienzle/) created many fortran scripts to automate parameterization for the calibration on the [ACRU - Agricultural Catchments Research Unit](http://unfccc.int/adaptation/nairobi_work_programme/knowledge_resources_and_publications/items/5299.php) model. He utilized the Fortran 77 language and compiled with Compaq Visual Fortran Version 6.6 compiler.

## Legacy

The purpose is to revise the scripts to Fortran 90 and compiled using the SilverFrost Plato Fortran 95 compiler.

# License

The modules and scripts are licensed under a GNU General Public License v2.0

Copyright (C) 2013-2015 Charmaine Michelle T. Bonifacio

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program; if not, write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
