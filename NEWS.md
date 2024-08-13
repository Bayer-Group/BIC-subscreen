# subscreen 1.0.0

## Bug fixes subscreencalc

* Fixed the example to make it work properly. Added some data pre-processing and handling of NAs. 
* Fixed the problem with max_comb=1 in combination with nkernel=1. Function sapply needed the option simplify=FALSE to keep the data structure  
* No error anymore if nkernel>1 and par_functions=""

## Bug fixes subscreenshow

* Factor levels 6 to 8 will now be displayed. They had no color assigned before.
* Reference line for overall result is now exact on the right place. The slider input has been removed as this caused inappropriate rounding in some cases.


## Enhancements subscreenshow

* The slider for the y-range is now improved. It will use nice numbers for the range selection. Thanks to Tommy (662787) from StackOverflow for roundDownNice(). And you can actually give a set of numbers you think of being nice in the new parameter NiceNumbers.
* Background shading including marks can be set by the new parameter StripesBGplot. The program will aim for the given number of stripes/marks but the actual display may differ to have nice intervals
* On the x-axis percentages of the total are shown

## Editorial changes

* Updated the description
* Packages shiny and DT are now imports although only needed in subscreenshow 
* Spelling errors corrected
* Changed some internal function and variable names for better readability
* Translated the rest of the German comments into English
* Deleted unused functions and program code
* Removed NA from the subgroup filter drop-down selection

# subscreen 2.0.1

* New Layout
* Added Subscreen Comparer
* Added Subscreen Mosaic
* Added Variable Importance calculation
* Added Colour Options Panel
* Added Display Options Panel
* Added identification of parent subgroups 
* Added option to memorize subgroups

# subscreen 3.0.0

## subscreencalc
* Add parameter 'factorial' for factorial context calculations
* Add parameter 'use_complement' for Subgroup complement calculations

## subscreen.vi
*Added possibility to calculate importance for multiple target variables

## subscreenshow
*Improve coloring running time
*Bug fix in function parents() for descending ID's
*Bug fix 'Infinite option'-loop
* Add 'Factorial Contexts'-calculation
* Add optional 'Subgroup Complement'-calculation
* Add Subscreen ASMUS-tab
*Add Subscreen Logo
*Change default background color
*Add Flexible Plot sizes
*Add Mousehover info for all plots
*Change Table header colour
*Add flexible Plot Legends

##Subscreen Explorer-tab
###Variable Options-tab
*Add logarithmic Slider 

###Importance-tab
*Add 'Select Variable'-option to select target variable
*Bug fix importance color 

###Display Options-tab
*Add 'Adjust point brightness'-option
*Remove 'Choose number of Stripes background'-option
*Add 'Plotting character'-option

###Colour Options
*Add 'Colour for factorial Context'-option
*Remove 'Choose Background Colour (Plot stripes)'-option
*Remove 'Choose font colour'-option

###Plots
*Add interaction Plot

###Tables
*Add 'Factorial Context'-table
*Add 'Subgroup Complement'-table

##Subscreen Comparer-tab
###Sidebarpanel
*Remove 'Subgroup Filter'-option
*Remove 'Subgroup level(s)'-option
###Compare-plots
*Add legend
###Bubble plot
*Add legend

##Subscreen Mosaic-tab
###Sidebarpanel
*Add help text

###Hovertable
*Change font color for hovertable

# subscreen 3.0.1

## Bug fixes subscreenshow

* An update of the package shinyjs required a change in the call of the extendShinyjs function to avoid an error while trying to start subscreenshow. This was corrected.

# subscreen 3.0.2

## General
* Dependency to package V8 removed
* Time report of subscreencalc() including the calculation of the factorial context

## Bug fix subscreenshow()
* reastablish compatibility with R version 3.6.2

## Bug fix Subscreen ASMUS-tab
* label for reference line corrected

# subscreen 3.0.3

## Bug fix Subscreen Comparer-tab
* label for top reference line corrected

## Bug fix Subscreen ASMUS-tab
* fixed initial subgroup information text reactivity after target variable change

# subscreen 3.0.4

## subscreencalc
* optimized calculation of complement included in regular calculation step to reduce computing time
* stepwise verbose output with additional details

# subscreen 3.0.5

## subscreencalc
* supress output of "joining by ..." while running R version 3.X.X

# subscreen 4.0.0

## subscreencalc
* parameter min_comb removed
* parameter endpoints removed
* bug fix subscreencalc factorial context calculation
* new factorial context calculation for each target variable
* multiple notes and custom error messages added
* parameter factors is now required

## subscreenshow
* new upload-tab in app
* re-sturcture app via modules
* add memorize subgroup labels
* add memorize subgroup color
* complete change of ASMUS-tab
* new hover and click feature in explorer- and comparer-tab
* legend collapsable in explorer-tab
* new feature x-axis slider
* new feature custom reference line
* new parameter favour_label_verum_name & favour_label_comparator_name
  in subscreenshow() for new feature of custom labels in explorer- and
  comparer-tab 
* second filter variable applicable in explorer-tab
* new header information in explorer-tab
* plotting character option removed
* bug fix mosaic plot number of subjects size


## additional changes
* update example/demo data and include them within app
* new vignette/readme
* changes in DESCRIPTION


# subscreen 4.0.3

## subscreencalc
* fixing sample size calculations for data with multiple rows for subjects

## subscreenshow 
* adding multiple new parameter to define settings
at app start

## subscreenfunnel
* adding new function subscreenfunnel



