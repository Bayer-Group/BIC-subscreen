#'This package allows analyzing a large number of subgroups simultaneously.
#'The workflow for this subgroup screening is split into two parts:
#'(i) calculation of the results for the subgroups (\code{\link{subscreencalc}}) and
#'(ii) visualization (\code{\link{subscreenshow}}).
#'Optionally variable importance can be calculated (\code{\link{subscreenvi}}).
#'
#'For the calculation of the subgroup results, a patient level dataset needs to be provided,
#'which (for each patient) contains the endpoint data, treatment assignment and all categorical
#'baseline characteristics, by which the subgroups are defined.
#'The results for all subgroups that can be defined by the combination of up to a fixed number of factors,
#'which is only limited by the computational time, will be calculated and stored for further analysis,
#'e.g., visualization.
#'In the second step these results will visualized in a Shiny based interactive graphical user interface,
#'called Subgroup Explorer.

