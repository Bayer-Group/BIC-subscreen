# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
########################################
#### CURRENT FILE: ON START SCRIPT #####
########################################

## Fill the DESCRIPTION ----
## Add meta data about your application and set some default {golem} options
##
## /!\ Note: if you want to change the name of your app during development,
## either re-run this function, call golem::set_golem_name(), or don't forget
## to change the name in the app_sys() function in app_config.R /!\
##
golem::fill_desc(
  pkg_name = "subscreen", # The name of the golem package containing the app (typically lowercase, no underscore or periods)
  pkg_title = "Systematic Screening of Study Data for Subgroup Effects", # What the Package Does (One Line, Title Case, No Period)
  pkg_description = "Identifying outcome relevant subgroups has now become as simple as possible! The formerly lengthy and tedious search for the needle in a haystack will be replaced by a single, comprehensive and coherent presentation. The central result of a subgroup screening is a diagram in which each single dot stands for a subgroup. The diagram may show thousands of them. The position of the dot in the diagram is determined by the sample size of the subgroup and the statistical measure of the treatment effect in that subgroup. The sample size is shown on the horizontal axis while the treatment effect is displayed on the vertical axis. Furthermore, the diagram shows the line of no effect and the overall study results. For small subgroups, which are found on the left side of the plot, larger random deviations from the mean study effect are expected, while for larger subgroups only small deviations from the study mean can be expected to be chance findings. So for a study with no conspicuous subgroup effects, the dots in the figure are expected to form a kind of funnel. Any deviations from this funnel shape hint to conspicuous subgroups.", # What the package does (one paragraph).
  authors = c(person(given = "Bodo",
                   family = "Kirsch",
                     role = c("aut", "cre"),
                    email = "kirschbodo@gmail.com"),
             person(given = "Steffen",
                   family = "Jeske",
                     role = "aut"),
             person(given = "Julia",
                   family = "Eichhorn",
                     role = "aut"),
             person(given = "Susanne",
                   family = "Lippert",
                     role = "aut"),
             person(given = "Thomas",
                   family = "Schmelter",
                     role = "aut"),
             person(given = "Christoph",
                   family = "Muysers",
                     role = "aut"),
             person(given = "Hermann",
                   family = "Kulmann",
                     role = "aut")),
  repo_url = "https://github.com/Bayer-Group/BIC-subscreen", # The URL of the GitHub repo (optional),
  pkg_version = "4.1.1", # The version of the package containing the app
  set_options = TRUE # Set the global golem options
)

## Install the required dev dependencies ----
golem::install_dev_deps()

## Create Common Files ----
## See ?usethis for more information
usethis::use_gpl3_license()
golem::use_readme_rmd(open = FALSE)
devtools::build_readme()
# Note that `contact` is required since usethis version 2.1.5
# If your {usethis} version is older, you can remove that param
usethis::use_news_md(open = FALSE)

## Init Testing Infrastructure ----
## Create a template for tests
# golem::use_recommended_tests()

## Favicon ----
# If you want to change the favicon (default is golem's one)
# golem::use_favicon() # path = "path/to/ico". Can be an online file.
golem::remove_favicon() # Uncomment to remove the default favicon

## Add helper functions ----
golem::use_utils_ui(with_test = TRUE)
golem::use_utils_server(with_test = TRUE)

## Use git ----
# usethis::use_git()
# ## Sets the remote associated with 'name' to 'url'
# usethis::use_git_remote(
#   name = "origin",
#   url = "https://github.com/<OWNER>/<REPO>.git"
# )

# You're now set! ----

# go to dev/02_dev.R
rstudioapi::navigateToFile("dev/02_dev.R")
