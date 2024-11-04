
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dpm.dashboard

## Overview

This interactive dashboard empowers you to explore and model your data
using the dynamic population model (DPM).

## Contributor guidance

**In brief:** All modifications to this package must be made through
**Pull requests**, ideally linked to a specific GitHub **Issue**.

Pull requests should have a suitable pull request review form attached,
with key notes/descriptions of changes made added by the requester and
if applicable additional notes on particular areas to focus the review
(syntax changes, key functional changes, documentational changes etc.)

## Getting started

### Installation

#### Install directly from GitHub

If you have linked your RStudio installation to GitHub you should be
able to install the package directly from GitHub using the
install_github method from the devtools package

``` r
# install.packages("devtools")
library(devtools)

devtools::install_github(".../dpm.dashboard", build_vignettes = TRUE, INSTALL_opts = "--no-multiarch")
```

If you have not/are unable to link your RStudio installation to GitHub
(you may encounter a 404 error when attempting the previous approach)
you can also install the package in two alternative ways

#### Install locally from .zip

Download a copy of the package repository as a .zip file (option in
‘Code’, below Open with GitHub Desktop) and install using the
install_local method from the devtools package (replace the path with
the path to the .zip file download location)

``` r
# install.packages("devtools")
library(devtools)

devtools::install_local("C:/.../Downloads/dpm.dashboard-main.zip", build_vignettes = TRUE, INSTALL_opts = "--no-multiarch")
```

#### Install locally from cloned repository

Clone the repository (options in ‘Code’ to HTTPS/SSH paths) and build
the package using the build() method from the devtools package

``` r
# install.packages("devtools")
library(devtools)

dpm.dashboard_build <- devtools::build("~/put/the/package/path/here")

devtools::install_local(dpm.dashboard_build, build_vignettes = TRUE, INSTALL_opts = "--no-multiarch")
```

### Usage (example)

Once you have installed the dpm.dashboard package all you need to do to
run the dashboard is using the run_dpm_dashboard() function

``` r
dpm.dashboard::run_dpm_dashboard()

or

library(dpm.dashboard)
run_dpm_dashboard()
```

This will launch the dashboard, allowing us to get started with using
the DPM.

## FAQs/Help

## License

By contributing, you agree that your contributions will be licensed
under its [MIT License](http://choosealicense.com/licenses/mit/). For
additional information regarding the licensing, and related copyright,
of this code please refer to the
[LICENSE](https://github.com/ONSdigital/accountTMB/blob/main/LICENSE.md)
