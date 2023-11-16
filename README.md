# `networkDynamic`:  Dynamic Extensions for Network Objects

[![R build status](https://github.com/statnet/networkDynamic/workflows/R-CMD-check/badge.svg)](https://github.com/statnet/networkDynamic/actions)
[![rstudio mirror downloads](https://cranlogs.r-pkg.org/badges/networkDynamic?color=2ED968)](https://cranlogs.r-pkg.org/)
[![cran version](https://www.r-pkg.org/badges/version/networkDynamic)](https://cran.r-project.org/package=networkDynamic)

`networkDynamic` is a network data management package that provides tools to import, transform and extract relational data with timing information from various data structures (matrices, spell lists, toggles, etc). The software manages temporal information for both vertices and edges, including presence/absence (which allows for changes in network size) and discrete or continuous attributes.  Both directed and undirected networks are supported.

This package is a part of the [Statnet](https://statnet.org) suite of packages for network analysis, so it is designed to work seamlessly with all of the packages in that suite. `networkDynamic` extends the capability of Statnet's `network` package to handle temporal information, and it is the data management foundation used by the higher level packages: 
* [`tsna`](https://github.com/statnet/tsna) Exploratory data analysis and summary statistics for temporal networks
* [`ndtv`](https://github.com/statnet/ndtv) Temporal network graphics and animation
* [`tergm`](https://github.com/statnet/tergm) Statistical analysis and simulation of temporal networks with TERGMs
* [`ergmgp`](https://github.com/statnet/ergmgp) Modeling continuous time graph processes with ERGM equilibria
* [`EpiModel`](https://www.epimodel.org/) Epidemic modeling on dynamic networks

Self-guided training materials can be found on the [statnet workshop page](https://statnet.org/workshops/) -- in particular the [Statnet workshop on `ndtv`](https://statnet.org/workshop-ndtv/) and the [statnet workshop on `tergm`](https://statnet.org/workshop-tergm/).  For `EpiModel` please see the [EpiModel organization site](https://www.epimodel.org/)



## Code of Conduct
  
Please note that the `networkDynamic` project is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.

