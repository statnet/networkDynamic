# `networkDynamic`:  Dynamic Extensions for Network Objects

[![Build Status](https://travis-ci.org/statnet/networkDynamic.svg?branch=master)](https://travis-ci.org/statnet/networkDynamic)
[![Build Status](https://ci.appveyor.com/api/projects/status/rmj7f1xmpikh2243?svg=true)](https://ci.appveyor.com/project/statnet/networkDynamic)
[![rstudio mirror downloads](https://cranlogs.r-pkg.org/badges/networkDynamic?color=2ED968)](https://cranlogs.r-pkg.org/)
[![cran version](https://www.r-pkg.org/badges/version/networkDynamic)](https://cran.r-project.org/package=networkDynamic)

`networkDynamic` is a network data management package that provides tools to import, transform and extract relational data with timing information from various data structures (matrices, spell lists, toggles, etc).  The software includes simple interface routines to facilitate the handling of network objects that feature complex intertemporal data. 

This package is a part of the [statnet](https://statnet.org) suite of packages for network analysis, so it is designed to work seamlessly with all of the packages in that suite.  `networkDynamic` extends the capability of statnet's `network` package to handle temporal information, and it is the data management foundation used by the higher level packages: 
* [`tsna`](https://github.com/statnet/tsna) Exploratory data analysis and summary statistics for temporal networks
* [`ndtv`](https://github.com/statnet/ndtv) Temporal network graphics and animation
* [`tergm`](https://github.com/statnet/tergm) Statistical analysis of temporal networks with TERGMs
* [`ergmgp`](https://github.com/statnet/ergmgp) Modeling continuous time graph processes with ERGM equilibria
* [`EpiModel`](https://www.epimodel.org/) Epidemic modeling on dynamic networks

Self-guided training materials can be found on the [statnet workshop page](https://statnet.org/workshops/) -- in particular the [statnet workshop on `ndtv`](https://statnet.org/workshop-ndtv/) and the [statnet workshop on `tergm`](https://statnet.org/workshop-tergm/).  For `EpiModel` please see the [EpiModel organization site](https://www.epimodel.org/)
