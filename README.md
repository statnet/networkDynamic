# `networkDynamic`:  Dynamic Extensions for Network Objects

[![Build Status](https://ci.appveyor.com/api/projects/status/rmj7f1xmpikh2243?svg=true)](https://ci.appveyor.com/project/statnet/networkDynamic)
[![rstudio mirror downloads](https://cranlogs.r-pkg.org/badges/networkDynamic?color=2ED968)](https://cranlogs.r-pkg.org/)
[![cran version](https://www.r-pkg.org/badges/version/networkDynamic)](https://cran.r-project.org/package=networkDynamic)

`networkDynamic` is a network data management package that provides tools to import, transform and extract relational data with timing information from various data structures (matrices, spell lists, toggles, etc). The software manages temporal information for both vertices and edges, including presence/absence (which allows for changes in network size) and discrete or continuous attributes.  Both directed and undirected networks are supported.

This package is a part of the [Statnet](https://statnet.org) suite of packages for network analysis, so it is designed to work seamlessly with all of the packages in that suite. `networkDynamic` extends the capability of Statnet's `network` package to handle temporal information, and it is the data management foundation used by the higher level packages: 
* [`tsna`](https://github.com/statnet/tsna) Exploratory data analysis and summary statistics for temporal networks
* [`ndtv`](https://github.com/statnet/ndtv) Temporal network graphics and animation
* [`tergm`](https://github.com/statnet/tergm) Statistical analysis and simulation of temporal networks with TERGMs
* [`ergmgp`](https://github.com/statnet/ergmgp) Modeling continuous time graph processes with ERGM equilibria
* [`EpiModel`](https://www.epimodel.org/) Epidemic modeling on dynamic networks

## Docs and Examples

`networkDynamic` [package vignette](https://cran.r-project.org/web/packages/networkDynamic/vignettes/networkDynamic.pdf)

Tutorials on using `networkDynamic` in data analysis workflows include the Statnet workshops [Temporal network tools in statnet: networkDynamic, ndtv and tsna](https://statnet.org/workshop-ndtv/ndtv_workshop.html) [Temporal Exponential Random Graph Models (TERGMs) for dynamic networks](https://statnet.org/workshop-tergm/)

Self-guided training materials for all of the Statnet packages can be found on the [statnet workshop page](https://statnet.org/workshops/) .  For `EpiModel` please see the [EpiModel organization site](https://www.epimodel.org/)



## Citation and License

This software is distributed under the GPL-3 license.  It is free, open source, and has the attribution requirements (GPL Section 7) at
http://statnet.org/attribution

To cite the package ‘networkDynamic’ in publications use:

  Butts C, Leslie-Cook A, Krivitsky P, Bender-deMoll S
  (2023). _networkDynamic: Dynamic Extensions for Network Objects_. R package version 0.11.3,
  <https://CRAN.R-project.org/package=networkDynamic>
  
This work was supported by grant R01HD68395 from the National Institute of Health.


## Code of Conduct
  
Please note that the `networkDynamic` project is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.

