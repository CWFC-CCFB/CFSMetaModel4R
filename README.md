[![License: LGPL v3](https://img.shields.io/badge/License-LGPL%20v3-blue.svg)](https://www.gnu.org/licenses/lgpl-3.0) [![R-CMD-check](https://github.com/CWFC-CCFB/CFSMetaModel4R/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/CWFC-CCFB/CFSMetaModel4R/actions/workflows/R-CMD-check.yaml)

## CFSMetaModel4R 

An R package allowing the fitting of meta-models from existing tree-level models

It allows running tree-level simulations on 

- Artemis-2009 (through the CAPSIS platform)
- Open Stand Model (OSM)
- Forest Vegetation Simulation, British-Columbia variant (FVS-BC)

These models are called and the simulations are run through HTTP requests. 

This R package depends on the J4R package and includes the repicea-metaModel Java library, which allows the fitting of the meta-models.


