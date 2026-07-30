# README

Firstly, the directories are split by analyses/, funcitons/ data/. 

## Analyses
Most of the analyses are written in Quarto. They can be rendered with the following command in terminal, while in the analyses directory:
`quarto render *your_analyses_script*.qmd --to html`

### `add_new_g2g_sites.R`

* This script makes the configuation file for running NRW sites in G2G.
* Note, that at the moment I filter sites automatically using `wales_cdata`, which is the `data/cdata/catchment_cdata_EA-NRW.csv` file. This should be changed, because the sites Brynkinalt and Three Cocks are filtered using these though they are actually new sites to be added.
* I also manually changed the G2G coordinates for several sites after looking on QGIS  (see lines 58-67). 
* This script also writes the metadata: `../data/new_g2g_sites_g2g_coords.csv`. This includes whether it has flow data or not, threshold data and the G2G coords (and original xy).

### `convert_new_site_levels.R`

* This script reads in the XML ratings sent by Sam `../data/g2g_data/observed_new_sites/ratingcurves_NRW_ExtraSites.xml` for new sites
* I converted the levels for new sites to flows and it writes `../data/g2g_data/observed_new_sites/2018_2021_flows_new_sites.csv` which has the same formatting convention at G2G outputs in terms of dates and time steps.

### `convert_threshold_levels.R`

* This script reads in the XML rating curves `../data/ratingcurves_NRW_May2026.xml` for NRW thresholds and converts `../data/nrw_level_threshold.csv` to flows  `../data/nrw_alert_warning_flow_thresh.csv` for alerts, warnings and severe warnings.

### `gridded_output_plots.qmd`

* This script was mainly used for plotting the cutouts of Login and Redbrook for presentation.
* The code could be reused for doing cutouts of whichever catchment of choice

### `new_sites_ungauged_test.qmd`

* This script reads in both the original run, and the new sites run.
* It calculates PODFAR (with & without tolerance) for the new and old sites and plots by map.
* It also breaks down PODFAR (with tolerance) by storm event, and then again by storm and catchment size.
* I have been unable to run the GLMM because I don't have the actual catchment boundaries for the new sites, but that is something that could be interesting to see if there is any interaction with being a new site and the rainfall, gauge distance or slope predictors affecting POD?

### `NRW_Thresholds_plots.qmd`

* This script generates a pdf when rendered with `quarto render *NRW_Thresholds_plots.qmd*.qmd --to pdf` which plots the PODFAR (with tolerance) across the three different NRW thresholds (alert, warning and severe warning).
* Note that the number of sites with threshold is ~ 52, so it has a lower sample size than when plotting QMED exceedance.

### `plot_event_hydrographs.R`

* This script plots the orginal sites hydrographs
* Uses two windows: 7 days either side of event and 36 hours either side of the event
* Plots qt values from  `../data/qt_grids`
* Plots rainfall (incl accumulating) from `../data/catchment_average_rg_precip.csv`
* Plots whether the site is sufi or not from `../data/sites_list_final.csv`
* Saves the pngs to `../data/figures/wales_events_2018_2021_hydrographs_7_day` & `../data/figures/wales_events_2018_2021_hydrographs_36_hours`


### `sim_sufi_qt_exceedance_pre_2022.qmd`

* Monster qmd file
* Has 90% of the work done
* Extracts the g2g output and modelled 2018-2021
* 9 events
* Extracts peak magnitude at every event
* First plots are the QT exceedance maps
* then qt heatmap for sim and sufi
* Calcualtes peak magnitude error and plots those
* Extracts PME values and writes to `../data/pme_sim_sufi_events_2018_2021.csv`
* Finds which sites have old vs new ratings and does boxplot to see if there is a difference in PME 
* Loads in different predictors for the GLMM:
  * Slope heterogeneity from `"../data/rasters/wales_dem_50m.tif"`
  * Uses `"../data/20250610_g2g_ffc_v2_0/fullcats_g2g_ffc_v2_0_loc.shp"` which is the catchment boundaries shape file
  * Distance to rain gauge using `"../data/rg_info_2022.csv"` which are the rain gauges and the coordinates
  * `"../data/catchment_average_rg_precip.csv"` loads in average gauge interpolated rainfall and sums across each event window
  * `"../data/rasters/suburban_LCM2019.asc` & `"../data/rasters/urban_LCM2019.asc"` which are the urban and suburban LCMs
* Calculates PODFAR with tolerance, runs GLMM and plots  for NRW thresholds (lines 779 - 921) and QMED threshold (lines 934 - 1237) across each site and event.


## Functions

### `extract.g2g.grid.R`

* reads in ascii files, extracts points within those grids using X and Y coordinates.
* `make.qt.csv`

Main use of this is this recurring block of code:

```{r}
qt_grid_paths <- mixedsort(sort(file.path("../data/qt_grids", list.files("../data/qt_grids")))) ## create relative paths
qt_val <- sub("_.*", "", mixedsort(sort(list.files("../data/qt_grids"))))
qt_grid_list <- lapply(qt_grid_paths, read.ascii)
names(qt_grid_list) <- qt_val ## name by qt
qt_dt <- make.qt.csv(qt_grid_list, cdata = wales_cdata, write = TRUE)
```

 This gets file names, extracts qt grids with read.ascii, then the list is applied to make.qt.csv and can use write = TRUE to generate csv

The other functions are used for the gridded outputs, specifically aligning the qt grids and max flow grids, then extracting the qt vals exceeded from the max flow.

### `load.g2g.runs.R`

This loads the original grid to grid runs (does not apply to new runs because paths are only for original runs)
`make.events` subsets both mod and obs into the correct time spans for each storm. There is a lot of redundant code in this function but mainly as optional args


### `make.hydrograph.R`
* utility functions: `accum.rainfall` & `accum.river.vol` generate the cumulative rainfall and river volume time series
* `make.hydrograph.sim.sufi` is the plotting function for generating hydrographs
  * this takes the event data, which is a nested element from `make.events` (i.e. `events_list[["Callum"]])` ), extracts the grid to grid id with a character string, takes the storm name (i.e. "Callum"), cdata is the metadata sheet (assigned `wales_cdata`), qt.data (this is the `qt_dt` written by the code on lines 82-88 on this readme) and sufi which is the `../data/sites_list_final.csv`. the g2g.id is used to extract all the data from these objects
  
### `obs.mod.stats.R`

* these are the functions for any calculations
* `peak.magnitude.error` which takes one nested list element of the `make.events` output. again this would look like `event_list[["Callum]]` Returns a list of the pme's
* `extract.peak.discharge` again takes one nested list element of `make.events` and qt vals
* extracts peak discharge, can return QT as a value if inputted.
* qt is the qt_dt is again the datatable from lines 82-88
* `thresh.exceed`

