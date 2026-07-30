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






TODO:
- barplot shows new -> sim_old -> sufi_old
- barplot shows qmed -> q5 -> nrw alert (just for sites we have alert data for  SUFI OLD)
- reorder bar plots so it goes from bottom up: hit, correct reject, near miss, NA, close false alarm, false alarm, miss 
-  move stuff onto S drive
-  do hydrographs if time