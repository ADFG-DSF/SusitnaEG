# SusitnaEG

This repository the run reconstruction and stock assessment for Susitna River Chinook salmon.

The network location for this repository is on Anchorage DSF S drive: "S:\RTS\Reimer\SusitnaEG".

The most recent report associated with this analysis can be found at <http://www.adfg.alaska.gov/FedAidPDFs/FMS20-01.pdf>.

The file/folder structure for this analysis is as follows:

-   data/: R data sets created by the R codes located in data-raw/.

-   data-raw/: raw data and data preparation/cleaning functions for the analysis. The main files in this folder are SusitnaEG age.xlsx, SusitnaEG Ha_post95.xlsx, SusitnaEG Hm.xlsx, SusitnaEG regs.xlsx, SusitnaEG sonar.xlsx, SusitnaEG survey.xlsx, and SusitnaEG weirs.xlsx which contain the raw data and need to be updated annually. The other date files in this directory are used in the analysis but represent early time periods and do not require an annual update.

-   docs/: this folder contains the html file which will be hosted on GitHub pages.

-   functions/: table, figure and helper functions for the SR analysis.

-   markdown/: RMarkdown and quarto document associated with this analysis. The 2020 report was prepared using markdown files included in this folder. In addition annual updates to the run reconstruction component (which inform area staff about annual escapements) are including in this folder.

-   models/: Jags code associated with this analysis. The file model_SuChin.R is the main jags code.

-   posts/: Posterior samples from the model. These files are large and not available on GItHUb however the code and data ro replicate each posterior should be available. The posterior sample are stored in the network location indicated above.

-   scripts/: R code to run analysis. The file script_SuChin.R is the main jags code.
