# RedDrumIBM
R script for Individual-based model I created for my Master's thesis. This project aimed to forecast the genetic influences of stocking to guide future stocking strategies for red drum in South Carolina.
The following packages contain the R script I used to run simulations for my Master's Thesis. This project aimed to forecast the genetic influences of stocking to guide future stocking strategies for red drum in South Carolina.
Packages included: "RDlandscape.R", "RunReplicate.R", "Sweepstakes.R", "Compile diversity files.R", "diversity.R", "DiversityStats.R", "landscape.sample.R", "Ne-sample.R", and "RunExperiment.R"

RDlandscape.R - Sets up the metapopulation landscape and contains input parameters for survival and reproduction. This script also sets the size for the total population, as well as the size of each local population in the landscape.

RunReplicate.R - Is the main file used to run simulations. The length of simulations, sample sizes, number of hatchery broodstock, contribution of stocked fish per year-class, and time points to estimate genetic diversity metrics are all editable in this script.

Sweepstakes.R - This allows the user to set the proportion of adults that successfully spawn each year.

diversity.R - contains the detailed functions used to calculate genetic diversity metrics.

DiversityStats.R - contains a simplified version of "diversity.R" to determine which genetic diversity metrics the user wishes to include in the output file.

Compile diversity files.R - compiles genetic diversity results into a useable output file.

landscape.sample.R - designates which stages in a local population to sample from in order to calculate genetic diversity metrics. We did this to be able to narrow in on specific year-classes.

Ne_sample.R - contains unique code to designate the appropriate stages to include for estimates of effective population size.

RunExperiment.R - is a simplied script that calls on all the previous files and sets the overall number of replicates to run.
