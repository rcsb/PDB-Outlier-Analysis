# PDB-Outlier-Analyses
## Directory structure:
### ./R/ contains the primary R code to run the calculation
* ./R/Util/ contains the fundamental R modules to run kernel calculation
### ./Data/ contains primary data in three subdirectories
* ./Data/Data_clean contains the PDB data sets used to start the camputation. The brief meaning of each data set is described in ./R/items.list-selected-meaning.csv, and simple filename list in ./R/items.list.
* ./Data/Density_hopt/ contains the estimated probability density for each data set, by using Gaussian kernel at bandwidth hopt calculated based on Equation (2) of the manuscript. 
* ./Data/Outlier_hopt/ contains the outlier identification by PDR method. 
### ./Images is an empty folder for use of creating graphs. 
### ./Compute_different_bandwidth/ contains R code and data for comparison of different bandwidth selection as described in Supplementary data.
* ./Compute_different_bandwidth/Data/ is similar to ./Data/ except that, (1) ./Data/Data_clean/ contains only a simple random sample of 10000 of estimated Wilson B factors. (2) ./Data/Density_hopt/ and ./Data/Outlier_hopt/ contains calculated density estimation and PDR outliers based on different bandwidths as described in Supplementary data. 

## How to run R code on data
### Compute probability density estimate, PDR outliers, and MPR on all 22 original data sets: 
#### (1) run p1_estimateDensity_all.R
* Input: "items.list" in the same folder that lists the data item name
all data files under ./Data/Data_clean/
* Outout: density estimate under ./Data/Density_hopt/
* Require: "parallel" package for R; Linux operation system. 

##### *Attention: The code use linux parallel mode. The parralle calculation will NOT work on windows machine due to different mechnisms of variable value transfer.
However, a non-paralle mode is also implemented in the code, although it will be very slow for big PDB data.
To use non-parallel mode, please do the following change in p1_estimateDensity_all.R
* comment out line of "cluster <- ..."
* comment out line of "stopCluster(cluster)"
* make the change of the following line from
data <- estimateDensityDF(cluster=cluster, data_ori, 2)
to
data <- estimateDensityDF(cluster=FALSE, data_ori, 2)

For how to use parallel mode on Windows machine, please refer to online help on how to transfer variable values from one function to another.

Please also note that the script may fail to run for the first time, with the following error message:
"Error in checkForRemoteErrors(val) : 
  X nodes produced errors; first error: could not find function XXXX"
Should this happen, please simply re-run the program from the first line to the last line within the same R session.  

##### *Note: the default kernel is Gaussian kernel and default bandwidth is from Equation (2) of the manuscript.
If one wants to change bandwidth, such change can be directly make change in "./R/Util/outlier.R" file
If one wants to Uniform/Rectangle kernel, one should make change to the header of p1_estimateDensity_all.R tosource "./R/Util/outlier_uniform_kernel.R"
If one wants to calculate the variance of the estimatiion, one should use "./R/Util/outlier_with_variance.R" that outputs variance as well, but this file cannot be directly used by p1_estimateDensity_all.R without some modification. Please read the comments in the file. 
For using other different kernels or bandwidths, please refer to the "compute probability density estimate with different dandwidth sections" below 

#### (2) run p2_findOutlier_all.R
* Input: density estimate under ./Data/Density_hopt/
* Output: PDR outliers identification under ./Data/Outlier_hopt/
For each data set, the output tab-deliminated file use column #4 and #5 to indicate whether a particular datum is PDR outlier or not.  

#### (3) run p3_drawOutlier_all.R
* Input: PDR outliers identification under ./Data/Outlier_hopt/
* Output: Graphs similar to Figure 1 under ./Image/

#### (4) run p4_summarizeOutlier.R
* Input: PDR outliers identification under ./Data/Outlier_hopt/
* Output: A summary file "summary.csv" with all statistics
* Require: "moments" package for R

#### (5) run p5_MPR.R
* Input: PDR outliers identification under ./Data/Outlier_hopt/
* Output: Most Probable Range (MPR) for each data set, written into file "MPRs.csv"

### Compute probability density estimate with different dandwidth sections
Please note that only one data set, Wilson B factor estimate, is used to demonstrate the calculation and comparision. Only 10000 samples are used for reasonable speed of the calculation.
#### (1) run p1_sampleData.R
* Input: original data file of ./Data/Data_clean/wwpdf_overall.WilsonBestimate
* Output: 10000 sample at ./Compute_different_bandwidth/Data/Data_clean/wwpdf_overall.WilsonBestimate

#### (2) run p2_getOtherBandwidth.R
* Input: ./Compute_different_bandwidth/Data/Data_clean/wwpdf_overall.WilsonBestimate
* Output: size of diffrent fixed-length bandwidths in ./Compute_different_bandwidth/Data/Data_clean/wwpdf_overall.WilsonBestimate_h
* Require: "kedd" package for R

#### (3) run p3_estimateDensity_all.R
* Input: ./Compute_different_bandwidth/Data/Data_clean/wwpdf_overall.WilsonBestimate
and ./Compute_different_bandwidth/Data/Data_clean/wwpdf_overall.WilsonBestimate_h
* Output: density estimate by various bandwidths, under ./Compute_different_bandwidth/Data/Density_hopt/
* Require: "parallel" package for R; Linux operation system.

#### (4) run p4_findOutlier_all.R
* Input: density estimate by various bandwidths, under ./Compute_different_bandwidth/Data/Density_hopt/
* Output: PDR outliers by various bandwidths, under ./Compute_different_bandwidth/Data/Outlier_hopt/
##### (4.1) run p41_estimateDensity_kNN.R to calculate density and PDR outliers from kNN kernel, written to ./Compute_different_bandwidth/Data/Outlier_hopt/
##### (4.2) run p42_estimateDensity_variable.R to calculate density and PDR outliers from variable kernel, written to ./Compute_different_bandwidth/Data/Outlier_hopt/

#### (5) run p5_summarizeOutlier.R
* Input: density estimate and PDR outliers by various bandwidths/kernels, under ./Compute_different_bandwidth/Data/Outlier_hopt/
* Output: "summary_h.csv" record all statistics for different bandwidths/kernels
* Require: "moments" package for R

#### (6) run p6_drawCompH.R
* Input: density estimate and PDR outliers by various bandwidths/kernels, under ./Compute_different_bandwidth/Data/Outlier_hopt/
* Output: "h_diff.png" graph to compare several bandwidths/kernels.
