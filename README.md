# soc_learning_network
## Datasets and scripts for paper "Network disance and centrality shape social learning in the classroom"

This folder contains all the data and the scripts to reproduce the analyses and the figures reported in the manuscript "Network Centrality Shape Social Learning in the Classroom", accepted for publication at “School Psychology”.

The script are written in R, using RStudio, so the most straitghtforward way to exectute the scripts is to import the entire folder as a RStudio project. If exectuing the scripts in basic R, make sure to replace 

The most comprehensive dataset, containing both the behavioral data and the social network data relative to the participants, is stored in the file ‘clean_data/BEAST_data.csv’ . In this dataset, each row contains the data from one round of the experimental game: the decisions of the player, and the information about the classroom peer that they observed in that round.

The dataset stored in the file ‘clean_data/BEAST_data_meanS.csv’ is a reduced version on the complete dataset, where data regarding the amount of social information used by participants in each in treatment is averaged and in one row (see the methods of the paper: we refer to s as the adjustment in each round, and to S as the mean adjustment over all the rounds of an experimental treatment). This data is used for some of the plots and for the analysis on the averaged data.

Further version of the datasets whith excluded participants are also provided. The number of participants present in is version can be gauged from the number present in the file name (e.g, BEAST_data_175 contains 175 participants). For more information about exclusion criteria, please conslut the Methods section of the paper.

Finally, the dataset ‘clean_data/full_dataset_egonets.csv’, contains the information of the social networks obtained with second order nominations (“friends of friends”). The data from these networks are analyzed and compared to the ‘baseline’ social networks’ in the script ‘scripts/compare_networks.r’.

The main analaysis of the paper are performed in the file ‘scripts/analysis_main_text.r’. An exception is the mediation analysis, which was run using SPSS.

The SPSS script is available in the file ‘scripts/MLMED_Beta_2.sps’. The mediation was run with the data contained in the file file ‘clean_data/BEAST_data_meanS.csv’.

For any question or clarification regarding this repository, please contact the responsible researcher Andrea Gradassi, at a.gradassi@uva.nl.
