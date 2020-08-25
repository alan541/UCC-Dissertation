# UCC-Dissertation

This is intended as an explanatory guide to the files listed here, which are all the files necessary to full my full dissertation project.

The main original data was obtained with thanks from https://github.com/JeffSackmann/tennis_atp, under licence from https://creativecommons.org/licenses/by-nc-sa/4.0/, for non commercial use only. Some small addtional data was added occasionally from https://www.atptour.com.  The betting odds data was sourced from www.tennis-data.co.uk. 

Note that changes and adaptation were made by me to the data to make it ready for use with the program, and the final data set into for initial further manipulation by the main program is 'Project_pt5.  The main program code is 'Dissertation Main Code', and the final data set produced by that code is 'Project_pt7'.  The initial training and testing sets immediately randomly created by that code are 'LRtrain' and 'LRtest'.

The final odds for use is contained in 'Project_pt5', but if you wish to see the background data sets used, and the code used for manipulation of the data for use with our program, it is contained in the csv files 'Odds1', 'Odds2', 'Odds3' & 'Odds4' and the R code files 'Odds' and 'Odds2'.

Please note that to run the full 'Dissertation Main Code', all that is needed is the original data file 'Project_pt5'. The other completed files are provided is case the user wished to jump forward (but see note below also).

Separate code was created in google colab for analysis by Artificial Neural Networks, and this code is entitled 'Dissertation (Full Data)' and Dissertation '(Filtered Data)'. The files utilised directly by the former, and provided, are 'DLtrain', 'DLtest', and 'tenniste'. The files utilised by the latter, and provided, are 'DLMtr', 'DLmte' and 'minnotest'.

Separate code was also created in google colab (with also some preparatory code in 'Dissertation Main Code') for the reinforcement learning test section. The files utilised and provided are 'RLprobs' and 'RLw_l'.

Note: if the user 'jumps ahead' and reads in directly the final data file 'Project_pt7' or the initial train/test files, 'LRtrain', 'LRtest', please beware that it will be necessary to delete the first column (as a redundant first column is created when they were written to csv).  However, this is not necessary for any csv files provided above that are ultimately used and that are read directly into any of the code (e.g. 'DLtrain'), as either the files or the code itself has been adapted to cater for this.
