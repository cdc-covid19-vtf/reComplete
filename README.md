## reComplete Repo

This repo is for generating the 'race ethnicity completeness by jurisdiction' slide for the Health Equity Report (HER). The HER presentation slide is made on Wednesday and the data is pulled on Tuesday. The plot is of the US along with five Federal Entities (BoP, DC, DoD, IHS, and VHA) and eight territories (AS, FM, GU, MH, MP, PR, PW, VI) and represents XXX. 

The [code](https://github.com/cdc-covid19-vtf/reComplete/blob/master/02_code/reCompletePlots.R) takes in one input file using `readxl::read_excel` as the input file is an excel file. This excel file has 15 columns of which the code uses four columns (Jurisdiction, 'Jurisdiciton Name',  'Valid, %...8', and 'Valid, %...14'). The 'Valid, %...' is the level of completeness and what generates the particular color code (<20%, 20.1 - 40.0%, 40.1 - 60.0%, 60.1 - 80.0%, and > 80%). The code assumes that the input file stayes the same but if the input file changes than this would need to be adjusted in the code. The input file will be updated weekly and if the file name changes, then that should be adjusted in the code. For ease of use; place the input file into the directory called 01_data in the reComplete repo. 

**How to access and use the code**

```
# clone the repo. I tend to clone to my desktop. 
git clone https://github.com/cdc-covid19-vtf/reComplete.git

# open the code in R (reCompletePlots.R) and confirm path using the here package
# this should point into the reComplete repo (i.e. C:/Users/ptx4/Desktop/reComplete)
here::here() 

# read in the data - lines 7 - 13 

# read in the function rePlot. The function takes 4 paramters: 
dataFrame, colName, ImageID, imageTitle.
* dataFrame is the excel input file 
* colName is either 'Valid, %...8'/overall or  'Valid, %...14'/two weeks
* imageID - unique identifier for the plots (e.g. overall or two weeks)
* imageTitle - title for plot (e.g. Cumulative or Last Two Weeks)

# Generate the plots
# Two plots are generated (pdf and png) per function call and will be
# located in 03_figures or the reComplete repo. 

rePlot(dataFrame = reComplete, colName = "overall", imageID = "overall",
       imageTitle = "Cumulative")
rePlot(dataFrame = reComplete, colName = "twoWeeks", imageID = "twoWeeks",
       imageTitle = "Last Two Weeks")
```
