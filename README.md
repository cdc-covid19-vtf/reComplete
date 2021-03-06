## reComplete Repo

Automated Process for generating the Race/Ethnicity Slide via R.

*Links go to sharepoint or github, must have access to successfully follow links*

This repo is for generating the 'race ethnicity completeness by jurisdiction' 
slide for the Health Equity Report (HER). The HER presentation slide is made on Tuesday, the data is pulled on Tuesday and the HER presentation occurs on Wednesday. The weekly updated data are located within the [Metrics subfolder](https://cdc.sharepoint.com/:f:/r/teams/VaccinePlanningUnit-COVID19/Shared%20Documents/Data%20Synthesis%20and%20Reporting/Routine%20Reporting/Health%20Equity%20Report/Metrics?csf=1&web=1&e=mKNvao) of the [HER folder](https://cdc.sharepoint.com/:f:/r/teams/VaccinePlanningUnit-COVID19/Shared%20Documents/Data%20Synthesis%20and%20Reporting/Routine%20Reporting/Health%20Equity%20Report?csf=1&web=1&e=hDQOnq). The plot is of the US along with five Federal Entities (BoP, DC, DoD, IHS, and VHA) and eight territories (AS, FM, GU, MH, MP, PR, PW, VI) and represents the level of completeness of race/ethnicity reporting to CDC among people receiving COVID-19 vaccinates by jurisdiction. 

The [code](https://github.com/cdc-covid19-vtf/reComplete/blob/master/02_code) takes in one input file using `readxl::read_excel`. The [excel file](https://github.com/cdc-covid19-vtf/reComplete/tree/master/01_data) has 15 columns of which the code uses four columns (Jurisdiction, 'Jurisdiction Name', 'Valid, %...8', and 'Valid, %...14'). The 'Valid, %...' is the level of completeness for overall or the last two weeks and what generates the particular color code (<20%, 20.1 - 40.0%, 40.1 - 60.0%, 60.1 - 80.0%, and > 80%) for each state, federal entities, and territories displayed in the produced images. A [previously used excel file](https://github.com/cdc-covid19-vtf/reComplete/blob/master/01_data/Previous_RE_BiweeklyJurisdictionCompleteness_4_2_2021.xlsx ) is provided to allow one to test the code and compare to [previously made images](https://github.com/cdc-covid19-vtf/reComplete/blob/master/03_figures/Previous_REComplete_Combined.2021-04-02.pdf). The code assumes that the input file stays the same but if the input file changes than this would need to be adjusted in the code (e.g. file name change or column header change). For ease of use place the updated input file into the directory called 01_data in the reComplete repo. Example output images are located [in this folder](https://github.com/cdc-covid19-vtf/reComplete/tree/master/03_figures) and an example combined image is located [here](https://github.com/cdc-covid19-vtf/reComplete/blob/master/03_figures/Previous_REComplete_Combined.2021-04-02.pdf).

Note that you may need to adjust the placement of the squares for the federal entities and the territories if the output image width and height is adjusted. If this is the case, please adjust the x and y in the addSquare function which is found in each .R code. 

**How to access**

```
# clone the repo. I tend to clone to my desktop. 
git clone https://github.com/cdc-covid19-vtf/reComplete.git
```

There are two versions of code available in 01_code sub-folder:
- [reCompletePlots.R](https://github.com/cdc-covid19-vtf/reComplete/blob/master/02_code/reCompletePlots.R): makes two separate plots (overall and last two weeks) as a .pdfs and .pngs
- [reCompleteAsOneFig.R](https://github.com/cdc-covid19-vtf/reComplete/blob/master/02_code/reCompleteAsOneFig.R): makes one plot with the two images combined as a png and pdf

**How to use the reCompletePlots.R code**

```
# Open code or open reComplete.Rproj 
# confirm path using the R here package
# should point into the reComplete repo (e.g. C:/Users/ptx4/Desktop/reComplete)
here::here() 

#copy data into this directory
~/Desktop/reComplete/01_data

# read in the data and the rePlot function - lines 7 - 22 

# The rePlot function takes 4 paramters: 
       *dataFrame*, *colName*, *imageID*, *imageTitle*
* dataFrame is the excel input file 
* colName is either 'Valid, %...8'(overall) or  'Valid, %...14'(two weeks)
* imageID - unique identifier for the plots (e.g. overall or two weeks)
* imageTitle - title for plot (e.g. Cumulative or Last Two Weeks)

# Generate the plots
# Two plots are generated (pdf and png) per function call and will be
# located in 03_figures of the reComplete repo. 

rePlot(dataFrame = reComplete, colName = "overall", imageID = "overall",
       imageTitle = "Cumulative")
rePlot(dataFrame = reComplete, colName = "twoWeeks", imageID = "twoWeeks",
       imageTitle = "Last Two Weeks")
```

**How to use the reCompleteAsOneFig.R**

```
# Open code or open reComplete.Rproj 
# confirm path using the R here package
# should point into the reComplete repo (e.g. C:/Users/ptx4/Desktop/reComplete)
here::here() 

#copy data into this directory
~/Desktop/reComplete/01_data

# highlight all code and hit 'Run'
# output images will be reComplete/03_figures

```
