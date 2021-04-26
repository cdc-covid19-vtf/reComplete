## reComplete Repo

This repo is for generating the 'race ethnicity completeness by jurisdiction' 
slide for the Health Equity Report (HER). The HER presentation slide is made on Tuesday, the data is pulled on Tuesday and the HER presentation occurs on Wednesday [note - confirm]. The weekly updated data are located within the [Metrics subfolder](https://teams.microsoft.com/_#/files/Data%20Synthesis%20and%20Reporting?threadId=19%3A1aca0b6a9d6b4cabafd7a0affe1f6b54%40thread.skype&ctx=channel&context=Metrics&rootfolder=%252Fteams%252FVaccinePlanningUnit-COVID19%252FShared%2520Documents%252FData%2520Synthesis%2520and%2520Reporting%252FRoutine%2520Reporting%252FHealth%2520Equity%2520Report%252FMetrics) of the [HER folder](https://teams.microsoft.com/_#/files/Data%20Synthesis%20and%20Reporting?threadId=19%3A1aca0b6a9d6b4cabafd7a0affe1f6b54%40thread.skype&ctx=channel&context=Health%2520Equity%2520Report&rootfolder=%252Fteams%252FVaccinePlanningUnit-COVID19%252FShared%2520Documents%252FData%2520Synthesis%2520and%2520Reporting%252FRoutine%2520Reporting%252FHealth%2520Equity%2520Report) (must have access to MS Teams). The plot is of the US along with five Federal Entities (BoP, DC, DoD, IHS, and VHA) and eight territories (AS, FM, GU, MH, MP, PR, PW, VI) and represents the level of completeness of race/ethnicity reporting to CDC among people receiving COVID-19 vaccinates by jurisdiction. 

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

#adjust cRange and tRange to reflect cumulative date range and last two weeks. 
cRange <- "12/15/2020 - 04/02/2021"
tRange <- "04/06/2021 - 04/02/2021"

# highlight all code and hit 'Run'
# output images will be reComplete/03_figures

```
