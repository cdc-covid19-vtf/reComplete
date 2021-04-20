library(readxl)
library(dplyr)
library(ggplot2)
library(grid)
library(magrittr)

# skip the first three rows, which is extra information. 
# this assumes the data is the first tab; if not then specify the tab 
reComplete <- readxl::read_excel(
  here::here("01_data",
             "RE_BiweeklyJurisdictionCompleteness.xlsx"), skip = 3)

# I do this for my own sanity, because I had a hard time remembering which were
# the data columns of interest. One could skip this step. 
reComplete <- reComplete %>%
  dplyr::rename(overall = `Valid, %...8`) %>%
  dplyr::rename(twoWeeks = `Valid, %...14`)

# as a function so one can just specify the column header, imageID, and 
# imageTitle as input parameters.This allows the creation of two maps: 
# Overall and Past two weeks.
rePlot <- function(dataFrame, colName, imageID, imageTitle) {
  
  # created pdf image that will be located in the 03_figures sub-directory
  # imageID, is some unique identifier that the user can specify. This is to 
  # differentiate between the overall and twoWeeks plots  
  
  pdf(file = here::here("03_figures", paste("REComplete", imageID,
                                            format(Sys.time(), "%Y-%m-%d"),
                                            "pdf", sep=".")),
      width = 12, height = 8)
  
  # how to save both png and pdf from stack overflow
  #https://stackoverflow.com/questions/26232103
  #also added dev.control('enable') along with a dev.copy call at end of script
  a<-dev.cur()
  
  #save as png 
  png(file = here::here("03_figures", paste("REComplete", imageID,
                                            format(Sys.time(), "%Y-%m-%d"),
                                            "png", sep=".")),
      width = 12, height = 8, units = "in", res = 100)
  
  dev.control("enable")
  
  #convert column as numeric because it is read in as character
  dataFrame$twoWeeks<- as.numeric(dataFrame$twoWeeks)
  
  #to get a percent and use plot_usmap by converting to column name 'state'
  dataFrame <- dataFrame %>%
    dplyr::mutate(colName  = dataFrame[[colName]] * 100) %>%
    dplyr::rename(state = "Jurisdiction Name")
  
  # switch California two week value to NA; they are not legally supposed 
  # report RE for the last two weeks. Other states include Minnesota, Texas, 
  # and Vermont. In this example input data, MN, TX, and VT are already dealt
  # with, in the future you may need to extend this if statement if necessary. 
  # This uses the colName input parameter to distinguish if this is necessary
  
  if (colName == "twoWeeks"){
    dataFrame$colName[8] <- 0.00}
  else {
    #skip
  }
  
  # provide binned ids for each state/jurisdiction based on percent for coloring
  dataFrame <- dataFrame %>%
    mutate (completeOne = case_when (
      colName == 0.00 ~ "NA",
      colName < 20.0 ~ "< 20.0%",
      colName >= 20.1 & colName <= 40.0 ~ "20.1 - 40.0%",
      colName >= 40.1 & colName<= 60.0 ~ "40.1 - 60.0%",
      colName >= 60.1 & colName < 80.0 ~ "60.1 - 80.0%",
      colName >= 80.0 ~ "> 80.0%") )
  
  # get the federal entity rows for plotting square boxes
  fedEnts <- dataFrame %>%
    filter_at(vars(Jurisdiction), any_vars(. %in% c("BP2",  "DCA",
                                                    "DD2", "IH2", "VA2")))  
  # get territory rows for plotting square boxes
  territory <-  dataFrame %>%
    filter_at(vars(Jurisdiction), 
              any_vars(. %in% c("ASA", "FMA", "GUA", "MHA",
                                "MPA", "PRA", "RPA", "VIA")))
  
  # convert state names to abbreviations for using plot_usmap
  dataFrame$state[48]<- "New York"
  
  # convert state names to abbreivations for using plot_usmap
  dataFrame$state <- state.abb[match(dataFrame$state, state.name)]
  
  # remove values that are NA, which includes federal entities, territories, and
  # pharmacies. This removal is done to use the plot_usmap function.
  dataFrame <- dataFrame %>%
    filter(state!="NA")
  
  # to be able to use drop = F; need to tell it the levels
  dataFrame$completeOne <- factor(dataFrame$completeOne, 
                                  levels = c("< 20.0%", "20.1 - 40.0%",
                                             "40.1 - 60.0%", "60.1 - 80.0%",
                                             "> 80.0%", "NA"))
  
  # this plot is colored by completeness and parsed by states; exclude DC in the
  # state abbreviations as we make a 'rectangle' for it below. 
  # want - make the state abbreviations bold and if possible a white background
  plot <- usmap::plot_usmap(data = dataFrame, values = "completeOne",
                            exclude = c("DC"), size = 0.5, color = "black", 
                            labels = T, label_color = "black" )+
    ggplot2::scale_fill_manual(name = "Percent\nComplete",
                               values = c("#b9e8eb", "#7cb5e5", "#4084c2",
                                          "#2d50ba", "#1c1b96",  "#f8f8f8"),
                               labels = c("< 20.0%", "20.1 - 40.0%",
                                          "40.1 - 60.0%", "60.1 - 80.0%",
                                          "> 80.0%", "NA"), drop = FALSE)+
    labs(title = imageTitle)+
    theme(plot.title = element_text(size = 35, hjust = 0.5),
          text = element_text(size = 15, face = "bold"), 
          legend.position = c(-0.04, 0.0), 
          panel.border = element_rect(colour = "black", 
                                      fill = NA, size = 1))
  
  # convert the state abbreviations to a different size. This is where I got
  # stuck; as I could not figure out how to adjust the state abbreviations
  # in any other way. Need to do a bit more reading how to manipulate; does not
  # seem to be standard ggplot manipulation...
  plot$layers[[2]]$aes_params$size <- 4
  
  print(plot)
  
  # define the colors to be able to use in the add Square funtion
  cols <- c("< 20.0%" = "#b9e8eb",  "20.1 - 40.0%" = "#7cb5e5",
            "40.1 - 60.0%" = "#4084c2", "60.1 - 80.0%"  = "#2d50ba",
            "> 80.0%" = "#1c1b96", "NA" = "#f8f8f8")
  
  # the inputs include the name to be displayed in the rectangle, the inputData
  # as either the fedEnties or territories dataframe created above, xLoc and 
  # yloc are the locations for where the rectangle should be and jurID maps back 
  # to the jurisdiction id
  
  addSquare <- function(name, inputData, xLoc, yLoc, jurID){
    vP <- viewport(xLoc, yLoc, width = 0.05, height = 0.05)
    
    # navigate into the viewport
    pushViewport(vP)
    
    # dynamically figure out what color for square given the input param jurID
    
    if(inputData$completeOne[which(inputData$Jurisdiction == jurID)] == 
       "< 20.0%"){
      col = "#b9e8eb"
      }
    else if(inputData$completeOne[which(inputData$Jurisdiction == jurID)] ==
            "20.1 - 40.0%"){
      col = "#7cb5e5"
    }
    else if (inputData$completeOne[which(inputData$Jurisdiction == jurID)] ==
             "40.1 - 60.0%"){
      col = "#4084c2"
    }
    else if (inputData$completeOne[which(inputData$Jurisdiction == jurID)] ==
             "60.1 - 80.0%"){
      col = "#2d50ba"
    }
    else if (inputData$completeOne[which(inputData$Jurisdiction == jurID)] ==
             "> 80.0%"){
      col = "#1c1b96"
    }
    else {col = "#f8f8f8"}
    
    # Visualize the vp area
    grid.draw(rectGrob(gp = gpar(fill = col)))
    
    # add text to the vp
    grid.draw(textGrob(name, gp = gpar(fontsize = 20),
                       x = unit(0.5, 'npc'),
                       y = unit(0.5, 'npc')))
    
    #exit viewport
    popViewport()
  }
  
  # call addSquare function to make all of the squares
  # federal entitites
  addSquare(name = "DC", inputData = fedEnts, 
            xLoc = .90, yLoc = 0.40, jurID = "DCA")
  addSquare(name = "BoP", inputData = fedEnts,
            xLoc = .90, yLoc = 0.33, jurID = "BP2")
  addSquare(name = "DoD", inputData = fedEnts,
            xLoc = .90, yLoc = 0.26, jurID = "DD2")
  addSquare(name = "IHS", inputData = fedEnts,
            xLoc = .90, yLoc = 0.19, jurID = "IH2")
  addSquare(name = "VHA", inputData = fedEnts,
            xLoc = .90, yLoc = 0.12, jurID = "VA2")
  
  # state territories
  addSquare(name = "VI", inputData = territory,
            xLoc = .90, yLoc = 0.05, jurID = "VIA")
  addSquare(name = "PW", inputData = territory,
            xLoc = 0.84, yLoc = 0.05, jurID = "RPA")
  addSquare(name = "PR", inputData = territory,
            xLoc = 0.78, yLoc = 0.05, jurID = "PRA")
  addSquare(name = "MP", inputData = territory,
            xLoc = 0.72, yLoc = 0.05, jurID = "MPA")
  addSquare(name = "MH", inputData = territory,
            xLoc = 0.66, yLoc = 0.05, jurID = "MHA")
  addSquare(name = "GU", inputData = territory,
            xLoc = 0.60, yLoc = 0.05, jurID = "GUA")
  addSquare(name = "FM", inputData = territory,
            xLoc = 0.54, yLoc = 0.05, jurID = "FMA")
  addSquare(name = "AS", inputData = territory,
            xLoc = 0.48, yLoc = 0.05, jurID = "ASA")

  dev.copy(which = a)
  dev.off()
  dev.off()
  
}

# call the function and make the plots

rePlot(dataFrame = reComplete, colName = "overall", imageID = "overall",
       imageTitle = "Cumulative")
rePlot(dataFrame = reComplete, colName = "twoWeeks", imageID = "2week",
       imageTitle = "Last Two Weeks")
