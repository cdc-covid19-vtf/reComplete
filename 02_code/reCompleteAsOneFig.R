library(readxl)
library(dplyr)
library(ggplot2)
library(grid)
library(magrittr)
library(cowplot)
library(stringr)

#created pdf image that will be located in the 03_figures sub-directory
#imageID, is some unique identifier that the user can specify. This is to 
#differentiate between the overall and twoWeeks plots  
# pdf(file = here::here("03_figures", paste("REComplete_Combined",
#                                           format(Sys.time(), "%Y-%m-%d"),
#                                           "pdf", sep=".")),
#     width = 15, height = 10)
# 
# #how to save both png and pdf from stack overflow 
# #https://stackoverflow.com/questions/26232103
# #also added dev.control('enable') along with a dev.copy call at end of script
# a<-dev.cur()
# 
# #save the images as png
# png(file = here::here("03_figures", paste("REComplete_Combined",
#                                           format(Sys.time(), "%Y-%m-%d"),
#                                           "png", sep=".")),
#     width = 15, height = 10, units = "in", res = 100)
# 
# dev.control("enable")

#skip the first three rows, which is extra information. 
#this assumes the data is the first tab; if not then specify the tab 
reComplete <- readxl::read_excel(
  here::here("01_data",
             "Previous_RE_BiweeklyJurisdictionCompleteness.xlsx"), skip = 3)

#I do this for my own sanity, because I had a hard time remembering which were
#the data columns of interest. One could skip this step. 
reComplete <- reComplete %>%
  dplyr::rename(overall = `Valid, %...8`) %>%
  dplyr::rename(twoWeeks = `Valid, %...14`)

#convert column as numeric because it is read in as character
reComplete$twoWeeks<- as.numeric(reComplete$twoWeeks)

#to get a percent and use plot_usmap by converting to column name 'state'
reComplete <- reComplete %>%
  dplyr::mutate(overall  = overall * 100) %>%
  dplyr::mutate(twoWeeks = twoWeeks * 100)%>%
  dplyr::rename(state = "Jurisdiction Name")


#switch California two week value to NA; they are not legally supposed 
#report RE for the last two weeks. Other states include Minnesota, Texas, 
#and Vermont. In this example input data, MN, TX, and VT are already dealt
#with, in the future you may need to extend this if statement if necessary. 
#This uses the colName input parameter to distinguish if this is necessary
reComplete$twoWeeks[8] <- 0.00

#get the federal entity rows for plotting square boxes
fedEnts <- reComplete %>%
  filter_at(vars(Jurisdiction), any_vars(. %in% c("BP2",  "DCA",
                                                  "DD2", "IH2", "VA2")))  
#get territory rows for plotting square boxes  
territory <-  reComplete %>%
  filter_at(vars(Jurisdiction), 
            any_vars(. %in% c("ASA", "FMA", "GUA", "MHA", "MPA", "PRA",
                              "RPA", "VIA")))

#combine federal entitites and terroties
ents <- dplyr::bind_rows(fedEnts, territory)

#function to add a column that bins the data for coloring for the last two weeks
#in theory, the column used (overall) and the new column name(completeOverall)
#should be exchanged out for parameters and thus not have two functions that
#do the same thing (i.e. overall and twoWeeks). But I could not get it to 
#do this - blerg 


overBin <- function(dataFrame, inputCol, colName){
  input = enquo(inputCol)
  col = enquo(colName)
  
  dataFrame <- dataFrame %>%
    mutate(!!quo_name(col) := case_when (
      (!!input) == 0.00 ~ "NA",
      (!!input) < 20.00 ~ "< 20.0%",
      (!!input) >= 20.0 & (!!input) < 40.0 ~ "20.0 - 40.0%",
      (!!input) >= 40.0 & (!!input) < 60.0 ~ "40.0 - 60.0%",
      (!!input) >= 60.0 & (!!input) < 80.0 ~ "60.0 - 80.0%",
      (!!input) >= 80.0 ~ "> 80.0%") )
  return(dataFrame)
}

reComplete <- overBin(dataFrame = reComplete, inputCol = overall, colName = completeOverall)
reComplete <- overBin(dataFrame = reComplete, inputCol = twoWeeks, colName = completeTwo)
ents <- overBin(dataFrame = ents, inputCol = overall, colName = completeOverall)
ents <- overBin(dataFrame = ents, inputCol = twoWeeks, colName = completeTwo)

#replace New York State with New York
reComplete$state[48]<- "New York"

#convert state names to abbreviations for using plot_usmap
reComplete$state <- state.abb[match(reComplete$state, state.name)]

#remove values that are NA, which includes federal entities, territories, and
#pharmacies. This removal is done to use the plot_usmap function.
reComplete <- reComplete %>%
  filter(state!="NA")

#convert to NA to character string 
reComplete[ reComplete == "NA" ] <- NA


#define the colors to be able to use in the add_Square funtion
cols <- c("< 20.0%" = "#b9e8eb",  "20.1 - 40.0%" = "#7cb5e5",
          "40.1 - 60.0%" = "#4084c2", "60.1 - 80.0%"  = "#2d50ba",
          "> 80.0%" = "#1c1b96", "NA" = "#f8f8f8")

#the inputs include the name to be displayed in the rectangle, the inputData
#as either the fedEnties or territories dataframe created above, xLoc and yLoc 
#are the locations for where the rectangle should be and jurID maps back to 
#the jurisdiction id. 

#pull out ents and overall for dataframe and column input name
addOverall <- function(name, xLoc, yLoc, jurID){
  vP <- viewport(xLoc, yLoc, width = 0.03, height = 0.03)
  
  #navigate into the viewport
  pushViewport(vP)
  
  #dynamically figure out what color for square given the input param jurID
  if(ents$completeOverall[which(ents$Jurisdiction == jurID)] == 
     "< 20.0%"){
    col = "#b9e8eb"
  }
  else if(ents$completeOverall[which(ents$Jurisdiction == jurID)] ==
          "20.0 - 40.0%"){
    col = "#7cb5e5"
  }
  else if (ents$completeOverall[which(ents$Jurisdiction == jurID)] ==
           "40.0 - 60.0%"){
    col = "#4084c2"
  }
  else if (ents$completeOverall[which(ents$Jurisdiction == jurID)] ==
           "60.0 - 80.0%"){
    col = "#2d50ba"
  }
  else if (ents$completeOverall[which(ents$Jurisdiction == jurID)] ==
           "> 80.0%"){
    col = "#1c1b96"
  }
  else {
    col = "#f8f8f8"
  }
  
  #visualize the vp area
  grid.draw(rectGrob(gp = gpar(fill = col)))
  
  #add text to the vp
  grid.draw(textGrob(name, gp = gpar(fontsize = 8, fontface = "bold"),
                     x = unit(0.5, 'npc'),
                     y = unit(0.5, 'npc')))
  
  #exit viewport
  popViewport()
}

addTwo <- function(name, xLoc, yLoc, jurID){
  vP <- viewport(xLoc, yLoc, width = 0.03, height = 0.03)
  
  #navigate into the viewport
  pushViewport(vP)
  
  #dynamically figure out what color for square given the input param jurID
  if(ents$completeTwo[which(ents$Jurisdiction == jurID)] == 
     "< 20.0%"){
    col = "#b9e8eb"
  }
  else if(ents$completeTwo[which(ents$Jurisdiction == jurID)] ==
          "20.0 - 40.0%"){
    col = "#7cb5e5"
  }
  else if (ents$completeTwo[which(ents$Jurisdiction == jurID)] ==
           "40.0 - 60.0%"){
    col = "#4084c2"
  }
  else if (ents$completeTwo[which(ents$Jurisdiction == jurID)] ==
           "60.0 - 80.0%"){
    col = "#2d50ba"
  }
  else if (ents$completeTwo[which(ents$Jurisdiction == jurID)] ==
           "> 80.0%"){
    col = "#1c1b96"
  }
  else {
    col = "#f8f8f8"
  }
  
  #visualize the vp area
  grid.draw(rectGrob(gp = gpar(fill = col)))
  
  #add text to the vp
  grid.draw(textGrob(name, gp = gpar(fontsize = 8, fontface = "bold"),
                     x = unit(0.5, 'npc'),
                     y = unit(0.5, 'npc')))
  
  #exit viewport
  popViewport()
}

#as a function so one can just specify the column header, imageID, and 
#imageTitle as input parameters.This allows the creation of two maps: 
#Overall and Past two weeks. 
rePlot <- function(dataFrame, colName, imageTitle) {
  
  #this plot is colored by completeness and parsed by states; exclude DC in the
  #state abbreviations as we make a 'rectangle' for it below. 
  #want - make the state abbreviations bold and if possible a white background
  plot <- usmap::plot_usmap(data = dataFrame, values = colName,
                            exclude = c("DC"), size = 0.5, color = "black", 
                            labels = T, label_color = "black" )+
    ggplot2::scale_fill_manual(name = "Percent\nComplete",
                               values = c("#b9e8eb", "#7cb5e5", "#4084c2",
                                          "#2d50ba", "#1c1b96", "white"))+
    labs(title = imageTitle)+
    theme(plot.title = element_text(size = 25, hjust = 0.5),
          text = element_text(size = 15, face = "bold"), 
          panel.border = element_rect(colour = "black", 
                                      fill = NA, size = 1))
  
  #convert the state abbreviations to a different size. This is where I got
  #stuck; as I could not figure out how to adjust the state abbreviations
  #in any other way. Need to do a bit more reading how to manipulate; does not
  #seem to be standard ggplot manipulation...
  plot$layers[[2]]$aes_params$size <- 2
  
  return(plot)
}


reComplete$completeOverall <- factor(reComplete$completeOverall, 
                                     levels = c("< 20.0%", "20.0 - 40.0%",
                                                "40.0 - 60.0%", "60.0 - 80.0%",
                                                "> 80.0%", "NA"))


reComplete$completeTwo <- factor(reComplete$completeTwo, 
                                     levels = c("< 20.0%", "20.0 - 40.0%",
                                                "40.0 - 60.0%", "60.0 - 80.0%",
                                                "> 80.0%", "NA"))

#call the function and make the plots
p1 <- rePlot(dataFrame = reComplete, colName = "completeOverall", 
             imageTitle = "Cumulative")



p2 <- rePlot(dataFrame = reComplete, colName = "completeTwo", 
             imageTitle = "Last Two Weeks")

#combine the plots with cowplot
prow <- cowplot::plot_grid(p1 + theme(legend.position = "none"),
                           p2 + theme(legend.position = "none"),
                           align = 'h')

#add legend
legend <- cowplot::get_legend(p1 +
                                theme(legend.position = c(0.25, 0.85), 
                                      legend.direction = "horizontal"))
#display plot
plot_grid(prow, legend, ncol = 1, rel_heights = c(2,1))


#call addOverall function to make all of the squares federal entities 
addOverall(name = "DC", xLoc = .47, yLoc = 0.575, jurID = "DCA")
addOverall(name = "BoP", xLoc = .47, yLoc = 0.535, jurID = "BP2")
addOverall(name = "DoD", xLoc = .47, yLoc = 0.495, jurID = "DD2")
addOverall(name = "IHS", xLoc = .47, yLoc = 0.455, jurID = "IH2")
addOverall(name = "VHA", xLoc = .47, yLoc = 0.415, jurID = "VA2")

#state territories
addOverall(name = "VI", xLoc = .435, yLoc = 0.415, jurID = "VIA")
addOverall(name = "PW", xLoc = 0.40, yLoc = 0.415, jurID = "RPA")
addOverall(name = "PR", xLoc = 0.365, yLoc = 0.415, jurID = "PRA")
addOverall(name = "MP", xLoc = 0.330, yLoc = 0.415, jurID = "MPA")
addOverall(name = "MH", xLoc = 0.295, yLoc = 0.415, jurID = "MHA")
addOverall(name = "GU", xLoc = 0.260, yLoc = 0.415, jurID = "GUA")
addOverall(name = "FM", xLoc = 0.225, yLoc = 0.415, jurID = "FMA")
addOverall(name = "AS", xLoc = 0.190, yLoc = 0.415, jurID = "ASA")

#call addTwo function to make squares for federal ents on last twoo week plot
addTwo(name = "DC", xLoc = .97, yLoc = 0.575, jurID = "DCA")
addTwo(name = "BoP", xLoc = .97, yLoc = 0.535, jurID = "BP2")
addTwo(name = "DoD", xLoc = .97, yLoc = 0.495, jurID = "DD2")
addTwo(name = "IHS", xLoc = .97, yLoc = 0.455, jurID = "IH2")
addTwo(name = "VHA", xLoc = .97, yLoc = 0.415, jurID = "VA2")

#state territories
addTwo(name = "VI", xLoc = .935, yLoc = 0.415, jurID = "VIA")
addTwo(name = "PW", xLoc = 0.90, yLoc = 0.415, jurID = "RPA")
addTwo(name = "PR", xLoc = 0.865, yLoc = 0.415, jurID = "PRA")
addTwo(name = "MP", xLoc = 0.830, yLoc = 0.415, jurID = "MPA")
addTwo(name = "MH", xLoc = 0.795, yLoc = 0.415, jurID = "MHA")
addTwo(name = "GU", xLoc = 0.760, yLoc = 0.415, jurID = "GUA")
addTwo(name = "FM", xLoc = 0.725, yLoc = 0.415, jurID = "FMA")
addTwo(name = "AS", xLoc = 0.690, yLoc = 0.415, jurID = "ASA")

# #to allow both files to be saved. 
# dev.copy(which = a)
# dev.off()
# dev.off()

