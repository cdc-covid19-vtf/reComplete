library(readxl)
library(dplyr)
library(ggplot2)
library(grid)
library(magrittr)
library(cowplot)
library(stringr)
library(magick)

#created pdf image that will be located in the 03_figures sub-directory
#imageID, is some unique identifier that the user can specify. This is to
#differentiate between the overall and twoWeeks plots
pdf(file = here::here("03_figures", paste("REComplete_Combined",
                                          format(Sys.time(), "%Y-%m-%d"),
                                          "pdf", sep=".")),
    width = 15, height = 10)

#how to save both png and pdf from stack overflow
#https://stackoverflow.com/questions/26232103
#also added dev.control('enable') along with a dev.copy call at end of script
a<-dev.cur()

#save the images as png
png(file = here::here("03_figures", paste("REComplete_Combined",
                                          format(Sys.time(), "%Y-%m-%d"),
                                          "png", sep=".")),
    width = 15, height = 10, units = "in", res = 100)

dev.control("enable")

#skip the first three rows, which is extra information. 
#this assumes the data is the first tab; if not then specify the tab 
reComplete <- readxl::read_excel(
  here::here("01_data",
             "RE_BiweeklyJurisdictionCompleteness.xlsx"), skip = 3)

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
#and Vermont. In the input data, MN, TX, and VT are already dealt with, in the 
#future you may need to extend/check this if necessary. 

#Note to self; I don't love that this is positional, could this be coded 
#differently
reComplete$twoWeeks[8] <- 0.00

#get the federal entity rows for plotting square boxes
fedEnts <- reComplete %>%
  dplyr::filter_at(dplyr::vars(Jurisdiction), 
                   dplyr::any_vars(. %in% c("BP2",  "DCA",
                                                  "DD2", "IH2", "VA2")))  
#get territory rows for plotting square boxes  
territory <-  reComplete %>%
  dplyr::filter_at(dplyr::vars(Jurisdiction), 
            dplyr::any_vars(. %in% c("ASA", "FMA", "GUA", "MHA", "MPA", "PRA",
                              "RPA", "VIA")))

#combine federal entitites and terroties
ents <- dplyr::bind_rows(fedEnts, territory)

#function to add a column that bins the data for coloring for the last two weeks
#this uses quasiquotation - quote some parts of an expression while evaluating
#and then inserting the results of others (unquoting; e.g !!input)
overBin <- function(dataFrame, inputCol, colName){
  input = enquo(inputCol)
  col = enquo(colName)
  
  dataFrame <- dataFrame %>%
    dplyr::mutate(!!quo_name(col) := dplyr::case_when (
      (!!input) == 0.00 ~ "N/A",
      (!!input) < 20.00 ~ "< 20.0%",
      (!!input) >= 20.0 & (!!input) < 40.0 ~ "20.0 - 40.0%",
      (!!input) >= 40.0 & (!!input) < 60.0 ~ "40.0 - 60.0%",
      (!!input) >= 60.0 & (!!input) < 80.0 ~ "60.0 - 80.0%",
      (!!input) >= 80.0 ~ "> 80.0%") )
  return(dataFrame)
}

#call function for the two datasets for the two different columns
reComplete <- overBin(dataFrame = reComplete,
                      inputCol = overall, colName = completeOverall)
reComplete <- overBin(dataFrame = reComplete,
                      inputCol = twoWeeks, colName = completeTwo)
ents <- overBin(dataFrame = ents, inputCol = overall,
                colName = completeOverall)
ents <- overBin(dataFrame = ents, inputCol = twoWeeks,
                colName = completeTwo)

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
#the jurisdiction id while colName is either completeOverall or completeTwo 

#pull out ents and overall for dataframe and column input name
#again uses quasiquotation and coverts to character class with as_name and 
#extracting with [[]]
addSquare <- function(name, colName, xLoc, yLoc, jurID){
  vP <- viewport(xLoc, yLoc, width = 0.03, height = 0.03)
  
  col <- enquo(colName)
  
  #navigate into the viewport
  pushViewport(vP)
  
  #dynamically figure out what color for square given the input param jurID
  if(ents[[rlang::as_name(col)]][which(ents$Jurisdiction == jurID)] == 
     "< 20.0%"){
    col = "#b9e8eb"
  }
  else if(ents[[rlang::as_name(col)]][which(ents$Jurisdiction == jurID)] ==
          "20.0 - 40.0%"){
    col = "#7cb5e5"
  }
  else if (ents[[rlang::as_name(col)]][which(ents$Jurisdiction == jurID)] ==
           "40.0 - 60.0%"){
    col = "#4084c2"
  }
  else if (ents[[rlang::as_name(col)]][which(ents$Jurisdiction == jurID)] ==
           "60.0 - 80.0%"){
    col = "#2d50ba"
  }
  else if (ents[[rlang::as_name(col)]][which(ents$Jurisdiction == jurID)] ==
           "> 80.0%"){
    col = "#1c1b96"
  }
  else {
    col = "#f8f8f8"
  }
  
  #visualize the vp area
  grid.draw(rectGrob(gp = gpar(fill = col)))
  
  #add text to the vp
  grid.draw(textGrob(name, gp = gpar(fontsize = 12, fontface = "bold"),
                     x = unit(0.5, 'npc'),
                     y = unit(0.5, 'npc')))
  
  #exit viewport
  popViewport()
}


#as a function so one can just specify the dataFrame column header, and 
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
                                          "#2d50ba", "#1c1b96", "white"),
                               drop = FALSE)+
    labs(title = imageTitle)+
    theme(plot.title = element_text(size = 25, hjust = 0.5),
          text = element_text(size = 15, face = "bold"), #for the legend
          panel.border = element_rect(colour = "black", 
                                      fill = NA, size = 1),
          legend.position = "none",
          plot.margin = unit(c(0, 0, 0, 0), "mm"))
  
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
                                                "> 80.0%", "N/A"))
  
reComplete$completeTwo <- factor(reComplete$completeTwo,
                                     levels = c("< 20.0%", "20.0 - 40.0%",
                                                "40.0 - 60.0%", "60.0 - 80.0%",
                                                "> 80.0%", "N/A"))
#call the function and make the plots
p1 <- rePlot(dataFrame = reComplete, colName = "completeOverall", 
             imageTitle = "12/15/2020 - 04/20/2021")

p2 <- rePlot(dataFrame = reComplete, colName = "completeTwo", 
             imageTitle = "04/06/2021 - 04/20/2021")

#combine the plots with cowplot
prow <- cowplot::plot_grid(p1 + theme(legend.position = "none"),
                           p2 + theme(legend.position = "none"),
                           align = 'h')

#add legend
legend <- cowplot::get_legend(p1 +
                                theme(legend.position = c(0.33, 1.2), 
                                      legend.direction = "horizontal"))
#display plot
plot_grid(prow, legend, ncol = 1, rel_heights = c(5,1))

#call addSquare function to make all of the squares federal entities 
addSquare(name = "DC", colName = 'completeOverall',
          xLoc = .47, yLoc = 0.465, jurID = "DCA")
addSquare(name = "BoP", colName = 'completeOverall', 
          xLoc = .47, yLoc = 0.43, jurID = "BP2")
addSquare(name = "DoD", colName = 'completeOverall',
          xLoc = .47, yLoc = 0.395, jurID = "DD2")
addSquare(name = "IHS", colName = 'completeOverall',
          xLoc = .47, yLoc = 0.36, jurID = "IH2")
addSquare(name = "VHA", colName = 'completeOverall',
          xLoc = .47, yLoc = 0.325, jurID = "VA2")

#state territories
addSquare(name = "VI", colName = 'completeOverall',
          xLoc = .435, yLoc = 0.325, jurID = "VIA")
addSquare(name = "PW", colName = 'completeOverall',
          xLoc = 0.40, yLoc = 0.325, jurID = "RPA")
addSquare(name = "PR", colName = 'completeOverall',
          xLoc = 0.365, yLoc = 0.325, jurID = "PRA")
addSquare(name = "MP", colName = 'completeOverall',
          xLoc = 0.330, yLoc = 0.325, jurID = "MPA")
addSquare(name = "MH", colName = 'completeOverall',
          xLoc = 0.295, yLoc = 0.325, jurID = "MHA")
addSquare(name = "GU", colName = 'completeOverall',
          xLoc = 0.260, yLoc = 0.325, jurID = "GUA")
addSquare(name = "FM", colName = 'completeOverall',
          xLoc = 0.225, yLoc = 0.325, jurID = "FMA")
addSquare(name = "AS", colName = 'completeOverall',
          xLoc = 0.190, yLoc = 0.325, jurID = "ASA")

#call addSquare function to make squares for federal ents on last twoo week plot
addSquare(name = "DC",  colName = 'completeTwo',
          xLoc = .97, yLoc = 0.465, jurID = "DCA")
addSquare(name = "BoP", colName = 'completeTwo',
          xLoc = .97, yLoc = 0.43, jurID = "BP2")
addSquare(name = "DoD", colName = 'completeTwo',
          xLoc = .97, yLoc = 0.395, jurID = "DD2")
addSquare(name = "IHS", colName = 'completeTwo',
          xLoc = .97, yLoc = 0.36, jurID = "IH2")
addSquare(name = "VHA", colName = 'completeTwo',
          xLoc = .97, yLoc = 0.325, jurID = "VA2")

#state territories
addSquare(name = "VI", colName = 'completeTwo',
          xLoc = .935, yLoc = 0.325, jurID = "VIA")
addSquare(name = "PW", colName = 'completeTwo',
          xLoc = 0.90, yLoc = 0.325, jurID = "RPA")
addSquare(name = "PR", colName = 'completeTwo',
          xLoc = 0.865, yLoc = 0.325, jurID = "PRA")
addSquare(name = "MP", colName = 'completeTwo',
          xLoc = 0.830, yLoc = 0.325, jurID = "MPA")
addSquare(name = "MH", colName = 'completeTwo',
          xLoc = 0.795, yLoc = 0.325, jurID = "MHA")
addSquare(name = "GU", colName = 'completeTwo',
          xLoc = 0.760, yLoc = 0.325, jurID = "GUA")
addSquare(name = "FM", colName = 'completeTwo',
          xLoc = 0.725, yLoc = 0.325, jurID = "FMA")
addSquare(name = "AS", colName = 'completeTwo',
          xLoc = 0.690, yLoc = 0.325, jurID = "ASA")

#would like to use magick package to trim but the resolution is not great
#for text 
# gg_file <- paste0("RE_Complete_Combined.", 
#                   format(Sys.time(), "%Y-%m-%d"), '.png')
# 
# suppressMessages(ggsave(gg_file, plot = last_plot(), 
#                         width = 15, height = 10, units = "in"))
# 
# m_png <- image_read(gg_file)
# 
# m_png <- image_trim(m_png)
# 
# image_write(m_png, gg_file, format = "png")

#to allow both files to be saved.
dev.copy(which = a)
dev.off()
dev.off()

