#Install required packages
install.packages("tidyr", "ggplot2", "classInt", "openxlsx")

#Load necessary packages
library(openxlsx)
library(ggplot2)
library(tidyr)
library(classInt)

#Generate random data, user should read own data
intensity <- data.frame(
  category = paste0("category", 1:6),
  setNames(data.frame(matrix(sample(0:20, 6*7, T), nrow=6)), paste0("interval", 1:7))
)

deviation <- data.frame(
  category = paste0("category", 1:6),
  setNames(data.frame(matrix(sample(-10:10, 6*7, T), nrow=6)), paste0("interval", 1:7))
)

#Export randomly generated data
write.xlsx(list(intensity=intensity, deviation=deviation), "data.xlsx", overwrite=TRUE)

#Please note that the above data is randomly generated. 
#Please use the following code to import your own data.
#Use the read.xlsx function to read your own data
intensity <- read.xlsx("data.xlsx",sheet = "intensity")
deviation <- read.xlsx("data.xlsx",sheet = "deviation")

#Convert intensity data to long format
data <- pivot_longer(
  intensity, cols = 2:8, #Read columns 2-8
  names_to = "interval", # Set column names
  values_to = "intensity") #Set the data column name

#Convert intensity deviation data to long format
data$deviation <- pivot_longer(
  deviation, cols = 2:8, #Same as above
  values_to = "deviation")$deviation #The last parameter is to import the result into data

#The intensity data has natural breakpoints, which are divided into 5 categories.
#You can change the number of categories by modifying the value of n.
intensity_classes <- classIntervals(data$intensity, n = 5, style = "jenks")

#Reclassify intensity data based on breakpoint values
data$intensity_jenks <- findInterval(
  data$intensity,intensity_classes$brks, rightmost.closed=TRUE)

#Change the size to a discrete value to facilitate customizing the size of the point later
data$intensity_jenks <- factor(data$intensity_jenks)

#Write the results to a data frame
classes <- data.frame(
  intensity_jenks = intensity_classes$brks,
  stringsAsFactors = FALSE
)

#Export breakpoint results
write.table(classes,file = "classes.csv",sep = ",",row.names = F) 
#Please note that the Jenks classification method may not be suitable for your data.
#If necessary, you can adjust the classification method yourself, specifically manually classify in the Excel file

#Define the y-axis drawing order
data$category <- factor(data$category,
                        levels = c("category6","category5","category4",
                                   "category3","category2","category1"))

#Set the color drawing environment
color_scale <- 
  scale_color_gradientn(colors = c("#212c5f","#d1eaf8","#ffebee", "#ff6a46"),
                        values = scales::rescale(c(min(data$deviation), -0.01,
                                                   0.01,max(data$deviation))),
                        limits = c(min(data$deviation), max(data$deviation)))

#Drawing Intensity Deviation Maps
p1 <- ggplot(data = data)+
  
  #Add horizontal background lines
  geom_hline(yintercept = c(1.5,2.5,3.5,4.5,5.5),
             color = "grey60",
             linewidth = 0.5,
             linetype = "dashed") + 
  
  #Add vertical dividers
  geom_vline(xintercept = c(1.5,2.5,3.5,4.5,5.5,6.5),
             color="grey60",
             linewidth=0.5,
             linetype = "dashed") + 
  
  #Draw dots of different sizes
  geom_point(mapping = aes(x = interval,
                           y = category, 
                           size = intensity_jenks,
                           color = deviation)) + 
  
  #Add a black outline to the points
  geom_point(mapping = aes(x = interval,
                           y = category, 
                           size = intensity_jenks,
                           color = deviation),
             colour = "black",
             shape = 21) + 
  #Select the color map you set earlier
  color_scale+
  
  #Set the point size
  scale_size_manual(values = c("1"=5,"2"=7,"3"=9,"4"=11,"5"=13)) +
  
  #Set the x-axis and y-axis
  scale_x_discrete(position = "top",expand = c(0.075,0.075))  +  
  
  scale_y_discrete(expand = c(0.075,0.075)) +
  
  # Cancel the X-axis and Y-axis titles and set the legend name
  labs(
    x = NULL, 
    y = NULL,
    color = "Intensity deviation",  # Set the name of the color legend
    size = "Intensity"    # Set the name of the size legend
  ) +  
  
  #Setting the Theme
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA,
                                    color = "black",
                                    linewidth = 1 ), #Set border
        axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
        axis.text.x = element_text(angle = 45, vjust = 0, hjust = 0),
        axis.text = element_text(size = 12,
                                 color = "black"),
        #Set the color, size, and font of the axis title
        axis.ticks.x = element_line(color = "grey40",
                                    linewidth = 0.5), 
        axis.ticks.y = element_line(color = "grey40",
                                    linewidth = 0.5), #Set the color and size of the tick marks
        axis.ticks.length.x = unit(4,"pt"), #Set the length of the x-axis tick marks
        axis.ticks.length.y = unit(5,"pt"), #Set the length of the y-axis tick marks
        plot.margin = margin(20,30,20,30), #Set the margins of the graph
        legend.position = 'right'
  )

#Export Intensity Deviation Maps
#width and height set the width and height respectively, add family="serif" to set Times New Roman font
pdf("Intensity Deviation Maps.pdf",width = 7.8,height = 5)
print(p1)
dev.off()
