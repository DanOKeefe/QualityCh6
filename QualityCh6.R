# Problem 1 : Soft Drinks
# A reading of zero corresponds to the correct fill height
# Fifteen samples of size n = 10 have been analyzed, and fill heights are given in table 6E.5

# Set up x and s control charts on this process. Does the process exhibit statistical control?

library(qcc)

Data1 <- within(read.csv(file="Q1Data.csv", header=T, sep=","), rm('X')) # removed the X column in the csv file
xbar_flow_data_graph1 <- qcc(data = Data1, type = "xbar")
s_piston_data_chart1 <- qcc(data = Data1, type = "S")

# Set up an R chart, and compare with the s chart in part (a)
R_flow_data_graph1 <- qcc(Data1, type = "R")

# Answer : The R and s charts have virtually the same shape. Both measure within-sample variability.

summary(R_flow_data_graph1)
summary(R_flow_data_graph1$limits) # all the same control limits bc sample sizes do not vary
#Remove the last 5 observations of x10 and set up x and s control charts on this process

# So do the same analysis, but only use the first 5 datapoints from the 10th sample.

Data1Edited <- Data1
Data1Edited[10,6:10] <- NA  # removed last 5 elements from row 10. (in columns 6 through 10 inclusive)
Data1Edited # check to make sure NA values are correctly implemented

xbar_flow_data_graph1Edited <- qcc(data = Data1Edited, type = "xbar")
s_piston_data_chart1Edited <- qcc(data = Data1Edited, type = "S")
R_flow_data_graph1Edited <- qcc(Data1Edited, type = "R")

# Answer: the control limits for the 10th sample are more spread out because only 5 datapoints are analyzed.

summary(R_flow_data_graph1Edited$limits) # max control limit is for the 10th sample.
summary(xbar_flow_data_graph1Edited$limits) # max control limit is for the 10th sample.

# ----------------------------

# Problem 2 : Piston Ring

# Consider the piston ring data shown in Table 6.3.
# Assume that the specifications on this component are 74.000 + 0.05 mm.
# Set up and R control charts on this process. Is the process in statistical control?

Data2 <- read.csv(file = 'Q2Data.csv', header = FALSE, sep=',')
xbar_flow_data_graph2 <- qcc(data = Data2, type = "xbar")
R_flow_data_graph2 <- qcc(Data2, type = "R")
summary(R_flow_data_graph2)
# Yes this process is in statistical control because all the of the sample means are within the control limits
# and the control limits are within the specification limits. (~ +/- 2 mm away from the mean)

# ----------------------------

# Problem 3 : Piston Ring Cont

# Table 6E.7 shows 15 additional samples for the piston ring process (Table 6.3), 
# taken after the initial control charts were established. 

Data3 <- read.csv(file = 'Q3Data.csv', header = FALSE, sep = ',')

# Plot these data on the x and R chart developed in Exercise 6.9.

xbar_flow_data_graph3 <- qcc(data = Data3, type = "xbar")
R_flow_data_graph3 <- qcc(Data3, type = "R")

# Q: Is the process in control?

# A: No, the sample means for sample 13 and 14 are above the Upper Control Limit. 
#    Sample 3's sample mean is below the Lower Control Limit.

# ----------------------------

# Problem 4 : Polymer Viscosity

# Continuation of Exercise 6.55. The next five measurements on viscosity are 3163, 3199, 3054, 3147, and 3156. 
# Do these measurements indicate that the process is in statistical control?

Data4P1 <- read.csv(file = 'Q4Data.csv', header = FALSE, sep = ',')
typeof(Data4P1) # reads it as a list

# change to matrix of integers
Data4P1 <- as.matrix(Data4P1)
typeof(Data4P1)

# input the additional data
newData <- c(3163, 3199, 3054, 3147, 3156)

# Combine the two as a matrix with one column
CombinedP4 <- Data4P1
CombinedP4[21:25] <- newData

# (a) Does viscosity follow a normal distribution?
library(ggplot2)
qplot(CombinedP4, binwidth = 50) # it does not appear normally distributed

# Set up a control chart on viscosity and a moving range chart. 
# Does the process exhibit statistical control?

xbar_flow_data_graphP4 <- qcc(data = Data4P1, type = "xbar") # says group size must be larger than one.

# change Data4P1 to make 4 groups of size n = 5
Data4P1Resized <- matrix(Data4P1, 4, 5)
Data4P1Resized

xbar_flow_data_graphP4 <- qcc(data = Data4P1Resized, type = "xbar")
xbar_flow_data_graphP4$limits # LCL = 2737, UCL = 3132

mean(newData) # mean of the new data is 3143.8, which is above the UCL.

R_flow_data_graphP4 <- qcc(data = Data4P1Resized, type = "R")
R_flow_data_graphP4$limits #LCL = 0, UCL = 705

range(newData)[2] - range(newData)[1] # Range = 145

# Answer: Conclude that the process is not in statistical control, because sample mean is above the UCL.

# Problem 5 : Oxide Thickness of Silicon Wafers

# (a) Thirty observations on the oxide thickness of individual silicon wafers are shown in Table 6E.22.

DataP5 <- read.csv(file = 'Q5Data.csv', header = FALSE)

# Use these data to set up a control chart on oxide thickness and a moving range chart. 

# I'm going to divide up the 30 observations into 3 samples of 10 observations.

DataP5 <- matrix(as.matrix(DataP5), 3, 10)
xbar_data_graphP5 <- qcc(data = DataP5, type = "xbar")
xbar_data_graphP5$limits # LCL = 45.5, UCL = 54.2
R_data_graphP5 <- qcc(data = DataP5, type = 'R')
R_data_graphP5$limits # LCL = 3.12, UCL = 24.88

# Does the process exhibit statistical control? 
# Answer : Yes, all xbar and R values are within control limits

# Does oxide thickness follow a normal distribution?
DataP5 <- read.csv(file = 'Q5Data.csv', header = FALSE) # convert back to list
qplot(DataP5, binwidth =2) # yes it appears to be normally distributed, a few outliers of larger thickness however
summary(DataP5)
