# EDA of Telco Customer Churn

# Load data and make a data frame called “Telco”
Telco<- read.csv(file = '/Users/yuki0416/Desktop/Q2-Spring/Intermediate Analytics/Final Project/WA_Fn-UseC_-Telco-Customer-Churn.csv',header = TRUE)


# Load package
library(ggplot2)
library(scales)
library(ggpubr)


#---Step 1: Variable Identification---

# Find data type, variable category 
str(Telco)
# Adjust data type (SeniorCitizen: Int -> Factor)
Telco$SeniorCitizen=as.factor(ifelse(Telco$SeniorCitizen==1, "Yes", "No")) 
attach(Telco)


#---Step 2: Univariate Analysis---

# 1) Numerical Variable
# a) 5 summary numbers (min, Q1, Q2/median, Q3, Max)
fivenum(tenure)
fivenum(MonthlyCharges)
fivenum(TotalCharges)

# b) Histogram (his)
#tenure
tenure_his <- ggplot(Telco, aes(x=tenure)) + 
  geom_histogram(binwidth = 1, color="black",fill="grey") + # create histogram
  geom_histogram(data=subset(Telco,tenure==1),binwidth=1, colour="black", fill="green4") + # highlight mode
  geom_histogram(data=subset(Telco,tenure==72),binwidth=1, colour="black", fill="green3") + # highlight second higer mode
  xlab("tenure (# of month)") # annotate X label

#MonthlyCharges
MonthlyCharges_his <- ggplot(Telco, aes(x=MonthlyCharges)) + 
  geom_histogram(binwidth = 5, color="black",fill="grey") + # create histogram
  geom_histogram(data=subset(Telco,MonthlyCharges<23),binwidth=5, colour="black", fill="green4") # highlight mode

#TotalCharges
TotalCharges_his <- ggplot(Telco, aes(x=TotalCharges)) + 
  geom_histogram(color="black",fill="grey") + # create histogram
  geom_text(x=4000, y=600, label="Right Skewed", size=3, color="green4") #annotate distribution

# Arrange on one page 
his_figure <- ggarrange(tenure_his, MonthlyCharges_his, TotalCharges_his, ncol = 2, nrow = 2)
# Annotate figure 
annotate_figure(his_figure, top = text_grob("Histogram of Numerical Variables", face = "bold", size = 12))


# c) Boxplot (bp)
# create 5 summary numbers labels
five <-c("min","Q1","Median","Q3","Max")

# Arrange on one page
layout(matrix(c(1,2,3,3), 2, 2, byrow = TRUE))

# tenure
boxplot(tenure, horizontal=TRUE, col="green4", axes=FALSE)
title("tenure", line = -1.5, font.main = 1) # Adjust plot title position
text(x=fivenum(tenure), labels =fivenum(tenure), y=1.3, cex=0.9) # put 5 summary numbers data values
text(x=fivenum(tenure), labels =five, y=0.7, cex=0.9) # put 5 summary numbers labels
lines(rep(mean(tenure),2),c(0.8, 1.2), col="white", lwd = 2) # add Mean line
text(x=mean(tenure)+6, labels ="Mean", y=1.05, col="white", cex=0.9) # add Mean text 
text(x=mean(tenure)+6, labels =round(mean(tenure),digit=1), y=0.95, col="white", cex=0.9) # add Mean data value

#MonthlyCharges
boxplot(MonthlyCharges, horizontal=TRUE, col="green4", axes=FALSE)
title("MonthlyCharges", line = -1.5, font.main = 1) # Adjust plot title position
text(x=fivenum(MonthlyCharges), labels =fivenum(MonthlyCharges), y=1.3, cex=0.9) # put 5 summary numbers data values
text(x=fivenum(MonthlyCharges), labels =five, y=0.7, cex=0.9) # put 5 summary numbers labels
lines(rep(mean(MonthlyCharges),2),c(0.8, 1.2), col="white", lwd = 2) # add Mean line
text(x=mean(MonthlyCharges)-6, labels ="Mean", y=1.05, col="white", cex=0.9) # add Mean text 
text(x=mean(MonthlyCharges)-6, labels =round(mean(MonthlyCharges),digit=1), y=0.95, col="white", cex=0.9) # add Mean data value

# TotalCharges
boxplot(TotalCharges, horizontal=TRUE, col="green4", axes=FALSE)
title("TotalCharges", line = -1.5, font.main = 1) # Adjust plot title position
text(x=fivenum(TotalCharges), labels =round(fivenum(TotalCharges),digit=1), y=1.3, cex=0.9) # put 5 summary numbers data values
text(x=fivenum(TotalCharges), labels =five, y=0.7, cex=0.9) # put 5 summary numbers labels
lines(rep(mean(TotalCharges, na.rm=TRUE),2),c(0.8, 1.2), col="white", lwd = 2) # add Mean line
text(x=mean(TotalCharges, na.rm=TRUE)+250, labels ="Mean", y=1.05, col="white", cex=0.9) # add Mean text 
text(x=mean(TotalCharges, na.rm=TRUE)+250, labels =round(mean(TotalCharges, na.rm=TRUE),digit=1), y=0.95, col="white", cex=0.9) # add Mean data value

# Add Title for a Page
mtext("Boxplot of Numerical Variables", side=3, line = -2, outer = TRUE, font=2)


# 2) Categorical Variable
# Pie chart - Churn
Churn_y <- sum(Churn=="Yes") # sum of churn
Churn_n <- sum(Churn=="No") # sum of non-churn
slices <- c(Churn_y,Churn_n) # create slices of pie
pct <- (slices/sum(slices)*100) # calculate % of slices
churn_dataframe <- data.frame(group = c("Yes","No"),value = c(pct)) # create churn data frame

# Pie of Churn
Churn_pie <- ggplot(churn_dataframe, aes(x="", y=value, fill=group)) + 
  geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0) + # create pie
  geom_text(aes(label=sprintf("%.1f%%",value)), position = position_stack(vjust = 0.5)) + #put data labels
  scale_fill_manual(values=c("grey","green4"),breaks=c("Yes","No")) + # change fill color and legend order
  labs(x = NULL, y = NULL, fill = "Churn", title = " Pie Chart of Categorical Variable") + # change legend title and put main title
  theme(plot.title = element_text(face="bold")) # bold title
Churn_pie


#---Step 3: Bi-variate Analysis--- 

# 1) Categorical & Categorical Variable

# 100% Stacked Bar Chart (sbc)
# Gender & Churn
Gender.Churn_sbc <- ggplot(Telco, aes(gender,fill=Churn)) + 
  geom_bar(position = 'fill') + # create 100% stacked bar chart
  scale_fill_manual(values=c("grey","green4"),breaks=c("Yes","No")) + # change fill color and legend order
  labs(x=NULL, y = NULL) + scale_y_continuous(labels = percent) # remove x, y label and change y axis to percent

# SeniorCitizen & Churn
SeniorCitizen_labels <- c("Non-SeniorCitizen", "SeniorCitizen") # create x label of "SeniorCitizen"
SeniorCitizen.Churn_sbc <- ggplot(Telco, aes(SeniorCitizen,fill=Churn)) + 
  geom_bar(position = 'fill') + # create 100% stacked bar chart
  scale_fill_manual(values=c("grey","green4"), breaks=c("Yes","No")) + # change fill color and legend order
  labs(x = NULL, y = NULL) + scale_y_continuous(labels = percent) + # remove x, y label and change y axis to percent
  scale_x_discrete(labels=SeniorCitizen_labels) # change x axis data label

# Arrange on one page
sbc_figure <- ggarrange(Gender.Churn_sbc, SeniorCitizen.Churn_sbc, ncol = 2, nrow = 1, common.legend = TRUE)
# Annotate figure 
annotate_figure(sbc_figure, top = text_grob("100% Stacked Bar Chart of Categorical & Categorical Variable", face = "bold", size = 12))

# 2) Categorical & Numerical Variable

Churn_label <- c("Non-Churn", "Churn") # create churn label

# Boxplot (bp)
# Churn & tenure
Churn.tenure_bp <- ggplot(Telco, aes(y= tenure, x = Churn, fill = Churn)) +
  geom_boxplot(outlier.colour = "yellow") + theme(legend.position = "none") + labs(x = NULL) +
  scale_fill_manual(values=c("grey","green4")) + scale_x_discrete(labels=Churn_label) 

# Churn & MonthlyCharges
Churn.MonthlyCharges_bp <- ggplot(Telco, aes(y= MonthlyCharges, x = Churn, fill = Churn)) +
  geom_boxplot(outlier.colour = "yellow") + theme(legend.position = "none") + labs(x = NULL) +
  scale_fill_manual(values=c("grey","green4")) + scale_x_discrete(labels=Churn_label)

# Arrange on one page
bp_figure <- ggarrange(Churn.tenure_bp, Churn.MonthlyCharges_bp, ncol = 2, nrow = 1)
# Annotate figure 
annotate_figure(bp_figure, top = text_grob("Boxplot of Categorical & Numerical Variable", face = "bold", size = 12))


# 3) Numerical & Numerical Variable

# tenure * MonthlyCharges & TotalCharges

# Create new data frame - charges
charges <- Telco[c(6,19,20)] # select tenure, MonthlyCharges, TotalCharges data
colSums(is.na(charges)) # Examin Na before calculating Correlation Coefficient  
charges <- na.omit(charges) # omit Na in charges data frame
charges$tenure.MonthlyCharges <- charges$tenure*charges$MonthlyCharges # add new column

# calculate Correlation Coefficient
charges_cor <- round(cor(charges$tenure.MonthlyCharges,charges$TotalCharges),digit=4)

# Scatter Plot (sp) of Charges
charges_sp <- ggplot(charges, aes(x=tenure.MonthlyCharges, y=TotalCharges)) +
  geom_point() + geom_smooth(method = "lm", colour = "green3") + # add point and line
  ggtitle("Scatter Plot of Numerical & Numerical Variable") + # put main title
  xlab("tenure * MonthlyCharges") + # adjust x axis label
  geom_text(x=2500, y=7500, label=paste0("r=",charges_cor), size=8, color="green4") + # put annotation
  theme(plot.title = element_text(face="bold")) # bold title
charges_sp


#---Step 4: Missing Value Treatment---

# Examin Na 
colSums(is.na(Telco)) 
# Create new data frame with no NA to find missing reasons
Telco.na <- Telco[is.na(Telco$TotalCharges),] 
# missing reason: tenure=0
Telco.na[c("tenure", "MonthlyCharges", "TotalCharges")]

# replace "TotalCharges" with "tenure*MonthlyCharges"
Telco$TotalCharges <- ifelse(is.na(TotalCharges), tenure*MonthlyCharges, TotalCharges)


#---Step 5: Outlier Treatment---


#---Step 6: Feature Engineering---

# 1) Variable Transformation
# a) Change target variable data type (churn: Factor -> Nnumeric)
Telco$Churn=as.numeric(ifelse(Telco$Churn=="Yes", 1, 0))

# b) Clean the categorical features 
# Replace column (No phone service, No internet servic -> No)
Telco$MultipleLines[which(Telco$MultipleLines == "No phone service")] = "No"
for(i in 10:15){
  Telco[,i][which(Telco[,i] == "No internet service")] = "No"
}

# Drop level
Telco$MultipleLines <- droplevels(Telco$MultipleLines)
for(j in 10:15){
  Telco[,j] <- droplevels(Telco[,j])
}

# Exanine result
str(Telco[c(8,10:15)])



