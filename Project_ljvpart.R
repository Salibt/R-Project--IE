# general clean up
rm(list = ls())

## set working enviroment
setwd("C:/Users/letic/Documents/GMBD/projects/R")

getwd()



## Install packages 
install.packages ("psych", lib = "C:/Users/letic/Documents/GMBD/projects/R")
install.packages ("gplots", lib = "C:/Users/letic/Documents/GMBD/projects/R")
install.packages ("gmodels", lib = "C:/Users/letic/Documents/GMBD/projects/R")
install.packages ("vcd")
install.packages("lsr")
install.packages("questionr")
library (gplots)
library (psych)
library (gmodels)
library(vcd)
library(lsr)
library(questionr)
library(zoo)

#Reads data from CSV file and stores it in a dataframecalled acc_15 and acc_16

acc_15 <- read.csv ("Accidents_2015.csv", header=TRUE, sep=",", dec=".")
dim(acc_15)
head(acc_15)

acc_16 <- read.csv ("Accidents_2016.csv", header=TRUE,sep=",",dec=".")
dim(acc_16)
head(acc_16)
#QC OK

acc <- rbind(acc_15,acc_16)
dim(acc)
head(acc)
###QC OK

# Define variable as factor and define levelsand labels.
acc$Accident_Severity_f <-factor (acc$Accident_Severity, levels= c(1,2,3),
                        labels = c("Fatal", "Serious", "Slight"))

acc$severity_combined [acc$Accident_Severity_f == "Fatal" | acc$Accident_Severity_f == "Serious"] <- "Serious"
acc$severity_combined [acc$Accident_Severity_f == "Slight" ] <- "Slight"
acc$severity_combined

# Describe numerically
# Frequency count
t <-table (acc$severity_combined)
t
# As proportion over sample size
t1 <-prop.table(t)
t1
# As percentage over sample size
t2 <-t1*100
t2

barplot(t2,
        beside = TRUE,
        col=c("450", "475"),
        xlab = "Accident Severity
        Source: Department of transport, UK Gov., www.data.gov.uk",
        main="Accident Severity Percentage")

### Convert date to quarter
yq <- as.yearqtr(as.yearmon(acc$Date, "%d/%m/%Y") + 1/12)
yq

##Convert quarter into season
acc$Season <- factor(format(yq, "%q"), levels = 1:4, 
                    labels = c("winter", "spring", "summer", "fall"))

#barplot 
t_season_f <- table ( acc$Season)
t1_season_f <- prop.table(t_season_f)*100
t1_season_f

barplot(t1_season_f,
        beside = TRUE,
        col=c("lightskyblue1", "lightskyblue2", "lightskyblue3"),
        xlab = "Season",
        main="Season Percentage")


###DESCRIBE DV (Accident severity) by the factor levels in the IV (cross tabulation of DV by IV)

describe(acc$severity_combined)
          n     %
Serious  45074  16.3
Slight  231603  83.7
Total   276677 100.0

barplot(t2,
        beside = TRUE,
        col=c("lightskyblue1", "lightskyblue2", "lightskyblue3"),
        xlab = "Accident Severity
        Source: Department of transport, UK Gov., www.data.gov.uk",
        main="Accident Severity Percentage")

describe(acc$Season)
        n     %
winter  67006  24.2
spring  66164  23.9
summer  70583  25.5
fall    72924  26.4
Total  276677 100.0

barplot(t1_season_f,
        beside = TRUE,
        col=c("lightskyblue1", "lightskyblue2", "lightskyblue3", "lightskyblue4"),
        xlab = "Season
        Source: Department of transport, UK Gov., www.data.gov.uk",
        ylab = "Percentage", 
        main="Season Percentage")

#DV aggregate distribution
TOTAL<- table(acc$severity_combined)

#Basic crosstable
ct1 <- table(acc$severity_combined,
             acc$Season)
ct1
#Merge together the Cross tab and DV dist
ct2 <- cbind(ct1, TOTAL)
ct2
# Express the table as column percentages
cp_ct2 <- prop.table (ct2,2)*100

# Add column totals (100)
addmargins (cp_ct2,1)

          winter    spring    summer      fall    TOTAL
Serious  15.58069  16.02382  16.88509  16.61182  16.2912
Slight   84.41931  83.97618  83.11491  83.38818  83.7088
Sum     100.00000 100.00000 100.00000 100.00000 100.0000



#### PERFOM THE NUMERIC TEST CHI2 TEST

# We apply the chisq.test() over the original crosstab containing FREQUENCIES
chisq.test(ct1)

### Pearson's Chi-squared test

###data:  ct1
###X-squared = 52.025, df = 3, p-value = 2.959e-11 (0.000)

###CONCLUSION 
### As P. Value < 0.05 we reject H0 
### H0.: Percentage of severity of accidents with fatal/serious/slight is the same in winter, spring, summer, fall
### H1.: Percentage of severity of accidents with fatal/serious/slight differes in winter, spring, summer, fall

###GRAPHIC REPRESENTATION COMBINED BARPLOT 

par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
barplot(cp_ct2,
        beside = TRUE,
        col=c("lightskyblue1", "lightskyblue2"), xlab = "SEASON
        Source: Department of transport, UK Gov., www.data.gov.uk", 
        main="Accident Severity, by Season (percentage)")
legend("topright", inset=c(-0.2,0),legend = c("Serious", "Slight", " ", "Chi2: 52.025", "P.Val: 0.000"), fill=c("lightskyblue1", "lightskyblue2", "white", "white", "white"), border="white")

##Cramer's V season  (0 no association, 1 perfect association)
cramersV(ct1)
##[1] 0.01371254


###MOSAIC PLOT
mosaic(~acc$Season + acc$severity_combined, data=acc, shade=TRUE, legend=TRUE)

############################################################################

### IV 2 AREA TYPE URBAN, RURAL, UNALLOCATED
##Avoiding artefacts
acc$Urban_or_Rural_Area_c <- acc$Urban_or_Rural_Area

###remove unallocated
acc$Urban_or_Rural_Area_c [acc$Urban_or_Rural_Area_c==3] <- NA

hist (acc$Urban_or_Rural_Area_c)
table (round(acc$Urban_or_Rural_Area_c))

# Define variable as factor and define levelsand labels.
acc$area_type_f <-factor (acc$Urban_or_Rural_Area_c, levels= c(1,2),
                                  labels = c("Urban", "Rural"))
# Describe numerically
# Frequency count
t_area <-table (acc$area_type_f)
t_area
# As proportion over sample size
t1_area <-prop.table(t_area)
t1_area
# As percentage over sample size
t2_area <-t1_area*100
t2_area

barplot(t2_area,
        beside = TRUE,
        col=c("lightskyblue1", "lightskyblue2"),
        xlab = "Area Type
        Source: Department of transport, UK Gov., www.data.gov.uk",
        ylab = "Percentage",
        main="Area Type Percentage")

describe(acc$area_type_f)

#              n     %
#Urban       180548  65.3
#Rural        96122  34.7
#NA               7   0.0
#Total       276677 100.0

#Basic crosstable
ct1_area <- table(acc$severity_combined,
             acc$area_type_f)
ct1_area
#Merge together the Cross tab and DV dist
ct2_area <- cbind(ct1_area, TOTAL)
ct2_area
# Express the table as column percentages
cp_ct2_area <- prop.table (ct2_area,2)*100
cp_ct2_area

# Add column totals (100)
addmargins (cp_ct2_area,1)

            Urban   Rural    TOTAL
Serious  13.96748  20.655  16.2912
Slight   86.03252  79.345  83.7088
Sum     100.00000 100.000 100.0000

#### PERFOM THE NUMERIC TEST CHI2 TEST

# We apply the chisq.test() over the original crosstab containing FREQUENCIES
chisq.test(ct1_area)


### Pearson's Chi-squared test

### data:  ct1_area
### X-squared = 2056.7, df = 1, p-value < 2.2e-16


###CONCLUSION 
### As P. Value < 0.05 we reject H0 
### H0.: Percentage of severity of accidents with fatal/serious/slight is the same in Urban and Rural area
### H1.: Percentage of severity of accidents with fatal/serious/slight is not the same in Urban and Rural area

###GRAPHIC REPRESENTATION COMBINED BARPLOT 

par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
barplot(cp_ct2_area,
        beside = TRUE,
        col=c("lightskyblue1", "lightskyblue2"),
        xlab = "Area Type
        Source: Department of transport, UK Gov., www.data.gov.uk",
        ylab = "Percentage",
        main="Accident Severity, 
        by Area Type (percentage)")
legend("topright", inset=c(-0.2,0),legend = c("Serious", "Slight", " ", "Chi2: 2056.7", "P.Val: 0.000"), fill=c("lightskyblue1", "lightskyblue2", "white", "white", "white"), border="white")

##Cramer's V season  (0 no association, 1 perfect association)
cramersV(ct1_area)
#[1] 0.08621851


###MOSAIC PLOT
mosaic(~acc$area_type_f + acc$severity_combined, data=acc, shade=TRUE, legend=TRUE)

##SQUARE PANEL OF PLOTS
par(mfrow=c(2,2))






