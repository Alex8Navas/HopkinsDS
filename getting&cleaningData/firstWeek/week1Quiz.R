# Week 1 Quiz
library(data.table)
library(xlsx)
library(XML)

dir.create("data")

url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
download.file(url = url, destfile = "data/AmericanCommunitySurvey.csv")

surveyAmerica <- read.csv("data/AmericanCommunitySurvey.csv")

names(surveyAmerica)

# VAL es la variable que indica el valor de la propiedad
# VAL == 24 significa más de $1,000,000
surveyMillion <- surveyAmerica[surveyAmerica$VAL == 24, ]
surveyMillion <- surveyMillion[!is.na(surveyMillion$VAL), ]

# Alternativa con DT: 
surveyAmericaDT <- data.table(surveyAmerica)
surveyAmericaDT[VAL == 24, .N]


# FES: Family type and employment status.
# En esta columna se contiene más de una variable (tipo de familia más estatus laboral). 

url2 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx"
download.file(url = url2, destfile = "data/NaturalGas.xlsx", method = "curl")

gasNat <- read.xlsx("data/NaturalGas.xlsx", 
                    sheetIndex = 1, 
                    rowIndex = 18:23, 
                    colIndex = 7:15)

sum(gasNat$Zip*gasNat$Ext, na.rm = TRUE)


url3 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"
restaurants <- xmlTreeParse(url3, useInternalNodes = TRUE) 

# La insrucción anterior da error. Hay que usar http y no https. 
url4 <- "http://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"
restaurants <- xmlTreeParse(url4, useInternalNodes = TRUE)
rootNode <- xmlRoot(restaurants)
zipcodes <- xpathSApply(rootNode, "//zipcode", xmlValue)
xmlZipcodeDT <- data.table(zipcode = zipcodes)
xmlZipcodeDT[zipcode == "21231", .N]


survey2006 <- fread("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv")
system.time(survey2006[,mean(pwgtp15), by = SEX])
system.time(mean(survey2006$pwgtp15, by = survey2006$SEX))
system.time(sapply(split(survey2006$pwgtp15, survey2006$SEX), mean))
system.time(rowMeans(survey2006)[survey2006$SEX == 1] + rowMeans(survey2006)[survey2006$SEX == 2])
system.time(tapply(survey2006$pwgtp15, survey2006$SEX, mean))
system.time(mean(survey2006[survey2006$SEX == 1, ]$pwgtp15) + mean(survey2006[survey2006$SEX == 2, ]$pwgtp15))

