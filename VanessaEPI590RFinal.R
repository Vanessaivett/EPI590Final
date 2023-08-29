#Uploading csv file
EbolaCases <- here::here("ebola_data_db_format.csv")
EbolaCases <- read.csv(EbolaCases)

#Reorganize data/ only keep confirmed cases of Ebola Cases by Country and then drop column "Indicator"
library(dplyr)
ConfirmedEbolaCases <- EbolaCases %>% filter(Indicator == "Cumulative number of confirmed Ebola cases")
ConfirmedEbolaCases = subset(ConfirmedEbolaCases, select =  -c(Indicator))

#Change Column Value to Confirmed Cases
colnames(ConfirmedEbolaCases)[3] <- "Confirmed_Cases"

#Separate Date into Month and Year
library(tidyverse)

ConfirmedEbolaCases <- ConfirmedEbolaCases %>% separate(Date, sep ="/", into = c("Month", "Day", "Year"))

#Edit Contry Names
table(ConfirmedEbolaCases$Country, useNA = "always")
ConfirmedEbolaCases$Country[ConfirmedEbolaCases$Country == "Guinea 2"] = "Guinea"
ConfirmedEbolaCases$Country[ConfirmedEbolaCases$Country == "Liberia 2"] = "Liberia"


#Objective 1: gtsummary
library(gtsummary)
tbl_summary(ConfirmedEbolaCases,
						by = Country,
						include = c(Country, Year, Confirmed_Cases))

#Objective 2 Regression

tbl_uvregression(ConfirmedEbolaCases,
								 y= Confirmed_Cases,
								 include = c(Country, Month),
								 method = lm)

Model1 <- lm(Confirmed_Cases ~ Country +Month,
									data = ConfirmedEbolaCases)

tbl_regression(Model1,
							 intercept = TRUE,
							 label = list(Month ~ "Month of Confirmed Case"))

#Objective 3 Figure

hist(ConfirmedEbolaCases$Confirmed_Cases)

# New Function to see the total cases of Ebola in each country
TotalCasesbyCountry <- ConfirmedEbolaCases %>%
	group_by(Country) %>%
	summarise(TotalCases = sum(Confirmed_Cases))

# Renv

renv::init()
