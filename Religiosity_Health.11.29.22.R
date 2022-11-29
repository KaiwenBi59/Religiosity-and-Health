###########################################################################################################
#################################Religiosity and Health: An Ecological Study###############################
###########################################Lucia Lam & Kaiwen Bi###########################################
###############Please contact Kaiwen Bi (kaiwenbi@connect.hku.hk), if any error is found###################
###########################################################################################################

library(tidyverse) #data wrangling
library(countrycode) #accommodate variations in country names
library(ggplot2) #data viz
library(ggrepel) #data viz
library(ggpmisc) #data viz
library(gvlma) #model checking
library(car) #model checking
library(stats) #cor.test

options(scipen = 999) #turn off scientific notation

setwd("~/Desktop/HALE and Religiosity/data")

#load outcome variable dataset
HALE <- read.csv("HALE_LE.csv")

##clean WHO HALE & LE data first

#HALE
table(HALE$Dim1)
HALE <- filter(HALE, HALE$Dim1 == "Both sexes")
table(HALE$Period)
HALE <- filter(HALE, HALE$Period == 2019)
table(HALE$Location.type)
table(HALE$Location)
table(HALE$Indicator)
HALE_birth <- filter(HALE, HALE$Indicator == "Healthy life expectancy (HALE) at birth (years)")
HALE_age60 <- filter(HALE, HALE$Indicator != "Healthy life expectancy (HALE) at birth (years)")
HALE_birth <- HALE_birth[,c("Location","Value")]

colnames(HALE_birth)[[1]] <- "country"
colnames(HALE_birth)[[2]] <- "HALE"

#countrycode conversion
HALE_birth$countrycode <- countrycode(HALE_birth$country, "country.name", "iso2c")

###LE
LE <- read.csv("LE.csv")
##clean WHO LE data first
table(LE$Dim1)
LE <- filter(LE, LE$Dim1 == "Both sexes")
LE <- filter(LE, LE$Period == 2019)

LE_birth <- filter(LE, LE$Indicator == "Life expectancy at birth (years)")
LE_age60 <- filter(LE, LE$Indicator != "Life expectancy at birth (years)")

LE_birth <- LE_birth[,c("Location","Value")]

colnames(LE_birth)[[1]] <- "country"
colnames(LE_birth)[[2]] <- "LE"
#countrycode conversion
LE_birth$countrycode <- countrycode(LE_birth$country, "country.name", "iso2c")

#merge HALE and LE
HALE_LE <- merge(HALE_birth, LE_birth, by.x = "countrycode", by.y = "countrycode", all=T)

##clean religiosity

religiosity <- readxl::read_xlsx("religiosity.xlsx") #https://ceoworld.biz/2020/05/16/revealed-the-worlds-most-and-least-religious-countries-based-on-religious-beliefs-2020/
#percentage of people who feel religious vs feel not religious based on 370000 citizens Global Business Policy Institute & CEOWORLD magazine

#countrycode conversion
religiosity$countrycode <- countrycode(religiosity$Country, "country.name", "iso2c")

dat <- merge(HALE_LE, religiosity, by.x = "countrycode", by.y = "countrycode", all.y=T)
colnames(dat)[[8]] <- "religiosity"

##add confounders: GDP, GDP per capita, inequality, peace, gender equality, infant mortality, literacy, healthcare

GDP_per <- read.csv("GDP_per.csv") #GDP in 2019 world bank
GDP_per <- GDP_per[,c("Country.Name","X2019")]
colnames(GDP_per)[2] <- "GDP_per"
GDP_per$countrycode <- countrycode(GDP_per$Country.Name, "country.name", "iso2c")
dat <- merge(dat, GDP_per, by.x = "countrycode", by.y = "countrycode", all.x=T)

GDP <- read.csv("GDP.csv") #GDP in 2019 world bank
GDP <- GDP[,c("Country.Name","X2019")]
colnames(GDP)[2] <- "GDP"
GDP$countrycode <- countrycode(GDP$Country.Name, "country.name", "iso2c")
dat <- merge(dat, GDP, by.x = "countrycode", by.y = "countrycode", all.x=T)

gender_equality <- readxl::read_xlsx("gender_inequality.xlsx")
gender_equality <- gender_equality[,c("Country","gender_inequality")]
gender_equality$countrycode <- countrycode(gender_equality$Country, "country.name", "iso2c")
dat <- merge(dat, gender_equality, by.x = "countrycode", by.y = "countrycode", all.x=T)

peace <- read.csv("gpi-2008-2019.csv") #GPI index The Institute for Economics and Peace 
peace <- peace[,c("Country","X2019.score")]
colnames(peace)[2] <- "peace"
peace$countrycode <- countrycode(peace$Country, "country.name", "iso2c")
dat <- merge(dat, peace, by.x = "countrycode", by.y = "countrycode", all.x=T)
cor.test(dat$HALE, dat$peace)
mortality <- read.csv("infant_mortality.csv") 
mortality <- mortality[,c("Country.Name","X2019")]
colnames(mortality)[2] <- "infant_mortality"
mortality$countrycode <- countrycode(mortality$Country.Name, "country.name", "iso2c")
dat <- merge(dat, mortality, by.x = "countrycode", by.y = "countrycode", all.x=T)

literacy <- read.csv("literacy.csv") 
literacy <- literacy[,c("Country.Name","X2019")]
colnames(literacy)[2] <- "literacy"
literacy$countrycode <- countrycode(literacy$Country.Name, "country.name", "iso2c")
dat <- merge(dat, literacy, by.x = "countrycode", by.y = "countrycode", all.x=T)

healthcare <- read.csv("health_expen.csv") 
healthcare <- healthcare[,c("Country.Name","X2019")]
colnames(healthcare)[2] <- "healthcare"
healthcare$countrycode <- countrycode(healthcare$Country.Name, "country.name", "iso2c")
dat <- merge(dat, healthcare, by.x = "countrycode", by.y = "countrycode", all.x=T)
colnames(dat)

dat$country <- countrycode(dat$countrycode,  "iso2c", "country.name")

skim(dat) #literacy will be excluded due to large amount of missing across years

#keep only used columns
dat <- dat[,c("country",
              "HALE","LE", 
              "religiosity",
              "GDP_per","GDP","gender_inequality","peace","infant_mortality","healthcare")]

skimr::skim(dat)

#listwise deletion
dat <- dat[complete.cases(dat),]

#correlation matrix
x<- dat[,-c(1:2)]
cor.table<-cor(x, method = "pearson", use = "pairwise.complete.obs")
cor.table

#data viz for HALE
ggplot(dat, aes(religiosity, HALE)) + stat_poly_line() +
  stat_poly_eq() +
  geom_point() +
  geom_text_repel(aes(label = country), max.overlaps = 50, min.segment.length	=10) + jtools::theme_apa() + xlab("Religiosity") + ylab("Healthy Life Expectancy")

ggplot(dat, aes(peace, HALE)) + stat_poly_line() +
  stat_poly_eq() +
  geom_point() +
  geom_text_repel(aes(label = country), max.overlaps = 50, min.segment.length	=10) + jtools::theme_apa() + xlab("Conflict Index") + ylab("Healthy Life Expectancy")

cor.test(dat$HALE, dat$religiosity)

cor.test(dat$gender_inequality, dat$infant_mortality) #0.86
dat$composite <- scale(dat$gender_inequality) + scale(dat$infant_mortality)

#multiple linear regression for HALE
lm1.HALE <- lm(sqrt(HALE) ~ scale(GDP_per) + scale(GDP) + scale(peace) + scale(composite) + 
           scale(healthcare) + 
            scale(religiosity), data = dat)
summary(lm1.HALE)
car::vif(lm1.HALE)
confint(lm1.HALE)
gvlma::gvlma(lm1.HALE)


#data viz for LE
ggplot(dat, aes(religiosity, LE)) + stat_poly_line() +
  stat_poly_eq() +
  geom_point() +
  geom_text_repel(aes(label = country), max.overlaps = 50, min.segment.length	=10) +
  jtools::theme_apa() + xlab("Religiosity") + ylab("Life Expectancy")

ggplot(dat, aes(peace, LE)) + stat_poly_line() +
  stat_poly_eq() +
  geom_point() +
  geom_text_repel(aes(label = country), max.overlaps = 50, min.segment.length	=10) + 
  jtools::theme_apa() + xlab("Conflict Index") + ylab("Life Expectancy")

#multiple linear regression for LE
lm1.LE <- lm(sqrt(LE) ~ scale(GDP_per) + scale(GDP) + scale(peace) +
             scale(composite) + 
             scale(healthcare) + 
             scale(religiosity), data = dat)
summary(lm1.LE)
car::vif(lm1.LE)
confint(lm1.LE)
gvlma::gvlma(lm1.LE)

