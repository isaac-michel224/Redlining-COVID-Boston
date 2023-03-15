
#---Bring Back File with Change Variables Updated----#

library(tidyverse)
library(readxl)
library(magrittr)
library(writexl)
library(RcppRoll)
library(janitor)
library(zoo)
library(ggplot2)



#Import Boston Data File 
fb <- read_xlsx("C:/Users/michelia/Documents/COVID-Redlining/Data/Full_Boston.xlsx")

fbx <- fb %>%
  filter(year == '2020')

#write_xlsx(fbx, "C:/Users/michelia/Documents/Boston Dems/fbx.xlsx")

view(fbx)
#---Unadjusted Models with Redlining Variable----#

t.test(Spring_Case_Rate ~ Redlined, data = fbx, var.equal = TRUE) 
t.test(`Fall Rate` ~ Redlined, data = fbx, var.equal = TRUE) 


s20 <- lm(formula = Spring_Case_Rate ~ Redlined, data = fbx)
print(summary(s20))
confint(s20, level=0.95)


f20 <- lm(formula = `Fall Rate` ~ Redlined, data = fbx)
print(summary(f20))
confint(f20, level=0.95)

#----Unadjusted Models with HOLC Variable----#

test1 <- aov(Spring_Case_Rate ~ HOLC_Grade, data = fbx)
summary(test1) #Significant

test2 <- aov(`Fall Rate` ~ HOLC_Grade, data = fbx)
summary(test2) #Significant


#Post Hoc Test
TukeyHSD(test1)
TukeyHSD(test2)


holc1 <- lm(formula = Spring_Case_Rate ~ HOLC_Grade, data = fbx)
print(summary(holc1))
confint(holc1, level=0.95)


holc2 <- lm(formula = `Fall Rate` ~ HOLC_Grade, data = fbx)
print(summary(holc2))
confint(holc2, level=0.95)

#----Adjusted Models/w Redlined Variable and Change Variables-----#

#POC
Ad <- lm(formula = `Spring_Case_Rate` ~ Redlined + poc_change, data = fbx)
print(summary(Ad))
confint(Ad, level=0.95)

Adx <- lm(formula = `Fall Rate` ~ Redlined + poc_change, data = fbx)
print(summary(Adx))
confint(Adx, level=0.95)


plot(Ad)

plot(Adx)

#Less than High School

Adm <- lm(formula = `Spring_Case_Rate` ~ Redlined + hs_change, data = fbx)
print(summary(Adm))
confint(Adm, level=0.95)

Ady <- lm(formula = `Fall Rate` ~ Redlined + hs_change, data = fbx)
print(summary(Ady))
confint(Ady, level=0.95)

#Renter-occupied

Adb <- lm(formula = `Spring_Case_Rate` ~ Redlined + rent_change, data = fbx)
print(summary(Adb))
confint(Adb, level=0.95)

Adc <- lm(formula = `Fall Rate` ~ Redlined + rent_change, data = fbx)
print(summary(Adc))
confint(Adc, level=0.95)

#Male

Ade <- lm(formula = `Spring_Case_Rate` ~ Redlined + male_change, data = fbx)
print(summary(Ade))
confint(Ade, level=0.95)

Adf <- lm(formula = `Fall Rate` ~ Redlined + male_change, data = fbx)
print(summary(Adf))
confint(Adf, level=0.95)


#Foreign Born

Adg <- lm(formula = `Spring_Case_Rate` ~ Redlined + foreign_change, data = fbx)
print(summary(Adg))
confint(Adg, level=0.95)

Adh <- lm(formula = `Fall Rate` ~ Redlined + foreign_change, data = fbx)
print(summary(Adh))
confint(Adh, level=0.95)


#65-Plus

Adi <- lm(formula = `Spring_Case_Rate` ~ Redlined + `65_change`, data = fbx)
print(summary(Adi))
confint(Adi, level=0.95)

Adk <- lm(formula = `Fall Rate` ~ Redlined + `65_change`, data = fbx)
print(summary(Adk))
confint(Adk, level=0.95)


#(Post) T-tests with Redlined Variable 


t.test(poc_change ~ Redlined, data = fbx, var.equal = TRUE) 
t.test(hs_change ~ Redlined, data = fbx, var.equal = TRUE) 
t.test(foreign_change ~ Redlined, data = fbx, var.equal = TRUE) 
t.test(rent_change ~ Redlined, data = fbx, var.equal = TRUE) 
t.test(`65_change` ~ Redlined, data = fbx, var.equal = TRUE) 
t.test(`male_change` ~ Redlined, data = fbx, var.equal = TRUE) 

t.test(POC ~ Redlined, data = fbx, var.equal = TRUE) 
t.test(`less_than_high_school` ~ Redlined, data = fbx, var.equal = TRUE) 
t.test(foreign_born ~ Redlined, data = fbx, var.equal = TRUE) 
t.test(renter_occupied ~ Redlined, data = fbx, var.equal = TRUE) 
t.test(`x65_years_and_over` ~ Redlined, data = fbx, var.equal = TRUE) 
t.test(`male` ~ Redlined, data = fbx, var.equal = TRUE) 


#---------R-Statistic Correlation between Outcome (Case Rates) + Indicators (Change Variables) ------#


#---Redlined Cor Test with Significance----#

#POC
cor.test(fbx$`poc_change`,fbx$Spring_Case_Rate)
cor.test(fbx$`poc_change`,fbx$`Fall Rate`)

#White
cor.test(fbx$`wh_change`,fbx$Spring_Case_Rate)
cor.test(fbx$`wh_change`,fbx$`Fall Rate`)

#Black
cor.test(fbx$`black_change`,fbx$Spring_Case_Rate)
cor.test(fbx$`black_change`,fbx$`Fall Rate`)

#Hispanic
cor.test(fbx$`hispanic_change`,fbx$Spring_Case_Rate)
cor.test(fbx$`hispanic_change`,fbx$`Fall Rate`)

#Asian/PI
cor.test(fbx$`api_change`,fbx$Spring_Case_Rate)
cor.test(fbx$`api_change`,fbx$`Fall Rate`)



#---Change Variable with 2020 Levels---#

cor.test(fbx$`renter_occupied`, fbx$`poc_change`)
cor.test(fbx$`foreign_born`, fbx$`poc_change`)
cor.test(fbx$`less_than_high_school`, fbx$`poc_change`)
cor.test(fbx$`x65_years_and_over`, fbx$`poc_change`)
cor.test(fbx$male, fbx$`poc_change`)




#------Full Model with Everything----#

Top <- lm(formula = `Spring_Case_Rate` ~ Redlined + `65_change` + `poc_change` 
          + `male_change` + `foreign_change` + `hs_change` + `rent_change`, data = fbx)
print(summary(Top))
confint(Top, level=0.95)

Tox <- lm(formula = `Fall Rate` ~ Redlined + `65_change` + `poc_change` 
          + `male_change` + `foreign_change` + `hs_change` + `rent_change`, data = fbx)
print(summary(Tox))
confint(Tox, level=0.95)


#--------Racial Composition Models-----#

rac_1 <- lm(formula = `Spring_Case_Rate` ~ `poc_change` + 
              `x65_years_and_over` + `less_than_high_school` + `foreign_born` + male + `renter_occupied`, data = fbx)
print(summary(rac_1))
confint(rac_1, level=0.95)

rac_2 <- lm(formula = `Fall Rate` ~ `poc_change` + 
              `x65_years_and_over` + `less_than_high_school` + `foreign_born` + male + `renter_occupied`, data = fbx)
print(summary(rac_2))
confint(rac_2, level=0.95)


rac_3 <- lm(formula = `Spring_Case_Rate` ~ `hispanic_change`, data = fbx)
print(summary(rac_3))
confint(rac_3, level=0.95)

rac_4 <- lm(formula = `Fall Rate` ~ `hispanic_change`, data = fbx)
print(summary(rac_4))
confint(rac_4, level=0.95)


change_data <- fbx %>%
  select(area, `65_change`, rent_change, hs_change, male_change, foreign_change, 
         poc_change, wh_change, api_change, black_change, hispanic_change)

#------2020 Demographics------#

demo20 <- fbx %>%
  select("x65_years_and_over", "renter_occupied", "less_than_high_school", "male",
         "foreign_born", "POC", "white", "asian_pi", "black_african_american",
         "hispanic")


library(vtable)
#Change Variables
st(change_data, add.median = TRUE)

#Demographics 2020
st(demo20, add.median = TRUE)

library(corrplot)
library(Hmisc)
#http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software
race_demo <- fbx %>%
  select(`Spring_Case_Rate`, `Fall Rate`,`65_change`, rent_change, hs_change, male_change, foreign_change, 
         poc_change)

corrplot(race_demo)


#write_xlsx(change_data, "C:/Users/michelia/Documents/Boston Dems/change_var_boston.xlsx")

#Intersectionality Data by State ??? 


#Black Change and Less than High School 

black_hs1 <- lm(formula = `Spring_Case_Rate` ~ `black_change` + 
                  `less_than_high_school`, data = fbx)
print(summary(black_hs1))
confint(black_hs1, level=0.95)


black_hs2 <- lm(formula = `Fall Rate` ~ `black_change` + 
                  `less_than_high_school`, data = fbx)
print(summary(black_hs2))
confint(black_hs2, level=0.95)

plot(black_hs1)
plot(black_hs2)


#Hispanic Change and Less than High School 

brown_1 <- lm(formula = `Spring_Case_Rate` ~ `hispanic_change` + `less_than_high_school`, data = fbx)
print(summary(brown_1))
confint(brown_1, level=0.95)


brown_2 <- lm(formula = `Fall Rate` ~ `hispanic_change`+ `less_than_high_school`, data = fbx)
print(summary(brown_2))
confint(brown_2, level=0.95)

plot(brown_1)
plot(brown_2)


