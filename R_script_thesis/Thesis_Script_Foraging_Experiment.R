install.packages('UsingR')
install.packages('snpar')
install.packages(c("ggplot2", "ggpubr", "tidyverse", "broom", "AICcmodavg"))
library(UsingR)
require(UsingR)
require(ggplot2)
library(snpar)
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)

### importing relative distance travelled ###
Data_monday_morning<-read.csv('Analyse_data_maandagochtend.csv')
Data_monday_afternoon<-read.csv('analyse_data_maandagmiddag.csv')
Data_tuesday_afternoon<-read.csv('Analyse_data_dinsdagmiddag_R.csv')
Data_tuesday_morning<-read.csv('analyse_data_dinsdagochtend.csv')
Data_Wednesday_morning<-read.csv('data_analyse_woensdagochtend.csv')
Data_Wednesday_afternoon<-read.csv('Analyse_data_woensdag_middag.csv')
Data_thursday_morning<-read.csv('analyse_data_donderdagochtend.csv')
Data_thursday_afternoon<-read.csv('analyse_data_donderdagmiddag.csv')
Data_friday_morning<-read.csv('Analyse_data_vrijdagochtend.csv')
Data_friday_afternoon<-read.csv('Analyse_data_vrijdagmiddag.csv')

### importing relative distance data corrected ###
Data_monday_afternoon_corrected<-read_xlsx('analyse_data_maandagmiddag.xlsx',2)
Data_tuesday_afternoon_corrected<-read_xlsx('Analyse_data_dinsdagmiddag.xlsx',2)
Data_tuesday_morning_corrected<-read_xlsx('Analyse_data_dinsdagochtend.xlsx',2)
Data_wednesday_morning_corrected<-read_xlsx('data_analyse_woensdagochtend.xlsx',2)
Data_wednesday_afternoon_corrected<-read_xlsx('Analyse_data_woensdag_middag.xlsx',2)
Data_thursday_morning_corrected<-read_xlsx('analyse_data_donderdagochtend.xlsx',2)
Data_thursday_afternoon_corrected<-read_xlsx('analyse_data_donderdagmiddag.xlsx',2)
Data_friday_morning_corrected<-read_xlsx('Analyse_data_vrijdagochtend.xlsx',2)
Data_friday_afternoon_corrected<-read_xlsx('analyse_data_vrijdagmiddag.xlsx',2)

### importing all morning and afternoon data combined ###
Data_afternoons_corrected<-read_xlsx('Afternoon_data.xlsx',2)
Data_afternoons<-read_xlsx('Afternoon_data.xlsx',1)
Data_mornings_corrected<-read.csv('corrected_morning_data.csv')
Data_mornings<-read.csv('Morning_data.csv')

## runs test to check for randomness in data ###
runs.test(Data_monday_morning$relative_distance_travelled,exact=TRUE) 
runs.test(Data_tuesday_morning$relative.distance,exact=TRUE) 
runs.test(Data_Wednesday_morning$relative_distance,exact=TRUE) 
runs.test(Data_thursday_morning$relative_distance,exact=TRUE)
runs.test(Data_friday_morning$relative.distance,exact=TRUE)
runs.test(Data_monday_afternoon$relative.distance.travelled,exact=TRUE) 
runs.test(Data_tuesday_afternoon$relative_distance,exact=TRUE) 
runs.test(Data_Wednesday_afternoon$relative_distance,exact=TRUE) 
runs.test(Data_thursday_afternoon$relative_distance,exact=TRUE) 
runs.test(Data_friday_afternoon$relative_distance,exact=TRUE) 

### Runs test to check for randomness in data corrected
runs.test(Data_monday_afternoon_corrected$`relative distance travelled`)
runs.test(Data_tuesday_afternoon_corrected$relative_distance)
runs.test(Data_tuesday_morning_corrected$`relative distance`)
runs.test(Data_wednesday_morning_corrected$relative_distance)
runs.test(Data_wednesday_afternoon_corrected$relative_distance)
runs.test(Data_thursday_morning_corrected$relative_distance)
runs.test(Data_thursday_afternoon_corrected$relative_distance)
runs.test(Data_friday_morning_corrected$`relative distance`)
runs.test(Data_friday_afternoon_corrected$relative_distance)


### linear regression of all the mornings, corrected ###
model<-ggplot(data=Data_mornings_corrected,mapping=aes(x=ï..Days,y=Relative_Distances))+
  geom_point()+geom_smooth(method='lm')+labs(title='Relative distances over time in the morning group (corrected)',x='Days',y='Relative Distance')
Model1<-lm(Relative_Distances~ï..Days,data=Data_mornings_corrected)
summary(Model1)

### linear regression of all the afternoons,corrected ###
model2<-ggplot(data=Data_afternoons_corrected,mapping=aes(x=Days,y=Relative_distance))+geom_point()+geom_smooth(method='lm')+labs(title='Relative distance over time in the afternoon group (corrected) ',x='Days',y='Relative Distance')
model3<-lm(Data_afternoons_corrected$Relative_distance~Data_afternoons_corrected$Days)
summary(model3)

### Linear regression of all the mornings uncorrected ###
model4<-ggplot(data=Data_mornings,mapping=aes(x=ï..days, y=Relative_Distances))+geom_point()+geom_smooth(method='lm')+labs(title='Relative distance over time in the morning group (Uncorrected)',x='Days',y='Relative Distance')
model5<-lm(Relative_Distances ~ ï..days, data=Data_mornings)
summary(model5)
### Linear regression of all the afternoons uncorrected ###
model4<-ggplot(data=Data_afternoons,mapping=aes(x=Days, y=Relative_distance))+geom_point()+geom_smooth(method='lm')+labs(title='Relative distance over time in the afternoon group (Uncorrected)',x='Days',y='Relative Distance')
model5<-lm(Relative_distance ~ Days, data=Data_afternoons)
summary(model5)

### Wilcoxon test to follow up on the linear regressions ### 
wilcoxA<-wilcox.test(Data_monday_morning$relative_distance_travelled,Data_friday_morning_corrected$`relative distance`)
WilcoxB<-wilcox.test(Data_monday_afternoon$relative.distance.travelled,Data_friday_afternoon$relative_distance)

### test showing the differences in retrieving of groupcluster 1 ###
X<-c(60,70,70,80,80)
Y<-c(50,50,40,60,50)

wilcox.test(X,Y)