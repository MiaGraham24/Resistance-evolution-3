#install and load packages----
library(dplyr)
library(ggplot2)
library(palmerpenguins)
library(glm2)
library(binom)
library(lme4)
library(tidyverse)
#________________----

#Input data and clean----
# IMPORT DATA ----
fungaltom_raw <- read.csv ("fungal_tomato.csv")
fungalaub_raw <- read.csv ("fungal_aubergine.csv")
controltom_raw <- read.csv ("control_tomato.csv")
controlaub_raw <- read.csv ("control_aubergine.csv")
wasptom_raw   <-read.csv ("encarsia_tomato.csv")
waspaub_raw   <-read.csv ("encarsia_aubergine.csv")
alttom_raw <-read.csv ("alternating_tomato.csv")
altaub_raw  <-read.csv ("alternating_aubergine.csv")
#________________----
#compare mortality probability at 140 timestep
#subset all data frames to 140 
#merge all together
#plot violin plot
#________________----
Fungalt140<-subset(fungaltom_raw, time_step=="140") 
Fungalt140<-Fungalt140[c("replicate", "ID", "Fun.mort","time_step")]
Fungalt140["Treatment"]<-"Fungal"
Fungalt140["Crop"]<-"Tomato"
Fungalt140 
names(Fungalt140)[names(Fungalt140) == "Fun.mort"] <- "prob_mortality"
Fungalt140
#
#same for fungal aubergine#
Fungala140<-subset(fungalaub_raw, time_step=="140") 
Fungala140<-Fungala140[c("replicate", "ID", "Fun.mort","time_step")]
Fungala140["Treatment"]<-"Fungal"
Fungala140["Crop"]<-"Aubergine"
Fungala140 
names(Fungala140)[names(Fungala140) == "Fun.mort"] <- "prob_mortality"
Fungala140

#merge fungal together
Fungal140<-rbind(Fungalt140,Fungala140)
Fungal140
#do the same for control##
controlt140<-subset(controltom_raw, time_step=="140") 
controlt140<-controlt140[c("replicate", "ID", "Con.mort","time_step")]
controlt140["Treatment"]<-"Control"
controlt140["Crop"]<-"Tomato"
controlt140 
names(controlt140)[names(controlt140) == "Con.mort"] <- "prob_mortality"
controlt140

#
#same for control aubergine#
controla140<-subset(controlaub_raw, time_step=="140") 
controla140<-controla140[c("replicate", "ID", "Con.mort","time_step")]
controla140["Treatment"]<-"Control"
controla140["Crop"]<-"Aubergine"
controla140 
names(controla140)[names(controla140) == "Con.mort"] <- "prob_mortality"
controla140
#merge#
Control140<- rbind(controlt140,controla140)

#same for encarisa##
encarsiat140<-subset(wasptom_raw, time_step=="140") 
encarsiat140<-encarsiat140[c("replicate", "ID", "Wasp.mort","time_step")]
encarsiat140["Treatment"]<-"Encarsia"
encarsiat140["Crop"]<-"Tomato"
encarsiat140 
names(encarsiat140)[names(encarsiat140) == "Wasp.mort"] <- "prob_mortality"
encarsiat140
#same encarsiaaub#
encarsiaa140<-subset(waspaub_raw, time_step=="140") 
encarsiaa140<-encarsiaa140[c("replicate", "ID", "Wasp.mort","time_step")]
encarsiaa140["Treatment"]<-"Encarsia"
encarsiaa140["Crop"]<-"Aubergine"
encarsiaa140 
names(encarsiaa140)[names(encarsiaa140) == "Wasp.mort"] <- "prob_mortality"
encarsiaa140
#merge# 
encarsia140<- rbind(encarsiat140, encarsiaa140)

#alternating -have to alter treatment so it alternates between fungal and encarsia
alternatet140<-subset(alttom_raw, time_step=="140")
alternatet140<-alternatet140[c("replicate", "ID", "Wasp.mort","Fun.mort", "application","time_step")]
alternatet140$application[alternatet140$application=="2"] <- "Encarsia"
alternatet140$application[alternatet140$application=="1"] <- "Fungal"
alternatewaspt140<-subset(alternatet140, application=="Encarsia")
alternatewaspt140<-alternatewaspt140[c("replicate", "ID", "Wasp.mort", "application","time_step")]
names(alternatewaspt140)[names(alternatewaspt140)=="Wasp.mort"] <- "prob_mortality"

alternatefunt140<-subset(alternatet140, application=="Fungal")
alternatefunt140<-alternatefunt140[c("replicate", "ID", "Fun.mort", "application","time_step")]
names(alternatewaspt140)[names(alternatewaspt140)=="Fun.mort"] <- "prob_mortality"
alternatet140<-rbind(alternatefunt140,alternatewaspt140)
alternatet140["Crop"]<-"Tomato"
alternatet140 


#do the same for alternating aubergine


#merge alternating together
#alternate140<-rbind(alternatet140,alternatea140)



#merge four data sets together to get treatment140 ADD IN ALTERNATE HERE
treatment140<-rbind(Control140,encarsia140,Fungal140)
treatment140


#box
treatment140 |> 
  drop_na(Crop) |> 
  ggplot(aes(x = Treatment, y = (1-prob_mortality))) +
  geom_boxplot(aes(fill = Crop), 
               width = 0.5)+
  labs(x="Treatment", y="Probability of survival")

# change width of boxplot
#violin plot for prob mortality and treatment colour by crop

treatment140|> 
  drop_na(Crop) |> 
  ggplot(aes(x = Treatment, y = 1-prob_mortality)) +
  geom_violin(aes(fill = Crop),
              width = 0.5)+
  labs(x="Treatment", y="Probability of survival")

#abundance plots use ID??
treatment140 |> 
  drop_na(Crop) |> 
  ggplot(aes(x = Treatment, y = ID)) +
  geom_boxplot(aes(fill = Crop), 
               width = 0.5)+
  labs(x="Treatment", y="Whitefly abundance")
  
 
#violin plot for abundance and treatment colour by crop

treatment140|> 
  drop_na(Crop) |> 
  ggplot(aes(x = Treatment, y = ID)) +
  geom_violin(aes(fill = Crop),
              width = 0.5)+
  labs(x="Treatment", y="Whitefly Abundance")

#compare mortality and abundance for timestep 1-140
#subset each crop with all treatments and  timestep 1 and 140 
Fungalt1<-subset(fungaltom_raw, time_step=="1")
Fungalt1<-Fungalt1[c("replicate", "ID", "Fun.mort","time_step")]
Fungalt1["Treatment"]<-"Fungal"
Fungalt1["Crop"]<-"Tomato"
names(Fungalt1)[names(Fungalt1) == "Fun.mort"] <- "prob_mortality"
Fungalt1140<-rbind(Fungalt140,Fungalt1)


controlt1<-subset(controltom_raw, time_step=="1") 
controlt1<-controlt1[c("replicate", "ID", "Con.mort","time_step")]
controlt1["Treatment"]<-"Control"
controlt1["Crop"]<-"Tomato"
controlt1 
names(controlt1)[names(controlt1) == "Con.mort"] <- "prob_mortality"
controlt1
Controlt1140<-rbind(controlt140,controlt1)

encarsiat1<-subset(wasptom_raw, time_step=="1") 
encarsiat1<-encarsiat1[c("replicate", "ID", "Wasp.mort","time_step")]
encarsiat1["Treatment"]<-"Encarsia"
encarsiat1["Crop"]<-"Tomato"
encarsiat1 
names(encarsiat1)[names(encarsiat1) == "Wasp.mort"] <- "prob_mortality"
encarsiat1

Encarsiat1140<-rbind(encarsiat140,encarsiat1)


alternatet1<-subset(alttom_raw, time_step=="1")
alternatet1<-alternatet1[c("replicate", "ID", "Wasp.mort","Fun.mort", "application","time_step")]
alternatet1$application[alternatet1$application=="2"] <- "Encarsia"
alternatet1$application[alternatet1$application=="1"] <- "Fungal"
alternatewaspt1<-subset(alternatet1, application=="Encarsia")
alternatewaspt1<-alternatewaspt1[c("replicate", "ID", "Wasp.mort", "application","time_step")]
names(alternatewaspt1)[names(alternatewaspt1)=="Wasp.mort"] <- "prob_mortality"

alternatefunt1<-subset(alternatet1, application=="Fungal")
alternatefunt1<-alternatefunt1[c("replicate", "ID", "Fun.mort", "application","time_step")]
names(alternatewaspt140)[names(alternatewaspt1)=="Fun.mort"] <- "prob_mortality"
alternatet1<-rbind(alternatefunt1,alternatewaspt1)
alternatet1["Crop"]<-"Tomato"
alternatet1 

Alternatet1140<-rbind(alternatet140,alternatet1)
#merge all in tomato## - add alternating in once fixed
tom1140<-rbind(Fungalt1140,Encarsiat1140,Controlt1140)

#box plot of probabilty of mortality by treatment for tomato crop 1 vs 140
#box
str(tom1140)
tom1140[,'time_step']<-factor(tom1140[,'time_step'])

tom1140 |> 
  drop_na(time_step) |> 
  ggplot(aes(x = Treatment, y = 1-prob_mortality)) +
  geom_boxplot(aes(fill = time_step), 
               width = 0.5)+
  labs(x="Treatment on tomato", y="Probability of survival")

# change width of boxplot
#violin plot for prob mortality and treatment colour by crop

tom1140|> 
  drop_na(time_step) |> 
  ggplot(aes(x = Treatment, y = 1-prob_mortality)) +
  geom_violin(aes(fill = time_step),
              width = 0.5)+
  labs(x="Treatment", y="Probability of survival")

#abundance plots use ID??- sum on number each replicatw 1-20
tom1140 |> 
  drop_na(Crop) |> 
  ggplot(aes(x = Treatment, y = )) +
  geom_boxplot(aes(fill = time_step), 
               width = 0.5)+
  labs(x="Treatment", y="Whitefly abundance")


#violin plot for abundance and treatment colour by crop





# do the same for aubergine
Fungala1<-subset(fungalaub_raw, time_step=="1")
Fungala1<-Fungala1[c("replicate", "ID", "Fun.mort","time_step")]
Fungala1["Treatment"]<-"Fungal"
Fungala1["Crop"]<-"Tomato"
names(Fungala1)[names(Fungala1) == "Fun.mort"] <- "prob_mortality"
Fungala1140<-rbind(Fungala140,Fungala1)


controla1<-subset(controlaub_raw, time_step=="1") 
controla1<-controla1[c("replicate", "ID", "Con.mort","time_step")]
controla1["Treatment"]<-"Control"
controla1["Crop"]<-"Tomato"
controla1 
names(controla1)[names(controla1) == "Con.mort"] <- "prob_mortality"
controla1
Controla1140<-rbind(controla140,controla1)

encarsiaa1<-subset(waspaub_raw, time_step=="1") 
encarsiaa1<-encarsiaa1[c("replicate", "ID", "Wasp.mort","time_step")]
encarsiaa1["Treatment"]<-"Encarsia"
encarsiaa1["Crop"]<-"Tomato"
encarsiaa1 
names(encarsiaa1)[names(encarsiaa1) == "Wasp.mort"] <- "prob_mortality"
encarsiaa1

Encarsiaa1140<-rbind(encarsiaa140,encarsiaa1)


alternatea1<-subset(alttom_raw, time_step=="1")
alternatea1<-alternatet1[c("replicate", "ID", "Wasp.mort","Fun.mort", "application","time_step")]
alternatea1$application[alternatea1$application=="2"] <- "Encarsia"
alternatea1$application[alternatea1$application=="1"] <- "Fungal"
alternatewaspa1<-subset(alternatea1, application=="Encarsia")
alternatewaspa1<-alternatewaspa1[c("replicate", "ID", "Wasp.mort", "application","time_step")]
names(alternatewaspa1)[names(alternatewaspa1)=="Wasp.mort"] <- "prob_mortality"

alternatefuna1<-subset(alternatea1, application=="Fungal")
alternatefuna1<-alternatefuna1[c("replicate", "ID", "Fun.mort", "application","time_step")]
names(alternatewaspa140)[names(alternatewaspa1)=="Fun.mort"] <- "prob_mortality"
alternatea1<-rbind(alternatefuna1,alternatewaspa1)
alternatea1["Crop"]<-"Tomato"
alternatea1 

Alternatea1140<-rbind(alternatea140,alternatea1)
#merge all in tomato## - add alternating in once fixed
aub1140<-rbind(Fungala1140,Encarsiaa1140,Controla1140)
str(aub1140)
aub1140[,'time_step']<-factor(aub1140[,'time_step'])




#box plot of probabilty of mortality by treatment for tomato crop 1 vs 140
#box
aub1140 |> 
  drop_na(time_step) |> 
  ggplot(aes(x = Treatment, y = 1-prob_mortality)) +
  geom_boxplot(aes(fill = time_step), 
               width = 0.5)+
  labs(x="Treatment on Aubergine", y="Probability of survival")

# change width of boxplot
#violin plot for prob mortality and treatment colour by crop

aub1140|> 
  drop_na(time_step) |> 
  ggplot(aes(x = Treatment, y = 1-prob_mortality)) +
  geom_violin(aes(fill = time_step),
              width = 0.5)+
  labs(x="Treatment", y="Probability of survival")



#


#decriptive stats##
summary(controlt140) #mean prob mortality 0.03293
summary(controlt1) # mean prob mortality 0.025
summary(controla140) # mean prob mortality 0.04131
summary(controla1)   # mean prob mortality 0.0321

summary(Fungalt140)  # mean prob mortality 0.577
summary(Fungalt1)    # mean prob mortality 0.658
summary (Fungala140) # mean prob mortality 0.567
summary(Fungala1)    # mean prob mortality 0.654


summary(encarsiat140) # mean prob mortality 0.602
summary(encarsiat1)   # # mean prob mortality 0.711
summary(encarsiaa140) # mean prob mortality 0.564
summary(encarsiaa1)   ## mean prob mortality 0.678

summary(alternatet140)# mean prob mortality
summary(alternatet1)   # mean prob mortality
summary(alternatea140)# mean prob mortality
summary(alternatea1)  # mean prob mortality



#stats for tomato comparing mortality 1-140
#
#fungal
m1<-lm(prob_mortality ~ time_step, data = Fungalt1140)
summary(m1)

# significant difference in mortality 1-140 p<2-16

# control
m2<-lm(prob_mortality ~ time_step, data = Controlt1140)
summary(m2)
# significant difference in mortality 1-140 p<2-16
# 
#encarsia

m3<-lm(prob_mortality ~ time_step, data = Encarsiat1140)
summary(m3)
# significant difference in mortality 1-140 p<2-16


#stats for aubergine
#fungal
m5<-lm(prob_mortality ~ time_step, data = Fungala1140)
summary(m5)

# significant difference in mortality 1-140 p<2-16

# control
m6<-lm(prob_mortality ~ time_step, data = Controla1140)
summary(m6)


# significant difference in mortality 1-140 p<2-16
# 
#encarsia

m7<-lm(prob_mortality ~ time_step, data = Encarsiaa1140)
summary(m7)
# significant difference in mortality 1-140 p<2-16

# stats- compare tomato to aubergine in 140 timestep
#control
m9<-lm(prob_mortality ~ Crop, data = Control140)
summary(m9)
# significant difference in mortality 1-140 p<2-16




# plots of survival over time, then compare f to w, f to a, w to a
# have to use raw data subset for each treatment 
# need to use average mortality for each time step for each replicate, each time step should have 20 points. 
# create new data frame for each treatment with replicate, time_Step treatment, crop av survival. 
# subset out contro

 
controlsurvivalt<-controltom_raw[c("replicate", "ID", "Con.mort","time_step")]
controlsurvivalt["Crop"]<-"Tomato"
controlsurvivalt
names(controlsurvivalt)[names(controlsurvivalt) == "Con.mort"] <- "prob_mortality"
controlsurvivalt
#group the mortality values average by replicate


controlt_survival <- controlsurvivalt %>%
  group_by(time_step, replicate) %>%
  mutate(
    Sum_Value = sum(prob_mortality),
    Mean_Value = mean(prob_mortality),
    Count = n()
  )
print(controlt_survival)

