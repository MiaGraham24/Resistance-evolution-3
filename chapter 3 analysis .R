install(tidyverse)  
install.packages("dplyr")
install.packages("sjmisc")
install.packages(c("ggplot2", "binom", "Barnard"))

#### Library####
library(tidyverse)
library(dplyr)
library(ggplot2) # load the package
library(binom)
library(ggpubr)
library(visreg)
library(sjmisc)
rm(list=ls())

#Enter data####
whitefly<-read.csv("Genetic.csv")
whitefly

#look at headers
head(whitefly)
summary(whitefly)
names(whitefly)
str(whitefly)


whitefly$treatment<-as.factor(whitefly$treatment)
summary(whitefly)

waspdata<-subset(whitefly, treatment == "Wasp")
fungaldata<-subset(whitefly, treatment == "Fungal")
controldata<-subset(whitefly, treatment == "Control")




#FIRST THE WASP GRAPH
#pool rows together for each genotype
wasp.pooled<- 
  waspdata %>%
  group_by (genotype) %>%
  summarise(across(c("nymph", "dead10", "alive10"), ~ sum(.x, na.rm = TRUE)))

summary(wasp.pooled)

wp<-ggplot(wasp.pooled, aes(x = genotype, y = (alive10/nymph))) +
  geom_point() +
  labs(x="Line", y = "Proportional survival")
wp


#SECOND THE FUNGUS GRAPH
#pool rows together for each genotype
fungal.pooled<- 
  fungaldata %>%
  group_by (genotype) %>%
  summarise(across(c("nymph", "dead10", alive10), ~ sum(.x, na.rm = TRUE)))

summary(fungal.pooled)
fp<-ggplot(fungal.pooled, aes(x = genotype, y = (alive10/nymph))) +
  geom_point() +
  labs(x="Line", y = "Proportional survival")
fp
# control data
control.pooled<- 
  controldata %>%
  group_by (genotype) %>%
  summarise(across(c("nymph", "dead10", "alive10", "treatment"), ~ sum(.x, na.rm = TRUE)))

summary(control.pooled)

cp<-ggplot(control.pooled, aes(x = genotype, y = (alive10/nymph))) +
  geom_point() +
  labs(x="Line", y = "Proportional survival")
cp


wp<-ggplot(wasp.pooled, aes(x = genotype, y = (alive10/nymph))) +
  geom_point() +
  labs(x="Line", y = "Proportional survival")


#bind pooled data back together to have 35 lines for each
total <- rbind(control.pooled,wasp.pooled,fungal.pooled)

library(colorBlindness)

x<-whitefly %>% 
  ggplot(aes(x=genotype, 
             y = alive10/nymph))+
  geom_jitter(aes(colour=treatment))+
  labs(x="Line", y = "Proportional survival")
library(RColorBrewer)
x <- whitefly %>% 
  ggplot(aes(x = genotype, y = alive10/nymph)) +
  geom_jitter(aes(colour = treatment)) +
  labs(x = "Line", y = "Proportional survival") +
  scale_color_manual(values = c("Control" = "purple", "Wasp" = "#0072B2", "Fungal" = "orange")) +  # Set specific colors
  theme(
    axis.text.x = element_text(size = 14),  # Increase x-axis text size
    axis.text.y = element_text(size = 14)   # Increase y-axis text size
  )

print(x)

cvdPlot(x) # test colourblindness


library(ggplot2)
library(dplyr)
library(forcats)

# Calculate proportional survival and reorder genotype
whitefly <- whitefly %>%
  mutate(proportional_survival = alive10 / nymph) %>%
  mutate(genotype = fct_reorder(genotype, proportional_survival, .desc=TRUE))
# plot
ggplot(whitefly, aes(x = genotype, y = proportional_survival)) +
  geom_jitter(aes(colour = treatment)) +
  labs(x = "Line", y = "Proportional survival")

# a violin plot of treatment ad mean mortality
# whitefly as data , treatment on x axis, y axis will be survival
# 
# 
v<-ggplot(whitefly, aes(x= treatment, y=(alive10/nymph), fill=treatment)) + 
  geom_violin(trim=FALSE)+
  labs(x="Treatment", y = "Proportional survival")+
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.4) +
  stat_summary(fun=mean, geom="point", size=3, color="black", shape=18) +
  scale_fill_brewer(palette="Set2") + 
  theme_classic()

v




# basic comparison model between treatments
library(lme4)
m1<-glmer(cbind(dead10, alive10) ~ treatment+genotype +(1|plant),
          family = "binomial", data = whitefly)
summary(m1)

treat <- car::Anova(m1)
treat

m1 <- glmer(cbind(dead10, alive10) ~ treatment * genotype + (1 | plant), 
            family = "binomial", data = whitefly)


#compare mortality in fungal treatment to wasp treatment
corr<-filter(.data = whitefly, treatment !="Control")
corr
m2<-glmer(cbind(dead10, alive10) ~ treatment+genotype +(1|plant),
          family = "binomial", data = corr)
summary(m2)


#reaction norm plot control'fungal

Treatments1<- subset(whitefly, treatment!="Wasp")

funcr<-ggplot(Treatments1, aes(x = treatment, y = alive10/nymph, group = genotype, colour= genotype)) +
  geom_line(alpha = 1.2, size=1)+ 
  geom_point(alpha = 1.6,size = 3) +ylab("Proportion of whitefly survived at 10 Days") + theme_classic() 

#reaction norm plot control/wasp
Treatments2<- subset(whitefly, treatment!="Fungal")

waspcr<-ggplot(Treatments2, aes(x = treatment, y = alive10/nymph, group = genotype, colour= genotype)) +
  geom_line(alpha = 1.2, size=1)+ 
  geom_point(alpha = 1.6,size = 3) +ylab("Proportion of whitefly survived at 10 Days") + theme_classic() 

Treatments3<-subset(whitefly, treatment!="Control")
efr<-ggplot(Treatments3, aes(x = treatment, y = alive10/nymph, group = genotype, colour= genotype)) +
  geom_line(alpha = 1.2, size=1)+ 
  geom_point(alpha = 1.6,size = 3) +ylab("Proportion of whitefly survived at 10 Days") + theme_classic() 

efr 
cross<-ggplot(whitefly, aes(x = treatment, y = alive10/nymph, group = genotype, colour= genotype)) +
  geom_line(alpha = 1.2, size=1)+ 
  geom_point(alpha = 1.6,size = 3) +ylab("Proportion of whitefly survived at 10 Days") + theme_classic() 
cross





