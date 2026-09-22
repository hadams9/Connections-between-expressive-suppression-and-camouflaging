# Authorship and licensing information ------------------------------------

#Original script created by Hannah Adams (hadams9@gmu.edu)
#Last updated 09.22.2026

#This file is part of materials distributed with the manuscript, "Connections 
#between emotion regulation and camouflaging 
#by Hannah Adams and Allison Jack.

#This file is a free script: you can redistribute it and/or modify it under 
#the terms of the GNU General Public License as published by the Free Software
#Foundation, either version 3 of the License, or (at your option) any later
#version.

#This script is distributed in the hope that it will be useful, but WITHOUT ANY 
#WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
#A PARTICULAR PURPOSE. See the GNU General Public License for more details.

#You should have received a copy of the GNU General Public License along with 
#the other materials made available on the manuscript's OSF page. 
#If not, see <https://www.gnu.org/licenses/>.

#Libraries needed
library(gvlma) #gvlma
library(car) #VIF
library(effectsize) #standardized parameters
library(interactions) #johnson-neyman interaction

###Variables 
#Y_cat_q_total = Camouflaging Autistic Traits Questionnaire total score
#X_supres.total = Emotion Regulation Questionnaire- Expressive Suppression subscore 
#W_bapq_center = Broad Autism Traits Questionnaire total score centered
#Z_bgd_total = Gender Self Report- Binary gender diversity // Female Male continuum total score
#CV1_age = Age
#CV2_race = Race/Ethnicity 
#CV3_ses = Socioeconomic status


##model 1 with covariates
mod1cov <- lm(Y_cat_q_total ~ X_supres.total + CV1_age + CV2_race + CV3_ses, data = my_data_frame)
summary(mod1cov) 
confint(mod1cov)
gvlma(mod1cov)
vif(mod1cov)

##model 1 no covariates cat<-es
mod1 <- lm(Y_cat_q_total ~ X_supres.total, data = my_data_frame)
summary(mod1)
confint(mod1)
standardize_parameters(mod1)
gvlma(mod1)
vif(mod1)

#mod2 cat <- es + bap
mod2<- lm(Y_cat_q_total ~ X_supres.total + W_bapq_center, data = my_data_frame)
summary(mod2)
standardize_parameters(mod2)
vif(mod2)


#mod3 cat <- es + bap + gender
mod3 <- lm(Y_cat_q_total ~ X_supres.total + W_bapq_center + Z_bgd_total, data = my_data_frame)
summary(mod3)
lm.beta(mod3)
vif(mod3)
standardize_parameters(mod3)

#mod4 cat <- es + bap + gender + bap*gender 
mod4 <- lm(Y_cat_q_total ~ X_supres.total +  W_bapq_center*Z_bgd_total, data = my_data_frame)
summary(mod4)
vif(mod4)
standardize_parameters(mod4)

#mod5 three way interaction
mod5 <- lm(Y_cat_q_total ~ X_supres.total + W_bapq_center + Z_bgd_total + X_supres.total*W_bapq_center*Z_bgd_total, data = my_data_frame)
summary(mod5)
standardize_parameters(mod5)


##Best fit analysis##

#with or without covariates
anova(mod1, mod1cov) ##no cov is better fit

#full models
anova(mod1, mod2, mod3, mod4, mod5)


###INTERACTION PROBE 
fiti <- lm(Y_cat_q_total ~ W_bapq_center * Z_bgd_total, data = my_data_frame)


interact_plot(fiti, pred = W_bapq_center, modx = Z_bgd_total, interval = TRUE)


# Main interaction plot with labels
plot1 <- interact_plot(fiti, 
                       pred = W_bapq_center, 
                       modx = Z_bgd_total, 
                       interval = TRUE,
                       x.label = "Standarized BAP-Q",
                       y.label = "CAT-Q",
                       legend.main = "Female Male Continuum Scores",
                       modx.labels = c("-1 SD 
(FMC = 0.19; 
greater binary maleness)", "Mean 
(FMC = 0.49)", "+1 SD 
(FMC = 0.78; 
greater binary femaleness)"))
plot1




