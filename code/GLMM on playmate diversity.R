#libraries, some or them aren´t needed, but good to have them
library(MuMIn)
library(ggpubr)
library(ggplot2)
library(lme4)
library(Matrix)
library(lmerTest)
library(ggeffects)
library(gridExtra)
library(sjPlot)
library(sjmisc)
library(dplyr)
library(glmmTMB)
library(broom)
library(car)
library(equatiomatic)
library(FSA)
library(DescTools)
library(AICcmodavg)

#equatiomatic::extract_eq(glm.play)

#load data
eigen.df <- read.csv2("play metrics per id.csv")

#Zs
cb.play<-cbind(eigen.df$degree,eigen.df$num.not.players)
eigen.df$z.eigen <- as.numeric(scale(eigen.df$eigen))
eigen.df$z.close <- as.numeric(scale(eigen.df$close))
eigen.df$z.btw <- as.numeric(scale(eigen.df$btw))
eigen.df$age.sex.class2 <- as.factor(eigen.df$age.sex.class2)
#check levels
levels(eigen.df$age.sex.class2)


#######check GLMMs
glmm.btw <- glmer(cb.play ~ z.playrate +
                 z.btw +
                 z.ir +
                 age.sex.class2 + 
                 z.group+
                   (1|group.name),
               family = binomial,
               data = eigen.df)#variance 0

glmm.cls <- glmer(cb.play ~ z.playrate +
                 z.close +
                 z.ir +
                 age.sex.class2 + 
                 z.group+
                   (1|group.name),
               family = binomial,
               data = eigen.df)

glmm.eigen <- glmer(cb.play ~ z.playrate +
                   z.eigen +
                   z.ir +
                   age.sex.class2 + 
                   z.group+
                     (1|group.name),
                 family = binomial,
                 data = eigen.df)

#all random variance is around 0 in all of them, so use GLMs
summary(glmm.eigen)
summary(glmm.cls)
summary(glmm.btw)


#############################GLM models
glm.btw <- glm(cb.play ~ z.playrate +
                 z.btw +
                 z.ir +
                 age.sex.class2 + 
                 z.group,
               family = binomial,
               data = eigen.df)

glm.cls <- glm(cb.play ~ z.playrate +
                 z.close +
                 z.ir +
                 age.sex.class2 + 
                 z.group,
               family = binomial,
               data = eigen.df)

glm.eigen <- glm(cb.play ~ z.playrate +
                   z.eigen +
                   z.ir +
                   age.sex.class2 + 
                   z.group,
                 family = binomial,
                 data = eigen.df)

glm.null <- glm(cb.play ~ 1,
                family = binomial,
                data = eigen.df)

#############################AICc between three candidate models SIMPLE
AIC(glm.null, glm.btw, glm.cls, glm.eigen)

#let´s arrange them better
# Create candidate set as a list
cand.set <- list(glm.null, glm.btw, glm.cls, glm.eigen)

# Define model names
modnames <- c("glm.null", "glm.betweenness", "glm.closeness", "glm.eigenvector")

# Calculate AIC table
aictab(cand.set, modnames = modnames, second.ord = TRUE, sort = TRUE)

#Check autocorrelation
vif(glm.btw)
vif(glm.cls)
vif(glm.eigen)

########################################PLOTING BEST MODEL
summary(glm.eigen)
# Generate predictions for z.eigen
pred_eigen <- ggpredict(glm.eigen, terms = "z.eigen")
pred_play.rate <- ggpredict(glm.eigen, terms = "z.playrate ")
pred_ir <- ggpredict(glm.eigen, terms = "z.ir")
pred_group <- ggpredict(glm.eigen, terms = "z.group")
pred_ageclass <- ggpredict(glm.eigen, terms = "age.sex.class2")

plot(pred_ageclass)+ggtitle("a")+ 
  theme(legend.position = c(0.25, 0.8),legend.title = element_blank())+
  xlab("age-sex class") + ylab("proportion of playmates")+
  theme(text = element_text(size=13))+
  theme(panel.background = element_blank())+
  theme(panel.border = element_rect(fill = NA, colour = "black", size = 1))+
  theme(strip.background = element_rect(fill = "white", color = "white", size = 1))


plot(pred_eigen)+ggtitle("b")+ 
  theme(legend.position = c(0.25, 0.8),legend.title = element_blank())+
  xlab("standarized eigenvector centrality") + ylab("proportion of playmates")+
  theme(text = element_text(size=13))+
  theme(panel.background = element_blank())+
  theme(panel.border = element_rect(fill = NA, colour = "black", size = 1))+
  theme(strip.background = element_rect(fill = "white", color = "white", size = 1))

plot(pred_ir)+ggtitle("c")+ 
  theme(legend.position = c(0.25, 0.8),legend.title = element_blank())+
  xlab("standarized immature to adult ratio") + ylab("proportion of playmates")+
  theme(text = element_text(size=13))+
  theme(panel.background = element_blank())+
  theme(panel.border = element_rect(fill = NA, colour = "black", size = 1))+
  theme(strip.background = element_rect(fill = "white", color = "white", size = 1))

plot(pred_group)+ggtitle("d")+ 
  theme(legend.position = c(0.25, 0.8),legend.title = element_blank())+
  xlab("standarized group size") + ylab("proportion of playmates")+
  theme(text = element_text(size=13))+
  theme(panel.background = element_blank())+
  theme(panel.border = element_rect(fill = NA, colour = "black", size = 1))+
  theme(strip.background = element_rect(fill = "white", color = "white", size = 1))


plot(pred_play.rate)+ggtitle("e")+ 
  theme(legend.position = c(0.25, 0.8),legend.title = element_blank())+
  xlab("standarized play rate (sec/hr)") + ylab("proportion of playmates")+
  theme(text = element_text(size=13))+
  theme(panel.background = element_blank())+
  theme(panel.border = element_rect(fill = NA, colour = "black", size = 1))+
  theme(strip.background = element_rect(fill = "white", color = "white", size = 1))



#######arrange plots together
plot_a <- plot(pred_ageclass) + ggtitle("a") +
  theme(legend.position = c(0.25, 0.8), legend.title = element_blank()) +
  xlab("age-sex class") + ylab("proportion of playmates") +
  theme(text = element_text(size=13)) +
  theme(panel.background = element_blank()) +
  theme(panel.border = element_rect(fill = NA, colour = "black", size = 1)) +
  theme(strip.background = element_rect(fill = "white", color = "white", size = 1))

plot_b <- plot(pred_eigen) + ggtitle("b") +
  theme(legend.position = c(0.25, 0.8), legend.title = element_blank()) +
  xlab("eigenvector centrality") + ylab("proportion of playmates") +
  theme(text = element_text(size=13)) +
  theme(panel.background = element_blank()) +
  theme(panel.border = element_rect(fill = NA, colour = "black", size = 1)) +
  theme(strip.background = element_rect(fill = "white", color = "white", size = 1))

plot_c <- plot(pred_ir) + ggtitle("c") +
  theme(legend.position = c(0.25, 0.8), legend.title = element_blank()) +
  xlab("immature to adult ratio") + ylab("proportion of playmates") +
  theme(text = element_text(size=13)) +
  theme(panel.background = element_blank()) +
  theme(panel.border = element_rect(fill = NA, colour = "black", size = 1)) +
  theme(strip.background = element_rect(fill = "white", color = "white", size = 1))

plot_d <- plot(pred_group) + ggtitle("d") +
  theme(legend.position = c(0.25, 0.8), legend.title = element_blank()) +
  xlab("group size") + ylab("proportion of playmates") +
  theme(text = element_text(size=13)) +
  theme(panel.background = element_blank()) +
  theme(panel.border = element_rect(fill = NA, colour = "black", size = 1)) +
  theme(strip.background = element_rect(fill = "white", color = "white", size = 1))

plot_e <- plot(pred_play.rate) + ggtitle("e") +
  theme(legend.position = c(0.25, 0.8), legend.title = element_blank()) +
  xlab("play rate (sec/hr)") + ylab("proportion of playmates") +
  theme(text = element_text(size=13)) +
  theme(panel.background = element_blank()) +
  theme(panel.border = element_rect(fill = NA, colour = "black", size = 1)) +
  theme(strip.background = element_rect(fill = "white", color = "white", size = 1))

figure <- ggarrange(plot_a,ncol = 1, nrow = 1)
edge.g <- ggarrange(figure, ggarrange(plot_b,plot_c,plot_d,plot_e,
                                          ncol = 2, nrow = 2,legend="top"))
edge.g 


tab_model(glm.eigen,show.se = TRUE)

# Fit the null model (only intercept) again
null_model <- glm(cb.play ~ 1, family = binomial, data = eigen.df)

# Perform Likelihood Ratio Test (LRT)
anova(null_model, glm.eigen, test = "Chisq")

# Extract residual deviance
dev_full <- deviance(glm.eigen)  # Residual deviance of the full model
dev_null <- deviance(null_model)
dev_glm.eigen <- 1-(dev_full/dev_null)
dev_glm.eigen

#do the same for glm.cls and glm.bty
# Compute Pseudo R²
dev_glm.eigen <- 1 - (deviance(glm.eigen) / deviance(null_model))
dev_glm.cls <- 1 - (deviance(glm.cls) / deviance(null_model))
dev_glm.btw <- 1 - (deviance(glm.btw) / deviance(null_model))

# Print results
cat("Pseudo R² for glm.eigen:", dev_glm.eigen, "\n")
cat("Pseudo R² for glm.cls:", dev_glm.cls, "\n")
cat("Pseudo R² for glm.btw:", dev_glm.btw, "\n")

#END of coding
