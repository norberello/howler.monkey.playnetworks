######################################################
library(lme4)
library(lmerTest)
library(emmeans)
library(ggplot2)
library(ggpubr)
library(sjPlot)

#centrality across age-sex classes
#load data
eigen.df <- read.csv2("play metrics per id.csv")

#LMM eigenvector
# Fit a mixed-effects model with groupd id as the fixed effect and monkey ID as the random effect
model.eigen <- lmer(eigen ~ age.sex.class2 + (1|group.name),
              data = eigen.df)
summary(model.eigen)

#since variance near 0: LM
model.eigen <- lm(eigen ~ age.sex.class2,
                    data = eigen.df)
summary(model.eigen)

#LMM closeness
model.close <- lmer(close ~ age.sex.class2 + (1|group.name),
                    data = eigen.df)
summary(model.close)#keep LMM

anova(model.close)
marg_means <- emmeans(model.close, ~ age.sex.class2)
pairs(marg_means)

#LMM betweenness
model.btw <- lmer(btw ~ age.sex.class2 + (1|group.name),
                    data = eigen.df)
summary(model.btw)#near 0 use LM
model.btw <- lm(btw ~ age.sex.class2,
                  data = eigen.df)

anova(model.btw)
marg_means <- emmeans(model.btw, ~ age.sex.class2)
pairs(marg_means)

#LMM play rate
model.play <- lmer(play.rate ~ age.sex.class2 + (1|group.name),
                   data = eigen.df)#KEEP LMM
summary(model.play)

# Compute the estimated marginal means
emmeans.eigen <- emmeans(model.eigen, "age.sex.class2")
head(emmeans.eigen)

# Create the plot
g.eigen <- ggplot(data = as.data.frame(emmeans.eigen),
       aes(x = age.sex.class2, y = emmean,
           ymin = lower.CL, ymax = upper.CL)) +
  geom_point(size = 3, color = "blue") +
  geom_errorbar(width = 0, color = "blue") +
  labs(x = "age sex class",
       y = "Eigenvector centrality",
       title="c")+
  theme_bw()+
  theme(text = element_text(size=13))+
  theme(panel.background = element_blank())+
  theme(panel.border = element_rect(fill = NA, colour = "black", size = 1))+
  theme(strip.background = element_rect(fill = "white", color = "white", size = 1))

# Compute the estimated marginal means
emmeans.close <- emmeans(model.close, "age.sex.class2")

# Create the plot
g.cls <- ggplot(data = as.data.frame(emmeans.close),
       aes(x = age.sex.class2, y = emmean,
           ymin = lower.CL, ymax = upper.CL)) +
  geom_point(size = 3, color = "red") +
  geom_errorbar(width = 0, color = "red") +
  labs(x = "age sex class",
       y = "Closeness centrality",
       title="b")+
  theme_bw()+
  theme(text = element_text(size=13))+
  theme(panel.background = element_blank())+
  theme(panel.border = element_rect(fill = NA, colour = "black", size = 1))+
  theme(strip.background = element_rect(fill = "white", color = "white", size = 1))

# Compute the estimated marginal means
emmeans.btw <- emmeans(model.btw, "age.sex.class2")

# Create the plot
g.btw <- ggplot(data = as.data.frame(emmeans.btw),
       aes(x = age.sex.class2, y = emmean,
           ymin = lower.CL, ymax = upper.CL)) +
  geom_point(size = 3, color = "darkgreen") +
  geom_errorbar(width = 0, color = "darkgreen") +
  labs(x = "age sex class",
       y = "Betweenness centrality",
       title="d")+
  theme_bw()+
  theme(text = element_text(size=13))+
  theme(panel.background = element_blank())+
  theme(panel.border = element_rect(fill = NA, colour = "black", size = 1))+
  theme(strip.background = element_rect(fill = "white", color = "white", size = 1))

#########

# Compute the estimated marginal means
emmeans.play <- emmeans(model.play, "age.sex.class2")
head(emmeans.play)
head(emmeans.eigen)

plot(eigen.df$eigen,eigen.df$play.rate)
anova(model.play,model.eigen)

# Create the plot
g.play <- ggplot(data = as.data.frame(emmeans.play),
                 aes(x = age.sex.class2, y = emmean,
                     ymin = lower.CL, ymax = upper.CL)) +
  geom_point(size = 3, color = "black") +
  geom_errorbar(width = 0, color = "black") +
  labs(x = "age sex class",
       y = "play rate",
       title="a")+
  theme_bw()+
  theme(text = element_text(size=13))+
  theme(panel.background = element_blank())+
  theme(panel.border = element_rect(fill = NA, colour = "black", size = 1))+
  theme(strip.background = element_rect(fill = "white", color = "white", size = 1))

g.play



figure <- ggarrange(g.play,g.cls, g.eigen, g.btw,
                    ncol = 2, nrow = 2)
figure
#this is it!!








################OTHERS
#####more figure options

figure <- ggarrange(g.cls, g.eigen, g.btw,
                    ncol = 3, nrow = 1)
figure


summary(model.close)



# Create the table of fixed effects
tab_model(model.close, show.ci = FALSE, show.std = TRUE, 
          p.style = "stars", title = "Fixed Effects")




#you can use a likelihood ratio test to compare the full mixed-effects model 
#with a reduced model that only includes the fixed effect. The difference in the log-likelihoods between the two models can be used to calculate a chi-squared statistic, which can then be used to test whether the random effect significantly improves the fit of the model.

reduced_model <- lm(close ~ age.sex.class2, data = eigen.df)

# Calculate log-likelihoods for each model
LL_full <- logLik(model.close)
LL_reduced <- logLik(reduced_model)

# Calculate difference in log-likelihoods
D <- -2 * (LL_reduced - LL_full)

# Calculate degrees of freedom for the chi-squared test
df <- attr(LL_full, "df") - attr(LL_reduced, "df")

# Calculate p-value for the likelihood ratio test
p_value <- pchisq(D, df, lower.tail = FALSE)
p_value


# Print results
cat("Likelihood ratio test: chi-squared =", round(D, 2),
    "df =", df, "p =", p_value, "\n")

####bu that is not what I want

library(glmmTMB)

# Fit the model using glmmTMB
model.close <- glmmTMB(close ~ age.sex.class2, data = eigen.df)

# Print the summary, including p-values
summary(model, test = list("t.values", "p.values"))


##########let´s start over


# Fit a mixed-effects model with race as the fixed effect and dog ID as the random effect
model.eigen <- lmer(eigen ~ age.sex.class2 + (1|group.name),
                    data = eigen.df)
summary(model.eigen)
anova(model.eigen)
anova(model.close)
anova(model.btw)


marg_means <- emmeans(model.eigen, ~ age.sex.class2)
pairs(marg_means)
anova(model.play)

anova(model.close)
marg_means <- emmeans(model.close, ~ age.sex.class2)
pairs(marg_means)

anova(model.eigen)
marg_means <- emmeans(model.eigen, ~ age.sex.class2)
pairs(marg_means)





#############play effort
# Fit a mixed-effects model with race as the fixed effect and dog ID as the random effect
model.play.rate <- lmer(play.rate ~ age.sex.class2 + (1|group.name),
                    data = eigen.df)
summary(model.play.rate)

# Load the required packages
library(lme4)
library(emmeans)
library(ggplot2)

# Compute the estimated marginal means
emmeans.play <- emmeans(model.play.rate, "age.sex.class2")
head(emmeans.play)

# Create the plot
g.play <- ggplot(data = as.data.frame(emmeans.play),
                  aes(x = age.sex.class2, y = emmean,
                      ymin = lower.CL, ymax = upper.CL)) +
  geom_point(size = 3, color = "black") +
  geom_errorbar(width = 0.2, color = "black") +
  labs(x = "age sex class",
       y = "play rate",
       title="b")+
  theme_bw()+
  theme(text = element_text(size=13))+
  theme(panel.background = element_blank())+
  theme(panel.border = element_rect(fill = NA, colour = "black", size = 1))+
  theme(strip.background = element_rect(fill = "white", color = "white", size = 1))

g.play

#you can use a likelihood ratio test to compare the full mixed-effects model 
#with a reduced model that only includes the fixed effect. The difference in the log-likelihoods between the two models can be used to calculate a chi-squared statistic, which can then be used to test whether the random effect significantly improves the fit of the model.

# Fit reduced model without random effect for dog ID
reduced_model <- lm(play.rate ~ age.sex.class2, data = eigen.df)

library(glmmTMB)

# Fit the model using glmmTMB
model.play <- glmmTMB(play.rate ~ age.sex.class2, data = eigen.df)

# Print the summary, including p-values
summary(model.play, test = list("t.values", "p.values"))


##########let´s start over


# Fit a mixed-effects model with race as the fixed effect and dog ID as the random effect
model.play.rate <- lmer(play.rate ~ age.sex.class2 + (1|group.name),
                    data = eigen.df)
summary(model.play.rate)
anova(model.play.rate)

marg_means <- emmeans(model.play.rate, ~ age.sex.class2)
pairs(marg_means)

citation("emmeans")




