#title - London Clay Fossil Fish 
#created - 22/07/2024 
################################################################################
################################# FOSSIL OTOLITHS ##############################
################################################################################
#install/load libraries
library(dplyr)
library(tidyverse)
library(patchwork)
library(ggsci)
library(ggrepel)
library(lme4)
library(ggthemes)
library(merTools)
library(ggplot2)

#load dataset
#Fossil Fish Only
Fossil_Fish <- read.csv("Fossil_Fish.csv")


##########################CALCULATIONS##############################
#Calculate Temperature in degrees C -Morissette et al 2023
Fossil_Fish<-
  Fossil_Fish %>%
  mutate(Temp=((d18Ooto-d18Ow)-3.465)/-0.209)
write.table(Fossil_Fish, file="FF_T.csv", sep=",") 


#calculate means and sd
Fossil_Fish %>%
  group_by(Species) %>%
  summarise(meanC=mean(d13Coto),
            sdC=sd(d13Coto),
            meanO=mean(d18Ooto),
            sdO=sd(d18Ooto),
            meanT=mean(Temp))
#extract table
mean_table<- Fossil_Fish %>%
  group_by(Species) %>%
  summarise(meanC=mean(d13Coto),
            sdC=sd(d13Coto),
            meanO=mean(d18Ooto),
            sdO=sd(d18Ooto),
            meanT=mean(Temp))
write.table(mean_table, file="means_table.csv", sep=",") 

levels(Fossil_Fish$Species)
# specify the factor levels in the order you want
Fossil_Fish$Species <- factor(Fossil_Fish$Species, levels = c("Spicara ministerensis","Scorpaenichthys subtilis", "Ampheristus obtusus",
                                                              "Zonobythites splendens","Grammonus argutus","Centroberyx eocenicus","Protocolliolus eocenicus",
                                                              "Rhinocephalus planiceps","Paraulopus davisi","Pterothrissus angulatus"))

#define custom color scale
myColors <- brewer.pal(10, "Paired")
names(myColors) <- levels(Fossil_Fish$Species)
custom_colors <- scale_colour_manual(name = "Species", values = myColors)

######### Mean calculations for orders ############ 

#mean d13C for each new name
mean_C<-aggregate(Fossil_Fish$d13Coto, list(Fossil_Fish$Species), FUN=mean)
print(mean_C)

#mean temp for each new name
mean_T<-aggregate(Fossil_Fish$Temp, list(Fossil_Fish$Species), FUN=mean)
print(mean_T)
#need to rename x to something else so I can combine
rename(mean_T, y = x)
#create dataframe
df3<-
  right_join(mean_C,mean_T, by='Group.1')
print(df3)
#rename column headings
renames<- c("Species","d13Coto", "Temp")
names(df3) <- renames
print(df3)


#mean d18O for each new name
mean_O<-aggregate(Fossil_Fish$d18Ooto, list(Fossil_Fish$Species), FUN=mean)
print(mean_O)

#need to rename x to something else so I can combine
rename(mean_O, y = x)
#create dataframe
df6<-
  right_join(mean_C,mean_O, by='Group.1')
print(df6)
#rename column headings
renames<- c("Species","d13Coto", "d18Ooto")
names(df6) <- renames
print(df6)

#############################FOSSIL FISH ONLY############################

# Get convex hull indices
hull_data <- Fossil_Fish %>%
  group_by(Species) %>%
  slice(chull(d13Coto, d18Ooto))

# Plot convex hulls ############## FIGURE 6 #################################
Fossil_Fish$Species <- factor(Fossil_Fish$Species, levels = c(
  "Zonobythites splendens",
  "Ampheristus obtusus",
  "Rhinocephalus planiceps",
  "Centroberyx eocenicus",
  "Grammonus argutus",
  "Pterothrissus angulatus",
  "Protocolliolus eocenicus",
  "Spicara ministerensis",
  "Paraulopus davisi",
  "Scorpaenichthys subtilis"
))

cb10 <- c(
  "Protocolliolus eocenicus" = "#000000",  # black
  "Ampheristus obtusus" = "#E69F00",             # orange
  "Paraulopus davisi" = "#56B4E9",             # sky blue
  "Zonobythites splendens" = "#009E73",             # bluish green
  "Spicara ministerensis" = "#F0E442",             # yellow
  "Centroberyx eocenicus" = "#0072B2",             # blue
  "Pterothrissus angulatus" = "#D55E00",             # vermillion
  "Scorpaenichthys subtilis" = "#CC79A7",             # reddish purple
  "Grammonus argutus" = "#999999",             # grey
  "Rhinocephalus planiceps" = "#7F3C8D"              # purple
)

ggplot(Fossil_Fish, aes(x = d18Ooto, y = d13Coto, color = Species)) +
  geom_point(size=2) +
  geom_polygon(data = hull_data, aes(fill = Species), alpha = 0.3, color = NA) +
  scale_colour_manual(values = cb10)+
  scale_fill_manual (values = cb10, guide="none")+
  theme_minimal() +
  labs(x=expression("δ"^18*"O"[oto]*"(‰)"),
       y=expression("δ"^13*"C"[oto]*"(‰)"))+
  guides(color = guide_legend(override.aes = list(fill = NA)))+
  theme(
    legend.text = element_text(face = "italic"))

#plot just fossil fish, Ooto against Coto
Fossil_O_C<-
  ggplot(Fossil_Fish)+
  aes(x=d18Ooto, y=d13Coto, color=Species)+
  geom_point(size=2, alpha=0.5)+
  geom_point(data=df6, size=4)+
  custom_colors+
  scale_shape_manual(values = c(17, 16))+
  stat_ellipse(aes(color=Species))+
  custom_colors+
  scale_shape_manual(values = c(17, 16))+
  theme(axis.title=element_text(size=20, colour="black"),
        axis.text=element_text(size=18, colour="black"),
        axis.line = element_line(colour='black'))+
  theme(panel.background = element_blank(),
        plot.background = element_rect(fill="white"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='white'),
        legend.box.background = element_rect(fill='white'),
        legend.text=element_text(size=15, colour="black"))+
  scale_fill_discrete(name = "New Legend Title")+
  labs(x=expression("δ"^18*"O"[oto]*"(‰)"),
       y=expression("δ"^13*"C"[oto]*"(‰)"))
Fossil_O_C

#plot just fossil fish, Temp against Coto
Fossil_Temp_C_Sp<-
  ggplot(Fossil_Fish)+
  aes(x=Temp, y=d13Coto, color=Species)+
  geom_point(size=2, alpha=0.5)+
  geom_point(data=df3, size=4)+
  custom_colors+
  scale_shape_manual(values = c(17, 16))+
  stat_ellipse(aes(color=Species))+
  theme(axis.title=element_text(size=20, colour="black"),
        axis.text=element_text(size=18, colour="black"),
        axis.line = element_line(colour='black'))+
  theme(panel.background = element_blank(),
        plot.background = element_rect(fill="white"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='white'),
        legend.box.background = element_rect(fill='white'),
        legend.text=element_text(size=15, colour="black"))+
  scale_fill_discrete(name = "New Legend Title")+
  labs(x="Temperature (°C)",
       y=expression("δ"^13*"C"[oto]*"(‰)"))
Fossil_Temp_C_Sp

#plot temperature of fossil fish as box and wiskar plot
box_temp<-
  ggplot(Fossil_Fish)+
  aes(x=Age,y=Temp, fill='red')+
  geom_boxplot()+
  geom_point()+
  theme_minimal()+
  labs(y="Temperature (°C)",
       x="Fossil Otoliths")
box_temp

#plot just fossil fish, C against temp, coloured by taxanomic Order
Fossil_Order<-
  ggplot(Fossil_Fish)+
  aes(x=Temp, y=d13Coto, color=Order)+
  geom_point(size=4, alpha=0.7)+
  scale_shape_manual(values = c(17, 16))+
  theme(axis.title=element_text(size=20, colour="white"),
        axis.text=element_text(size=18, colour="white"),
        axis.line = element_line(colour='white'))+
  theme(panel.background = element_blank(),
        plot.background = element_rect(fill="black"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='black'),
        legend.box.background = element_rect(fill='black'),
        legend.text=element_text(size=15, colour="white"))+
  scale_fill_discrete(name = "New Legend Title")+
  labs(x="Temperature (°C)",
       y="δ13Coto (‰)")
Fossil_Order


##########FOSSIL FISH STATS / EXPERIMENTAL MODELLING#############

FF_GLM1<-glm(d13Coto ~ Temp + Species, data=Fossil_Fish, family=gaussian(link="identity"))
FF_GLM1

FF_GLM2<-glm(d13Coto ~ Temp + (Species*Location), data=Fossil_Fish, family=gaussian(link="identity"))
FF_GLM2

FF_GLM3<-glm(d13Coto ~ Temp + Species + Location, data=Fossil_Fish, family=gaussian(link="identity"))
FF_GLM3

FF_GLM4<-glm(d13Coto ~ Temp + Order, data=Fossil_Fish, family=gaussian(link="identity"))
FF_GLM4

FF_GLM5<-glm(d13Coto ~ Temp + Family, data=Fossil_Fish, family=gaussian(link="identity"))
FF_GLM5

FF_GLM6<-glm(d13Coto ~ Temp + Genus, data=Fossil_Fish, family=gaussian(link="identity"))
FF_GLM6

#GLM calculations
Sum_GLM1<- summary(FF_GLM1)$coefficients
conf_GLM1<- confint(FF_GLM1)

#calculate AIC
AIC(FF_GLM1,FF_GLM2,FF_GLM3,FF_GLM4,FF_GLM5,FF_GLM6)

#calculate R-squared using McFaddens pseudo-R squared
#R-squared represents the proportion of the deviance explained by the model.
#1-(devience/null.devience)

#GLM1&2&3&6- 0.91
1-(29.35/335.2)
#GLM4- 0.83
1-(57.91/335.2)
#GLM5- 0.90
1-(34.14/335.2)

plot(FF_GLM1)


#Kruskal-Wallis test

df7<-Fossil_Fish %>% group_by(Species)
df7 %>%
  summarise(
    count = n(),
    mean = mean(d13Coto, na.rm = TRUE),
    sd = sd(d13Coto, na.rm = TRUE),
    median = median(d13Coto, na.rm = TRUE),
    IQR = IQR(d13Coto, na.rm = TRUE)
  )
#visualise
boxplot(d13Coto~Species, data=df7)

kruskal.test(d13Coto ~ Species, data = df7) #yes at least one group is significantly different

#pairwise comparisons to find which groups differ:
pairwise.wilcox.test(df7$d13Coto, df7$Species,
                     p.adjust.method = "BH")


######################## MIXED EFFECTS MODELS ###########################
library(lmerTest)
#models to explain d13C variance in FOSSIL fish only
#difference in d13C between species for the same d18O value (temp)
LME_1<-lmer(formula = d13Coto ~ d18Ooto + (1|Species),  data   = Fossil_Fish)
summary(LME_1)


# Extract variance components
variance_components_fos <- as.data.frame(VarCorr(LME_1))
random_effect_variance_fos <- variance_components_fos$vcov[variance_components_fos$grp == "Species"]
residual_variance_fos <- attr(VarCorr(LME_1), "sc")^2
total_variance_fos <- random_effect_variance_fos + residual_variance_fos
proportion_random_variance_fos <- random_effect_variance_fos / total_variance_fos
3.4391/(3.4391 + 0.3668)
# 90% variance by d13C explained by species, 10% by d18O



fixed_eff_fos<-data.frame(fixef(LME_1))
fixed_eff_fos

rand_eff_fos<-data.frame(ranef(LME_1))
rand_eff_fos

#plot ran_eff fossil
ggplot(data=rand_eff_fos,
       aes(x=grp, y=condval))+
  geom_errorbar(aes(x=grp, ymin=condval-condsd, ymax=condval+condsd), colour='deeppink', data=rand_eff_fos)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(x="Fossil Species",
       y=expression("δ"^13*"C"[oto]*"(‰)"))


#########################################################################
#########################################################################

#dataframe for species level 
df_groups<- Fossil_Fish %>%
  group_by(Species) %>%
  summarise(
    d18O = mean(d18Ooto))

# Create 3 groups (terciles)
group <- cut(df_groups$d18O, breaks = quantile(df_groups$d18O, probs = c(0, 1/3, 2/3, 1)), 
             include.lowest = TRUE, labels = c("Shelf", "Slope", "Deep"))

# View result
Depth_df2<- data.frame(df_groups$Species, df_groups$d18O, group)

#depth plot
Depth_plot<-
  ggplot(Depth_df2, aes(x = group, y = df_groups.d18O)) +
  geom_boxplot(width = 0.6, fill = "blue") +
  labs(y = "d18Ooto", x = "Depth Estimate") +
  theme_minimal()
Depth_plot


#dataframe for species level 
df_groups2<- Fossil_Fish %>%
  group_by(Species) %>%
  summarise(
    d13C = mean(d13Coto))

# Create 3 groups (terciles)
group2 <- cut(df_groups2$d13C, breaks = quantile(df_groups2$d13C, probs = c(0, 1/3, 2/3, 1)), 
             include.lowest = TRUE, labels = c("Pelagic", "Demersal", "Benthic"))

# View result
Depth_df3<- data.frame(df_groups2$Species, df_groups2$d13C, group2)

#MOL plot
MOL_plot<-
  ggplot(Depth_df3, aes(x = group2, y = df_groups2.d13C)) +
  geom_boxplot(width = 0.6, fill = "orange") +
  labs(y = "d13Coto", x = "Mode of Life Estimate") +
  theme_minimal()
MOL_plot


######################################################################
#Test species effect on δ¹³C and δ¹⁸O separately (univariate ANOVAs)
# Make sure Species is a factor
Fossil_Fish$Species <- as.factor(Fossil_Fish$Species)

# δ13C
aov_d13C <- aov(d13Coto ~ Species, data = Fossil_Fish)
summary(aov_d13C)

# δ18O
aov_d18O <- aov(d18Ooto ~ Species, data = Fossil_Fish)
summary(aov_d18O)

#Post-hoc test (which species differ?)
TukeyHSD(aov_d13C)
TukeyHSD(aov_d18O)

#testing whether Species has a significant multivariate effect on both δ¹³C and δ¹⁸O.
# Fit MANOVA model
manova_model <- manova(cbind(d13Coto, d18Ooto) ~ Species, data = Fossil_Fish)

# Summary of MANOVA (default = Pillai's trace)
summary(manova_model)
summary.aov(manova_model)

#quantify how much of the variance is explained by species using eta squared (η²):
library(effectsize)

eta_squared(aov_d13C)
eta_squared(aov_d18O)

#LDA
library(MASS)
lda_res <- lda(Species ~ d13Coto + d18Ooto, data=Fossil_Fish)
lda_res$scaling

lda_res


#pairwise comparisons on d18O
# Pairwise t-tests with Holm correction
pairwise_result <- pairwise.wilcox.test(
  x = Fossil_Fish$d18Ooto,
  g = Fossil_Fish$Species,
  p.adjust.method = "holm"   # correction for multiple comparisons
)


# View results
pairwise_result$p.value

# Extract all p-values
p_values <- as.vector(pairwise_result$p.value)
p_values <- p_values[!is.na(p_values)]  # remove NAs

# Count insignificant (p >= 0.05)
significant_count <- sum(p_values <= 0.05)
total_comparisons <- length(p_values)

# Percentage
percentage_significant <- (significant_count / total_comparisons) * 100

cat("Percentage of species pairs with significant δ18O differences:",
    round(percentage_significant, 1), "%\n")


# Covariance
cov(Fossil_Fish$d13Coto, Fossil_Fish$d18Ooto)
#pearsons coefficient 
cov(Fossil_Fish$d13Coto, Fossil_Fish$d18Ooto) / (sd(Fossil_Fish$d13Coto) * sd(Fossil_Fish$d18Ooto))
#stat sig
cor.test(Fossil_Fish$d13Coto, Fossil_Fish$d18Ooto)


################################################################################
############################## MODERN UK OTOLITHS ##############################
################################################################################

#load dataset
UK_Otoliths <- read.csv("UK_Otoliths.csv")

#arrange UK data
UK_Otoliths<- arrange(UK_Otoliths, Species)

################## 1) plot Carbon against Oxygen - Species #################
O_C_Sp<-
  ggplot(UK_Otoliths)+
  aes(x=d18Ooto, y=d13Coto, color=Species)+
  geom_point(size=2, alpha=0.5)+
  stat_ellipse(aes(color=Species))+
  geom_smooth(method = "lm", se = TRUE, color="black", fill="lightgrey")+
  theme(axis.title=element_text(size=20, colour="black"),
        axis.text=element_text(size=18, colour="black"),
        axis.line = element_line(colour='black'))+
  theme(panel.background = element_blank(),
        plot.background = element_rect(fill="white"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='white'),
        legend.box.background = element_rect(fill='white'),
        legend.text=element_text(size=15, colour="black"))+
  scale_fill_discrete(name = "New Legend Title")+
  labs(x=expression("δ"^18*"O"[oto]*"(‰)"),
       y=expression("δ"^13*"C"[oto]*"(‰)"))
O_C_Sp

Sp_means <- UK_Otoliths %>%
  group_by(Species) %>%
  summarise(
    meanO = mean(d18Ooto, na.rm = TRUE),
    meanC = mean(d13Coto, na.rm = TRUE),
  )


#glm
FF_GLM1<-glm(d13Coto ~ d18Ooto + Species, data=UK_Otoliths, family=gaussian(link="identity"))
FF_GLM1

#GLM calculations
Sum_GLM1<- summary(FF_GLM1)$coefficients
conf_GLM1<- confint(FF_GLM1)

#calculate R-squared using McFaddens pseudo-R squared
#R-squared represents the proportion of the deviance explained by the model.
#1-(devience/null.devience)

#GLM1- 0.91
1-(576.2/6939)

plot(FF_GLM1)




#linear regression to find r2
model_lm <- lm(d18Ooto ~ d13Coto, data = UK_Otoliths)

# Summary of the model
summary(model_lm)
# Get R²
r2 <- summary(model_lm)$r.squared

################## 2) Linear Mixed Effects Models ##################

model <- lmer(d13Coto ~ d18Ooto + (1 | Species), data = UK_Otoliths)

summary(model)      # Model coefficients and statistics
ranef(model)        # Random effects
fixef(model)        # Fixed effects
confint(model)      # Confidence intervals
plot(model)         # Residual diagnostics
summary(model)$coefficients     #SE of the fixed intercept

#Get Random Intercepts + SEs Together
# Get random effects with conditional variances
re <- ranef(model, condVar = TRUE)

# Extract random intercepts for "group"
intercepts <- re$Species  # assuming grouping variable is called "group"

# Extract conditional variances (a 3D array)
post_var <- attr(re$Species, "postVar")

# Convert variances to standard errors (square root of diagonal)
# Each group gets 1 SE, since it's a random intercept model
se <- sapply(1:dim(post_var)[3], function(i) sqrt(post_var[,,i]))

# Build a data frame
random_effects_df <- data.frame(
  Species = rownames(intercepts),
  intercept = intercepts[, 1],
  se = se
)


############### 4) compare d13C with oxygen consumption ###############

#plot C and O bar plots for each species (with o2 consumption data)
O2_con <- read.csv("UK_Otoliths_Ocon.csv")

#arrange UK data
O2_con<- arrange(O2_con, Species)

#remove species with no o2 estimates
O2_df <- O2_con[!is.na(O2_con$O.con), ]

species_order <- O2_df %>%
  group_by(Species) %>%
  summarise(mean_d13C = mean(d13Coto, na.rm = TRUE)) %>%
  arrange(desc(mean_d13C)) %>%
  pull(Species)

O2_df$Species <- factor(O2_df$Species, levels = species_order)


my_colors <- c(
  "Clupea harengus" = "#88CCEE",
  "Ctenolabrus rupestris" = "#CC6677",
  "Gadus morhua" = "#DDCC77",  
  "Labrus bergylta" = "#117733",
  "Pleuronectes platessa" = "#332288",
  "Pollachius virens" = "#AA4499",
  "Scomber scombrus" = "#999933",
  "Sprattus sprattus" = "#882255",
  "Trachurus trachurus" = "#661100"
)


#####linear regression to find r2 #############
model_LR <- lm(d13Coto ~ O_con, data = O2_df)

# Summary of the model
summary(model_LR)
# Get R²
r2 <- summary(model_LR)$r.squared
# p-value for the regression
summary(model_LR)$coefficients[2, 4]

#model slope and intercept
m <- coef(model_LR)[2]
b <- coef(model_LR)[1]
eq_label <- paste0("y = ", round(m,2), "x + ", round(b,2))

#want the plot order to be C, A, B
O2_df$Species <- factor(O2_df$Species, levels = c("Pollachius virens",
                                                  "Pleuronectes platessa",
                                                  "Trachurus trachurus",
                                                  "Gadus morhua",
                                                  "Ctenolabrus rupestris",
                                                  "Clupea harengus",
                                                  "Scomber scombrus",
                                                  "Labrus bergylta",
                                                  "Sprattus sprattus"))
#plot d13C against 02 consumption ######## FIGURE 4 #############################
O2_con_plot<-
  ggplot(O2_df)+
  aes(x=O.con, y=d13Coto, colour=Species)+
  geom_point(size=2, alpha=0.8)+
  geom_smooth(method=lm,se = TRUE, color = "black")+
  scale_colour_manual(
    values = my_colors,
    labels = c(
      expression ("Pollachius virens"= "Pollock ("*italic ("Pollachius virens")*")"),
      expression ("Pleuronectes platessa"= "Plaice ("*italic ("Pleuronectes platessa")*")"),
      expression ("Trachurus trachurus"= "Horse mackerel ("*italic ("Trachurus trachurus")*")"),
      expression ("Gadus morhua"= "Atlantic cod ("*italic ("Gadus morhua")*")"),
      expression ("Ctenolabrus rupestris"= "Goldsinny wrasse ("*italic ("Ctenolabrus rupestris")*")"),
      expression ("Clupea harengus" = "Herring ("*italic ("Clupea harengu")*")"),
      expression ("Scomber scombrus" = "Mackerel ("*italic ("Scomber scombrus")*")"),
      expression ("Labrus bergylta" = "Ballan wrasse ("*italic ("Labrus bergylta")*")"),
      expression ("Sprattus sprattus" = "Spratt ("*italic ("Sprattus sprattus")*")")
    )
  ) +
  theme(axis.title=element_text(size=20, colour="black"),
        axis.text=element_text(size=18, colour="black"),
        axis.line = element_line(colour='black'))+
  theme(panel.background = element_blank(),
        plot.background = element_rect(fill="white"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='white'),
        legend.box.background = element_rect(fill='white'),
        legend.text=element_text(size=10, colour="black"))+
  annotate("text",
           x = -Inf,
           y = Inf,
           label = eq_label,
           hjust = -0.1,
           vjust = 1.1,
           size = 6)+
  scale_fill_discrete(name = "New Legend Title")+
  labs(x = expression("O"[2]*" Consumption (mg O"[2]*" kg"^{-1}*" h"^{-1}*")"),
       y= expression("δ"^13*"C"[oto]*"(‰)"))
O2_con_plot


########################## STATS to compare C and O ################################

#load data
Combined_df <- read.csv("combined.csv")

#test normally distributed
shapiro.test(Combined_df$d13Coto[Combined_df$Age == "Fossil"])
shapiro.test(Combined_df$d13Coto[Combined_df$Age == "Modern"])

#calculate the range
Combined_df %>%
  group_by(Age) %>%
  summarise(
    min_d13C = min(d13Coto),
    max_d13C = max(d13Coto),
    range_d13C = max(d13Coto) - min(d13Coto)
  )

Combined_df %>%
  group_by(Age) %>%
  summarise(
    min_d18O = min(d18Ooto),
    max_d18O = max(d18Ooto),
    range_d18O = max(d18Ooto) - min(d18Ooto)
  )

#Statistical Comparison of Spread
library(car)
leveneTest(d13Coto ~ Age, data = Combined_df) #statistically significant difference
leveneTest(d18Ooto ~ Age, data = Combined_df) #statistically similar


#Test species effect on δ¹³C and δ¹⁸O separately (univariate ANOVAs)
# Make sure Species is a factor
UK_Otoliths$Species <- as.factor(UK_Otoliths$Species)

# δ13C
aov_d13C <- aov(d13Coto ~ Species, data = UK_Otoliths)
summary(aov_d13C)

# δ18O
aov_d18O <- aov(d18Ooto ~ Species, data = UK_Otoliths)
summary(aov_d18O)

#Post-hoc test (which species differ?)
TukeyHSD(aov_d13C)
TukeyHSD(aov_d18O)

# Covariance
cov(UK_Otoliths$d13Coto, UK_Otoliths$d18Ooto)
#pearsons coefficient 
cov(UK_Otoliths$d13Coto, UK_Otoliths$d18Ooto) / (sd(UK_Otoliths$d13Coto) * sd(UK_Otoliths$d18Ooto))
#stat sig
cor.test(UK_Otoliths$d13Coto, UK_Otoliths$d18Ooto)

#testing whether Species has a significant multivariate effect on both δ¹³C and δ¹⁸O.
# Fit MANOVA model
manova_model <- manova(cbind(d13Coto, d18Ooto) ~ Species, data = UK_Otoliths)

# Summary of MANOVA (default = Pillai's trace)
summary(manova_model)
summary.aov(manova_model)

#quantify how much of the variance is explained by species using eta squared (η²):
library(effectsize)

eta_squared(aov_d13C)
eta_squared(aov_d18O)


# see how well species covary with temp(depth) and activity niches
aov_MOL <- aov(d13Coto ~ MOL, data = UK_Otoliths)
summary(aov_MOL)

aov_Depth <- aov(d18Ooto ~ Depth, data = UK_Otoliths)
summary(aov_Depth)


#LDA
library(MASS)
lda_res <- lda(Species ~ d13Coto + d18Ooto, data=UK_Otoliths)
lda_res$scaling

lda_res

#pairwise comparisons on d18O
# Pairwise t-tests with Holm correction

pairwise_result <- pairwise.wilcox.test(
  x = UK_Otoliths$d18Ooto,
  g = UK_Otoliths$Species,
  p.adjust.method = "holm"   # correction for multiple comparisons
)


# View results
pairwise_result$p.value

# Extract all p-values
p_values <- as.vector(pairwise_result$p.value)
p_values <- p_values[!is.na(p_values)]  # remove NAs

# Count insignificant (p >= 0.05)
significant_count <- sum(p_values <= 0.05)
total_comparisons <- length(p_values)

# Percentage
percentage_significant <- (significant_count / total_comparisons) * 100

cat("Percentage of species pairs with significant δ18O differences:",
    round(percentage_significant, 1), "%\n")


######z scored manova
data_z <- UK_Otoliths
data_z[, c("d18Ooto", "d13Coto")] <- scale(UK_Otoliths[, c("d18Ooto", "d13Coto")])

# Fit MANOVA model
manova_model <- manova(cbind(d18Ooto, d13Coto) ~ Species, data = data_z)

# Summary of results
summary(manova_model, test = "Wilks")
summary.aov(manova_model)


################################################################################
############################## THERMAL NICHE FIGURE 3 ##########################
################################################################################

library(tidyverse) #data namipulation and plotting
data<-read.csv("UK_otoliths_CT.csv", header = TRUE, fill=TRUE)
library(patchwork)
library(gridExtra)

library(ggsci)
#check species IDs by depth category

lower_slope_species <- data %>%
  filter(Depth == "Lower slope") %>%
  pull(Species) %>%
  unique()

print(lower_slope_species)

slope_species <- data %>%
  filter(Depth == "Slope") %>%
  pull(Species) %>%
  unique()

print(slope_species)

upper_slope_species <- data %>%
  filter(Depth == "Upper slope") %>%
  pull(Species) %>%
  unique()

print(upper_slope_species)

mesopelagic_species <- data %>%
  filter(Depth == "Mesopelagic") %>%
  pull(Species) %>%
  unique()

print(mesopelagic_species)


shelf_species <- data %>%
  filter(Depth == "Shelf") %>%
  pull(Species) %>%
  unique()

print(shelf_species)

# Welch ANOVA with pairwise Games -Howell


library(rstatix)
library(DescTools)



# Welch ANOVA (rstatix)
welch_result <- welch_anova_test(data, d18Ooto ~ Species)
print(welch_result)

# 2. Then Games-Howell post hoc
gh_res <- data %>%
  games_howell_test(d18Ooto ~ Species, detailed = TRUE)

# extract unique depth values
species_depth <- data %>%
  select(Species, Depth) %>%
  distinct()
# append to gh results
gh_res_depth <- gh_res %>%
  left_join(species_depth, by = c("group1" = "Species")) %>%
  rename(Depth1 = Depth) %>%
  left_join(species_depth, by = c("group2" = "Species")) %>%
  rename(Depth2 = Depth)

print(gh_res_depth, n=Inf)
# Filter only p < 0.05
non_sig_pairs <- gh_res_depth %>%
  filter(p.adj > 0.05)


non_sig_pairs2 <- non_sig_pairs %>%
  mutate(
    Depth1 = as.character(Depth1),
    Depth2 = as.character(Depth2)
  )



grouped <- non_sig_pairs2 %>% group_by(Depth1)

split_tables <- grouped %>% group_split(.keep = TRUE)

# Names from group_keys() (one name per group)
names(split_tables) <- grouped %>% group_keys() %>% pull(Depth1)

# Verify
names(split_tables)

same_depth_counts <- non_sig_pairs2 %>%
  group_by(Depth1) %>%
  summarise(
    n_same = sum(Depth1 == Depth2, na.rm = TRUE),
    total = n(),
    .groups = "drop"
  ) %>%
  mutate(prop_same = n_same / total)

print(same_depth_counts)


#plot all modern d13C against d18O color by depth and shape by MOL
oto_guild <- ggplot(
  data,
  aes(
    x = d18Ooto,
    y = d13Coto,
    shape = MOL,
    fill  = Depth   # map fill at top-level aes
  )
) +
  geom_point(
    size = 4,
    color = "black",    # outline
    stroke = 0.6,
    alpha=0.8
  ) +
  scale_shape_manual(values = c(21, 22, 23, 24, 25)) +
  scale_fill_npg() +
  guides(
    shape = guide_legend(override.aes = list(fill = "grey80")), # keep outline legend
    fill  = guide_legend(override.aes = list(shape = 21, color = "black")) # show filled legend
  ) +
  theme_classic(base_size = 14) +
  theme(
    legend.position = c(1.1, 0.05),  # bottom-right inside the panel
    legend.justification = c("right", "bottom"), # anchor the legend box correctly
    legend.background = element_rect(fill = "white", color = "grey70"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey85"),
    plot.title = element_text(face = "bold", size = 25, hjust = 0.5)
  ) +
  labs(
    x = expression(delta^18*"O"[oto]*"(‰)"),
    y = expression(delta^13*"C"[oto]*"(‰)"),
    fill = "Site",
    shape = "MOL"
  )+
  coord_cartesian(xlim = c(-0.5, 4.5), clip = "off") 

oto_guild  # left pane of figure 3



### show the distributions of d13C values by MOL for a given d18O range
# Filter data within ±0.1 of 1 in d18Ooto
subset_data1 <- data %>%
  dplyr::filter(d18Ooto >= 0.9 & d18Ooto <= 1.1)

subset_data2 <- data %>%
  dplyr::filter(d18Ooto >= 1.9 & d18Ooto <= 2.1)

subset_data3 <- data %>%
  dplyr::filter(d18Ooto >= 2.9 & d18Ooto <= 3.1)


subset_plot1 <- ggplot(
  subset_data1,
  aes(
    x = MOL,
    y = d13Coto
  )
) +
  # one violin per MOL only
  geom_violin(
    fill = "grey85",   # neutral violin fill
    color = "black",
    alpha = 0.6,
    trim = FALSE
  ) +
  # Depth still shown by color, MOL still shown by shape
  geom_jitter(
    aes(fill = Depth, shape = MOL),
    position = position_jitter(width = 0.2),
    size = 3,
    color = "black",
    stroke = 0.6,
    alpha = 0.85,
    show.legend = TRUE
  ) +
  scale_shape_manual(values = c(21, 22, 23, 24, 25)) +
  scale_fill_manual(
    values = c(
      "Mesopelagic" = "#4DBBD5FF",
      "Shelf" = "#00A087FF",
      "Slope" = "#3C5488FF"
    ))+
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey85"),
    plot.title = element_text(face = "bold", size = 22, hjust = 0.5)
  ) +
  ylim(-10,1)+
  labs(
    x = "MOL",
    y = expression(delta^13*"C"[oto]*"(‰)"),
    fill = "Site",
    shape = "MOL"
  )+
  guides(
    fill = guide_legend(override.aes = list(shape = 21, color = "black"))
  )+
  labs(subtitle = expression("Relatively Warm/Shallow Water " * "(~1" * "‰" ~ delta^18 * "O" * "~11°C)"))


subset_plot1


subset_plot2 <- ggplot(
  subset_data2,
  aes(
    x = MOL,
    y = d13Coto
  )
) +
  # one violin per MOL only
  geom_violin(
    fill = "grey85",   # neutral violin fill
    color = "black",
    alpha = 0.6,
    trim = FALSE
  ) +
  # Depth still shown by color, MOL still shown by shape
  geom_jitter(
    aes(fill = Depth, shape = MOL),
    position = position_jitter(width = 0.2),
    size = 3,
    color = "black",
    stroke = 0.6,
    alpha = 0.85
  ) +
  scale_shape_manual(values = c(21, 22, 23, 24, 25)) +
  scale_fill_manual(
    values = c(
      "Lower slope" = "#E64B35FF",
      "Mesopelagic" = "#4DBBD5FF",
      "Shelf" = "#00A087FF",
      "Slope" = "#3C5488FF",
      "Upper slope" = "#F39B7FFF"
    ))+
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey85"),
    plot.title = element_text(face = "bold", size = 22, hjust = 0.5)
  ) +
  ylim(-10,1)+
  
  labs(
    x = "MOL",
    y = expression(delta^13*"C"[oto]*"(‰)"),
    fill = "Site",
    shape = "MOL"
  )+
  guides(
    fill = guide_legend(override.aes = list(shape = 21, color = "black"))
  )+
  labs(subtitle = expression("Intermediate Temperature/Depth " * "(~2" * "‰" ~ delta^18 * "O" * "~7°C)"))

subset_plot2


subset_plot3 <- ggplot(
  subset_data3,
  aes(
    x = MOL,
    y = d13Coto
  )
) +
  # one violin per MOL only
  geom_violin(
    fill = "grey85",   # neutral violin fill
    color = "black",
    alpha = 0.6,
    trim = FALSE
  ) +
  # Depth still shown by color, MOL still shown by shape
  geom_jitter(
    aes(fill = Depth, shape = MOL),
    position = position_jitter(width = 0.2),
    size = 3,
    color = "black",
    stroke = 0.6,
    alpha = 0.85
  ) +
  scale_shape_manual(values = c(21, 22, 23, 24, 25)) +
  scale_fill_manual(
    values = c(
      "Lower slope" = "#E64B35FF",
      "Mesopelagic" = "#4DBBD5FF",
      "Shelf" = "#00A087FF",
      "Slope" = "#3C5488FF",
      "Upper slope" = "#F39B7FFF"
    ))+
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey85"),
    plot.title = element_text(face = "bold", size = 22, hjust = 0.5)
  ) +
  ylim(-10,1)+
  
  labs(
    x = "MOL",
    y = expression(delta^13*"C"[oto]*"(‰)"),
    fill = "Site",
    shape = "MOL"
  )+
  guides(
    fill = guide_legend(override.aes = list(shape = 21, color = "black"))
  )+
  labs(subtitle = expression("Relatively Cool/Deep Water " * "(~3" * "‰" ~ delta^18 * "O" * "~3°C)"))

subset_plot3

library(patchwork)

# Remove legends and x labels for the top two plots
subset_plot1_clean <- subset_plot1 + 
  theme(legend.position = "none") +
  guides(fill = "none", shape = "none", color = "none")+
  labs(y = expression(delta^13*"C"[oto]*"(‰)"))

subset_plot2_clean <- subset_plot2 + 
  theme(legend.position = "none") +
  guides(fill = "none", shape = "none", color = "none")+
  labs(y = expression(delta^13*"C"[oto]*"(‰)"))

subset_plot3_clean <- subset_plot3 + 
  theme(legend.position = "none") +
  guides(fill = "none", shape = "none", color = "none")+
  labs(x = "MOL",
    y = expression(delta^13*"C"[oto]*"(‰)"))




# Combine
combined_plot_full <-
  (
    oto_guild +
      (subset_plot1_clean / subset_plot2_clean / subset_plot3_clean)
  ) +
  plot_layout(widths = c(2, 1)) &
  theme(
    legend.position = "right"
  )

combined_plot_full # Full Figure 3

#save plot
ggsave(
  filename = "combined_plot_full.pdf",
  plot = combined_plot_full,
  width = 18,        # inches
  height = 12,        # inches
  dpi = 300          # high resolution
)

###################################################################################
# Explicit factor levels (make sure these match your dataset)
all_mol_levels <- c("Shelf", "Mesopelagic", "Upper_slope", "Slope", "Lower_slope")  # replace with real MOL categories
all_depth_levels <- unique(data$Depth)  # or hardcode if you want fixed order

# Ensure factors are consistent
data$MOL <- factor(data$MOL, levels = all_mol_levels)
data$Depth <- factor(data$Depth, levels = all_depth_levels)

man <- manova(cbind(d13Coto, d18Ooto) ~ Species, data = data)
summary(man, test = "Pillai")

summary.aov(man)

library(effectsize)
eta_squared(man, partial = TRUE)

eta_squared(aov(d13Coto ~ Species, data=data))
eta_squared(aov(d18Ooto ~ Species, data=data))


library(MASS)
lda_res <- lda(Species ~ d13Coto + d18Ooto, data=data)
lda_res$scaling

lda_res


lda_pred <- data.frame(predict(lda_res)$x, group = data$Species)


ggplot(lda_pred, aes(x = LD1, y = LD2, color = group)) +
  geom_point(size = 3, alpha = 0.7) +
  stat_ellipse(level = 0.68, linewidth = 1) +
  theme_minimal(base_size = 14) +
  labs(
    title = "LDA Group Separation",
    x = "Linear Discriminant 1",
    y = "Linear Discriminant 2"
  ) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )

################################################################################
############################## NEW FIGURE 8 ##########################
################################################################################

#check species IDs by depth category

upper_slope_species <- Fossil_Fish %>%
  filter(Depth == "Upper slope") %>%
  pull(Species) %>%
  unique()

print(upper_slope_species)

shelf_species <- Fossil_Fish %>%
  filter(Depth == "Shelf") %>%
  pull(Species) %>%
  unique()

print(shelf_species)


#plot all modern d13C against d18O color by depth and shape by MOL
fossil_oto_guild <- ggplot(
  Fossil_Fish,
  aes(
    x = d18Ooto,
    y = d13Coto,
    shape = MOL,
    fill  = Depth   # map fill at top-level aes
  )
) +
  geom_point(
    size = 4,
    color = "black",    # outline
    stroke = 0.6,
    alpha=0.8
  ) +
  scale_shape_manual(values = c(21, 23)) +
  scale_fill_manual(
    values = c(
      "Shelf" = "#00A087FF",
      "Upper slope" = "#F39B7FFF"
    ))+
  guides(
    shape = guide_legend(override.aes = list(fill = "grey80")), # keep outline legend
    fill  = guide_legend(override.aes = list(shape = 21, color = "black")) # show filled legend
  ) +
  theme_classic(base_size = 14) +
  theme(
    legend.position = c(1.1, 0.05),  # bottom-right inside the panel
    legend.justification = c("right", "bottom"), # anchor the legend box correctly
    legend.background = element_rect(fill = "white", color = "grey70"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey85"),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5)
  ) +
  labs(
    x = expression(delta^18*"O"[oto]*"(‰)"),
    y = expression(delta^13*"C"[oto]*"(‰)"),
    fill = "Site",
    shape = "MOL"
  )+
  coord_cartesian(clip = "off") 

fossil_oto_guild  # left pane of figure 6





### show the distributions of d13C values by MOL for a given d18O range
# Filter data within ±0.1 of 1 in d18Ooto
F_subset_data1 <- Fossil_Fish %>%
  dplyr::filter(d18Ooto >= -2.2 & d18Ooto <= -1.8)

F_subset_data2 <- Fossil_Fish %>%
  dplyr::filter(d18Ooto >= -3.2 & d18Ooto <= -2.8)

F_subset_data3 <- Fossil_Fish %>%
  dplyr::filter(d18Ooto >= -4.2 & d18Ooto <= -3.8)


F_subset_plot1 <- ggplot(
  F_subset_data1,
  aes(
    x = MOL,
    y = d13Coto
  )
) +
  # one violin per MOL only
  geom_violin(
    fill = "grey85",   # neutral violin fill
    color = "black",
    alpha = 0.6,
    trim = FALSE
  ) +
  # Depth still shown by color, MOL still shown by shape
  geom_jitter(
    aes(fill = Depth, shape = MOL),
    position = position_jitter(width = 0.2),
    size = 3,
    color = "black",
    stroke = 0.6,
    alpha = 0.85,
    show.legend = TRUE
  ) +
  scale_shape_manual(values = c(21, 23)) +
  scale_fill_manual(
    values = c(
      "Shelf" = "#00A087FF",
      "Upper slope" = "#F39B7FFF"
    ))+
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey85"),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5)
  ) +
  ylim(-10,1)+
  labs(
    x = "MOL",
    y = expression(delta^13*"C"[oto]*"(‰)"),
    fill = "Site",
    shape = "MOL"
  )+
  guides(
    fill = guide_legend(override.aes = list(shape = 21, color = "black"))
  )+
  labs(subtitle = expression("Relatively Cool/Deep Water " * "(~-2" * "‰" ~ delta^18 * "O" * "~21°C)"))


F_subset_plot1


F_subset_plot2 <- ggplot(
  F_subset_data2,
  aes(
    x = MOL,
    y = d13Coto
  )
) +
  # one violin per MOL only
  geom_violin(
    fill = "grey85",   # neutral violin fill
    color = "black",
    alpha = 0.6,
    trim = FALSE
  ) +
  # Depth still shown by color, MOL still shown by shape
  geom_jitter(
    aes(fill = Depth, shape = MOL),
    position = position_jitter(width = 0.2),
    size = 3,
    color = "black",
    stroke = 0.6,
    alpha = 0.85
  ) +
  scale_shape_manual(values = c(21, 23)) +
  scale_fill_manual(
    values = c(
      "Shelf" = "#00A087FF",
      "Upper slope" = "#F39B7FFF"
    ))+
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey85"),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5)
  ) +
  ylim(-10,1)+
  
  labs(
    x = "MOL",
    y = expression(delta^13*"C"[oto]*"(‰)"),
    fill = "Site",
    shape = "MOL"
  )+
  guides(
    fill = guide_legend(override.aes = list(shape = 21, color = "black"))
  )+
  labs(subtitle = expression("Intermediate Temperature/Depth " * "(~-3" * "‰" ~ delta^18 * "O" * "~26°C)"))

F_subset_plot2


F_subset_plot3 <- ggplot(
  F_subset_data3,
  aes(
    x = MOL,
    y = d13Coto
  )
) +
  # one violin per MOL only
  geom_violin(
    fill = "grey85",   # neutral violin fill
    color = "black",
    alpha = 0.6,
    trim = FALSE
  ) +
  # Depth still shown by color, MOL still shown by shape
  geom_jitter(
    aes(fill = Depth, shape = MOL),
    position = position_jitter(width = 0.2),
    size = 3,
    color = "black",
    stroke = 0.6,
    alpha = 0.85
  ) +
  scale_shape_manual(values = c(21, 23)) +
  scale_fill_manual(
    values = c(
      "Shelf" = "#00A087FF",
      "Upper_slope" = "#F39B7FFF"
    ))+
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey85"),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5)
  ) +
  ylim(-10,1)+
  
  labs(
    x = "MOL",
    y = expression(delta^13*"C"[oto]*"(‰)"),
    fill = "Site",
    shape = "MOL"
  )+
  guides(
    fill = guide_legend(override.aes = list(shape = 21, color = "black"))
  )+
  labs(subtitle = expression("Relatively Warm/Shallow Water " * "(~-4" * "‰" ~ delta^18 * "O" * "~31°C)"))

F_subset_plot3

library(patchwork)

# Remove legends and x labels for the top two plots
F_subset_plot1_clean <- F_subset_plot1 + 
  theme(legend.position = "none") +
  guides(fill = "none", shape = "none", color = "none")+
  labs(y = expression(delta^13*"C"[oto]*"(‰)"))

F_subset_plot2_clean <- F_subset_plot2 + 
  theme(legend.position = "none") +
  guides(fill = "none", shape = "none", color = "none")+
  labs(y = expression(delta^13*"C"[oto]*"(‰)"))

F_subset_plot3_clean <- F_subset_plot3 + 
  theme(legend.position = "none") +
  guides(fill = "none", shape = "none", color = "none")+
  labs(x = "MOL",
       y = expression(delta^13*"C"[oto]*"(‰)"))


# Combine
fossil_combined_plot_full <-
  (
    fossil_oto_guild +
      (F_subset_plot3_clean / F_subset_plot2_clean / F_subset_plot1_clean)
  ) +
  plot_layout(widths = c(2, 1)) &
  theme(
    legend.position = "right"
  )


fossil_combined_plot_full # Full Figure 6

#save plot
ggsave(
  filename = "fossil_combined_plot_full.pdf",
  plot = fossil_combined_plot_full,
  width = 18,        # inches
  height = 12,        # inches
  dpi = 300          # high resolution
)
