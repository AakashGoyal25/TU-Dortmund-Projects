#####################-----All libraries--------#################################
library(rstan)
library(brms)
library(bayesplot)
library(loo)
library(stats)
library(dplyr)
library(posterior)
library(ggplot2)
library(magrittr)
library(car)
library(AER)
library(scales)

################################################################################
#######################----Data Import----######################################
suicide_df <- read.csv("/home/aakash/TU Dortmund/ADBA/Suicides_in_India_full.csv",
                       header = TRUE, sep = ",")

######################----Data Preprocessing----################################
suicide_df <- suicide_df[suicide_df$Year == 2012, ]
suicide_df <- suicide_df[!(suicide_df$Age_group == '0-100+'), ]
subset_df1 <- suicide_df[suicide_df$State %in% c("HARYANA", "UTTAR PRADESH", 
                                                 "ANDHRA PRADESH", "TAMIL NADU", 
                                                 "MAHARASHTRA", "GUJARAT", 
                                                 "WEST BENGAL", "ASSAM"), ]

dim(subset_df1)
colSums(subset_df1==0)

#subset_df1 : Processed Data
subset_df1$Type <- gsub("[^a-zA-Z0-9]", "_", subset_df1$Type) #Removing special char

#Mapping Categories
subset_df1 <- subset_df1 %>%
  mutate(
    Cause = case_when(
      Type %in% c("By_Consuming_Insecticides","Consuming_Insecticides", "By_Consuming_Other_Poison", 
                  "By_Over_Alcoholism", "By_Overdose_of_sleeping_pills", "Drug_Abuse_Addiction") ~ "Drugs",
      
      Type %in% c("Ideological_Causes_Hero_Worshipping", "Love_Affairs", 
                  "Fall_in_Social_Reputation", "Physical_Abuse__Rape_Incest_Etc__", 
                  "Cancellation_Non_Settlement_of_Marriage", "Dowry_Dispute") ~ "Social",
      
      Type %in% c("Family_Problems", "Married", "Never_Married", "Divorcee","Seperated",
                  "Divorce", "Widowed_Widower","Death_of_Dear_Person",
                  "Suspected_Illicit_Relation") ~ "Family",
      
      Type %in% c("Insanity_Mental_Illness", "Illness__Aids_STD_", "Not_having_Children_Barrenness_Impotency", 
                  "Other_Prolonged_Illness", "Cancer","Paralysis", 
                  "Insanity_Mental_Illness", "Illegitimate_Pregnancy") ~ "Health",
      
      Type %in% c("No_Education", "Student", "Hr__Secondary_Intermediate_Pre_Universit",
                  "Failure_in_Examination","Post_Graduate_and_Above", "Middle", "Primary", 
                  "Diploma", "Matriculate_Secondary", "Unemployed", "Graduate") ~ "Educational",
      
      Type %in% c("Property_Dispute","Self_employed__Business_activity", "Service__Private_", 
                  "Poverty", "Professional_Activity", "Farming_Agriculture_Activity",
                  "Professional_Career_Problem", "Retired_Person", "House_Wife", 
                  "Bankruptcy_or_Sudden_change_in_Economic", "Unemployment",
                  "Service__Government_","Public_Sector_Undertaking",
                  "Self_employed__Business_activity_", "By_Machine") ~ "Financial & Career",
      
      Type %in% c("By_Hanging", "By_Jumping_from__Building_", "By_Fire_Arms", "By_Fire_Self_Immolation",
                  "By_Jumping_from__Other_sites_", "By_Jumping_off_Moving_Vehicles_Trains", "By_Drowning", 
                  "By_touching_electric_wires","By_coming_under_running_vehicles_trains", 
                  "By_Self_Infliction_of_injury") ~ "Freewill",
      
      Type %in% c("Others__Please_Specify_", "By_Other_means__please_specify_",
                  "Other_Causes__Please_Specity_", "Causes_Not_known", 
                  "Others__Please_Specify_") ~ "Others",
      TRUE ~ Type  # If not matching any of the specified conditions, keep the original value
    )
  )
print(unique(subset_df1$Cause))
head(subset_df1)
subset_df1 <- subset_df1 %>% select(-Type_code, -Type) # Removing two columns

# Aggregate rows based on equality in columns State, Year, Gender, Age_group, Cause
subset_df1 <- subset_df1 %>% group_by(State, Year, Gender, Age_group, Cause) %>%
  summarise(Total = sum(Total))

# View the aggregated DataFrame
head(subset_df1)
################################## EDA ###############################################

#Cause v/s Total
ggplot(subset_df1, aes(x = Cause, y= Total)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(x = "Cause", y = "Total", title = "Total by Cause")+
  theme(text = element_text(size = 16),
        axis.title = element_text(size = 14), # Axis titles
        axis.text = element_text(size = 14), # Axis text (x and y axis labels)
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) # Plot title

#State v/s Total
ggplot(subset_df1, aes(x = State, y= Total)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(x = "State", y = "Total", title = "Total by State")+
  theme(text = element_text(size = 16),
        axis.title = element_text(size = 14), # Axis titles
        axis.text = element_text(size = 14), # Axis text (x and y axis labels)
        plot.title = element_text(size = 16, face = "bold", hjust=0.5),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) # Plot title


#Gender v/s Total
ggplot(subset_df1, aes(x = Gender, y= Total)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.6) +
  theme_minimal() +
  labs(x = "Gender", y = "Total", title = "Total by Gender")+
  theme(text = element_text(size = 14),
        axis.title = element_text(size = 14), # Axis titles
        axis.text = element_text(size = 14), # Axis text (x and y axis labels)
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5))+
  scale_y_continuous(labels = label_number())

#Age-group v/s Total
ggplot(subset_df1, aes(x = Age_group, y= Total)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(x = "Age Group", y = "Total", title = "Total by Age Group")+
  theme(text = element_text(size = 16),
        axis.title = element_text(size = 14), # Axis titles
        axis.text = element_text(size = 14), # Axis text (x and y axis labels)
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) # Plot title


#Distribution of Cleaned dataset Response Variable (Target Variable)
ggplot(subset_df1, aes(x = Total)) +
  geom_histogram(binwidth = 250, fill = "steelblue", color = "black") +
  theme_minimal() +
  labs(x = "Total", title = "Distribution of Total (Target Variable)")+
  theme(text = element_text(size = 16),
        axis.title = element_text(size = 14), # Axis titles
        axis.text = element_text(size = 14), # Axis text (x and y axis labels)
        plot.title = element_text(size = 16, face = "bold", hjust=0.5)) # Plot title
#####################################################################################################

#checking zeroes in dataset
colSums(subset_df1==0)

#XXXXXXXXXXXXXXXXXXXX------------Model Fitting----------XXXXXXXXXXXXXXXXXXXXXXXX
#############################----Poisson Model-----#############################
Poisson_ <- brm(Total ~ Cause + Gender + (1 + Age_group|State), family=poisson(),
                     data = subset_df1, control = list(max_treedepth = 15, adapt_delta = 0.99), core=26, iter=3000)

saveRDS(Poisson_, file = "/home/aakash/TU Dortmund/ADBA/Model/poisson/poisson_model.rds")
Poisson_ <- readRDS("/home/aakash/TU Dortmund/ADBA/Model/poisson/poisson_model.rds")
summary(Poisson_)

########## Graphs ##########
mcmc_trace(Poisson_)
pp_check(Poisson_, ndraws = 100, type = 'ecdf_overlay') + theme(aspect.ratio = 1)
plot(Poisson_)
conditional_effects(Poisson_)

#####################################----NegBinomial----########################
NegBino_ <- brm(Total ~ Cause + Gender + (1 + Age_group|State), 
                family=negbinomial(),
                data = subset_df1, 
                control = list(max_treedepth = 15,adapt_delta = 0.99),
                core=26, iter=3000)

saveRDS(NegBino_, file = "/home/aakash/TU Dortmund/ADBA/Model/NegBinomial/negBinomial_model.rds")
NegBino_ <- readRDS("/home/aakash/TU Dortmund/ADBA/Model/NegBinomial/negBinomial_model.rds")

summary(NegBino_)

#       ######### Graphs ##########
mcmc_trace(NegBino_)
pp_check(NegBino_, ndraws = 100, type = 'ecdf_overlay') + theme(aspect.ratio = 1)
plot(NegBino_)
conditional_effects(NegBino_)

#################################################################################
################################### Zero_inflated neg binomial model##############
ZINB <- brm(Total ~ Cause + Gender + (1 + Age_group|State), family=zero_inflated_negbinomial(),
              data = subset_df1,control = list(max_treedepth = 20, adapt_delta = 0.99), core=26, iter=3000)

saveRDS(ZINB, file = "/home/aakash/TU Dortmund/ADBA/Model/zero_inflated_negbinomial/zi_nbm.rds")
ZINB <- readRDS("/home/aakash/TU Dortmund/ADBA/Model/zero_inflated_negbinomial/zi_nbm.rds")
summary(ZINB)

########## Graphs ##########
mcmc_trace(ZINB)
pp_check(ZINB, ndraws = 100, type = 'ecdf_overlay') + theme(aspect.ratio = 1)
plot(ZINB)
ce_ZINB <- conditional_effects(ZINB)
plot(ce_ZINB) 

# --------------------- LOO compare -----------------------

loo(Poisson_, NegBino_, ZINB, compare = TRUE)

################################################################################
###############--- Prior Sensitivity Analysis ---###############################

get_prior(Total ~ Cause + Gender + (1 + Age_group|State), 
          family=zero_inflated_negbinomial(),
          data = subset_df1,
          control = list(max_treedepth = 20, adapt_delta = 0.99), 
          core=26, iter=3000)

# PSA1 MODEL
# Define the Weakly informative priors
priors_set1<- c(
  set_prior("normal(0, 50)", class = "b"), 
  set_prior("normal(0, 50)", class = "Intercept"), 
  set_prior("cauchy(0, 10)", class = "sd"), 
  set_prior("beta(1, 1)", class = "zi"), 
  set_prior("gamma(1, 0.01)", class = "shape")
)
# Fit the model with Weak priors
PSA1 <- brm(
  formula = Total ~ Cause + Gender+(1 + Age_group|State),
  family = zero_inflated_negbinomial(),
  data = subset_df1,
  prior = priors_set1,
  control = list(max_treedepth = 20, adapt_delta = 0.99),
  core=26, iter=3000)

saveRDS(PSA1, file = "/home/aakash/TU Dortmund/ADBA/Model/zxprior_sensitivity/PSA1.rds")
PSA1 <- readRDS("/home/aakash/TU Dortmund/ADBA/Model/zxprior_sensitivity/PSA1.rds")

prior_summary(PSA1)
summary(PSA1)
plot(PSA1)

#------------------------ Prior Setting 2---------------------------------------
# PSA Model 2 : Informative model
priors_set2 <- c(
  set_prior("normal(0, 5)", class = "b"),
  set_prior("normal(0, 10)", class = "Intercept"),
  set_prior("cauchy(0, 2)", class = "sd"),
  set_prior("beta(1, 1)", class = "zi"), 
  set_prior("gamma(2, 0.5)", class = "shape")
)

# Fit the model with informative priors
PSA2 <- brm(
  formula = Total ~ Cause + Gender + (1 + Age_group|State),
  family = zero_inflated_negbinomial(),
  data = subset_df1,
  prior = priors_set2,
  control = list(max_treedepth = 20, adapt_delta = 0.99), 
  core=26, iter=3000)

saveRDS(PSA2, file = "/home/aakash/TU Dortmund/ADBA/Model/zxprior_sensitivity/PSA2.rds")
PSA2 <- readRDS("/home/aakash/TU Dortmund/ADBA/Model/zxprior_sensitivity/PSA2.rds")

prior_summary(PSA2)
summary(PSA2)
plot(PSA2)


# -----------------Comparing with LOO --------------------------
loo(ZINB, PSA1, PSA2, compare = TRUE)

################################################################################
#############-------Convergence Diagnostics--------------------#################

#Function to generate Rhat graph
custom_mcmc_rhat <- function(mcmc_samples, threshold = 1.05) {
  # Create a data frame for plotting
  rhat_data <- data.frame(
    parameter = names(mcmc_samples),
    rhat = as.numeric(mcmc_samples),
    above_threshold = as.numeric(mcmc_samples) > threshold
  )
  
  start_x <- min(c(min(rhat_data$rhat), threshold)) - 0.01
  
  # Create the plot
  p <- ggplot(rhat_data, aes(x = rhat, y = reorder(parameter, rhat), color = above_threshold)) +
    geom_point() + # Plot points
    geom_segment(aes(x = start_x, xend = rhat, yend = parameter, y = parameter), 
                 linetype = "solid", color = "grey") + 
    scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red")) + 
    geom_vline(xintercept = threshold, linetype = "dashed", color = "red", size = 1) + # Threshold line
    theme_minimal() +
    labs(x = "R-hat", y = "Parameter", title = paste("R-hat values with threshold", threshold)) +
    theme(legend.position = "none")
  
  return(p)
}

rhat_p <- rhat(Poisson_)
rhat_zinb <- rhat(ZINB)
rhat_nb<- rhat(NegBino_)

custom_mcmc_rhat(rhat_p, 1.01)
custom_mcmc_rhat(rhat_zinb, 1.01)
custom_mcmc_rhat(rhat_nb, 1.01)

##################-----Generating Citations----#################################

print(citation(package="rstan"), bibtex=TRUE)
print(citation(package="brms"), bibtex=TRUE)
print(citation(package="bayesplot"), bibtex=TRUE)
print(citation(package="loo"), bibtex=TRUE)
print(citation(package="stats"), bibtex=TRUE)
print(citation(package="dplyr"), bibtex=TRUE)
print(citation(package="patchwork"), bibtex=TRUE)
print(citation(package="posterior"), bibtex=TRUE)
print(citation(package="ggplot2"), bibtex=TRUE)
print(citation(package="magrittr"), bibtex=TRUE)
print(citation(package="car"), bibtex=TRUE)
print(citation(package="AER"), bibtex=TRUE)
print(citation(package="scales"), bibtex=TRUE)

####################------Generating STANCODE ------------######################

p_model<-stancode(Poisson_)
nb_model<-stancode(NegBino_)
ZINB_model<-stancode(ZINB)
PSA1_stancode<- stancode(PSA1)
PSA2_stancode<- stancode(PSA2)

# Save Stan code to a file
writeLines(p_model, "/home/aakash/TU Dortmund/ADBA/Stancodes/poisson_model.stan")
writeLines(nb_model, "/home/aakash/TU Dortmund/ADBA/Stancodes/negbino_model.stan")
writeLines(ZINB_model, "/home/aakash/TU Dortmund/ADBA/Stancodes/ZINB.stan")
writeLines(PSA1_stancode, "/home/aakash/TU Dortmund/ADBA/Stancodes/PSA1.stan")
writeLines(PSA2_stancode, "/home/aakash/TU Dortmund/ADBA/Stancodes/PSA2.stan")
