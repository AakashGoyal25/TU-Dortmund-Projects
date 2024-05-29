library(rpart)
library(party)
library(caret)

#Reading Data File
sml_df <- read.csv("/home/aakash/TU Dortmund/SML/SML Dataset.csv", header = TRUE, sep = ",")
participants_df <- read.csv("/home/aakash/TU Dortmund/SML/participants.csv", header = TRUE, sep = ",")

summary(sml_df)
barplot(table(sml_df$Target),
        main = "Bar Plot of Review Type",
        xlab = "Review Type",
        ylab = "Count",
        col = "gray",
        border = "black",
        width=0.1,
        space=0.7)

barplot(table(participants_df$Age),
        main = "Bar Plot of Age",
        xlab = "Age",
        ylab = "Count",
        col = "gray",
        border = "black")

barplot(table(participants_df$Gender),
        main = "Bar Plot of Gender",
        xlab = "Gender",
        ylab = "Count",
        col = "gray",
        border = "black")

barplot(table(participants_df$Academic.level),
        main = "Bar Plot of Academic Level",
        xlab = "Academic Level",
        ylab = "Count",
        col = "skyblue",
        border = "black")

par(mfrow = c(3, 2))  # Set up a 2x2 layout for the plots

boxplot(compound_sentences ~ Target, data = sml_df, main = "Compound Sentences by Review Type", ylab = "Compound Senteces", xlab="Review Type",col = c("lightblue", "lightgreen"), border = "black", cex.main = 1.4, cex.lab = 1.4)

boxplot(personal_pronouns ~ Target, data = sml_df, main = "Personal Pronouns by Review Type", ylab = "Personal Pronouns", xlab="Review Type", col = c("lightblue", "lightgreen"), border = "black", cex.main = 1.4, cex.lab=1.4)

boxplot(adjectives ~ Target, data = sml_df, main = "Adjectives by Review Type", ylab = "Adjectives", xlab="Review Type", col = c("lightblue", "lightgreen"), border = "black",  cex.main = 1.4,cex.lab=1.4)

boxplot(spelling_mistakes ~ Target, data = sml_df, main = "Spelling Mistakes by Review Type", ylab = "Spelling Mistakes", xlab="Review Type", col = c("lightblue", "lightgreen"), cex.main = 1.4,cex.lab=1.4,border = "black")

boxplot(word_count ~ Target, data = sml_df, main = "Word Count by Review Type", ylab = "Word Count", xlab="Review Type", col = c("lightblue", "lightgreen"), border = "black",)

sml_df$Review <- NULL
str(sml_df)

sml_df$Target= as.factor(sml_df$Target)
ct <- ctree(Target ~ ., data=sml_df, controls = ctree_control(mincriterion = 0.90))
ct
plot(ct)

cm <- confusionMatrix(predict(ct),sml_df$Target)
cm

bacc <- (cm$table[1,1] /
           length(sml_df$Target[sml_df$Target == "AI"]) +
           cm$table[2,2] / length(sml_df$Target[sml_df$Target
                                            == "Human"]))/2
bacc


###########################2nd Tree#########################3
ct2 <- ctree(Target ~ word_count+adjectives+compound_sentences, data=sml_df, controls = ctree_control(mincriterion = 0.90))
ct2
plot(ct2)


cm2 <- confusionMatrix(predict(ct2),sml_df$Target)
cm2

bacc2 <- (cm2$table[1,1] /
           length(sml_df$Target[sml_df$Target == "AI"]) +
           cm2$table[2,2] / length(sml_df$Target[sml_df$Target
                                                == "Human"]))/2
bacc2

##########################3rd Tree#####################
ct3 <- ctree(Target ~ spelling_mistakes+adjectives+word_count+personal_pronouns, data=sml_df)
ct3
plot(ct3)


cm3 <- confusionMatrix(predict(ct3),sml_df$Target)
cm3

bacc2 <- (cm2$table[1,1] /
            length(sml_df$Target[sml_df$Target == "AI"]) +
            cm2$table[2,2] / length(sml_df$Target[sml_df$Target
                                                  == "Human"]))/2
bacc2


#library(PrInDT)
#help(package=PrInDT)
#N <- 1001 # define the number of repetitions
#sl <- 0.05 # define the significance level as 1%
#out <- PrInDT(sml_df,"Target",ctestv=NA,N,percl=0.09,percs=1,conf.level=(1-sl)) # may run 1 min
#out # print out the results of the function
#plot(out$tree1st) # plot best tree (tree1st)


citation(package = "rpart")
citation(package ="party")
citation(package ="caret")
