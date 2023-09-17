## ================================================================================================================
## Analysis, Chatbot Stigma -- Interaction Study
## ================================================================================================================

## clear workspace
rm(list = ls())

options(download.file.method = "libcurl")

if (!require(pacman)) { install.packages("pacman") }
pacman::p_load('ggplot2',
               'effsize',
               'rstatix',
               'sjstats',
               'ggforce',
               'ggpubr',
               'ltm',
               'tidyverse',
               'corrplot',
               'Hmisc',
               'boot',
               'glmnet',
               'lsr')

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set working directory to current directory
source('process.R')

data_setup <- function() {
    ## ================================================================================================================
    ##                                               PRE-PROCESSING, INCL. EXCLUSIONS
    ## ================================================================================================================

    ## READ INTERACTION PRESENT DATA

    # 'human' == 1, 'ai' == 2
    d_int <- read.csv('./data/data_interaction.csv')
    d_int$X <- NULL
    d_int$X.1 <- NULL
    d_int$FL_51_DO <- 0
    d_int$FL_61_DO <- 0

    # READ ADDITIONAL HUMAN TRIAL
    d_int_extra <- read.csv('./data/human_trial_addition.csv')
    d_int_extra <- d_int_extra[d_int_extra[, 'Q14'] != 23, ]  # Exclude participant who did not talk to chatbot
    d_int_extra$X <- NULL
    d_int_extra$FL_51_DO <- 0
    d_int_extra$FL_61_DO <- 0

    d_int <- rbind(d_int, d_int_extra)

    d_int <- subset(d_int, d_int$Finished == 1)
    d_int <- subset(d_int, (d_int$interaction_type != ""))

    # Attention Check
    d_int <- subset(d_int, (d_int$att1 == 2 & d_int$att2 == 2))
    d_int <- subset(d_int, (d_int$will_interact == "yes" & d_int$finished_conversation == "yes"))

    ## READ INTERACTION ABSENT DATA

    d_noInt <- read.csv('data/data_noInteraction.csv')
    d_noInt$X <- NULL
    d_noInt <- subset(d_noInt, d_noInt$Finished == 1)
    d_noInt <- subset(d_noInt, (d_noInt$interaction_type != ""))
    d_noInt <- subset(d_noInt, (d_noInt$att1 == 2 & d_noInt$att2 == 2))
    
    print(paste0("Number of participants hired: ", dim(d_int)[1] + dim(d_noInt)[1] + 1)) # Added one since we exclude a participant who did not talk to the chatbot
    n_d_int_before <- dim(d_int)[1] + 1
    d_noInt_before <- dim(d_noInt)[1]
    ## ================================================================================================================
    ##                                                    SUBSETTING
    ## ================================================================================================================

    ## define new data frame to extract pre-processed data into:
    d_subset_noInt <- array(dim = c(dim(d_noInt)[1], 16))
    colnames(d_subset_noInt) <- c('agent_cond', 'interact_cond', 'willing_friend', 'willing_romantic', 'wtp',
                                  'true', 'talk_listen', 'not_judge', 'emotion', 'understand', 'body', 'comp1', 'comp2',
                                  'belief', 'worker_id', 'explain')
    d_subset_noInt <- as.data.frame(d_subset_noInt, stringsAsFactors = FALSE)

    ## extract good data from the middle part of raw data:
    for (i in 1:dim(d_noInt)[1]) {
        curr <- d_noInt[i, c(21:42)][!is.na(d_noInt[i, c(21:42)])] # for a given row, get only the non-NA values
        d_subset_noInt[i, 3:13] <- as.numeric(curr[curr != ""])
        d_subset_noInt[i, 14] <- 0
        d_subset_noInt[i, 16] <- ""
        d_subset_noInt[i, 1] <- d_noInt$interaction_type[i]
        d_subset_noInt[i, 2] <- "no"
    }

    d_subset_int <- array(dim = c(dim(d_int)[1], 16))
    colnames(d_subset_int) <- c('agent_cond', 'interact_cond', 'willing_friend', 'willing_romantic', 'wtp',
                                'true', 'talk_listen', 'not_judge', 'emotion', 'understand', 'body', 'comp1', 'comp2',
                                'belief', 'worker_id', 'explain')
    d_subset_int <- as.data.frame(d_subset_int, stringsAsFactors = FALSE)

    ## extract good data from the middle part of raw data:
    for (i in 1:dim(d_int)[1]) {
        curr <- d_int[i, c(59:80)][!is.na(d_int[i, c(59:80)])] # for a given row, get only the non-NA values
        d_subset_int[i, 1] <- d_int$interaction_type[i]
        d_subset_int[i, 2] <- d_int$will_interact[i]
        d_subset_int[i, 3:13] <- as.numeric(curr[curr != ""])

        d_subset_int[i, 14] <- d_int$believe_q[i]
        d_subset_int[i, 15] <- d_int$Q14[i]
        d_subset_int[i, 16] <- d_int$explain[i]
    }

    d_subset <- rbind(d_subset_noInt, d_subset_int)
    d_subset_rest <- rbind(d_noInt[, 43:55], d_int[, 43:55])
    d_merged <- cbind(d_subset, d_subset_rest)

    d_merged$agent_cond_n <- ifelse(d_merged$agent_cond == 'human', 1, 2)
    d_merged$interact_cond_n <- ifelse(d_merged$interact_cond == "yes", 1, 2)

    # Make wtp values bigger than 10 as 10, since we said that in the survey but did not put a constraint
    d_merged[d_merged$wtp > 10,] <- 10.0

    ## Check if WTP is skewed and fix
    hist(d_merged$wtp, main = "Willingness to Pay", xlab = "Money (USD)", border = "black", col = "light blue")
    shapiro.test(d_merged$wtp)

    ## transform values to reduce skew
    d_merged$wtp_logged <- log(d_merged$wtp + 1) + 1
    hist(d_merged$wtp_logged, main = "Willingness to Pay", xlab = "Money (USD)", border = "black", col = "light blue")
    shapiro.test(d_merged$wtp_logged)

    ## ================================================================================================================
    ##                                              PERFORM EXCLUSIONS
    ## ================================================================================================================

    ## get number of participants BEFORE exclusions:
    print(paste0("Number of participants hired (after attention checks): ", dim(d_merged)[1] + 1))

    ## perform comprehension exclusions:
    # this will remove responses from the dataframe that failed comprehension checks (i.e., "2")
    d_merged <- subset(d_merged, (d_merged$comp1 == d_merged$agent_cond_n & d_merged$comp2 == 2))
    print(paste0("Number of participants after comprehension: ", dim(d_merged)[1]))
    
    mean(as.numeric(d_merged[as.numeric(d_merged$age) < 100, 'age']))
    table(d_merged$ai_companion_exp)[1] / (table(d_merged$ai_companion_exp)[1] + table(d_merged$ai_companion_exp)[2])
    # For true AI experience, see d_merged$ai_companion_exp2 column
    14 / dim(d_merged)[1]
    
    # Belief == 1: Believed they were talking to a human (Not Excluded), Belief == 2: Did not believe they were talking to a human (Excluded)
    # Compare education level, age, AI experience and gender between Belief groups
    print("Believed vs. not demographics comparisons")
    
    ################ AGE ################
    print("AGE")
    vart <- var.test(as.numeric(na.omit(d_merged[((d_merged$agent_cond == 'human') & (d_merged$belief == 1) & (as.numeric(d_merged$age) < 100)), 'age'])), as.numeric(na.omit(d_merged[((d_merged$agent_cond == 'human') & (d_merged$belief == 2) & (as.numeric(d_merged$age) < 100)), 'age'])))
    print(t.test(as.numeric(na.omit(d_merged[((d_merged$agent_cond == 'human') & (d_merged$belief == 1) & (as.numeric(d_merged$age) < 100)), 'age'])), as.numeric(na.omit(d_merged[((d_merged$agent_cond == 'human') & (d_merged$belief == 2) & (as.numeric(d_merged$age) < 100)), 'age'])), var.equal = vart$p.value > 0.05))
    print(cohen.d(as.numeric(na.omit(d_merged[((d_merged$agent_cond == 'human') & (d_merged$belief == 1) & (as.numeric(d_merged$age) < 100)), 'age'])), as.numeric(na.omit(d_merged[((d_merged$agent_cond == 'human') & (d_merged$belief == 2) & (as.numeric(d_merged$age) < 100)), 'age']))))

    ################ EDU ################
    print("EDU")
    vart <- var.test(as.numeric(na.omit(d_merged[(d_merged$agent_cond == 'human') & (d_merged$belief == 1), 'edu'])), as.numeric(na.omit(d_merged[(d_merged$agent_cond == 'human') & (d_merged$belief == 2), 'edu'])))
    print(t.test(as.numeric(na.omit(d_merged[(d_merged$agent_cond == 'human') & (d_merged$belief == 1), 'edu'])), as.numeric(na.omit(d_merged[(d_merged$agent_cond == 'human') & (d_merged$belief == 2), 'edu'])), var.equal = vart$p.value > 0.05))
    print(cohen.d(as.numeric(na.omit(d_merged[(d_merged$agent_cond == 'human') & (d_merged$belief == 1), 'edu'])), as.numeric(na.omit(d_merged[(d_merged$agent_cond == 'human') & (d_merged$belief == 2), 'edu']))))
    
    ################ AI EXPERIENCE ################
    print("AI EXPERIENCE")
    ai_exp_believed <- as.numeric(na.omit(d_merged[(d_merged$agent_cond == 'human') & (d_merged$belief == 1), 'ai_companion_exp']))
    print(paste0("Percentage of participants who had AI experience for subset who believed: ", 100*(sum(ai_exp_believed == 1) / length(ai_exp_believed))))

    ai_exp_not <- as.numeric(na.omit(d_merged[(d_merged$agent_cond == 'human') & (d_merged$belief == 2), 'ai_companion_exp']))
    print(paste0("Percentage of participants who had AI experience for subset who did not believe: ", 100*(sum(ai_exp_not == 1) / length(ai_exp_not))))

    print(prop.test(c(sum(ai_exp_believed == 1), sum(ai_exp_not == 1)), c(length(ai_exp_believed), length(ai_exp_not))))

    # Test if AI experience has a main effect on all DV's (willingness to friend, romance, and pay)
    d_int$ai_companion_exp_f <- as.factor(d_int$ai_companion_exp)
    
    # Combine d_int$willing_friend_post_1 and d_int$willing_friend_post_1.1
    d_int[is.na(d_int$willing_friend_post_1), 'willing_friend_post_1'] <- "" # Make NA's ""
    d_int[is.na(d_int$willing_friend_post_1.1), 'willing_friend_post_1.1'] <- "" # Make NA's ""
    d_int$willing_friend <- as.numeric(paste0(d_int$willing_friend_post_1, d_int$willing_friend_post_1.1))
    
    d_int[is.na(d_int$willing_partner_post_1), 'willing_partner_post_1'] <- "" # Make NA's ""
    d_int[is.na(d_int$willing_partner_post_1.1), 'willing_partner_post_1.1'] <- "" # Make NA's ""
    d_int$willing_partner <- as.numeric(paste0(d_int$willing_partner_post_1, d_int$willing_partner_post_1.1))
    
    d_int[is.na(d_int$wtp_post), 'wtp_post'] <- "" # Make NA's ""
    d_int[is.na(d_int$wtp_post.1), 'wtp_post.1'] <- "" # Make NA's ""
    d_int$wtp <- as.numeric(paste0(d_int$wtp_post, d_int$wtp_post.1))
    
    ################ GENDER ################
    print("GENDER")
    gender_believed <- as.numeric(na.omit(d_merged[(d_merged$agent_cond == 'human') & (d_merged$belief == 1), 'gender']))
    print(paste0("Percentage of females in participants who had AI experience for subset who believed: ", 100*(sum(gender_believed == 2) / length(gender_believed))))

    gender_not <- as.numeric(na.omit(d_merged[(d_merged$agent_cond == 'human') & (d_merged$belief == 2), 'gender']))
    print(paste0("Percentage of females for subset who did not believe: ", 100*(sum(gender_not == 2) / length(gender_not))))
    
    print(prop.test(c(sum(gender_believed == 2), sum(gender_not == 2)), c(length(gender_believed), length(gender_not))))


    #################### EXCLUDE ONES WHO DID NOT BELIEVE ##################
    d_merged <- d_merged[(d_merged$belief == 1) |
                         (d_merged$belief == 0) |
                         (is.na(d_merged$belief)),]

    # Print n_participants in all conditions
    print("Number of participants in each condition:")
    print(table(d_merged$agent_cond, d_merged$interact_cond))


    # Compare human v. chatbot demographics
    d_merged_ai <- d_merged[d_merged$interact_cond == 'yes' & d_merged$agent_cond == 'ai',]
    d_merged_human <- d_merged[d_merged$interact_cond == 'yes' & d_merged$agent_cond == 'human',]
    
    print("*-*-*-*-* AGE *-*-*-*-*")
    vart <- var.test(as.numeric(na.omit(d_merged_ai[((as.numeric(d_merged_ai$age) < 100)), 'age'])), as.numeric(na.omit(d_merged_human[((as.numeric(d_merged_human$age) < 100)), 'age'])))
    print(t.test((as.numeric(na.omit(d_merged_ai[((as.numeric(d_merged_ai$age) < 100)), 'age']))), as.numeric(na.omit(d_merged_human[((as.numeric(d_merged_human$age) < 100)), 'age'])), var.equal = vart$p.value > 0.05))
    print(cohen.d((as.numeric(na.omit(d_merged_ai[((as.numeric(d_merged_ai$age) < 100)), 'age']))), as.numeric(na.omit(d_merged_human[((as.numeric(d_merged_human$age) < 100)), 'age']))))

    print("*-*-*-*-* EDU *-*-*-*-*")
    vart <- var.test(as.numeric(na.omit(d_merged_ai[, 'edu'])), as.numeric(na.omit(d_merged_human[, 'edu'])))
    print(t.test(as.numeric(na.omit(d_merged_ai[, 'edu'])), as.numeric(na.omit(d_merged_human[, 'edu'])), var.equal = vart$p.value > 0.05))
    print(cohen.d(as.numeric(na.omit(d_merged_ai[, 'edu'])), as.numeric(na.omit(d_merged_human[, 'edu']))))
    
    print("*-*-*-*-* AI EXP *-*-*-*-*")
    ai_exp_human <- as.numeric(na.omit(d_merged_ai[, 'ai_companion_exp']))
    print(paste0("Percentage of participants who had AI experience in AI condition: ", 100 * (sum(ai_exp_human == 1) / length(ai_exp_human))))

    ai_exp_ai <- as.numeric(na.omit(d_merged_human[, 'ai_companion_exp']))
    print(paste0("Percentage of participants who had AI experience in Human condition: ", 100 * (sum(ai_exp_ai == 1) / length(ai_exp_ai))))

    print(prop.test(c(sum(ai_exp_human == 1), sum(ai_exp_ai == 1)), c(length(ai_exp_human), length(ai_exp_ai))))
    
    print("*-*-*-*-* GENDER *-*-*-*-*")
    
    gender_ai <- as.numeric(na.omit(d_merged_ai[, 'gender']))
    print(paste0("Percentage of females in AI condition: ", 100*(sum(gender_ai == 2) / length(gender_ai))))
    
    gender_human <- as.numeric(na.omit(d_merged_human[, 'gender']))
    print(paste0("Percentage of females in Human condition: ", 100*(sum(gender_human == 2) / length(gender_human))))
    
    print(prop.test(c(sum(gender_ai == 2), sum(gender_human == 2)), c(length(gender_ai), length(gender_human))))


    write.csv(d_merged, "./data/clean_combined_data.csv")
    return(d_merged)
}

######################################################## MAIN ########################################################

if(!file.exists("./data/clean_combined_data.csv")) { data_setup() }
if (file.exists("./data/clean_detailed_int_data.csv")) {
    d_int <- read.csv("./data/clean_detailed_int_data.csv")
    d_int$Unnamed..0 <- NULL

    # Merge with no interaction
    d_int_e <- read.csv("./data/clean_combined_data.csv")
    d_int_e <- d_int_e[d_int_e$interact_cond == "no",]
    d_int_e[colnames(d_int)[!(colnames(d_int) %in% colnames(d_int_e))]] <- NA
    d_merged <- rbind(d_int, d_int_e)
} else {
    if(!file.exists("./data/clean_combined_data.csv")) {
        data_setup()
    }

    print("You do not have the required file")
    stop()
}

# Function below sets up the data and prints the demographics and demographics comparisons between conditions
data_setup()

loneliness <- mutate_all(d_merged[, c('loneliness_1_1', 'loneliness_2_1', 'loneliness_3_1')], function(x) as.numeric(as.character(x)))
print(paste0("Loneliness cronbach's alpha: ", cronbach.alpha(loneliness)[[1]]))

d_merged$loneliness_scale <- rowMeans(loneliness)

# SAVE TURING TEST DATA
write.csv(d_merged[d_merged$belief == 2 & d_merged$explain != "", c('worker_id', 'explain')], './data/turing_explain.csv')

d_merged$belief[d_merged$belief == 2] <- 1

d_merged <- d_merged[(d_merged$belief == 1) |
                         (d_merged$belief == 0) |
                         (is.na(d_merged$belief)),]

table(d_merged[, c('agent_cond', 'interact_cond')])

## ================================================================================================================
##                                            PARTICIPANT CHARACTERISTICS
## ================================================================================================================

## age
mean(as.numeric(d_merged[d_merged$age < 100,]$age), trim = 0, na.rm = TRUE) ## mean age

## gender
table(d_merged$gender)[1] / sum(table(d_merged$gender)) ## percentage of males
table(d_merged$gender)[2] / sum(table(d_merged$gender)) ## percentage of females

sum(as.numeric(d_merged$ai_capability_1)) / dim(d_merged)[1] ## percentage of ai capability

## ai experience
table(d_merged$ai_companion_exp)[1] / sum(table(d_merged$ai_companion_exp)) ## percentage of yes
table(d_merged$ai_companion_exp)[2] / sum(table(d_merged$ai_companion_exp)) ## percentage of no

## ai capability
mean(as.numeric(d_merged$ai_capability), trim = 0, na.rm = TRUE) ## mean ai capability

## ================================================================================================================
##                                             DATA ANALYSIS - REGRESSIONS
## ================================================================================================================

print("Descriptive analysis")
for(dv in c('willing_friend', 'willing_romantic', 'wtp_logged')) {
  print(paste0("*-*-* ", dv, "*-*-*"))
  print(paste0("Human condition: ", mean(d_merged[d_merged$agent_cond == 'human', dv])))
  print(paste0("AI condition: ", mean(d_merged[d_merged$agent_cond == 'ai', dv])))
  
  print(paste0("Interaction present: ", mean(d_merged[d_merged$interact_cond == 'yes', dv])))
  print(paste0("Interaction absent: ", mean(d_merged[d_merged$interact_cond == 'no', dv])))
  
  print(paste0("Human Interaction present: ", mean(d_merged[d_merged$interact_cond == 'yes' & d_merged$agent_cond == "human", dv])))
  print(paste0("Human Interaction absent: ", mean(d_merged[d_merged$interact_cond == 'no' & d_merged$agent_cond == "human", dv])))
  
  print(paste0("AI Interaction present: ", mean(d_merged[d_merged$interact_cond == 'yes' & d_merged$agent_cond == "ai", dv])))
  print(paste0("AI Interaction absent: ", mean(d_merged[d_merged$interact_cond == 'no' & d_merged$agent_cond == "ai", dv])))
}

summary(lm(willing_friend ~ as.factor(ai_companion_exp), data = d_merged))
summary(lm(willing_romantic ~ as.factor(ai_companion_exp), data = d_merged))
summary(lm(wtp_logged ~ as.factor(ai_companion_exp), data = d_merged))

t.test(x = d_merged$willing_friend[d_merged$interact_cond == "yes" & d_merged$agent_cond == "ai"], mu = 50)
cohensD(d_merged$willing_friend[d_merged$interact_cond == "yes" & d_merged$agent_cond == "ai"], mu = 50)


## (1) FRIEND --------
## anova
friend_mod <- aov(willing_friend ~ as.factor(agent_cond) * as.factor(interact_cond), data = d_merged)
summary(friend_mod)
anova_stats(friend_mod)

print("Participants were more willing to befriend a companion when provided a trial than when not in human cond")
vart <- var.test(d_merged$willing_friend[d_merged$interact_cond == 'yes' & d_merged$agent_cond == 'human'],
                 d_merged$willing_friend[d_merged$interact_cond == 'no' & d_merged$agent_cond == 'human'])
tt <- t.test(d_merged$willing_friend[d_merged$interact_cond == 'yes'& d_merged$agent_cond == 'human'],
             d_merged$willing_friend[d_merged$interact_cond == 'no'& d_merged$agent_cond == 'human'], paired = FALSE, var.equal = vart$p.value > 0.05)
print(tt)
print(cohen.d(d_merged$willing_friend[(d_merged$interact_cond == 'yes') & (d_merged$agent_cond == 'human')],
             d_merged$willing_friend[(d_merged$interact_cond == 'no') & (d_merged$agent_cond == 'human')]))


print("Participants were more willing to befriend a companion when provided a trial than when not in AI cond")
vart <- var.test(d_merged$willing_friend[d_merged$interact_cond == 'yes' & d_merged$agent_cond == 'ai'],
                 d_merged$willing_friend[d_merged$interact_cond == 'no' & d_merged$agent_cond == 'ai'])
tt <- t.test(d_merged$willing_friend[d_merged$interact_cond == 'yes' & d_merged$agent_cond == 'ai'],
             d_merged$willing_friend[d_merged$interact_cond == 'no' & d_merged$agent_cond == 'ai'], paired = FALSE, var.equal = vart$p.value > 0.05)
print(tt)
print(cohen.d(d_merged$willing_friend[d_merged$interact_cond == 'yes'& d_merged$agent_cond == 'ai'],
             d_merged$willing_friend[d_merged$interact_cond == 'no'& d_merged$agent_cond == 'ai']))

# Replicate when controlling for AI experience
friend_mod_aiexp <- aov(willing_friend ~ as.factor(ai_companion_exp) * as.factor(agent_cond) * as.factor(interact_cond), data = d_merged)
summary(friend_mod_aiexp)
anova_stats(friend_mod_aiexp)

print("Participants were more willing to befriend a companion when experienced vs not")
vart <- var.test(d_merged$willing_friend[d_merged$ai_companion_exp == '1'],
                 d_merged$willing_friend[d_merged$ai_companion_exp == '2'])
tt <- t.test(d_merged$willing_friend[d_merged$ai_companion_exp == '1'],
             d_merged$willing_friend[d_merged$ai_companion_exp == '2'], paired = FALSE, var.equal = vart$p.value > 0.05)
print(tt)
print(cohen.d(d_merged$willing_friend[d_merged$ai_companion_exp == '1'],
              d_merged$willing_friend[d_merged$ai_companion_exp == '2']))



## (2) PARTNER ---------
## get summary statistics
print("PARTNER")

## anova
romantic_mod <- aov(willing_romantic ~ agent_cond * interact_cond, data = d_merged)
summary(romantic_mod)
anova_stats(friend_mod)

# Participants were more willing to romance when provided a trial than when not in both cond
vart <- var.test(d_merged$willing_romantic[d_merged$interact_cond == 'yes' & d_merged$agent_cond == 'human'],
                 d_merged$willing_romantic[d_merged$interact_cond == 'no' & d_merged$agent_cond == 'human'])
tt <- t.test(d_merged$willing_romantic[d_merged$interact_cond == 'yes'& d_merged$agent_cond == 'human'],
             d_merged$willing_romantic[d_merged$interact_cond == 'no'& d_merged$agent_cond == 'human'],
             paired = FALSE, var.equal = vart$p.value > 0.05)
print(tt)

print(cohen.d(d_merged$willing_romantic[d_merged$interact_cond == 'yes'& d_merged$agent_cond == 'human'],
             d_merged$willing_romantic[d_merged$interact_cond == 'no'& d_merged$agent_cond == 'human'], paired = FALSE, var.equal = vart$p.value > 0.05))


vart <- var.test(d_merged$willing_romantic[d_merged$interact_cond == 'yes' & d_merged$agent_cond == 'ai'],
                 d_merged$willing_romantic[d_merged$interact_cond == 'no' & d_merged$agent_cond == 'ai'])
tt <- t.test(d_merged$willing_romantic[d_merged$interact_cond == 'yes'& d_merged$agent_cond == 'ai'],
             d_merged$willing_romantic[d_merged$interact_cond == 'no'& d_merged$agent_cond == 'ai'],
             paired = FALSE, var.equal = vart$p.value > 0.05)
print(tt)
print(cohen.d(d_merged$willing_romantic[d_merged$interact_cond == 'yes'& d_merged$agent_cond == 'ai'],
             d_merged$willing_romantic[d_merged$interact_cond == 'no'& d_merged$agent_cond == 'ai'],
             paired = FALSE, var.equal = vart$p.value > 0.05))

# Replicate when controlling for AI experience
romantic_mod_aiexp <- aov(willing_romantic ~ as.factor(ai_companion_exp) * as.factor(agent_cond) * as.factor(interact_cond), data = d_merged)
summary(romantic_mod_aiexp)
anova_stats(romantic_mod_aiexp)

print("Participants were more willing to befriend a companion when experienced vs not")
vart <- var.test(d_merged$willing_romantic[d_merged$ai_companion_exp == '1'],
                 d_merged$willing_romantic[d_merged$ai_companion_exp == '2'])
tt <- t.test(d_merged$willing_romantic[d_merged$ai_companion_exp == '1'],
             d_merged$willing_romantic[d_merged$ai_companion_exp == '2'], paired = FALSE, var.equal = vart$p.value > 0.05)
print(tt)
print(cohen.d(d_merged$willing_romantic[d_merged$ai_companion_exp == '1'],
              d_merged$willing_romantic[d_merged$ai_companion_exp == '2']))


## (3) WTP ---------
print("WTP")
wtp_mod <- aov(wtp_logged ~ agent_cond * interact_cond, data = d_merged)
print(summary(wtp_mod))
print(anova_stats(friend_mod))

# Replicate when controlling for AI experience
wtp_logged_aiexp <- aov(wtp_logged ~ as.factor(ai_companion_exp) * as.factor(agent_cond) * as.factor(interact_cond), data = d_merged)
summary(wtp_logged_aiexp)
anova_stats(wtp_logged_aiexp)

print("Participants were more willing to befriend a companion when experienced vs not")
vart <- var.test(d_merged$wtp_logged[d_merged$ai_companion_exp == '1'],
                 d_merged$wtp_logged[d_merged$ai_companion_exp == '2'])
tt <- t.test(d_merged$wtp_logged[d_merged$ai_companion_exp == '1'],
             d_merged$wtp_logged[d_merged$ai_companion_exp == '2'], paired = FALSE, var.equal = vart$p.value > 0.05)
print(tt)
print(cohen.d(d_merged$wtp_logged[d_merged$ai_companion_exp == '1'],
              d_merged$wtp_logged[d_merged$ai_companion_exp == '2']))


## ================================================================================================================
##                                              PLOTTING MAIN FIGURES: EFFECT OF AGENT COND
## ================================================================================================================

## plotting all measures
a_names <- c("Chatbot", "Chatbot Acting\nLike Human")
interaction_conds <- c('Absent', 'Present')


fname <- "fig_believed"

## (1) FRIEND
p1 <- ggplot(d_merged, aes(x = factor(agent_cond), y = willing_friend, fill = factor(interact_cond)), color = factor(interact_cond)) +
    theme_bw() +
    coord_cartesian(ylim = c(1, 110)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 5))

p1 <- p1 +
    theme(text = element_text(size = 18), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    scale_x_discrete(labels = a_names) +
    ggtitle("Willingness to\nFind a Friend") +
    scale_fill_manual(values = c("#cccccc", "#666666"), name = "Trial:",
                      labels = interaction_conds, guide = guide_legend(reverse = FALSE)) +
    xlab("") +
    ylab("") +
    theme_classic() +
    theme(axis.text.x = element_text(size = 16)) +
    theme(axis.text.y = element_text(size = 16)) +
    theme(plot.title = element_text(size = 18, hjust = 0.5)) +
    theme(legend.text = element_text(size = 16), legend.title = element_text(size = 18)) +
    geom_bar(position="dodge", stat="summary", width = 0.9, alpha = 1, size = 0.75) +
    stat_summary(fun.data = "mean_cl_boot", color = "black",
                 size = 0.4, 
                 position = position_dodge(width = 0.9)) +
    stat_summary(fun.data = "mean_cl_boot", color = "black",
                 
                 position = position_dodge(width = 0.9),
                 geom = "errorbar", width = 0.2)

## (2) PARTNER
p2 <- ggplot(d_merged, aes(x = factor(agent_cond), y = willing_romantic, fill = factor(interact_cond)), color = factor(interact_cond)) +
    theme_bw() +
    coord_cartesian(ylim = c(1, 110)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 5))

p2 <- p2 +
    theme(text = element_text(size = 16), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    scale_x_discrete(labels = a_names) +
    ggtitle("Willingness to\nFind Romantic Partner") +
    scale_fill_manual(values = c("#cccccc", "#666666"), name = "Trial:",
                      labels = interaction_conds, guide = guide_legend(reverse = FALSE)) +
    xlab("") +
    ylab("") +
    theme_classic() +
    theme(axis.text.x = element_text(size = 16)) +
    theme(axis.text.y = element_text(size = 16)) +
    theme(plot.title = element_text(size = 18, hjust = 0.5)) +
    theme(legend.text = element_text(size = 16), legend.title = element_text(size = 18)) +
    geom_bar(position="dodge", stat="summary", width = 0.9, alpha = 1, size = 0.75) +
    stat_summary(fun.data = "mean_cl_boot", color = "black",
                 size = 0.4, 
                 position = position_dodge(width = 0.9)) +
    stat_summary(fun.data = "mean_cl_boot", color = "black",
                 
                 position = position_dodge(width = 0.9),
                 geom = "errorbar", width = 0.2)

## (3) PAYMENT
p3 <- ggplot(d_merged, aes(x = factor(agent_cond), y = wtp_logged, fill = factor(interact_cond)), color = factor(interact_cond)) +
    theme_bw() +
    coord_cartesian(ylim = c(0, 6))

p3 <- p3 +
    theme(text = element_text(size = 16), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    scale_x_discrete(labels = a_names) +
    ggtitle("Willingness to Pay") +
    scale_fill_manual(values = c("#cccccc", "#666666"), name = "Trial:",
                      labels = interaction_conds, guide = guide_legend(reverse = FALSE)) +
    xlab("") +
    ylab("") +
    theme_classic() +
    theme(axis.text.x = element_text(size = 16)) +
    theme(axis.text.y = element_text(size = 16)) +
    theme(plot.title = element_text(size = 18, hjust = 0.5)) +
    theme(legend.text = element_text(size = 16), legend.title = element_text(size = 18)) +
    geom_bar(position="dodge", stat="summary", width = 0.9, alpha = 1, size = 0.75) +
    stat_summary(fun.data = "mean_cl_boot", color = "black",
                 size = 0.4, 
                 position = position_dodge(width = 0.9)) +
    stat_summary(fun.data = "mean_cl_boot", color = c("black"),
                 
                 position = position_dodge(width = 0.9),
                 geom = "errorbar", width = 0.2)

dev.new(width = 13, height = 5, noRStudioGD = TRUE)

## (4) ALL FIGURES

figure <- ggarrange(p1, p2, p3, nrow = 1, ncol = 3, common.legend = TRUE, legend = "top", vjust = 1.0, hjust = 0.5)
annotate_figure(figure, left = text_grob("Mean", color = "black", face = "plain", size = 26, rot = 90),
                bottom = text_grob("Agent Condition", color = "black", face = "plain", size = 26, margin(b = 2), hjust = 0.33))

ggsave(paste0(fname, ".pdf"), last_plot(), dpi = 500)

################### CONVERSATIONAL ANALYSES ###################

library(effsize)

d_merged_int <- subset(d_merged, d_merged$interact_cond == 'yes')
d_merged_int_human <- subset(d_merged_int, d_merged_int$agent_cond == 'human')
d_merged_int_ai <- subset(d_merged_int, d_merged_int$agent_cond == 'ai')

##### t-tests
for(iv in c('HumanMessageCount', 'AvgHumanWordCount', 'AvgAIWordCount','AvgResponseTime',
            'AvgHumanCompoundSentiment', 'AvgSimSpacy', 'AvgSimTurnLength', 'AvgSimValence')) {
    print(paste(".......................................", iv, "......................................"))
    vart <- var.test(d_merged_int_human[,iv], d_merged_int_ai[,iv])
    tt <- t.test(d_merged_int_human[,iv], d_merged_int_ai[,iv], paired = FALSE, var.equal = vart$p.value > 0.05)
    print(tt)

    print(cohen.d(d_merged_int_human[,iv], d_merged_int_ai[,iv], paired = FALSE, var.equal = vart$p.value > 0.05))
}

##### Lasso Regression #####
ivs <- c('AvgHumanCompoundSentiment',
                'AvgSimValence',
                'AvgHumanWordCount',
                'AvgSimTurnLength',
                'AvgResponseTime',
                'HumanMessageCount',
                'AvgSimSpacy', 'agent_cond_n')

# Standardize the vars
d_merged_standardized <- d_merged_int %>% mutate_at(c('willing_friend', 'willing_romantic', 'wtp_logged', ivs), ~(scale(.) %>% as.vector))
d_merged_standardized$agent_cond_n <- as.numeric(as.factor(d_merged_int$agent_cond))

for(dv in c('willing_friend')) { #, 'willing_romantic', 'wtp_logged'
  set.seed(123)
  print(paste("............................................", dv, "............................................"))
  
  #define matrix of predictor variables
  x <- data.matrix(d_merged_standardized[, c(ivs)])
  y <- d_merged_standardized[, dv]
  
  #fit Lasso regression model
  model <- glmnet(x, y, alpha = 1)
  
  #view summary of model
  print(summary(model))
  
  #perform k-fold cross-validation to find optimal lambda value
  cv_model <- cv.glmnet(x, y, nfolds = 10)
  
  #find optimal lambda value that minimizes test MSE
  best_lambda <- cv_model$lambda.min
  print(paste0("Best Lambda: ", best_lambda))
  
  #produce plot of test MSE by lambda value
  plot(cv_model) 
  
  ## fit the model with best lambda
  best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
  print("Model Coefficients:")
  print(coef(best_model))
  
  plot(model, xvar = "lambda")
  
  #use fitted best model to make predictions
  y_predicted <- predict(model, s = best_lambda, newx = x)
  
  #find SST and SSE, and R-squared
  sst <- sum((y - mean(y))^2)
  sse <- sum((y_predicted - y)^2)
  print(paste("R-squared: ", 1 - sse/sst))
  
  if(FALSE) {
    ###### Perform bootstrap to estimate the stability of the coefficients:
    ridge.coef <- function(data, indices) {
      x <- data[indices, -1]  # predictor variables
      y <- data[indices, 1]   # response variable
      cv.ridge <- cv.glmnet(x, y, alpha = 1, nfolds = 10)
      fit.ridge <- glmnet(x, y, alpha = 1, lambda = cv.ridge$lambda.min)
      coef_matrix <- coef(fit.ridge)
      full_coef_matrix <- matrix(0, nrow = length(coef_matrix), ncol = 1)
      full_coef_matrix[seq_along(coef_matrix)] <- coef_matrix
      return(full_coef_matrix)
    }
    
    data_boot <- cbind(y, x)
    
    # Perform bootstrap with 1000 iterations
    boot_results <- boot(data_boot, ridge.coef, R = 1000)
    
    # Calculate standard deviations for each coefficient
    coef_std <- apply(boot_results$t, 2, sd)
    
    # Calculate z-scores
    z_scores <- boot_results$t0 / coef_std
    
    # Calculate p-values
    p_values <- 2 * (1 - pnorm(abs(z_scores)))
    
    # Display p-values for each coefficient
    cat("P-values for coefficients:\n")
    for (i in 1:length(p_values)) {
      cat("Coefficient", i, ":", p_values[i], "\n")
    }
  }
}

summary(lm(as.formula('willing_friend ~ AvgHumanCompoundSentiment + AvgSimValence + AvgHumanWordCount + agent_cond_n'), data = d_merged_standardized))
summary(lm(as.formula('willing_romantic ~ AvgSimValence + AvgHumanWordCount + AvgResponseTime + agent_cond_n'), data = d_merged_standardized))
summary(lm(as.formula('wtp_logged ~ AvgHumanCompoundSentiment + AvgHumanWordCount + AvgResponseTime + agent_cond_n'), data = d_merged_standardized))

## ================================================================================================================
##                        PLOTS FOR SENTIMENT, WORD COUNT, NESSAGE COUNT, AVG RESPONSE TIME
## ================================================================================================================

a_names <- c("Chatbot", "Chatbot Acting\nLike Human")
interaction_conds <- c('Absent', 'Present')
d_merged_int <- d_merged[d_merged$interact_cond == "yes",]

d_merged_int_human <- d_merged_int[d_merged_int$ageent_cond == "human",]
d_merged_int_ai <- d_merged_int[d_merged_int$ageent_cond == "ai",]

## Human Word Amt
p1 <- ggplot(d_merged_int, aes(x = factor(agent_cond), y = AvgHumanWordCount))
p1 <- p1 +
    theme(text = element_text(size = 18), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    scale_x_discrete(labels = a_names) +
    ggtitle("Human Word Amount\nPer Message") +
    xlab("") +
    ylab("") +
    theme_classic() +
    theme(axis.text.x = element_text(size = 18)) +
    theme(axis.text.y = element_text(size = 18)) +
    theme(plot.title = element_text(size = 22, hjust = 0.5)) +
    theme(legend.text = element_text(size = 18), legend.title = element_text(size = 18)) +
    geom_bar(position="dodge", stat="summary", width = 0.9, alpha = 0.38, size = 0.75) +
    stat_summary(fun.data = "mean_cl_boot", color = "black",
                 size = 0.4, 
                 position = position_dodge(width = 0.9)) +
    stat_summary(fun.data = "mean_cl_boot", color = "black",
                 
                 position = position_dodge(width = 0.9),
                 geom = "errorbar", width = 0.2) +
    geom_signif(y_position = 40, comparisons = list(c("ai", "human")), map_signif_level = TRUE, textsize = 3.5, test = "t.test", tip_length = 0.05)


# Human Sentiment
p2 <- ggplot(d_merged_int, aes(x = factor(agent_cond), y = AvgHumanCompoundSentiment))
p2 <- p2 +
    theme(text = element_text(size = 18), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    scale_x_discrete(labels = a_names) +
    ggtitle("Human Sentiment \n(Compound)") +
    xlab("") +
    ylab("") +
    theme_classic() +
    theme(axis.text.x = element_text(size = 18)) +
    theme(axis.text.y = element_text(size = 18)) +
    theme(plot.title = element_text(size = 22, hjust = 0.5)) +
    theme(legend.text = element_text(size = 18), legend.title = element_text(size = 18)) +
    geom_bar(position="dodge", stat="summary", width = 0.9, alpha = 0.38, size = 0.75) +
    stat_summary(fun.data = "mean_cl_boot", color = "black",
                 size = 0.4, 
                 position = position_dodge(width = 0.9)) +
    stat_summary(fun.data = "mean_cl_boot", color = "black",
                 
                 position = position_dodge(width = 0.9),
                 geom = "errorbar", width = 0.2) +
    geom_signif(y_position = 1, comparisons = list(c("ai", "human")), map_signif_level = TRUE, textsize = 3.5, test = "t.test", tip_length = 0.02)

# Human Message Count
p3 <- ggplot(d_merged_int, aes(x = factor(agent_cond), y = HumanMessageCount))
p3 <- p3 +
    theme(text = element_text(size = 18), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    scale_x_discrete(labels = a_names) +
    ggtitle("Human Message Count") +
    xlab("") +
    ylab("") +
    theme_classic() +
    theme(axis.text.x = element_text(size = 18)) +
    theme(axis.text.y = element_text(size = 18)) +
    theme(plot.title = element_text(size = 22, hjust = 0.5)) +
    theme(legend.text = element_text(size = 18), legend.title = element_text(size = 18)) +
    geom_bar(position="dodge", stat="summary", width = 0.9, alpha = 0.38, size = 0.75) +
    stat_summary(fun.data = "mean_cl_boot", color = "black",
                 size = 0.4, 
                 position = position_dodge(width = 0.9)) +
    stat_summary(fun.data = "mean_cl_boot", color = "black",
                 
                 position = position_dodge(width = 0.9),
                 geom = "errorbar", width = 0.2) +
    geom_signif(y_position = 47, comparisons = list(c("ai", "human")), map_signif_level = TRUE, textsize = 3.5, test = "t.test", tip_length = 0.03)

# Response Time
p4 <- ggplot(d_merged_int, aes(x = factor(agent_cond), y = AvgResponseTime))
p4 <- p4 +
    theme(text = element_text(size = 18), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    scale_x_discrete(labels = a_names) +
    ggtitle("Response Time") +
    xlab("") +
    ylab("") +
    theme_classic() +
    theme(axis.text.x = element_text(size = 18)) +
    theme(axis.text.y = element_text(size = 18)) +
    theme(plot.title = element_text(size = 22, hjust = 0.5)) +
    theme(legend.text = element_text(size = 18), legend.title = element_text(size = 18)) +
    geom_bar(position="dodge", stat="summary", width = 0.9, alpha = 0.38, size = 0.75) +
    stat_summary(fun.data = "mean_cl_boot", color = "black",
                 size = 0.4, 
                 position = position_dodge(width = 0.9)) +
    stat_summary(fun.data = "mean_cl_boot", color = "black",
                 
                 position = position_dodge(width = 0.9),
                 geom = "errorbar", width = 0.2) +
    geom_signif(y_position = 135, comparisons = list(c("ai", "human")), map_signif_level = TRUE, textsize = 3.5, test = "t.test", tip_length = 0.03)

## (4) ALL FIGURES
dev.new(width = 13, height = 7, noRStudioGD = TRUE)


# Perhaps add similarities here in two conditions::::

figure <- ggarrange(p1, p4, p2, p3, nrow = 1, ncol = 4, common.legend = TRUE, legend = "top", vjust = 1.0, hjust = 1.1)
annotate_figure(figure, left = text_grob("Mean", color = "black", face = "plain", size = 26, rot = 90),
                bottom = text_grob("Agent Condition", color = "black", face = "plain", size = 26, margin(b = 2)))

## ================================================================================================================
##                                      DATA ANALYSIS - MEDIATION USING 'PROCESS'
## ================================================================================================================

d_merged$int <- as.numeric(d_merged$agent_cond_n) * d_merged$interact_cond_n

## (1) FRIEND

## With mediators: true friendship, reliance, judgment
process(data = d_merged, y = "willing_friend", x = "int",
        m =c("true", "talk_listen", "not_judge"), model = 4, effsize =1, total =1, stand =1,
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)

#process(data = d_merged, y = "willing_friend", x = "int",
#        m =c("true", "talk_listen", "not_judge"), w = c("loneliness_scale"), model = 14, effsize =1, total =1, stand =1,
#        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)

d_merged$agent_cond_n <- ifelse(d_merged$agent_cond == 'human', 0, 1)
d_merged$interact_cond_n <- ifelse(d_merged$interact_cond == "yes", 1, 0)

## (2) PARTNER
## With mediators: true friendship, reliance, judgment
process(data = d_merged, y = "willing_romantic", x = "int",
        m =c("true", "talk_listen", "not_judge"), model = 4, effsize =1, total =1, stand =1,
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)

#process(data = d_merged, y = "willing_romantic", x = "int",
#        m =c("true", "talk_listen", "not_judge"), w = c("loneliness_scale"), model = 14, effsize =1, total =1, stand =1,
#        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)


######################## CONVERSATION QUALITY ANALYSES ########################
# Check reliability
k <- read.csv('./data/rate_2.csv', head = TRUE)
k <- k[, c('worker_id', 'agent_cond', 'inquisitiveness', 'listening', 'avoiding_repetition', 'fluency', 'making_sense', 'belief')]

z <- read.csv('./data/rate_1.csv', head = TRUE)
z <- z[, c('worker_id', 'agent_cond', 'inquisitiveness', 'listening', 'avoiding_repetition', 'fluency', 'making_sense', 'belief')]

k_ai <- read.csv('./data/rate_2_ai.csv', head = TRUE)
k_ai$agent_cond <- "ai"
k_ai <- k_ai[, c('worker_id', 'agent_cond', 'inquisitiveness', 'listening', 'avoiding_repetition', 'fluency', 'making_sense')]
k_ai$belief <- NA

k_combined <- rbind(k, k_ai)

z_ai <- read.csv('./data/rate_1_ai.csv', head = TRUE)
z_ai$agent_cond <- "ai"
z_ai <- z_ai[, c('worker_id', 'agent_cond', 'inquisitiveness', 'listening', 'avoiding_repetition', 'fluency', 'making_sense')]
z_ai$belief <- NA

z_combined <- rbind(z, z_ai)

questions <- c('inquisitiveness', 'listening', 'avoiding_repetition', 'fluency', 'making_sense')
for(q in questions) {
    print(paste0("*-*-*-*-*-*-*-*", q, "*-*-*-*-*-*-*-*"))
    df <- data.frame(R1=k_combined[, q], R2=z_combined[, q])
    print(cronbach.alpha(df))

    ##### t-test Between the subset who believed vs. not
    # Take the common ratings of both raters
    common_ratings_1 <- data.frame(cbind(k[(k$belief == 1), q], z[(z$belief == 1), q]))
    common_ratings_1 <- common_ratings_1[common_ratings_1$X1 == common_ratings_1$X2, 'X1']

    common_ratings_2 <- data.frame(cbind(k[(k$belief == 2), q], z[(z$belief == 2), q]))
    common_ratings_2 <- common_ratings_2[common_ratings_2$X1 == common_ratings_2$X2, 'X1']

    #print(shapiro.test(b1))
    #print(shapiro.test(b2))

    #print(paste0("Median of participants who believed they were talking to a human: ", median(b1)))
    #print(paste0("Median of participants who did NOT believe they were talking to a human: ", median(b2)))
    print(paste0("Mean of participants who believed they were talking to a human: ", mean(common_ratings_1)))
    print(paste0("Mean of participants who did NOT believe they were talking to a human: ", mean(common_ratings_2)))

    wt <- wilcox.test(common_ratings_1, common_ratings_2, paired = FALSE)
    print(wt)

    print(paste0("Z value: ", qnorm(wt$p.value / 2)))
    print(cohen.d(common_ratings_1, common_ratings_2))
}

# AI vs. Human condition
for(q in questions) {
    print(paste0("*-*-*-*-*-*-*-*", q, "*-*-*-*-*-*-*-*"))

    ##### t-test Between Human vs. AI
    # Take the common ratings of both raters
    common_ratings_ai <- data.frame(cbind(k_combined[(k_combined$agent_cond == 'ai'), q], z_combined[(z_combined$agent_cond == 'ai'), q]))
    common_ratings_ai <- common_ratings_ai[common_ratings_ai$X1 == common_ratings_ai$X2, 'X1']

    common_ratings_human <- data.frame(cbind(k_combined[(k_combined$agent_cond == 'human') & (k_combined$belief == 1), q], z_combined[(z_combined$agent_cond == 'human')  & (z_combined$belief == 1), q]))
    common_ratings_human <- common_ratings_human[common_ratings_human$X1 == common_ratings_human$X2, 'X1']

    #print(paste0("Median of participants who believed they were talking to a human: ", median(b1)))
    #print(paste0("Median of participants who did NOT believe they were talking to a human: ", median(b2)))
    print(paste0("Mean of AI condition: ", mean(common_ratings_ai)))
    print(paste0("Mean of Human Condition: ", mean(common_ratings_human)))

    wt <- wilcox.test(common_ratings_ai, common_ratings_human, paired = FALSE)
    print(wt)

    print(paste0("Z value: ", qnorm(wt$p.value / 2)))
    print(cohen.d(common_ratings_ai, common_ratings_human))
}

## ================================================================================================================
##                                                  END OF ANALYSIS
## ================================================================================================================

