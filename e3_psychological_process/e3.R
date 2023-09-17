## ================================================================================================================
## DATA ANALYSIS | CHATBOT STIGMA STUDY | EXPERIMENT 2            
## ================================================================================================================

## clear workspace
rm(list = ls())

options(download.file.method = "libcurl")

## install packages
library(ggpubr)
library(rstatix)
if (!require(pacman)) { install.packages("pacman") }
pacman::p_load('ggplot2',         # plotting
               'ggsignif',        # plotting significance bars
               'lme4',            # functions for fitting linear regression models
               'ggforce',         # make ggplot even fancier
               'ggpubr',          # arrange plots in a grid, if needed
               'ltm',             # probably not using..
               'tidyr',           # tools for cleaning messy data
               'stringr',         # perform string substitutions easily
               'assertthat',      # allows me to check whether a variable is a string, with is.string
               'lsmeans',         # contrast analysis for regression models
               'stats',           # use function to adjust for multiple comparisons
               'filesstrings',    # create and move files
               'simr',            # power analysis for mixed models
               'compute.es',      # effect size package
               'effsize',         # another effect size package
               'pwr',             # package for power calculation
               'nlme',            # get p values for mixed effect model
               'DescTools'        # get Cramer's V
)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set working directory to current directory
source('process.R')

## ================================================================================================================
##                                                  PRE-PROCESSING                 
## ================================================================================================================

## read in data: 
# if importing from Qualtrics: (i) export data as numeric values, and (ii) delete rows 2 and 3 of the .csv file.
d <- read.csv('data.csv')
d$pide <- NULL
d$X <- NULL
d$X.1 <- NULL

## explore dataframe: 
dim(d) # will provide dimensions of the dataframe by row [1] and column [2]
colnames(d) # will provide all column names
summary(d)
dim(d)[1]

## subjects randomized:
table(d$CONDITION)

## perform attention exclusions: 
# this will remove responses from the dataframe that failed attention checks (i.e., "1" or "2")
d <- subset(d, (d$att1 == 2 & d$att2 == 2))
print(paste0("Number of participants hired: ", dim(d)[[1]]))

## ================================================================================================================
##                                                    SUBSETTING                 
## ================================================================================================================

## define new data frame to extract pre-processed data into:
d_subset <- array(dim = c(dim(d)[1], 14))
colnames(d_subset) <- c('cond', 'willing_friend', 'willing_romantic', 'wtp', 'true', 'talk_listen',
                        'not_judge', 'understand', 'feel', 'body', 'mutual', 'non_member', 'comp_1', 'comp_2')
d_subset <- as.data.frame(d_subset, stringsAsFactors = FALSE)

## extract good data from the middle part of raw data:
for (i in 1:dim(d)[1]) {
    curr <- d[i, c(21:44)][!is.na(d[i, c(21:44)])] # for a given row, get only the non-NA values
    d_subset[i, 1] <- d$CONDITION[i]
    d_subset[i, 2:14] <- as.numeric(curr[curr != ""])
}

## merge good data with first and last halves of raw data:
# this is the new dataframe to work with.
d_merged <- cbind(d_subset, d[, 45:67])
d_merged$ss <- 1:dim(d_merged)[1]

d_merged$cond_name <- ifelse(d_merged$CONDITION == 1, 'ai', 'human')

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
n_original <- dim(d_merged)[1] # extracting number of rows only, not columns
n_original

d_merged$comp1_recode <- ifelse(d_merged$comp_1 == 1, 2, 1)

## perform comprehension exclusions: 
# this will remove responses from the dataframe that failed comprehension checks (i.e., "2")
d_merged <- subset(d_merged, (d_merged$comp1_recode == d_merged$CONDITION & d_merged$comp_2 == 2))
print(paste0("Number of participants after comprehension exclusions: ", dim(d_merged)[[1]]))
names(d_merged)[names(d_merged) == "CONDITION"] <- "cond_n"

## ================================================================================================================
##                                            PARTICIPANT CHARACTERISTICS                 
## ================================================================================================================

## age
mean(as.numeric(d_merged$age), trim = 0, na.rm = TRUE) ## mean age 

## gender
table(d_merged$gender)[2] / sum(table(d_merged$gender)) ## percentage of males
table(d_merged$gender)[3] / sum(table(d_merged$gender)) ## percentage of females

## ai experience 
table(d_merged$ai_companion_exp)[1] / sum(table(d_merged$ai_companion_exp)) ## percentage of yes
table(d_merged$ai_companion_exp)[2] / sum(table(d_merged$ai_companion_exp)) ## percentage of no

15 / sum(table(d_merged$ai_companion_exp)) ## percentage of actual yes (see d$ai_companion_exp2)

## ai capability 
mean(as.numeric(d_merged$ai_capability), trim = 0, na.rm = TRUE) ## mean ai capability

## ================================================================================================================
##                                              DATA ANALYSIS - MEASURES                
## ================================================================================================================

## (1) LONELINESS
d_merged$loneliness_1_1 <- as.numeric(d_merged$loneliness_1_1)
d_merged$loneliness_2_1 <- as.numeric(d_merged$loneliness_2_1)
d_merged$loneliness_3_1 <- as.numeric(d_merged$loneliness_3_1)

## calculate average items if cronbach's alpha > 0.80
cronbach.alpha(d_merged[, c("loneliness_1_1", "loneliness_2_1", "loneliness_3_1")])
d_merged$lonely <- rowMeans(d_merged[, c("loneliness_1_1", "loneliness_2_1", "loneliness_3_1")])
mean(d_merged$lonely, trim = 0, na.rm = TRUE)

# Make all columns containing 'sensation' numeric
d$sensation_1_1 <- as.numeric(d$sensation_1_1)
d$sensation_2_1 <- as.numeric(d$sensation_2_1)
d$sensation_3_1 <- as.numeric(d$sensation_3_1)
d$sensation_4_1 <- as.numeric(d$sensation_4_1)
d$sensation_5_1 <- as.numeric(d$sensation_5_1)
d$sensation_6_1 <- as.numeric(d$sensation_6_1)
d$sensation_7_1 <- as.numeric(d$sensation_7_1)
d$sensation_8_1 <- as.numeric(d$sensation_8_1)



## (2) SENSATION 
cronbach.alpha(d[, c("sensation_1_1", "sensation_2_1", "sensation_3_1",
                     "sensation_4_1", "sensation_5_1", "sensation_6_1",
                     "sensation_7_1", "sensation_8_1")])

## average items if cronbach's alpha > 0.80
d$sensation <- rowMeans(d[, c("sensation_1_1", "sensation_2_1", "sensation_3_1",
                              "sensation_4_1", "sensation_5_1", "sensation_6_1",
                              "sensation_7_1", "sensation_8_1")])

d_raw <- d

## ================================================================================================================
##                                             DATA ANALYSIS - REGRESSIONS               
## ================================================================================================================

## (1) FRIEND --------
## get summary statistics
d_merged %>%
    group_by(cond) %>%
    get_summary_stats(willing_friend, type = "mean_sd")

mean(d_merged$willing_friend[d_merged$cond == 1])
mean(d_merged$willing_friend[d_merged$cond == 2])

mean(d_merged$willing_romantic[d_merged$cond == 1])
mean(d_merged$willing_romantic[d_merged$cond == 2])

mean(d_merged$wtp_logged[d_merged$cond == 1])
mean(d_merged$wtp_logged[d_merged$cond == 2])

vart <- var.test(d_merged$willing_friend[d_merged$cond == 1], d_merged$willing_friend[d_merged$cond == 2])
friend_mod <- t.test(willing_friend ~ cond_n, paired = FALSE, var.equal = vart$p.value > .05, data = d_merged)
friend_mod
d_merged %>% cohens_d(willing_friend ~ cond, paired = FALSE, var.equal = vart$p.value > .05)

## (2) PARTNER ---------
## get summary statistics
d_merged %>%
    group_by(cond) %>%
    get_summary_stats(willing_romantic, type = "mean_sd")

vart <- var.test(d_merged$willing_romantic[d_merged$cond == 1], d_merged$willing_romantic[d_merged$cond == 2])
romantic_mod <- t.test(willing_romantic ~ cond_n, paired = FALSE, var.equal = vart$p.value > .05, data = d_merged)
romantic_mod
d_merged %>% cohens_d(willing_romantic ~ cond, paired = FALSE, var.equal = vart$p.value > .05)


## (3) PAYMENT ---------
## get summary statistics
d_merged %>%
    group_by(cond) %>%
    get_summary_stats(wtp_logged, type = "mean_sd")

vart <- var.test(d_merged$wtp_logged[d_merged$cond == 1], d_merged$wtp_logged[d_merged$cond == 2])
wtp_mod <- t.test(wtp_logged ~ cond_n, paired = FALSE, var.equal = vart$p.value > .05, data = d_merged)
wtp_mod
d_merged %>% cohens_d(wtp_logged ~ cond, paired = FALSE, var.equal = vart$p.value > .05)

## (4) NON-MEMBER -------
d_merged %>%
    group_by(cond) %>%
    get_summary_stats(non_member, type = "mean_sd")

mean(d_merged$non_member[d_merged$cond == 1])
mean(d_merged$non_member[d_merged$cond == 2])
vart <- var.test(d_merged$non_member[d_merged$cond == 1], d_merged$non_member[d_merged$cond == 2])
nonMember_mod <- t.test(non_member ~ cond_n, paired = FALSE, var.equal = vart$p.value > .05, data = d_merged)
nonMember_mod
d_merged %>% cohens_d(non_member ~ cond, paired = FALSE, var.equal = vart$p.value > .05)

## (4) TRUE
## get summary statistics
d_merged %>%
    group_by(cond) %>%
    get_summary_stats(true, type = "mean_sd")

mean(d_merged$true[d_merged$cond == 1])
mean(d_merged$true[d_merged$cond == 2])

## run two sample t-test
vart <- var.test(d_merged$true[d_merged$cond == 1], d_merged$true[d_merged$cond == 2])
t.test(true ~ cond, data = d_merged, paired = FALSE, var.equal = vart$p.value > .05)
d_merged %>% cohens_d(true ~ cond, paired = FALSE, var.equal = vart$p.value > .05)

## (5) TALK/LISTEN
## get summary statistics
d_merged %>%
    group_by(cond) %>%
    get_summary_stats(talk_listen, type = "mean_sd")

mean(d_merged$talk_listen[d_merged$cond == 1])
mean(d_merged$talk_listen[d_merged$cond == 2])

## run two sample t-test
vart <- var.test(d_merged$talk_listen[d_merged$cond == 1], d_merged$talk_listen[d_merged$cond == 2])
t.test(talk_listen ~ cond, data = d_merged, paired = FALSE, var.equal = vart$p.value > .05)
d_merged %>% cohens_d(talk_listen ~ cond, paired = FALSE, var.equal = vart$p.value > .05)


## (6) NOT MUTUAL
## get summary statistics
d_merged %>%
    group_by(cond) %>%
    get_summary_stats(mutual, type = "mean_sd")

mean(d_merged$mutual[d_merged$cond == 1])
mean(d_merged$mutual[d_merged$cond == 2])
## run two sample t-test
vart <- var.test(d_merged$mutual[d_merged$cond == 1], d_merged$mutual[d_merged$cond == 2])
t.test(mutual ~ cond, data = d_merged, paired = FALSE, var.equal = vart$p.value > .05)
d_merged %>% cohens_d(mutual ~ cond, paired = FALSE, var.equal = vart$p.value > .05)


## (7) NOT JUDGE
## get summary statistics
d_merged %>%
    group_by(cond) %>%
    get_summary_stats(not_judge, type = "mean_sd")

mean(d_merged$not_judge[d_merged$cond == 1])
mean(d_merged$not_judge[d_merged$cond == 2])

## run two sample t-test
vart <- var.test(d_merged$not_judge[d_merged$cond == 1], d_merged$not_judge[d_merged$cond == 2])
t.test(not_judge ~ cond, data = d_merged, paired = FALSE, var.equal = vart$p.value > .05)
d_merged %>% cohens_d(not_judge ~ cond, paired = FALSE, var.equal = vart$p.value > .05)


## (8) UNDERSTAND
## get summary statistics
d_merged %>%
    group_by(cond) %>%
    get_summary_stats(understand, type = "mean_sd")

mean(d_merged$understand[d_merged$cond == 1])
mean(d_merged$understand[d_merged$cond == 2])

## run two sample t-test
vart <- var.test(d_merged$understand[d_merged$cond == 1], d_merged$understand[d_merged$cond == 2])
t.test(understand ~ cond, data = d_merged, paired = FALSE, var.equal = vart$p.value > .05)
d_merged %>% cohens_d(understand ~ cond, paired = FALSE, var.equal = vart$p.value > .05)


## (9) EMOTIONS
## get summary statistics
d_merged %>%
    group_by(cond) %>%
    get_summary_stats(feel, type = "mean_sd")

mean(d_merged$feel[d_merged$cond == 1])
mean(d_merged$feel[d_merged$cond == 2])

## run two sample t-test
vart <- var.test(d_merged$feel[d_merged$cond == 1], d_merged$feel[d_merged$cond == 2])
t.test(feel ~ cond, data = d_merged, paired = FALSE, var.equal = vart$p.value > .05)
d_merged %>% cohens_d(feel ~ cond, paired = FALSE, var.equal = vart$p.value > .05)


## (9) PHYSICAL
## get summary statistics
d_merged %>%
    group_by(cond) %>%
    get_summary_stats(body, type = "mean_sd")

mean(d_merged$body[d_merged$cond == 1])
mean(d_merged$body[d_merged$cond == 2])

## run two sample t-test
vart <- var.test(d_merged$body[d_merged$cond == 1], d_merged$body[d_merged$cond == 2])
t.test(body ~ cond, data = d_merged, paired = FALSE, var.equal = vart$p.value > .05)
d_merged %>% cohens_d(body ~ cond, paired = FALSE, var.equal = vart$p.value > .05)


## ================================================================================================================
##                                              PLOTTING MAIN FIGURES                 
## ================================================================================================================
## plotting all measures

dev.off()

t_names <- c("AI", "Human")
title_size <- 18
axis_size <- 16

## plotting all measures
t_names <- c("AI", "Human")

## (1) FRIEND
p1 <- ggplot(d_merged, aes(x = factor(cond_name), y = willing_friend)) +
    theme_bw() +
    coord_cartesian(ylim = c(1, 110)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 3)) +
    geom_signif(comparisons = list(c("ai", "human")), map_signif_level = TRUE, textsize = 5.5)

p1

p1 <- p1 +
    theme(text = element_text(size = title_size), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    scale_x_discrete(labels = t_names) +
    ggtitle("Willingness to\nFind a Friend") +
    xlab("") +
    ylab("") +
    theme_classic() +
    theme(axis.text.x = element_text(size = axis_size)) +
    theme(axis.text.y = element_text(size = axis_size)) +
    theme(plot.title = element_text(size = title_size, hjust = 0.5)) +
    geom_bar(position="dodge", stat="summary", width = 0.9, alpha = 0.38, size = 0.75) +
    stat_summary(fun.data = "mean_cl_boot", color = "black",
                 size = 0.4,
                 position = position_dodge(width = 0.9)) +
    stat_summary(fun.data = "mean_cl_boot", color = "black",
                 position = position_dodge(width = 0.9),
                 geom = "errorbar", width = 0.2)
p1


## (2) PARTNER
p2 <- ggplot(d_merged, aes(x = factor(cond_name), y = willing_romantic)) +
    theme_bw() +
    coord_cartesian(ylim = c(1, 110)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 3)) +
    geom_signif(comparisons = list(c("ai", "human")), map_signif_level = TRUE, textsize = 3.5)

p2 <- p2 +
    theme(text = element_text(size = title_size), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    scale_x_discrete(labels = t_names) +
    ggtitle("Willingness to\nFind Romantic Partner") +
    xlab("") +
    ylab("") +
    theme_classic() +
    theme(axis.text.x = element_text(size = axis_size)) +
    theme(axis.text.y = element_text(size = axis_size)) +
    theme(plot.title = element_text(size = title_size, hjust = 0.5)) +
    geom_bar(position="dodge", stat="summary", width = 0.9, alpha = 0.38, size = 0.75) +
    stat_summary(fun.data = "mean_cl_boot", color = "black",
                 size = 0.4,
                 position = position_dodge(width = 0.9)) +
    stat_summary(fun.data = "mean_cl_boot", color = "black",
                 position = position_dodge(width = 0.9),
                 geom = "errorbar", width = 0.2)
p2


## (3) PAYMENT
p3 <- ggplot(d_merged, aes(x = factor(cond_name), y = wtp_logged)) +
    theme_bw() +
    coord_cartesian(ylim = c(1, 5)) +
    geom_signif(comparisons = list(c("ai", "human")), map_signif_level = TRUE, textsize = 5.5)

p3 <- p3 +
    theme(text = element_text(size = title_size), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    scale_x_discrete(labels = t_names) +
    ggtitle("Willingness to Pay") +
    xlab("") +
    ylab("") +
    theme_classic() +
    theme(axis.text.x = element_text(size = axis_size)) +
    theme(axis.text.y = element_text(size = axis_size)) +
    theme(plot.title = element_text(size = title_size, hjust = 0.5)) +
    geom_bar(position="dodge", stat="summary", width = 0.9, alpha = 0.38, size = 0.75) +
    stat_summary(fun.data = "mean_cl_boot", color = "black",
                 size = 0.4,
                 position = position_dodge(width = 0.9)) +
    stat_summary(fun.data = "mean_cl_boot", color = "black",
                 position = position_dodge(width = 0.9),
                 geom = "errorbar", width = 0.2)
p3


## (4) TRUE
p4 <- ggplot(d_merged, aes(x = factor(cond_name), y = true)) +
    theme_bw() +
    coord_cartesian(ylim = c(1, 110)) +
    geom_signif(comparisons = list(c("ai", "human")), map_signif_level = TRUE, textsize = 5.5)

p4 <- p4 +
    theme(text = element_text(size = title_size), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    scale_x_discrete(labels = t_names) +
    ggtitle("Not True Friendship") +
    xlab("") +
    ylab("") +
    theme_classic() +
    theme(axis.text.x = element_text(size = axis_size)) +
    theme(axis.text.y = element_text(size = axis_size)) +
    theme(plot.title = element_text(size = title_size, hjust = 0.5)) +
    geom_bar(position="dodge", stat="summary", width = 0.9, alpha = 0.38, size = 0.75) +
    stat_summary(fun.data = "mean_cl_boot", color = "black",
                 size = 0.4,
                 position = position_dodge(width = 0.9)) +
    stat_summary(fun.data = "mean_cl_boot", color = "black",
                 position = position_dodge(width = 0.9),
                 geom = "errorbar", width = 0.2)
p4


## (5) TALK/LISTEN
p5 <- ggplot(d_merged, aes(x = factor(cond_name), y = talk_listen)) +
    theme_bw() +
    coord_cartesian(ylim = c(1, 110)) +
    geom_signif(comparisons = list(c("ai", "human")), map_signif_level = TRUE, textsize = 5.5)

p5 <- p5 +
    theme(text = element_text(size = title_size), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    scale_x_discrete(labels = t_names) +
    ggtitle("Will Talk/Listen") +
    xlab("") +
    ylab("") +
    theme_classic() +
    theme(axis.text.x = element_text(size = axis_size)) +
    theme(axis.text.y = element_text(size = axis_size)) +
    theme(plot.title = element_text(size = title_size, hjust = 0.5)) +
    geom_bar(position="dodge", stat="summary", width = 0.9, alpha = 0.38, size = 0.75) +
    stat_summary(fun.data = "mean_cl_boot", color = "black",
                 size = 0.4,
                 position = position_dodge(width = 0.9)) +
    stat_summary(fun.data = "mean_cl_boot", color = "black",
                 position = position_dodge(width = 0.9),
                 geom = "errorbar", width = 0.2)
p5


## (6) NOT JUDGE
p6 <- ggplot(d_merged, aes(x = factor(cond_name), y = not_judge)) +
    theme_bw() +
    coord_cartesian(ylim = c(1, 110)) +
    geom_signif(comparisons = list(c("ai", "human")), map_signif_level = TRUE, textsize = 5.5)

p6 <- p6 +
    theme(text = element_text(size = title_size), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    scale_x_discrete(labels = t_names) +
    ggtitle("Will Not Judge") +
    xlab("") +
    ylab("") +
    theme_classic() +
    theme(axis.text.x = element_text(size = axis_size)) +
    theme(axis.text.y = element_text(size = axis_size)) +
    theme(plot.title = element_text(size = title_size, hjust = 0.5)) +
    geom_bar(position="dodge", stat="summary", width = 0.9, alpha = 0.38, size = 0.75) +
    stat_summary(fun.data = "mean_cl_boot", color = "black",
                 size = 0.4,
                 position = position_dodge(width = 0.9)) +
    stat_summary(fun.data = "mean_cl_boot", color = "black",
                 position = position_dodge(width = 0.9),
                 geom = "errorbar", width = 0.2)
p6


## (7) UNDERSTAND
p7 <- ggplot(d_merged, aes(x = factor(cond_name), y = understand)) +
    theme_bw() +
    coord_cartesian(ylim = c(1, 110)) +
    geom_signif(comparisons = list(c("ai", "human")), map_signif_level = TRUE, textsize = 5.5)

p7 <- p7 +
    theme(text = element_text(size = title_size), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    scale_x_discrete(labels = t_names) +
    ggtitle("Unable to Understand") +
    xlab("") +
    ylab("") +
    theme_classic() +
    theme(axis.text.x = element_text(size = axis_size)) +
    theme(axis.text.y = element_text(size = axis_size)) +
    theme(plot.title = element_text(size = title_size, hjust = 0.5)) +
    geom_bar(position="dodge", stat="summary", width = 0.9, alpha = 0.38, size = 0.75) +
    stat_summary(fun.data = "mean_cl_boot", color = "black",
                 size = 0.4,
                 position = position_dodge(width = 0.9)) +
    stat_summary(fun.data = "mean_cl_boot", color = "black",
                 position = position_dodge(width = 0.9),
                 geom = "errorbar", width = 0.2)
p7


## (8) FEEL
p8 <- ggplot(d_merged, aes(x = factor(cond_name), y = feel)) +
    theme_bw() +
    coord_cartesian(ylim = c(1, 110)) +
    geom_signif(comparisons = list(c("ai", "human")), map_signif_level = TRUE, textsize = 5.5)

p8 <- p8 +
    theme(text = element_text(size = title_size), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    scale_x_discrete(labels = t_names) +
    ggtitle("Unable to Feel") +
    xlab("") +
    ylab("") +
    theme_classic() +
    theme(axis.text.x = element_text(size = axis_size)) +
    theme(axis.text.y = element_text(size = axis_size)) +
    theme(plot.title = element_text(size = title_size, hjust = 0.5)) +
    geom_bar(position="dodge", stat="summary", width = 0.9, alpha = 0.38, size = 0.75) +
    stat_summary(fun.data = "mean_cl_boot", color = "black",
                 size = 0.4,
                 position = position_dodge(width = 0.9)) +
    stat_summary(fun.data = "mean_cl_boot", color = "black",
                 position = position_dodge(width = 0.9),
                 geom = "errorbar", width = 0.2)
p8


## (9) BODY
p9 <- ggplot(d_merged, aes(x = factor(cond_name), y = body)) +
    theme_bw() +
    coord_cartesian(ylim = c(1, 110)) +
    geom_signif(comparisons = list(c("ai", "human")), map_signif_level = TRUE, textsize = 5.5)

p9 <- p9 +
    theme(text = element_text(size = title_size), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    scale_x_discrete(labels = t_names) +
    ggtitle("Cannot Be Physically Intimate") +
    xlab("") +
    ylab("") +
    theme_classic() +
    theme(axis.text.x = element_text(size = axis_size)) +
    theme(axis.text.y = element_text(size = axis_size)) +
    theme(plot.title = element_text(size = title_size, hjust = 0.5)) +
    geom_bar(position="dodge", stat="summary", width = 0.9, alpha = 0.38, size = 0.75) +
    stat_summary(fun.data = "mean_cl_boot", color = "black",
                 size = 0.4,
                 position = position_dodge(width = 0.9)) +
    stat_summary(fun.data = "mean_cl_boot", color = "black",
                 position = position_dodge(width = 0.9),
                 geom = "errorbar", width = 0.2)
p9


## (10) One-sided
p10 <- ggplot(d_merged, aes(x = factor(cond_name), y = mutual)) +
    theme_bw() +
    coord_cartesian(ylim = c(1, 110)) +
    geom_signif(comparisons = list(c("ai", "human")), map_signif_level = TRUE, textsize = 5.5)

p10 <- p10 +
    theme(text = element_text(size = title_size), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    scale_x_discrete(labels = t_names) +
    ggtitle("Not Mutual") +
    xlab("") +
    ylab("") +
    theme_classic() +
    theme(axis.text.x = element_text(size = axis_size)) +
    theme(axis.text.y = element_text(size = axis_size)) +
    theme(plot.title = element_text(size = title_size, hjust = 0.5)) +
    geom_bar(position="dodge", stat="summary", width = 0.9, alpha = 0.38, size = 0.75) +
    stat_summary(fun.data = "mean_cl_boot", color = "black",
                 size = 0.4,
                 position = position_dodge(width = 0.9)) +
    stat_summary(fun.data = "mean_cl_boot", color = "black",
                 position = position_dodge(width = 0.9),
                 geom = "errorbar", width = 0.2)
p10


## (4) ALL FIGURES

dev.new(width = 13 / 3 * 2, height = 10, noRStudioGD = TRUE)

figure <- ggarrange(p10, p7, p8, p9, nrow = 2, ncol = 2, common.legend = TRUE, legend = "top", vjust = 1.0, hjust = 0.5)
annotate_figure(figure, left = text_grob("Mean Rating", color = "black", face = "plain", size = 26, rot = 90),
                bottom = text_grob("Agent Type", color = "black", face = "plain", size = 26, margin(b = 2), hjust = 0.25))

ggsave(
    "serialmed.pdf",
    last_plot(),
    dpi = 500
)

## ================================================================================================================
##                                      DATA ANALYSIS - MEDIATION USING 'PROCESS'                 
## ================================================================================================================

t = as.factor(d_merged$cond) ## turn into factor
d_merged$cond = as.numeric(t) ## set numeric of factor to variable
d_merged$cond_n <- as.numeric(d_merged$cond_n)

## PARALLEL MEDIATION:

## (1) FRIEND
## With mediators: true friendship, reliance, judgment
process(data = d_merged, y = "willing_friend", x = "cond_n",
        m = c("true", "talk_listen", "not_judge"), model = 4, effsize = 1, total = 1, stand = 1,
        contrast = 1, boot = 10000, modelbt = 1, seed = 654321)

## (2) PARTNER
## With mediators: true friendship, reliance, judgment
process(data = d_merged, y = "willing_romantic", x = "cond_n",
        m = c("true", "talk_listen", "not_judge"), model = 4, effsize = 1, total = 1, stand = 1,
        contrast = 1, boot = 10000, modelbt = 1, seed = 654321)

process(data = d_merged, y = "willing_romantic", x = "cond_n",
        m = c("true", "not_judge"), model = 4, effsize = 1, total = 1, stand = 1,
        contrast = 1, boot = 10000, modelbt = 1, seed = 654321)

## (3) PAYMENT
## With mediators: true friendship, reliance, judgment
process(data = d_merged, y = "wtp_logged", x = "cond_n",
        m = c("true", "talk_listen", "not_judge"), model = 4, effsize = 1, total = 1, stand = 1,
        contrast = 1, boot = 10000, modelbt = 1, seed = 654321)


#What explains 'TRUE' judgments

## With mediators: true friendship, reliance, judgment
process(data = d_merged, y = "mutual", x = "cond_n",
        m = c("feel", "understand", "body"), model = 4, effsize = 1, total = 1, stand = 1,
        contrast = 1, boot = 10000, modelbt = 1, seed = 654321)


## Serial Mediation

## (1) FRIEND
process(data = d_merged, y = "willing_friend", x = "cond_n",
        m = c("understand", "mutual"), model = 6, effsize = 1, total = 1, stand = 1,
        contrast = 1, boot = 10000, modelbt = 1, seed = 654321)

process(data = d_merged, y = "willing_friend", x = "cond_n",
        m = c("feel", "mutual"), model = 6, effsize = 1, total = 1, stand = 1,
        contrast = 1, boot = 10000, modelbt = 1, seed = 654321)

## (2) PARTNER
process(data = d_merged, y = "willing_romantic", x = "cond_n",
        m = c("understand", "mutual"), model = 6, effsize = 1, total = 1, stand = 1,
        contrast = 1, boot = 10000, modelbt = 1, seed = 654321)

## (3) WTP
process(data = d_merged, y = "wtp_logged", x = "cond_n",
        m = c("understand", "mutual"), model = 6, effsize = 1, total = 1, stand = 1,
        contrast = 1, boot = 10000, modelbt = 1, seed = 654321)

## ================================================================================================================
##                                                  END OF ANALYSIS                 
## ================================================================================================================