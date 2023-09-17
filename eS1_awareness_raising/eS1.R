## ================================================================================================================
##                                DATA ANALYSIS | CHATBOT STIGMA STUDY | EXPERIMENT 3               
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
               'nlme',             # get p values for mixed effect model
               'lmerTest'
)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set working directory to current directory

## ================================================================================================================
##                                                  PRE-PROCESSING                 
## ================================================================================================================

## read in data: 
# if importing from Qualtrics: (i) export data as numeric values, and (ii) delete rows 2 and 3 of the .csv file.

d <- read.csv('data_ws.csv')
d$X <- NULL

## rename condition variables:
names(d)[names(d) == 'FL_4_DO'] <- 'agent_order'

## perform attention exclusions: 
# this will remove responses from the dataframe that failed attention checks (i.e., "1" or "2")
d <- subset(d, (d$att_1 == 2 & d$att_2 == 2))
print(paste0("Number of participants hired: ", dim(d)[[1]]))

## ================================================================================================================
##                                              PERFORM EXCLUSIONS                
## ================================================================================================================

## get number of participants BEFORE exclusions: 
n_original <- dim(d)[1] # extracting number of rows only, not columns
n_original

## perform comprehension exclusions: 
# this will remove responses from the dataframe that failed comprehension checks (i.e., "2")
d <- subset(d, (d$comp_1 == 1 & d$comp_2 == 2))
print(paste0("Number or participants after comprehension exclusion: ", dim(d)[[1]]))

## ================================================================================================================
##                                                    SUBSETTING                 
## ================================================================================================================

## define new data frame to extract pre-processed data into:
d_subset <- array(dim = c(dim(d)[1], 7))
colnames(d_subset) <- c('agent_order', 'friend', 'partner', 'wtp', 'friend_2', 'partner_2', 'wtp_2')
d_subset <- as.data.frame(d_subset, stringsAsFactors = FALSE)

## extract good data from the middle part of raw data:
for (i in 1:dim(d)[1]) {
    curr <- d[i, c(21:32)][!is.na(d[i, c(21:32)])] # for a given row, get only the non-NA values
    d_subset[i, 2:7] <- as.numeric(curr[curr != ""]) # and only the non-empty values

    cond_names <- d[i, 47][!is.na(d[i, 47])]
    d_subset[i, 1] <- strsplit(cond_names[[1]], "_")[[1]][2]
}

## define new data frame to extract pre-processed data into:
f_subset <- array(dim = c(dim(d)[1], 7))
colnames(f_subset) <- c('agent_order', 'ai_friend', 'ai_partner', 'ai_wtp', 'h_friend', 'h_partner', 'h_wtp')
f_subset <- as.data.frame(f_subset, stringsAsFactors = FALSE)

for (i in 1:dim(d_subset)[1]) {
    f_subset[i, 1] <- d_subset[i, 1]
    if (d_subset$agent_order[i] == 1) {
        f_subset[i, 2:7] <- d_subset[i, 2:7]
    }
    else {
        f_subset[i, 2:4] <- d_subset[i, 5:7]
        f_subset[i, 5:7] <- d_subset[i, 2:4]
    }
}

d_merged <- array(dim = c(dim(d)[1] * 2, 5))
colnames(d_merged) <- c('agent_order', 'agent_cond', 'friend', 'partner', 'wtp')
d_merged <- as.data.frame(d_merged, stringsAsFactors = FALSE)
d_merged$agent_order <- c(f_subset$agent_order, f_subset$agent_order)
d_merged$agent_cond <- c(rep(c(1), each = dim(d)[1]), rep(c(2), each = dim(d)[1])) #1= AI, 2 =Human

d_merged$friend <- c(f_subset$ai_friend, f_subset$h_friend)
d_merged$partner <- c(f_subset$ai_partner, f_subset$h_partner)
d_merged$wtp <- c(f_subset$ai_wtp, f_subset$h_wtp)

d_merged$agent_cond_name <- ifelse(d_merged$agent_cond == 1, "ai", "human")

## ================================================================================================================
##                                            PARTICIPANT CHARACTERISTICS                 
## ================================================================================================================

## % prior experience apps
print(paste0("Prior AI experience: ", table(d$ai_companion_exp)[1] / sum(table(d$ai_companion_exp))))
7 / dim(d)[1] ## see d$ai_companion_exp2

## age
print(paste0("Mean age: ", mean(as.numeric(d$age), trim = 0, na.rm = TRUE)))

## gender
print(paste0("Percentage of females: ", table(d$gender)[2] / sum(table(d$gender))))

## ================================================================================================================
##                                              DATA ANALYSIS - DISTRIBUTIONS                
## ================================================================================================================

## (3) PAYMENT
## check distribution
hist(d_merged$wtp,
     main = "Willingness to Pay",
     xlab = "Money (USD)",
     border = "black",
     col = "light blue")

## check for normal distribution (shapiro-wilk normality test)
shapiro.test(d_merged$wtp)

## transform values to reduce skew
d_merged$wtp_logged <- log(d_merged$wtp + 1) + 1

shapiro.test(d_merged$wtp_logged)

hist(d_merged$wtp_logged,
     main = "Willingness to Pay",
     xlab = "Money (USD)",
     border = "black",
     col = "light blue")

## ================================================================================================================
##                                             DATA ANALYSIS - T-TESTS               
## ================================================================================================================

## (1) FRIEND
## get summary statistics
mean(d_merged[d_merged$agent_cond == 1, 'friend'])
mean(d_merged[d_merged$agent_cond == 2, 'friend'])

friend_mod <- lmer(friend ~ agent_cond + (1 | agent_order), data = d_merged)
print(summary(friend_mod))

## (2) PARTNER
## get summary statistics
mean(d_merged[d_merged$agent_cond == 1, 'partner'])
mean(d_merged[d_merged$agent_cond == 2, 'partner'])

partner_mod <- lmer(partner ~ agent_cond + (1 | agent_order), data = d_merged)
print(summary(partner_mod))


## (3) PAYMENT
## get summary statistics
mean(d_merged[d_merged$agent_cond == 1, 'wtp_logged'])
mean(d_merged[d_merged$agent_cond == 2, 'wtp_logged'])

wtp_mod <- lmer(wtp_logged ~ agent_cond + (1 | agent_order), data = d_merged)
print(summary(wtp_mod))

## ================================================================================================================
##                                              PLOTTING MAIN FIGURES                 
## ================================================================================================================

## plotting all measures
t_names <- c("AI", "Human")
title_size <- 18
axis_size <- 16

## (1) FRIEND
p1 <- ggplot(d_merged, aes(x = factor(agent_cond_name), y = friend)) +
    theme_bw() +
    coord_cartesian(ylim = c(1, 110)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 3)) +
    geom_signif(comparisons = list(c("ai", "human")), map_signif_level = TRUE, textsize = 5.5)

#p1

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
    geom_bar(stat="summary", width = 0.9, alpha = 0.38, size = 0.75) +
    stat_summary(fun.data = "mean_cl_boot", color = "black",
                 size = 0.4,
                 position = position_dodge(width = 0.9)) +
    stat_summary(fun.data = "mean_cl_boot", color = "black",
                 position = position_dodge(width = 0.9),
                 geom = "errorbar", width = 0.2)
#p1


## (2) PARTNER
p2 <- ggplot(d_merged, aes(x = factor(agent_cond_name), y = partner)) +
    theme_bw() +
    coord_cartesian(ylim = c(1, 110)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 3)) +
    geom_signif(comparisons = list(c("ai", "human")), map_signif_level = TRUE, textsize = 5.5)

#p2

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
    geom_bar(stat="summary", width = 0.9, alpha = 0.38, size = 0.75) +
    stat_summary(fun.data = "mean_cl_boot", color = "black",
                 size = 0.4,
                 position = position_dodge(width = 0.9)) +
    stat_summary(fun.data = "mean_cl_boot", color = "black",
                 position = position_dodge(width = 0.9),
                 geom = "errorbar", width = 0.2)
#p2


## (3) PAYMENT
p3 <- ggplot(d_merged, aes(x = factor(agent_cond_name), y = wtp_logged)) +
    theme_bw() +
    coord_cartesian(ylim = c(0, 8)) +
    geom_signif(y_position = 7, comparisons = list(c("ai", "human")), map_signif_level = TRUE, textsize = 5.5)

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
    geom_bar(stat="summary", width = 0.9, alpha = 0.38, size = 0.75) +
    stat_summary(fun.data = "mean_cl_boot", color = "black",
                 size = 0.4,
                 position = position_dodge(width = 0.9)) +
    stat_summary(fun.data = "mean_cl_boot", color = "black",
                 position = position_dodge(width = 0.9),
                 geom = "errorbar", width = 0.2)
#p3


## (4) ALL FIGURES
dev.new(width = 13, height = 6, noRStudioGD = TRUE)

figure <- ggarrange(p1, p2, p3, nrow = 1, ncol = 3, common.legend = TRUE, legend = "top", vjust = 1.0, hjust = 0.5)
annotate_figure(figure, left = text_grob("Mean Rating", color = "black", face = "plain", size = 26, rot = 90),
                bottom = text_grob("Agent Type", color = "black", face = "plain", size = 26, hjust = 0.25, margin(b = 2)))

ggsave(
    "./within_subjects.pdf",
    last_plot(),
    dpi = 500,
    width = 13, height = 6
)

## ================================================================================================================
##                                                  END OF ANALYSIS                 
## ================================================================================================================