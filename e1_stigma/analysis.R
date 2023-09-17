## ================================================================================================================
##  DATA ANALYSIS - CHATBOT STIGMA STUDY                 
## ================================================================================================================

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
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set working directory to current directory
d <- read.csv('data_stigma.csv')

## explore dataframe: 
dim(d) # will provide dimensions of the dataframe by row [1] and column [2]
colnames(d) # will provide all column names
summary(d)
dim(d)[1]
table(d$FL_4_DO)

## perform attention exclusions: 
# this will remove responses from the dataframe that failed attention checks (i.e., "1" or "2")
d <- subset(d, (d$att1 == 2 & d$att2 == 2))
dim(d) # number of participants should decrease after attention exclusions

## get number of participants BEFORE exclusions: 
n_original <- dim(d)[1] # extracting number of rows only, not columns
print(paste0("Number or participants hired: ", n_original))

## perform comprehension exclusions: 
# this will remove responses from the dataframe that failed comprehension checks (i.e., "2")
d <- subset(d, (d$comp_1 == 2 & d$comp_2 == 2 | d$comp_1.1 == 1 & d$comp_2.1 == 2))
print(paste0("Number or participants after comprehension exclusion: ", dim(d)[[1]]))

## get number of participants AFTER exclusions: 
n_final <- dim(d)[1] # extracting number of rows only, not columns
n_final
percent_excluded <- (n_original - n_final) / n_original
percent_excluded
table(d$FL_4_DO)

## ================================================================================================================
##                                            PARTICIPANT CHARACTERISTICS                 
## ================================================================================================================

## age
print(paste0("Mean age: ", mean(d$age, trim = 0, na.rm = TRUE)))

## gender
print(paste0("Percentage of females: ", table(d$gender)[2] / sum(table(d$gender))))

## ai experience 
ai_exp <- table(d$ai_companion_exp)[1] / sum(table(d$ai_companion_exp)) ## percentage of yes
print(paste0("AI experience: ", ai_exp))

# Only 2% of them are actually AI companion apps (see d$ai_companion_exp2)

print(paste0("AI Capability: ", mean(d$ai_capability_1, trim = 0, na.rm = TRUE)))

## ================================================================================================================
##                                                    SUBSETTING                 
## ================================================================================================================

## define new data frame to extract pre-processed data into:
d_subset <- array(dim = c(dim(d)[1], 9))
colnames(d_subset) <- c('cond', 'willing_friend', 'willing_romantic', 'wtp', 'true', 'talk_listen',
                        'not_judge', 'comp1', 'comp2')
d_subset <- as.data.frame(d_subset, stringsAsFactors = FALSE)

## extract good data from the middle part of raw data:
for (i in 1:dim(d)[1]) {
    curr <- d[i, 21:36][!is.na(d[i, 21:36])] # for a given row, get only the non-NA values
    d_subset[i, 2:9] <- as.numeric(curr[curr != ""]) # and only the non-empty values
    d_subset[i, 1] <- d[i, 58][!is.na(d[i, 58])]
}

## merge good data with first and last halves of raw data:
# this is the new dataframe to work with.
d_merged <- cbind(d_subset, d[, 37:57])
d_merged$ss <- 1:dim(d_merged)[1]

## Check if WTP is skewed and fix
hist(d_merged$wtp, main = "Willingness to Pay", xlab = "Money (USD)", border = "black", col = "light blue")
shapiro.test(d_merged$wtp)

## transform values to reduce skew
d_merged$wtp_logged <- log(d_merged$wtp + 1) + 1
hist(d_merged$wtp_logged, main = "Willingness to Pay", xlab = "Money (USD)", border = "black", col = "light blue")
shapiro.test(d_merged$wtp_logged)

## ================================================================================================================
##                                              DATA ANALYSIS - MEASURES                
## ================================================================================================================

## (1) LONELINESS
## calculate scale loading, frequency counts, and reliability 
keys.list <- list(lonely = c("loneliness_1_1", "loneliness_2_1", "loneliness_3_1"))
scores <- psych::scoreItems(keys.list, d_merged)
print(scores, short = FALSE)

## calculate average items if cronbach's alpha > 0.80
d_merged$lonely <- rowMeans(d_merged[, c("loneliness_1_1", "loneliness_2_1", "loneliness_3_1")])

## (2) SENSATION 
## calculate scale loading, frequency counts, and reliability 
keys.list <- list(sensation = c("sensation_1_1", "sensation_2_1", "sensation_3_1",
                                "sensation_4_1", "sensation_5_1", "sensation_6_1",
                                "sensation_7_1", "sensation_8_1"))
scores <- psych::scoreItems(keys.list, d_merged)
print(scores, short = FALSE)

## calculate average items if cronbach's alpha > 0.80
d_merged$sensation <- rowMeans(d_merged[, c("sensation_1_1", "sensation_2_1", "sensation_3_1",
                                            "sensation_4_1", "sensation_5_1", "sensation_6_1",
                                            "sensation_7_1", "sensation_8_1")])

## ================================================================================================================
##                                             DATA ANALYSIS - T-TESTS               
## ================================================================================================================

#experience with AI companion apps
table(d_merged$ai_companion_exp)[1] / sum(table(d_merged$ai_companion_exp))
d_merged$ai_companion_exp2
#looking at the explanations, only three count
3 / dim(d_merged)[1]

## (1) FRIEND
## get summary statistics
print("WILLINGNESS TO FRIEND")
print(d_merged %>%
    group_by(cond) %>%
    get_summary_stats(willing_friend, type = "mean_sd"))

## run two sample t-test
var.test(d_merged$willing_friend[d_merged$cond == 'human'], d_merged$willing_friend[d_merged$cond == 'ai'])
print(t.test(willing_friend ~ cond, data = d_merged, paired = FALSE, var.equal = TRUE))
print(d_merged %>% cohens_d(willing_friend ~ cond, paired = FALSE, var.equal = TRUE))


## (2) PARTNER
## get summary statistics
print("WILLINGNESS TO PARTNER")
print(d_merged %>%
    group_by(cond) %>%
    get_summary_stats(willing_romantic, type = "mean_sd"))

var.test(d_merged$willing_romantic[d_merged$cond == 'human'], d_merged$willing_romantic[d_merged$cond == 'ai'])
print(t.test(willing_romantic ~ cond, data = d_merged, paired = FALSE, var.equal = TRUE))
print(d_merged %>% cohens_d(willing_romantic ~ cond, paired = FALSE, var.equal = TRUE))


## (3) PAYMENT
## get summary statistics
print("WILLINGNESS TO PAY")
print(d_merged %>%
    group_by(cond) %>%
    get_summary_stats(wtp_logged, type = "mean_sd"))

var.test(d_merged$wtp_logged[d_merged$cond == 'human'], d_merged$wtp_logged[d_merged$cond == 'ai'])
print(t.test(wtp_logged ~ cond, data = d_merged, paired = FALSE, var.equal = TRUE))
print(d_merged %>% cohens_d(wtp_logged ~ cond, paired = FALSE, var.equal = TRUE))

## create box-plot visual
bxp_3 <- ggboxplot(
    d_merged, x = "cond", y = "wtp_logged",
    ylab = "Willingness to Pay", xlab = "Conditions",
    add = "jitter", color = "black", fill = "light blue")

#bxp_3


## (4) TRUE
## get summary statistics
print("TRUE")
print(d_merged %>%
        group_by(cond) %>%
        get_summary_stats(true, type = "mean_sd"))

## run two sample t-test
var.test(d_merged$true[d_merged$cond == 'human'], d_merged$true[d_merged$cond == 'ai'])
print(t.test(true ~ cond, data = d_merged, paired = FALSE, var.equal = TRUE))
print(d_merged %>% cohens_d(true ~ cond, paired = FALSE, var.equal = TRUE))


## (5) TALK/LISTEN
## get summary statistics
print("TALK/LISTEN")
print(d_merged %>%
    group_by(cond) %>%
    get_summary_stats(talk_listen, type = "mean_sd"))

## run two sample t-test
var.test(d_merged$talk_listen[d_merged$cond == 'human'], d_merged$talk_listen[d_merged$cond == 'ai'])
print(t.test(talk_listen ~ cond, data = d_merged, paired = FALSE, var.equal = TRUE))
print(d_merged %>% cohens_d(talk_listen ~ cond, paired = FALSE, var.equal = TRUE))


## (6) NOT JUDGE
## get summary statistics
print("NOT JUDGE")
print(d_merged %>%
    group_by(cond) %>%
    get_summary_stats(not_judge, type = "mean_sd"))

## run two sample t-test
var.test(d_merged$not_judge[d_merged$cond == 'human'], d_merged$not_judge[d_merged$cond == 'ai'])
print(t.test(not_judge ~ cond, data = d_merged, paired = FALSE, var.equal = TRUE))
print(d_merged %>% cohens_d(not_judge ~ cond, paired = FALSE, var.equal = TRUE))

## ================================================================================================================
##                                              PLOTTING MAIN FIGURES                 
## ================================================================================================================

## plotting all measures
t_names <- c("AI", "Human")
title_size <- 18
axis_size <- 16

## (1) FRIEND
p1 <- ggplot(d_merged, aes(x = factor(cond), y = willing_friend)) +
    theme_bw() +
    coord_cartesian(ylim = c(1, 110)) +
    geom_signif(comparisons = list(c("ai", "human")), map_signif_level = TRUE, textsize = 5.5)

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
                 position = position_dodge(width = 0.9), geom = "errorbar", width = 0.2) +
    stat_summary(fun.data = "mean_cl_boot", color = "black",
                 position = position_dodge(width = 0.9))

p1

## (2) PARTNER
t_names <- c("AI", "Human")
title_size <- 18
axis_size <- 16

## (1) FRIEND
p2 <- ggplot(d_merged, aes(x = factor(cond), y = willing_romantic)) +
    theme_bw() +
    coord_cartesian(ylim = c(1, 110)) +
    geom_signif(comparisons = list(c("ai", "human")), map_signif_level = TRUE, textsize = 5.5)

p2 <- p2 +
    theme(text = element_text(size = title_size), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    scale_x_discrete(labels = t_names) +
    ggtitle("Willingness to Find\nRomantic Partner") +
    xlab("") +
    ylab("") +
    theme_classic() +
    theme(axis.text.x = element_text(size = axis_size)) +
    theme(axis.text.y = element_text(size = axis_size)) +
    theme(plot.title = element_text(size = title_size, hjust = 0.5)) +
    geom_bar(stat="summary", width = 0.9, alpha = 0.38, size = 0.75) +
    stat_summary(fun.data = "mean_cl_boot", color = "black",
                 size = 0.4,
                 position = position_dodge(width = 0.9), geom = "errorbar", width = 0.2) +
    stat_summary(fun.data = "mean_cl_boot", color = "black",
                 position = position_dodge(width = 0.9))

p2


## (3) PAYMENT
t_names <- c("AI", "Human")
title_size <- 18
axis_size <- 16

p3 <- ggplot(d_merged, aes(x = factor(cond), y = wtp_logged)) +
    theme_bw() +
    coord_cartesian(ylim = c(1, 5)) +
    geom_signif(comparisons = list(c("ai", "human")), map_signif_level = TRUE, textsize = 5.5, annotation = c("+"))

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
                 position = position_dodge(width = 0.9), geom = "errorbar", width = 0.2) +
    stat_summary(fun.data = "mean_cl_boot", color = "black",
                 position = position_dodge(width = 0.9))
 
p3


## (4) TRUE
p4 <- ggplot(d_merged, aes(x = factor(cond), y = true)) +
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
    geom_bar(stat="summary", width = 0.9, alpha = 0.38, size = 0.75) +
    stat_summary(fun.data = "mean_cl_boot", color = "black",
                 size = 0.4,
                 position = position_dodge(width = 0.9), geom = "errorbar", width = 0.2) +
    stat_summary(fun.data = "mean_cl_boot", color = "black",
                 position = position_dodge(width = 0.9))
 
p4

## (5) TALK/LISTEN
p5 <- ggplot(d_merged, aes(x = factor(cond), y = talk_listen)) +
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
    geom_bar(stat="summary", width = 0.9, alpha = 0.38, size = 0.75) +
    stat_summary(fun.data = "mean_cl_boot", color = "black",
                 size = 0.4,
                 position = position_dodge(width = 0.9), geom = "errorbar", width = 0.2) +
    stat_summary(fun.data = "mean_cl_boot", color = "black",
                 position = position_dodge(width = 0.9))
 
p5

## (5) NOT JUDGE
p6 <- ggplot(d_merged, aes(x = factor(cond), y = not_judge)) +
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
    geom_bar(stat="summary", width = 0.9, alpha = 0.38, size = 0.75) +
    stat_summary(fun.data = "mean_cl_boot", color = "black",
                 size = 0.4,
                 position = position_dodge(width = 0.9), geom = "errorbar", width = 0.2) +
    stat_summary(fun.data = "mean_cl_boot", color = "black",
                 position = position_dodge(width = 0.9))
 
p6


## (4) ALL FIGURES

dev.new(width = 13, height = 10, noRStudioGD = TRUE)

figure <- ggarrange(p1, p2, p3, p4, p5, p6, nrow = 2, ncol = 3, common.legend = TRUE, legend = "top")
annotate_figure(figure, left = text_grob("Mean Rating", color = "black", face = "plain", size = 26, rot = 90),
                bottom = text_grob("Agent Type", color = "black", face = "plain", size = 26, hjust = 0.25, margin(b = 2)))
ggsave(
    "panel_fig_new.pdf",
    last_plot(),
    dpi = 500
)

## ================================================================================================================
##                                      DATA ANALYSIS - MEDIATION USING 'PROCESS'                 
## ================================================================================================================

t = as.factor(d_merged$cond) ## turn into factor
d_merged$cond <- as.numeric(t) ## set numeric of factor to variable

## PARALLEL MEDIATION:
## bind mediators together with the c function 
## compare strengths of mediators by using contrast function: 
## (1) = compare indirect effects, (2) = compares absolute values of indirect effect


process(data = d_merged, y = "willing_friend", x = "cond",
        m = c("true", "talk_listen", "not_judge"), w="lonely", model = 14, effsize = 1, total = 1, stand = 1,
        contrast = 1, boot = 10000, modelbt = 1, seed = 654321)


process(data = d_merged, y = "willing_friend", x = "cond",
        m = c("true", "talk_listen", "not_judge"), w="lonely", model = 7, effsize = 1, total = 1, stand = 1,
        contrast = 1, boot = 10000, modelbt = 1, seed = 654321)

if(FALSE) {
    ## (1) FRIEND
    ## With mediators: true friendship, reliance, judgment
    process(data = d_merged, y = "willing_friend", x = "cond",
            m = c("true", "talk_listen", "not_judge"), w="lonely", model = 14, effsize = 1, total = 1, stand = 1,
            contrast = 1, boot = 10000, modelbt = 1, seed = 654321)

    process(data = d_twocond, y = dv, x = "response_type.x",
            m = c("comprehend", "is_harmful"), w = "lonely", model = 14, effsize = 1, total = 1, stand = 1,
            contrast = 1, boot = 10000, modelbt = 1, seed = 654321)


    ## (2) PARTNER
    ## With mediators: true friendship, reliance, judgment
    process(data = d_merged, y = "willing_romantic", x = "cond",
            m = c("true", "talk_listen", "not_judge"), model = 4, effsize = 1, total = 1, stand = 1,
            contrast = 1, boot = 10000, modelbt = 1, seed = 654321)


    ## (3) PAYMENT
    ## With mediators: true friendship, reliance, judgment
    process(data = d_merged, y = "wtp_logged", x = "cond",
            m = c("true", "talk_listen", "not_judge"), model = 4, effsize = 1, total = 1, stand = 1,
            contrast = 1, boot = 10000, modelbt = 1, seed = 654321)

}

## ================================================================================================================
##                                                  END OF ANALYSIS                 
## ================================================================================================================