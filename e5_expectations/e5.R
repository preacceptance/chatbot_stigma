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
#source('process.R')

## ================================================================================================================
##                                                  PRE-PROCESSING                 
## ================================================================================================================

## read in data: 
# if importing from Qualtrics: (i) export data as numeric values, and (ii) delete rows 2 and 3 of the .csv file.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set working directory to current directory
d <- read.csv('data.csv')

## explore dataframe: 
dim(d) # will provide dimensions of the dataframe by row [1] and column [2]
colnames(d) # will provide all column names
summary(d)
dim(d)[1]
table(d$FL_4_DO)

## perform attention exclusions: 
# this will remove responses from the dataframe that failed attention checks (i.e., "1" or "2")
d <- subset(d, (d$att_1 == 2 & d$att_2 == 2))
dim(d) # number of participants should decrease after attention exclusions

## get number of participants BEFORE exclusions: 
n_original <- dim(d)[1] # extracting number of rows only, not columns
n_original

## age
mean(d$age, trim = 0, na.rm = TRUE) ## mean age 

## gender
table(d$gender)[2] / sum(table(d$gender)) ## percentage of females

## loneliness
lonely_mat <- array(0, dim = c(dim(d)[1], 4)) #get cronbach's alpha, then average items
lonely_mat[, 1] <- d$lonely_1_1
lonely_mat[, 2] <- d$lonely_2_1
lonely_mat[, 3] <- d$lonely_3_1
lonely_mat[, 4] <- d$lonely_4_1
cronbach.alpha(lonely_mat)

d$lonely_ind <- rowMeans(lonely_mat)

## read in data: 
# if importing from Qualtrics: (i) export data as numeric values, and (ii) delete rows 2 and 3 of the .csv file.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set working directory to current directory
d <- read.csv('data.csv')

## perform attention exclusions: 
# this will remove responses from the dataframe that failed attention checks (i.e., "1" or "2")
d <- subset(d, (d$att_1 == 2 & d$att_2 == 2))
print(paste0("Number of participants hired: ", dim(d)[1]))

## ================================================================================================================
##                                            PARTICIPANT CHARACTERISTICS                 
## ================================================================================================================

print(paste0("Mean age: ", mean(d$age, trim = 0, na.rm = TRUE)))
print(paste0("Mean gender: ", table(d$gender)[2] / sum(table(d$gender))))

# For chatbot experience, see chatbot_exp column
relational_ai_exp <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
sum(relational_ai_exp) / length(relational_ai_exp)

## loneliness
lonely_mat <- array(0, dim = c(dim(d)[1], 4)) #get cronbach's alpha, then average items
lonely_mat[, 1] <- d$lonely_1_1
lonely_mat[, 2] <- d$lonely_2_1
lonely_mat[, 3] <- d$lonely_3_1
lonely_mat[, 4] <- d$lonely_4_1
cronbach.alpha(lonely_mat)

d$lonely_ind <- rowMeans(lonely_mat)

print("Choices: ")
print(table(d$choice))
print(chisq.test(table(d$choice)))

d_subset <- subset(d, d$choice != 3)
chisq.test(table(d_subset$choice))

## ================================================================================================================
##                                                    SUBSETTING                 
## ================================================================================================================

d_merged <- array(dim = c(dim(d)[1] * 3, 10))
d_merged$cond <- c(rep(c(1), each = dim(d)[1]), rep(c(2), each = dim(d)[1]), rep(c(3), each = dim(d)[1])) #1 = person, 2 = chatbot, 3 = video

d_merged$entertain <- c(d$entertain_person_3, d$entertain_chatbot_3, d$entertain_youtube_3)
d_merged$lonely <- c(d$lonely_person_3, d$lonely_chatbot_3, d$lonely_youtube_3)
d_merged$novel <- c(d$novel_person_3, d$novel_chatbot_3, d$novel_youtube_3)
d_merged$engage <- c(d$engage_person_3, d$engage_chatbot_3, d$engage_youtube_3)
d_merged$comfort <- c(d$comfort_person_3, d$comfort_chatbot_3, d$comfort_youtube_3)
d_merged$interest <- c(d$interest_person_3, d$interest_chatbot_3, d$interest_youtube_3)
d_merged$connect <- c(d$connect_person_3, d$connect_chatbot_3, d$connect_youtube_3)

d_merged <- as.data.frame(d_merged, stringsAsFactors = FALSE)

# Combine lonely and connect:
# First check cronbach's alpha between those two questions
lc_mat <- array(0, dim = c(dim(d_merged)[1], 2)) # Get cronbach's alpha, then average items

lc_mat[, 1] <- 100 - d_merged$lonely
lc_mat[, 2] <- 100 - d_merged$connect

cronbach.alpha(lc_mat)

d_merged$lonely_connect_mean <- rowMeans(cbind(100 - d_merged$lonely, 100 - d_merged$connect))


#----- lonely + connect mean:
print(aggregate(d_merged[, 'lonely_connect_mean'], list(d_merged$cond), FUN = mean))
lonely_connect_stars <- c("***", "*", "**")

#person vs. chatbot
vt <- var.test(d_merged$lonely_connect_mean[d_merged$cond == 1], d_merged$lonely_connect_mean[d_merged$cond == 2])
t.test(d_merged$lonely_connect_mean[d_merged$cond == 1], d_merged$lonely_connect_mean[d_merged$cond == 2], paired = TRUE, var.equal = vt$p.value > 0.05)
cohen.d(d_merged$lonely_connect_mean[d_merged$cond == 1], d_merged$lonely_connect_mean[d_merged$cond == 2])

#person vs. youtube
vt <- var.test(d_merged$lonely_connect_mean[d_merged$cond == 1], d_merged$lonely_connect_mean[d_merged$cond == 3])
t.test(d_merged$lonely_connect_mean[d_merged$cond == 1], d_merged$lonely_connect_mean[d_merged$cond == 3], paired = TRUE, var.equal = vt$p.value > 0.05)
cohen.d(d_merged$lonely_connect_mean[d_merged$cond == 1], d_merged$lonely_connect_mean[d_merged$cond == 3])

#chatbot vs. youtube
vt <- var.test(d_merged$lonely_connect_mean[d_merged$cond == 2], d_merged$lonely_connect_mean[d_merged$cond == 3])
t.test(d_merged$lonely_connect_mean[d_merged$cond == 2], d_merged$lonely_connect_mean[d_merged$cond == 3], paired = TRUE, var.equal = vt$p.value > 0.05)
cohen.d(d_merged$lonely_connect_mean[d_merged$cond == 2], d_merged$lonely_connect_mean[d_merged$cond == 3])


#-----entertain: 
d_merged %>%
    group_by(cond) %>%
    get_summary_stats(entertain, type = "mean_sd")
entertain_stars <- c("ns", "***", "***")

#person vs. chatbot
vt <- var.test(d_merged$entertain[d_merged$cond == 1], d_merged$entertain[d_merged$cond == 2])
t.test(d_merged$entertain[d_merged$cond == 1], d_merged$entertain[d_merged$cond == 2], paired = TRUE, var.equal = vt$p.value > 0.05)

#chatbot vs. youtube
vt <- var.test(d_merged$entertain[d_merged$cond == 2], d_merged$entertain[d_merged$cond == 3])
t.test(d_merged$entertain[d_merged$cond == 2], d_merged$entertain[d_merged$cond == 3], paired = TRUE, var.equal = vt$p.value > 0.05)

#person vs. youtube
vt <- var.test(d_merged$entertain[d_merged$cond == 1], d_merged$entertain[d_merged$cond == 3])
t.test(d_merged$entertain[d_merged$cond == 1], d_merged$entertain[d_merged$cond == 3], paired = TRUE, var.equal = vt$p.value > 0.05)

#-----novel:
d_merged %>%
    group_by(cond) %>%
    get_summary_stats(novel, type = "mean_sd")
novel_stars <- c("ns", "ns", "*")

#person vs. chatbot
vt <- var.test(d_merged$novel[d_merged$cond == 1], d_merged$novel[d_merged$cond == 2])
t.test(d_merged$novel[d_merged$cond == 1], d_merged$novel[d_merged$cond == 2], paired = TRUE, var.equal = vt$p.value > 0.05)

#chatbot vs. youtube
vt <- var.test(d_merged$novel[d_merged$cond == 2], d_merged$novel[d_merged$cond == 3])
t.test(d_merged$novel[d_merged$cond == 2], d_merged$novel[d_merged$cond == 3], paired = TRUE, var.equal = vt$p.value > 0.05)

#person vs. youtube
vt <- var.test(d_merged$novel[d_merged$cond == 1], d_merged$novel[d_merged$cond == 3])
t.test(d_merged$novel[d_merged$cond == 1], d_merged$novel[d_merged$cond == 3], paired = TRUE, var.equal = vt$p.value > 0.05)


#-----engage:
d_merged %>%
    group_by(cond) %>%
    get_summary_stats(engage, type = "mean_sd")
engage_stars <- c("ns", "+", "ns")

#person vs. chatbot
vt <- var.test(d_merged$engage[d_merged$cond == 1], d_merged$engage[d_merged$cond == 2])
t.test(d_merged$engage[d_merged$cond == 1], d_merged$engage[d_merged$cond == 2], paired = TRUE, var.equal = vt$p.value > 0.05)

#chatbot vs. youtube
vt <- var.test(d_merged$engage[d_merged$cond == 2], d_merged$engage[d_merged$cond == 3])
t.test(d_merged$engage[d_merged$cond == 2], d_merged$engage[d_merged$cond == 3], paired = TRUE, var.equal = vt$p.value > 0.05)

#person vs. youtube
vt <- var.test(d_merged$engage[d_merged$cond == 1], d_merged$engage[d_merged$cond == 3])
t.test(d_merged$engage[d_merged$cond == 1], d_merged$engage[d_merged$cond == 3], paired = TRUE, var.equal = vt$p.value > 0.05)


#-----comfort:
d_merged %>%
    group_by(cond) %>%
    get_summary_stats(comfort, type = "mean_sd")
comfort_stars <- c("ns", "***", "***")

#person vs. chatbot
vt <- var.test(d_merged$comfort[d_merged$cond == 1], d_merged$comfort[d_merged$cond == 2])
t.test(d_merged$comfort[d_merged$cond == 1], d_merged$comfort[d_merged$cond == 2], paired = TRUE, var.equal = vt$p.value > 0.05)

#chatbot vs. youtube
vt <- var.test(d_merged$comfort[d_merged$cond == 2], d_merged$comfort[d_merged$cond == 3])
t.test(d_merged$comfort[d_merged$cond == 2], d_merged$comfort[d_merged$cond == 3], paired = TRUE, var.equal = vt$p.value > 0.05)

#person vs. youtube
vt <- var.test(d_merged$comfort[d_merged$cond == 1], d_merged$comfort[d_merged$cond == 3])
t.test(d_merged$comfort[d_merged$cond == 1], d_merged$comfort[d_merged$cond == 3], paired = TRUE, var.equal = vt$p.value > 0.05)


#-----interest:
d_merged %>%
    group_by(cond) %>%
    get_summary_stats(interest, type = "mean_sd")
interest_stars <- c("ns", "***", "***")

#person vs. chatbot
vt <- var.test(d_merged$interest[d_merged$cond == 1], d_merged$interest[d_merged$cond == 2])
t.test(d_merged$interest[d_merged$cond == 1], d_merged$interest[d_merged$cond == 2], paired = TRUE, var.equal = vt$p.value > 0.05)

#chatbot vs. youtube
vt <- var.test(d_merged$comfort[d_merged$cond == 2], d_merged$comfort[d_merged$cond == 3])
t.test(d_merged$comfort[d_merged$cond == 2], d_merged$comfort[d_merged$cond == 3], paired = TRUE, var.equal = vt$p.value > 0.05)

#person vs. youtube
vt <- var.test(d_merged$comfort[d_merged$cond == 1], d_merged$comfort[d_merged$cond == 3])
t.test(d_merged$comfort[d_merged$cond == 1], d_merged$comfort[d_merged$cond == 3], paired = TRUE, var.equal = vt$p.value > 0.05)

## ================================================================================================================
##                                              PLOTTING MAIN FIGURES                 
## ================================================================================================================

## plotting all measures
t_names <- c("Human", "Chatbot", "YouTube")
title_size <- 18
axis_size <- 16

plotter <- function(y_var, title, stars, supplement=FALSE) {
    star_size <- 7
    if(supplement) {
        title_size <- 18
        axis_size <- 16
    }

    yc <- 115
    p1 <- ggplot(d_merged, aes(x = factor(cond), y = y_var)) +
        theme_bw() +
        coord_cartesian(ylim = c(1, yc)) +
        scale_y_continuous(breaks = scales::pretty_breaks(n = 3)) +
        geom_signif(y_position = 96, comparisons = list(c(1, 2)), textsize = star_size, test = "t.test", test.args = c(paired = TRUE), annotation = stars[1]) +
        geom_signif(y_position = 90, comparisons = list(c(2, 3)), textsize = star_size, test = "t.test", test.args = c(paired = TRUE), annotation = stars[2]) +
        geom_signif(y_position = 105, comparisons = list(c(1, 3)), textsize = star_size, test = "t.test", test.args = c(paired = TRUE), annotation = stars[3])

    p1 <- p1 +
        theme(text = element_text(size = title_size), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        scale_x_discrete(labels = t_names) +
        ggtitle(title) +
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
    p1

}

p_loneliness <- plotter(d_merged$lonely_connect_mean, 'Loneliness', lonely_connect_stars, supplement=TRUE)

p1_entertain <- plotter(d_merged$entertain, 'Entertained', entertain_stars, supplement=TRUE)
p3_novel <- plotter(d_merged$novel, 'Novel', novel_stars, supplement=TRUE)
p4_engage <- plotter(d_merged$engage, 'Engage', engage_stars, supplement=TRUE)
p5_comfort <- plotter(d_merged$comfort, 'Comfort', comfort_stars, supplement=TRUE)
p6_interest <- plotter(d_merged$interest, 'Interest', interest_stars, supplement=TRUE)

# supplementary plot
dev.new(width = 13, height = 10, noRStudioGD = TRUE)
figure <- ggarrange(p_loneliness, p1_entertain, p3_novel, p4_engage,
                    p5_comfort, p6_interest,
                    nrow = 2, ncol = 3, common.legend = TRUE, legend = "top", hjust = 0.25)
annotate_figure(figure, left = text_grob("Mean Rating", color = "black", face = "plain", size = 26, rot = 90),
                bottom = text_grob("Condition", color = "black", face = "plain", size = 26, hjust = 0.30))

ggsave(
    "main.pdf",
    last_plot(),
    dpi = 500
)

dir.create(file.path('plots'))
plot_files <- list.files(pattern = c("(.pdf|.png)"))
file.move(plot_files, "./plots", overwrite = TRUE)

## ================================================================================================================
##                   WHAT IS DRIVING DIFFERENCE BETWEEN YOUTUBE AND CHATBOT?                 
## ================================================================================================================

d$entertain_d <- d$entertain_youtube_3 - d$entertain_chatbot_3
d$novel_d <- d$novel_youtube_3 - d$novel_chatbot_3
d$engage_d <- d$engage_youtube_3 - d$engage_chatbot_3
d$comfort_d <- d$comfort_youtube_3 - d$comfort_chatbot_3
d$interest_d <- d$interest_youtube_3 - d$interest_chatbot_3
d$lonely_connect_d <- -1 * (rowMeans(cbind(d$lonely_youtube_3, d$connect_youtube_3)) - rowMeans(cbind(d$lonely_chatbot_3, d$connect_chatbot_3)))

d_subset_c_y <- subset(d, d$choice != 1)
d_subset_c_y$choice <- d_subset_c_y$choice - 2 #0 = chatbot, 1 = youtube

mod <- glm(choice ~ entertain_d +
    lonely_connect_d +
    novel_d +
    engage_d +
    comfort_d +
    interest_d,
           data = d_subset_c_y, family = 'binomial')
summary(mod)

## ================================================================================================================
##                                                  END OF ANALYSIS                 
## ================================================================================================================