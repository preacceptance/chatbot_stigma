## ================================================================================================================
##  DATA ANALYSIS - CHATBOT STIGMA STUDY
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
               'DescTools',        # get Cramer's V
               'dplyr',
               'car'
)

## ================================================================================================================
##                                                  PRE-PROCESSING
## ================================================================================================================

## read in data:
# if importing from Qualtrics: (i) export data as numeric values, and (ii) delete rows 2 and 3 of the .csv file.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set working directory to current directory
d <- read.csv('./data/final_data.csv')
d <- d[d$Finished == 1,]

## perform attention exclusions:
# this will remove responses from the dataframe that failed attention checks (i.e., "1" or "2")
size_before <- dim(d)[1]
d <- d[(d$att_1 == "2" & d$att_2 == "2"),]
print(paste0("Exclusions from attention check: ", size_before - dim(d)[1]))
d <- subset(d, (d$att_1 == 2 & d$att_2 == 2))

print("Exclude participants who completed shorter than 15 minutes")
print(d[as.numeric(d$Duration..in.seconds) < 900, 'workerId'])
d <- d[!as.numeric(d$Duration..in.seconds) < 900, ]

# Remove duplicates
d <- d[!duplicated(d$workerId),]

print(paste0("Number of participants hired: ", dim(d)[1]))

############################################ EXCLUSIONS ############################################
## Perform comprehension exclusion:
size_before <- dim(d)[1]

# Which questions did they pick?
table(d$comp_1)

d <- d[(d$comp_2 == "1"), ] #d$comp_1 == "2" & 
print(paste0("Exclusions from comprehension check: ", size_before - dim(d)[1]))

# Excluding participants who did not believe they were talking to a human, in the human-chatbot condition
size_before <- dim(d)[1]
d_not_believed <- d[d$condition == 'another person' & d$condition_2 == 'ai' & d$believe_q == "2", ]  # Exclude participants who did not believe
d <- d[!(d$workerId %in% d_not_believed$workerId), ]
print(paste0("Exclusions from ai condition that participants believed they were talking to a human: ", dim(d_not_believed)[1]))
print(paste0("Percentage of participants that didn't believe they were talking to a human: ", (100 * dim(d_not_believed)[1] /
 (dim(d_not_believed)[1] + dim(d[d$condition == 'another person' & d$condition_2 == 'ai',])[1]))))

print("Excluding participants who failed youtube...")
size_before <- dim(d)[1]
exclude_list <- c(586, 564, 548, 498, 497, 491, 466, 462, 437, 412, 410, 396, 372, 353, 352, 299, 289, 287, 284, 257, 248, 238, 237, 233, 330, 232)

d <- d[!d$workerId %in% exclude_list, ]
print(paste0("Excluding participants in YouTube condition who did not follow the instructions. N=", size_before - dim(d)[1]))

print("Excluding participants who was not able to do nothing")
d <- d[d$able_nothing != 2,]
############################################ END OF EXCLUSIONS ############################################

# Print number of participants from each condition
print(paste0("Number of participants in each condition: "))
print(table(d$condition, d$condition_2))


############################################ Demographics ############################################
print(paste0("Age: ", mean(as.numeric(d$age), trim = 0, na.rm = TRUE))) ## mean age

print(paste0("Percentage of females: ", dim(d[d$gender == 2,])[1] / dim(d)[1]))

print(paste0("AI Experience: ", dim(d[d$chatbot == 1,])[1] / dim(d)[1]))


## LONELINESS SCALE
loneliness_before <- mutate_all(d[, c('lonely_1_3', 'lonely_2_3', 'lonely_3_3', 'lonely_4_3')], function(x) as.numeric(as.character(x)))
print(paste0("Before loneliness cronbach's alpha: ", cronbach.alpha(loneliness_before)[[1]]))

d$loneliness_scale_predict <- rowMeans(loneliness_before)

loneliness_after <- mutate_all(d[, c('lonely_1_after_3', 'lonely_2_after_3', 'lonely_3_after_3', 'lonely_4_after_3')], function(x) as.numeric(as.character(x)))
print(paste0("After loneliness cronbach's alpha: ", cronbach.alpha(loneliness_after)[[1]]))

d$loneliness_scale_exp <- rowMeans(loneliness_after)

## Attitude Towards AI
# Questions (positive (1) and negative (0)):
question_types <- c(1, 1, 1, 1, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1) # Manually took these from the qualtrics form
keys <- c()
d[, "ai_attitude_1_1"] <- d[, "ai_attitude_1_6"]
for (i in (1:16)) {
    d[, paste0("ai_attitude_", i, "_1")] <- as.numeric(d[, paste0("ai_attitude_", i, "_1")])
    keys <- append(keys, paste0("ai_attitude_", i, "_1"))
    if (question_types[i] == 0) {  # Negative question
        d[, paste0("ai_attitude_", i, "_1")] <- 100 - d[, paste0("ai_attitude_", i, "_1")]
    }
}

## calculate average items if cronbach's alpha > 0.80
d[,keys] <- replace(d[,keys], is.na(d[,keys]), 0)
print(paste0("AI attitude cronbach alpha: ", cronbach.alpha(d[, keys])[1]))
d$ai_att <- rowMeans(d[, keys])

print_sig <- function(p) {
    if (p < 0.001) {
        return("***")
    } else if (p < 0.01) {
        return("**")
    } else if (p < 0.05) {
        return("*")
    } else if (p < 0.1) {
        return("+")
    } else {
        return("ns")
    }
}

# Cronbach's alpha between loneliness and social connection measures

# Combine before and after columns (rbind), and calculate alpha
lc_mat <- array(0, dim = c(dim(d)[1] * 2, 2)) # Get cronbach's alpha, then average items

lc_mat[, 1] <- 100 - na.omit(as.numeric(c(c(paste(d$lonely_predict_3, d$lonely_predict_3.1, sep="")),
                                          c(paste(d$lonely_exp_3, d$lonely_exp_3.1, sep="")))))
lc_mat[, 2] <- na.omit(as.numeric(c(c(paste(d$connect_predict_3, d$connect_predict_3.1, sep="")),
                                    c(paste(d$connect_exp_3, d$connected_exp_3, sep="")))))
cronbach.alpha(lc_mat)

######### T-TESTS COMPARING PREDICTION VS. AFTER INTERACTION FOR EACH CONDITION #########
#### WE HAVE 5 CONDITIONS: Real Person vs. Chatbot vs. Chatbot Acting Like Person vs. YouTube vs. Do Nothing
conditions <- c('Doing Nothing', 'Real Person', 'Chatbot', 'Chatbot Acting Like Human', 'YouTube')
d_merged_list <- list()
significance_list <- list()

for(condition in conditions) {
    q_significance_list <- list()

    print(paste0("*--*-*-*-*-*-*-*-*-*-*-*-*  ", condition, " *-*-*-*-*-*-*-*-*-*-*-*--*"))
    if(condition == 'Real Person') {
        d_cond <- d[(d$condition == 'another person') & (d$condition_2 == '0'),]
        d[(d$condition == 'another person') & (d$condition_2 == '0'), 'agent_cond'] <- condition
    } else if(condition == 'Chatbot Acting Like Human') {
        d_cond <- d[(d$condition == 'another person') & (d$condition_2 == 'ai'),]
        d[(d$condition == 'another person') & (d$condition_2 == 'ai'), 'agent_cond'] <- condition
    } else if(condition == 'Chatbot') {
        d_cond <- d[(d$condition == 'conversational AI companion'),]
        d[(d$condition == 'conversational AI companion'), 'agent_cond'] <- condition
    } else if(condition == 'YouTube') {
        d_cond <- d[(d$condition == 'youtube'),]
        d[(d$condition == 'youtube'), 'agent_cond'] <- condition
    } else if(condition == 'Doing Nothing') {
        d_cond <- d[(d$condition == 'nothing'),]
        d[(d$condition == 'nothing'), 'agent_cond'] <- condition
    }
    
    qs <- c('loneliness_scale', 'lonely_connect_mean', 'entertain', 'novel', 'engage', 'comfort', 'interest')
    d_merged_cond <- data.frame(matrix(nrow = dim(d_cond)[1] * 2, ncol = length(qs) + 2))

    d_merged_cond$cond <- c(rep(c(1), each = dim(d_cond)[1]), rep(c(2), each = dim(d_cond)[1])) #1 = prediction, 2 = after interaction
    d_merged_cond$interacting_with <- c(rep(condition, each = dim(d_cond)[1] * 2))
    for (question in qs) {
        print(paste0("*-*-*-*-*-*-*-*-*-* ", question, " *-*-*-*-*-*-*-*-*-*"))
      
        if(question == 'lonely_connect_mean') {
          lonely_before <- as.numeric(d_cond[,grepl(paste0("lonely_predict"), colnames(d_cond)) & (colSums(d_cond == "") == 0)])
          lonely_after <- as.numeric(d_cond[,grepl(paste0("lonely_exp"), colnames(d_cond)) & (colSums(d_cond == "") == 0)])  
          
          connect_before <- 100 - as.numeric(d_cond[,grepl(paste0("connect_predict"), colnames(d_cond)) & (colSums(d_cond == "") == 0)])
          connect_after <- 100 - as.numeric(d_cond[,grepl(paste0("connect_exp"), colnames(d_cond)) & (colSums(d_cond == "") == 0)])
          
          if(length(connect_after) == 0) {
            connect_after <- 100 - as.numeric(d_cond[,grepl(paste0("connected_exp"), colnames(d_cond)) & (colSums(d_cond == "") == 0)])
          }

          before <- rowMeans(cbind(lonely_before, connect_before))
          after <- rowMeans(cbind(lonely_after, connect_after))
        } else {
          before <- as.numeric(d_cond[,grepl(paste0(question, "_predict"), colnames(d_cond)) & (colSums(d_cond == "") == 0)])
          after <- as.numeric(d_cond[,grepl(paste0(question, "_exp"), colnames(d_cond)) & (colSums(d_cond == "") == 0)])  
        }

        if(length(after) == 0) {
            after <- as.numeric(d_cond[,grepl(paste0(question, "ed_exp"), colnames(d_cond)) & (colSums(d_cond == "") == 0)])
        }

        # Different column name for comfort
        if(length(after) == 0 && question == 'comfort') {
            after <- as.numeric(d_cond[,grepl(paste0("comfor_exp"), colnames(d_cond)) & (colSums(d_cond == "") == 0)])
        }

        d_merged_cond[, question] <- as.numeric(c(before, after))
        d_merged_cond[, paste0(question, "_diff")] <- as.numeric(before - after)

        if(length(before) > 0) {
            print(paste0("Sample Size:", length(before)))
            print(paste0("Sample Size:", length(after)))
            vart <- var.test(before, after)
            tt <- t.test(before, after, paired = TRUE, var.equal = vart$p.value > 0.05)
            q_significance_list[[question]] <- print_sig(tt$p.value)
            print(tt)
            print(paste0("Mean of before: ", mean(before)))
            print(paste0("Mean of after: ", mean(after)))

            print(cohen.d(before, after))
        } else {
            print("ERROR: Check the code")
        }
    }

    significance_list[[condition]] <- q_significance_list
    d_merged_list[[condition]] <- d_merged_cond
}

d_merged_large <- bind_rows(d_merged_list, .id = "column_label")

######################## Does loneliness or ai attitude predict the change in qs? ######################################

if(FALSE) {
    d_merged_1 <- d_merged[d_merged$cond == 1,]
    d_merged_1$loneliness_scale <- d$loneliness_scale
    d_merged_1$ai_att <- d$ai_att

    for (dv in qs) {
        print(paste("............................................", dv, "............................................"))
        for (iv in c('loneliness_scale', 'ai_att')) {
            fm <- as.formula(paste(iv, " ~ ", dv, "_diff", sep = ""))
            print(fm)
            print(summary(lm(fm, data = d_merged_1)))
        }
    }
}


######################## SINGLE PLOT, CONSISTING OF ALL CONDITIONS AND LONELY & CONNECTED DV's ########################

## plotting all measures
a_names <- conditions
interaction_conds <- c('Pre', 'Post')

plotter <- function(y_var, y_var_str, title) {
    stars <- c()
    for(condition in conditions) {
        stars <- append(stars, significance_list[[condition]][[y_var_str]])
    }
    
    if(title == "State Loneliness") {
        y_lim <- c(1, 70)
        y_pos <- c(60, 60, 60, 60, 60)
    } else {
        y_lim <- c(1, 110)
        y_pos <- c(90, 90, 90, 90, 90)
    }

    p1 <- ggplot(d_merged_large, aes(x = factor(column_label, level=conditions), y = y_var, fill = factor(cond)), color = factor(cond)) +
    theme_bw() +
    coord_cartesian(ylim = y_lim) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
    geom_signif(
        y_position = y_pos, xmin = c(0.8, 1.8, 2.8, 3.8, 4.8), xmax = c(1.2, 2.2, 3.2, 4.2, 5.2),
        annotation = stars,
        textsize = 8
    ) +
    theme(text = element_text(size = 18), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    scale_x_discrete(labels = a_names) +
    ggtitle(title) +
    scale_fill_manual(values = c("#cccccc", "#666666"), name = "Timing Relative to Manipulation:",
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
    return(p1)
}

p1 <- plotter(d_merged_large$lonely_connect_mean, 'lonely_connect_mean', 'Expected v. Actual\nLoneliness After Interaction')

a_names <- conditions
interaction_conds <- c('Before', 'After')

p2 <- plotter(d_merged_large$loneliness_scale, 'loneliness_scale', 'Actual Loneliness\nBefore v. After Interaction')

dev.new(width = 13, height = 10, noRStudioGD = TRUE)

figure <- ggarrange(p1, p2, nrow = 2, ncol = 1, common.legend = TRUE, legend = "top", vjust = 1.0, hjust = 0.5)
annotate_figure(figure, left = text_grob("Mean Rating", color = "black", face = "plain", size = 26, rot = 90),
                bottom = text_grob("Condition", color = "black", face = "plain", size = 26, margin(b = 2), hjust = 0.3))

ggsave("./plots/combined_plot.pdf", last_plot(), dpi = 500)

#### SUPPLEMENTAL PLOT

# 'entertain', 'novel', 'engage', 'comfort', 'interest'
a_names <- conditions
interaction_conds <- c('Pre', 'Post')

p1 <- plotter(d_merged_large$entertain, 'entertain', 'Entertain')
p2 <- plotter(d_merged_large$novel, 'novel', 'Novel')
p3 <- plotter(d_merged_large$engage, 'engage', 'Engage')
p4 <- plotter(d_merged_large$comfort, 'comfort', 'Comfort')
p5 <- plotter(d_merged_large$interest, 'interest', 'Interest')

dev.new(width = 13, height = 20, noRStudioGD = TRUE)

figure <- ggarrange(p1, p2, p3, p4, p5, nrow = 5, ncol = 1, common.legend = TRUE, legend = "top", vjust = 1.0, hjust = 0.5)
annotate_figure(figure, left = text_grob("Mean Rating", color = "black", face = "plain", size = 26, rot = 90),
                bottom = text_grob("Condition", color = "black", face = "plain", size = 26, margin(b = 2), hjust = 0.25))

ggsave("./plots/supplemental_plot.pdf", last_plot(), dpi = 500, limitsize = FALSE)


# Check results
d_merged_large %>%
    group_by(interacting_with, cond) %>%
    summarise(mean_lonely_connect=mean(lonely_connect_mean), mean_state_loneliness_scale=mean(loneliness_scale))

############### CONVERSATIONAL ANALYSES ###############
# Comparing agent conditions::
d_merged_int_chatbot_acting <- d[d$condition == 'another person' & d$condition_2 == 'ai',]
d_merged_int_chatbot <- d[d$condition == 'conversational AI companion',]
d_merged_int_real_person <- d[d$condition == 'another person' & d$condition_2 == '0',]

##### t-tests
for(iv in c('AvgHumanWordCount', 'AvgSimTurnLength', 'AvgSimSpacy', 'AvgHumanCompoundSentiment', 'AvgSimValence', 'AvgResponseTime')) {

    print(paste0("*-*-*-*-*-*-*-*-*-*-* ", iv, " *-*-*-*-*-*-*-*-*-*-*"))

    print(paste("....................................... Real Person vs. Chatbot", iv, "......................................"))
    vart <- var.test(d_merged_int_real_person[,iv], d_merged_int_chatbot[,iv])
    tt <- t.test(d_merged_int_real_person[,iv], d_merged_int_chatbot[,iv], paired = FALSE, var.equal = vart$p.value > 0.05)
    print(tt)
    print(paste0("Significance: ", print_sig(tt$p.value)))
    print(cohen.d(d_merged_int_real_person[,iv], d_merged_int_chatbot[,iv], paired = FALSE, var.equal = vart$p.value > 0.05))

    print(paste("....................................... Real Person vs. Chatbot Acting Like Person", iv, "......................................"))
    vart <- var.test(d_merged_int_real_person[,iv], d_merged_int_chatbot_acting[,iv])
    tt <- t.test(d_merged_int_real_person[,iv], d_merged_int_chatbot_acting[,iv], paired = FALSE, var.equal = vart$p.value > 0.05)
    print(tt)
    print(paste0("Significance: ", print_sig(tt$p.value)))
    print(cohen.d(d_merged_int_real_person[,iv], d_merged_int_chatbot_acting[,iv], paired = FALSE, var.equal = vart$p.value > 0.05))

    print(paste("....................................... Chatbot vs. Chatbot Acting Like Person", iv, "......................................"))
    vart <- var.test(d_merged_int_chatbot[,iv], d_merged_int_chatbot_acting[,iv])
    tt <- t.test(d_merged_int_chatbot[,iv], d_merged_int_chatbot_acting[,iv], paired = FALSE, var.equal = vart$p.value > 0.05)
    print(tt)
    print(paste0("Significance: ", print_sig(tt$p.value)))
    print(cohen.d(d_merged_int_chatbot[,iv], d_merged_int_chatbot_acting[,iv], paired = FALSE, var.equal = vart$p.value > 0.05))
}
