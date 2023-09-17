## ================================================================================================================
##  DATA ANALYSIS - CHATBOT STIGMA STUDY - CAUSUAL EVIDENCE                 
## ================================================================================================================

## clear workspace
rm(list = ls()) 

options(download.file.method="libcurl")

## install packages
library(ggpubr)
library(rstatix)
if (!require(pacman)) {install.packages("pacman")}

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

## ================================================================================================================
##                                                  PRE-PROCESSING                 
## ================================================================================================================

## read in data: 
# if importing from Qualtrics: (i) export data as numeric values, and (ii) delete rows 2 and 3 of the .csv file.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set working directory to current directory
d <- read.csv('causal_v2.csv') 
d$X <- NULL

## explore dataframe: 
dim(d) # will provide dimensions of the dataframe by row [1] and column [2]
colnames(d) # will provide all column names
summary(d)
dim(d)[1]
table(d$FL_4_DO)
names(d)[names(d) == 'FL_4_DO'] <- 'cond'

## perform attention exclusions: 
# this will remove responses from the dataframe that failed attention checks (i.e., "1" or "2")
d <- subset(d, (d$att1 == 2 & d$att2 == 2))
print(paste0("Number of participants after attention checks: ", dim(d)[1]))

d <- subset(d, (d$essence == 2 | d$essence.1 == 1))
print(paste0("Number of participants after essence checks: ", dim(d)[1]))

## ================================================================================================================
##                                                    SUBSETTING                 
## ================================================================================================================

## define new data frame to extract pre-processed data into:
d_subset <- array(dim=c(dim(d)[1], 7))
colnames(d_subset) <- c('cond', "essence", 'willing_friend', 'willing_romantic', 'wtp', 'comp_1', 'comp_2')
d_subset <- as.data.frame(d_subset, stringsAsFactors=FALSE) 

## extract good data from the middle part of raw data:
for(i in 1:dim(d)[1]) {
  curr <- d[i,21:32][!is.na(d[i,21:32])] # for a given row, get only the non-NA values
  d_subset[i,2:7] <- as.numeric(curr[curr!= ""]) # and only the non-empty values
  d_subset[i,1] <- d[i,43][!is.na(d[i,43])]
}

## merge good data with first and last halves of raw data:
# this is the new dataframe to work with.
d_merged <- cbind(d_subset, d[,33:42])
d_merged$ss <- 1:dim(d_merged)[1]     

## ================================================================================================================

## Check if WTP is skewed and fix
hist(d_merged$wtp,  main = "Willingness to Pay", xlab = "Money (USD)", border = "black", col = "light blue")
shapiro.test(d_merged$wtp)

## transform values to reduce skew
d_merged$wtp_logged <- log(d_merged$wtp+1) + 1
hist(d_merged$wtp_logged,  main = "Willingness to Pay", xlab = "Money (USD)", border = "black", col = "light blue")
shapiro.test(d_merged$wtp_logged)

## ================================================================================================================
##                                              PERFORM EXCLUSIONS                
## ================================================================================================================
## perform comprehension exclusions: 
# this will remove responses from the dataframe that failed comprehension checks (i.e., "2")
d_merged <- subset(d_merged, (d_merged$comp_1 == 2 & d_merged$comp_2 == 2))
print(paste0("Number of participants after comprehension checks: ", dim(d_merged)[[1]]))

## ================================================================================================================
##                                            PARTICIPANT CHARACTERISTICS                 
## ================================================================================================================

## age
print(paste0("Age: ", mean(d$age, trim = 0, na.rm = TRUE)))

## gender
print(paste0("Percentage of females: ", table(d$gender)[2]/sum(table(d$gender))))

## ai experience 
print(paste0("AI experience: ", table(d$ai_companion_exp)[1]/sum(table(d$ai_companion_exp))))
# Actually 7% (see d$ai_companion_exp2)

## ai capability 
print(paste0("AI capability: ", mean(d$ai_capability_1, trim = 0, na.rm = TRUE)))


## ================================================================================================================
##                                             DATA ANALYSIS - T-TESTS               
## ================================================================================================================

#experience with AI companion apps
table(d_merged$ai_companion_exp)[1]/sum(table(d_merged$ai_companion_exp))
d_merged$ai_companion_exp2
#looking at the explanations, only three count
3/dim(d_merged)[1]


## (1) FRIEND
## get summary statistics
print("FRIEND")
print(paste0("Essential mean: ", mean(d_merged$willing_friend[d_merged$cond=='essential'])))
print(paste0("Superficial mean: ", mean(d_merged$willing_friend[d_merged$cond=='superficial'])))

## run two sample t-test
vt <- var.test(d_merged$willing_friend[d_merged$cond=='essential'],
         d_merged$willing_friend[d_merged$cond=='superficial'])
print(t.test(willing_friend ~ cond, data = d_merged, paired = FALSE, var.equal = vt$p.value > 0.05))
print(d_merged %>% cohens_d(willing_friend ~ cond, paired = FALSE, var.equal = vt$p.value > 0.05))


## (2) PARTNER
## get summary statistics
print("PARTNER")
print(paste0("Essential mean: ", mean(d_merged$willing_romantic[d_merged$cond=='essential'])))
print(paste0("Superficial mean: ", mean(d_merged$willing_romantic[d_merged$cond=='superficial'])))

var.test(d_merged$willing_romantic[d_merged$cond=='essential'], 
         d_merged$willing_romantic[d_merged$cond=='superficial'])
print(t.test(willing_romantic ~ cond, data = d_merged, paired = FALSE, var.equal = vt$p.value > 0.05))
print(d_merged %>% cohens_d(willing_romantic ~ cond, paired = FALSE, var.equal = vt$p.value > 0.05))


## (3) PAYMENT
## get summary statistics
print("PAY")
print(paste0("Essential mean: ", mean(d_merged$wtp[d_merged$cond=='essential'])))
print(paste0("Superficial mean: ", mean(d_merged$wtp[d_merged$cond=='superficial'])))

vt <- var.test(d_merged$wtp[d_merged$cond=='essential'],
         d_merged$wtp[d_merged$cond=='superficial'])
print(t.test(wtp_logged ~ cond, data = d_merged, paired = FALSE, var.equal = vt$p.value > 0.05))
print(d_merged %>% cohens_d(wtp_logged ~ cond, paired = FALSE, var.equal = vt$p.value > 0.05))

## create box-plot visual
bxp_3 <- ggboxplot(
  d_merged, x = "cond", y = "wtp_logged", 
  ylab = "Willingness to Pay", xlab = "Conditions", 
  add = "jitter", color ="black", fill = "light blue")
#bxp_3

## ================================================================================================================
##                                              PLOTTING MAIN FIGURES                 
## ================================================================================================================

## plotting all measures
t_names <- c("Abstract\nValues", "Concrete\nFeatures")
title_size <- 18
axis_size <- 16

## (1) FRIEND
p1 <- ggplot(d_merged,aes(x = factor(cond),y = willing_friend)) +  
  theme_bw() +coord_cartesian(ylim = c(1,110)) + scale_y_continuous(breaks = scales::pretty_breaks(n = 3))+ 
  geom_signif(comparisons = list(c("essential", "superficial")),map_signif_level=TRUE, textsize = 5.5,
  test = "t.test",tip_length = 0.03)

#p1

p1 <- p1 + theme(text = element_text(size = title_size), 
                 panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  scale_x_discrete(labels = t_names) +
  xlab ("") + ylab ("") +
  theme_classic() +
  ggtitle("Willingness to\nFind a Friend") +
  theme(axis.text.x = element_text(size=axis_size)) +
  theme(axis.text.y = element_text(size=axis_size)) +
  theme(plot.title = element_text(size=title_size, hjust=0.5)) +
  theme(plot.title = element_text(size = 18, hjust = 0.5)) +
  geom_bar(stat="summary", width = 0.9, alpha = 0.38, size = 0.75) +
  stat_summary(fun.data = "mean_cl_boot", color = "black", 
               size = 0.4, 
               position = position_dodge(width = 0.9)) +
  stat_summary(fun.data = "mean_cl_boot", color = "black", 
               position = position_dodge(width = 0.9),
               geom="errorbar", width = 0.2)

## (1) PARTNER
p2 <- ggplot(d_merged,aes(x = factor(cond),y = willing_romantic)) +  
  theme_bw() +coord_cartesian(ylim = c(1,110)) + scale_y_continuous(breaks = scales::pretty_breaks(n = 3))+ 
  geom_signif(comparisons = list(c("essential", "superficial")),map_signif_level=TRUE, textsize = 5.5,
              test = "t.test",tip_length = 0.03)

#p2

p2 <- p2 + theme(text = element_text(size = title_size), 
                 panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  scale_x_discrete(labels = t_names) +
  xlab ("") + ylab ("") +
  theme_classic() +
  ggtitle("Willingness to\nFind Romantic Partner") +
  theme(axis.text.x = element_text(size=axis_size)) +
  theme(axis.text.y = element_text(size=axis_size)) +
  theme(plot.title = element_text(size=title_size, hjust=0.5)) +
  theme(plot.title = element_text(size = 18, hjust = 0.5)) +
  geom_bar(stat="summary", width = 0.9, alpha = 0.38, size = 0.75) +
  stat_summary(fun.data = "mean_cl_boot", color = "black", 
               size = 0.4, 
               position = position_dodge(width = 0.9)) +
  stat_summary(fun.data = "mean_cl_boot", color = "black", 
               position = position_dodge(width = 0.9),
               geom="errorbar", width = 0.2)
#p2


## (3) PAYMENT
p3 <- ggplot(d_merged,aes(x = factor(cond), y = wtp_logged)) +  
  theme_bw() +coord_cartesian(ylim = c(0, 4)) + scale_y_continuous(breaks = scales::pretty_breaks(n = 3))+ 
  geom_signif(comparisons = list(c("essential", "superficial")),map_signif_level=TRUE, textsize = 5.5,
              test = "t.test", tip_length = 0.03, y_position = c(3.3))
#p3

p3 <- p3 + theme(text = element_text(size = title_size), 
                 panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  xlab ("") + ylab ("") +
  scale_x_discrete(labels = t_names) +
  theme_classic() +
  ggtitle("Willingness to Pay") +
  theme(axis.text.x = element_text(size=axis_size)) +
  theme(axis.text.y = element_text(size=axis_size)) +
  theme(plot.title = element_text(size=title_size, hjust=0.5)) +
  theme(plot.title = element_text(size = 18, hjust = 0.5)) +
  geom_bar(stat="summary", width = 0.9, alpha = 0.38, size = 0.75) +
  stat_summary(fun.data = "mean_cl_boot", color = "black", 
               size = 0.4,
               position = position_dodge(width = 0.9)) +
  stat_summary(fun.data = "mean_cl_boot", color = "black", 
               position = position_dodge(width = 0.9),
               geom="errorbar", width = 0.2)
#p3

## (4) ALL FIGURES

dev.new(width = 13, height = 5, noRStudioGD = TRUE)

figure <- ggarrange(p1, p2, p3, nrow = 1, ncol = 3, common.legend = TRUE, legend="top", vjust = 1.0, hjust=0.5) 
annotate_figure(figure,left = text_grob("Mean Rating", color = "black", face ="plain", size = 26, rot = 90),
                bottom = text_grob("Essence Type", color = "black", face = "plain", size = 26, margin(b = 2), hjust=0.3)) 

ggsave(paste0("causal.pdf"), last_plot(), dpi = 500)

## ================================================================================================================
##                                                  END OF ANALYSIS                 
## ================================================================================================================