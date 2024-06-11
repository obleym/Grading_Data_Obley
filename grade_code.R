## R code for looking at variance difference between length among families in control vs treatment tanks
# Install from CRAN
library('rmarkdown')
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(tinytex)


# packages needed
library(glmmTMB)
library(MuMIn) 
library(car)

# reading the data into R
grade.dat.fam <- read.csv("/Volumes/Extreme SSD/Graduate /MS/FISH SCIENCE/Data Analysis/Grading/Grading_data_forR.csv")
View(grade.dat.fam)

grade.dat.fam <- na.omit(grade.dat.fam)

# Before and After ----
# read data in
g.ba.dat <- read.csv("/Volumes/Extreme SSD/Graduate /MS/FISH SCIENCE/Data Analysis/Grading/grading_before_after_data.csv")
g.ba.dat


g.ba.dat.means <- aggregate(g.ba.dat[c("length")], 
                        by = g.ba.dat[c("treatment","type")], FUN=mean)
g.ba.dat.means
g.ba.dat.means$order = factor(g.ba.dat.means$type, levels=c("before","after"), labels=c("Beginning","End")) 

g.ba.plot <- ggplot(data=g.ba.dat.means, 
                    aes(x=order, 
                        y= length, 
                        color=treatment, 
                        label=treatment)) +
  geom_point(position=position_dodge(width=1)) +
  ggtitle("Comparison of Average Fork Length in Tanks at\n Beginning and End of Grading Treatment") + 
  geom_text(position=position_dodge(width=1.2), 
            hjust=.5, 
            vjust=-1,
            size = 5.5)  +
  scale_y_continuous(name="Length (cm)", 
                     limits=c(8, 20)) +
  theme(panel.grid = element_line(),
        legend.position="none", 
        text = element_text(size=rel(4.5)),
        plot.title = element_text(size=rel(3.5)))

g.ba.plot


## example
g.example.plot <- ggplot(data=g_example, 
                    aes(x=order, 
                        y= length, 
                        color=treatment, 
                        label=treatment)) +
  geom_point(position=position_dodge(width=1)) +
  ggtitle("Example Comparison of Average Fork Length in Tanks at\n Beginning and End of Grading Treatment") + 
  geom_text(position=position_dodge(width=1.2), 
            hjust=.5, 
            vjust=-1,
            size = 5.5)  +
  scale_y_continuous(name="Length (cm)", 
                     limits=c(4, 23)) +
  theme(panel.grid = element_line(),
        legend.position="none", 
        text = element_text(size=rel(4.5)),
        plot.title = element_text(size=rel(3.5)))

g.example.plot

# Fam in grade tanks ----
# Is there an association between family ID and representation in treatment tanks?
# subset by condition with %in%
grade2 = subset(grade.dat.fam, grade %in% c('Medium','Large','Small'))

## frequency table ----
grade_table <- table(grade2$family, grade2$grade)
grade_table

## mosaic graph of frequency ----
grade.mosaic = mosaicplot(grade_table, 
                          color = c("gold", "darkblue", "darkgreen"), 
                          xlab ="Family", 
                          ylab = "Treatment",
                          main = "Family Frequency in Grading Groups", 
                          cex.axis = 1.1)


# freqency table of expected values
gradechi_table = chisq.test(grade_table)$expected

# mosaic graph of expected values
grade.mosaic.chi = mosaicplot(chigrade_table, color = c("gold", "darkblue", "darkgreen"), xlab ="Family", ylab = "Treatment")

# chisq test
chisq.test(grade_table, correct=FALSE)



# Mixed Model ----
g.dat = grade.dat.fam
g.dat$treatment = factor(ifelse(g.dat$treatment == "Control", "CTRL", "TRT"))
g.dat$family = factor(g.dat$family)
g.dat$tank = factor(g.dat$tank)
g.treatment_tanks = unique(g.dat$tank[g.dat$treatment == "TRT"])
g.control_tanks = unique(g.dat$tank[g.dat$treatment == "CTRL"])


g.new_fit = glmmTMB::glmmTMB(length ~ treatment + 
                               (treatment|family) + 
                               (1|treatment) + 
                               (1|tank), data = grade.dat.fam)
g.new_fit


g.fit_effects = glmmTMB::glmmTMB(length ~ treatment + (treatment|family) + (1|tank), data = g.dat)
summary(g.fit_effects); round(confint(g.fit_effects), 2)

g.fit_means = glmmTMB::glmmTMB(length ~ treatment - 1 + (treatment - 1|family) + (1|tank), data = g.dat)
summary(g.fit_means); round(confint(g.fit_means), 2)



### Subset all grading tanks and control tanks ----
graded= subset(grade.dat.fam, treatment == 'Grading')
g.control= subset(grade.dat.fam, treatment == 'Control')

### Subset each tank ----
S1 = subset(grade.dat.fam, tank == 'S1')
S2 = subset(grade.dat.fam, tank == 'S2')
S4 = subset(grade.dat.fam, tank == 'S4')
S6 = subset(grade.dat.fam, tank == 'S6')
S7 = subset(grade.dat.fam, tank == 'S7')
U4 = subset(grade.dat.fam, tank == 'U4')
U5 = subset(grade.dat.fam, tank == 'U5')

### Subset each family by treatment ----
Ac = subset(g.control, family == 'A')
Cc = subset(g.control, family == 'C')
Dc = subset(g.control, family == 'D')
Ec = subset(g.control, family == 'E')
Fc = subset(g.control, family == 'F')
Gc = subset(g.control, family == 'G')
Hc = subset(g.control, family == 'H')
Ic = subset(g.control, family == 'I')
Jc = subset(g.control, family == 'J')
Kc = subset(g.control, family == 'K')
Lc = subset(g.control, family == 'L')
Mc = subset(g.control, family == 'M')
Nc = subset(g.control, family == 'N')
Oc = subset(g.control, family == 'O')
Pc = subset(g.control, family == 'P')

Ag = subset(graded, family == 'A')
Cg = subset(graded, family == 'C')
Dg = subset(graded, family == 'D')
Eg = subset(graded, family == 'E')
Fg = subset(graded, family == 'F')
Gg = subset(graded, family == 'G')
Hg = subset(graded, family == 'H')
Ig = subset(graded, family == 'I')
Jg = subset(graded, family == 'J')
Kg = subset(graded, family == 'K')
Lg = subset(graded, family == 'L')
Mg = subset(graded, family == 'M')
Ng = subset(graded, family == 'N')
Og = subset(graded, family == 'O')
Pg = subset(graded, family == 'P')

# Adjust for tank effects ----
## adjust small, medium, large, control tanks first - THEN combine to look at ICC##

## subset treatment groups ----           
g.small = subset(grade.dat.fam, grade == 'Small')
g.med = subset(grade.dat.fam, grade == 'Medium')
g.large = subset(grade.dat.fam, grade == 'Large')
g.control = subset(grade.dat.fam, grade == 'Control')

## Adjust lengths for treatment ----
# create a dataframe with the average length for each tank
# find the average of each treatment and subtract to find variance of tank from treatment. 
# add this (var) to each fish length to account for tank effect
g.tankmean <- aggregate(grade.dat.fam[c("length")],
                      by= grade.dat.fam[c("tank","treatment")], mean)

g.grademean <- aggregate(grade.dat.fam[c("length")],
                       by= grade.dat.fam[c("grade","treatment")], mean)

g.treatmentmean <- aggregate(grade.dat.fam[c("length")],
                           by= grade.dat.fam[c("treatment")], mean)

g.tankmean$g.var <- c((18.90842-19.19463), (19.46195-19.19463), (18.93858-19.03551), (19.11812-19.03551), (17.84186-17.56395), (17.28605-17.56395), (18.74948-18.74948))

S1$adjusted <-c(t(S1[, 5] - -0.09693))
S2$adjusted <-c(t(S2[, 5] - -0.28621))
S4$adjusted <-c(t(S4[, 5] - 0.26732))
S6$adjusted <-c(t(S6[, 5] - 0.08261))
S7$adjusted <-c(t(S7[, 5] - 0.27791))
U4$adjusted <-c(t(U4[, 5] - -0.27790))
U5$adjusted <-c(t(U5[, 5] - 0.0))


g.adjusted = c(S1$adjusted, S2$adjusted, S4$adjusted, S6$adjusted, S7$adjusted, U4$adjusted, U5$adjusted)
grade.dat.fam['adjusted'] <- g.adjusted
grade.dat.fam

## Create family means for adjusted.t ----
g.fam.mean.a=aggregate(grade.dat.fam[c("adjusted")],
                    by= grade.dat.fam[c("family","treatment")], mean)
g.fam.mean.a

## check
check <- aggregate(grade.dat.fam[c("adjusted")],
                   by= grade.dat.fam[c("tank","treatment")], mean)

### Re-subset all grading tanks and control tanks ----

grade = subset(grade.dat.fam, treatment == 'Grading')
g.control= subset(grade.dat.fam, treatment == 'Control')



# t test
t.test(grade$adjusted, g.control$adjusted,
       alternative = c("two.sided", "less", "greater"),
       mu = 0, paired = FALSE, var.equal = FALSE,
       conf.level = 0.95)

# size diff
19.19463-18.70809
#  0.48654

# CV ----
# control cv
gc.cv.fish <- sd(g.control$adjusted) / mean(g.control$adjusted) * 100
gc.cv.fish # 8.967056

# control ofs
gc.ofs.fish <- ((gc.cv.fish / 100) ^2)
gc.ofs.fish # 0.00804081

# grade cv
gg.cv.fish <- (sd(grade$adjusted) * 100) / mean(grade$adjusted) 
gg.cv.fish # 7.975215

# grade ofs
gg.ofs.fish <- ((gg.cv.fish / 100) ^2)
gg.ofs.fish # 0.006360405

# percent decrease
0.00804081-0.006360405
0.001680405/0.00804081
0.1320726*100 # 0.2089845


# fam mean
# subset fam means for Control and Grading
g.control.fam.means<-subset(g.fam.mean.a, treatment!="Grading")
grade.fam.means<-subset(g.fam.mean.a, treatment!="Control")

# control cv
gc.cv.fam <- sd(g.control.fam.means$adjusted) / mean(g.control.fam.means$adjusted) * 100
gc.cv.fam # 4.15659

# control ofs
gc.ofs.fam <- ((gc.cv.fam / 100) ^2)
gc.ofs.fam # 0.001727724

# grade cv
gg.cv.fam <- sd(grade.fam.means$adjusted) / mean(grade.fam.means$adjusted) * 100
gg.cv.fam # 3.338311

# grade ofs
gg.ofs.fam <- ((gg.cv.fam / 100) ^2)
gg.ofs.fam # 0.001114432

# percent decrease
0.001727724-0.001114432
0.000613292/0.001727724
0.354971*100 # 35.4971



# ICC Adjusted ----
# ICC for graded and control (length adjusted per grade)
require(ICC)
ICCest(family, adjusted, data = grade, CI.type = "S")
# ICC = 0.1533226
# varw = 1.904674
# vara = 0.3449126
# upper CI = 0.2670923
# lower CI = 0.03955297
ICCest(family, adjusted, data = g.control, CI.type = "S")
# ICC = 0.2107517
# varw = 2.372076
# vara = 0.6334116
# upper CI = 0.3489214
# lower CI = 0.07258195

# Create ICC dataframe
g.treatment <- c('graded', 'control')
g.ICC <- c(0.1533226,0.2107517)
g.vara <- c(0.3449126, 0.6334116)
g.varw <- c(1.904674, 2.372076)
g.upperCI <- c(0.2670923, 0.3489214)
g.lowerCI <- c(0.03955297, 0.07258195)

g.ICCdf <- data.frame(g.treatment, g.ICC, g.vara, g.varw, g.upperCI, g.lowerCI)
g.ICCdf

# Plot ICCs
require(ggplot2)
# with error bars
g.ICCplot <- ggplot(data = g.ICCdf, aes(x = g.treatment, y = g.ICC)) +
  geom_point(aes(color=g.treatment),size=3.5, position = position_dodge2(w = 0.15)) +
  scale_x_discrete(expand=c(0.1, 0.5)) + 
  scale_y_continuous(name="ICC", limits=c(0, .4)) +
  theme(panel.grid = element_line()) + 
  ggtitle("ICC comparison between Control and Graded\n(length adjusted per grade)") + 
  geom_errorbar(
    aes(x=g.treatment, 
        ymin = g.lowerCI, 
        ymax = g.upperCI), width=0.1, 
    color = "red"
  ) + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  labs(x="Treatment", y = "ICC") + 
  labs(colour = "Treatment") +
  theme(axis.text=element_text(size=15),
        title=element_text(size=20),
        legend.text=element_text(size=15))
g.ICCplot



### F test treat ----
# F test to compare variances between graded and control tanks
# (length adjusted per grade)
g.ftest.treat <- var.test(adjusted ~ treatment, data = grade.dat.fam)
g.ftest.treat
# F test to compare two variances
# 
# data:  adjusted by treatment
# F = 1.3308, num df = 614, denom df = 555, p-value = 0.0005962
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
#   1.130821 1.565105
# sample estimates:
#   ratio of variances 
# 1.330808 

### Fligner ----
fligner.test(adjusted ~ treatment, data = grade.dat.fam)


#### variances ----
g.control.var.treat <- var(g.control$adjusted)
g.control.var.treat
# 2.962506

grade.var.treat <- var(grade$adjusted)
grade.var.treat
# 2.226096


### F test fam ----
# F test to compare variances between families 
# (length adjusted per grade)
g.fam.ftest <- var.test(adjusted ~ treatment, data = g.fam.mean.a)
g.fam.ftest  
# F test to compare two variances
# 
# data:  adjusted by treatment
# F = 1.6294, num df = 14, denom df = 14, p-value = 0.3719
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
#   0.54704 4.85333
# sample estimates:
#   ratio of variances 
# 1.629407 

### Fligner ----
fligner.test(adjusted ~ interaction(treatment, family), data = grade.dat.fam)

#### variances ----
g.control.var.fam <- var(subset(g.fam.mean.a, treatment == 'Control', adjusted))
g.control.var.fam
# 0.6359087

grade.var.fam <- var(subset(g.fam.mean.a, treatment == 'Grading', adjusted))
grade.var.fam
# 0.3902702


# Fam Plot ----
# plot family mean lengths comparing control and graded
g.famplot <- ggplot(data = g.fam.mean.a, 
                    aes(x = treatment, 
                        y = adjusted, 
                        label = family, 
                        group = family)) +
            geom_point() + 
            geom_line(aes(color=family)) + 
            scale_y_continuous(limits=c(17,21)) + 
            theme(panel.grid = element_line(),
                  legend.position="none",
                  text = element_text(size=rel(4.5)),
                  plot.title = element_text(size=rel(3.5), hjust = 0.5)) + 
            labs(title="Mean family length in Control and Graded\n (length adjusted per grade)") 
g.famplot

g.famplot + 
  geom_label_repel(data = subset(g.fam.mean.a, treatment %in% c("Control")),
                   nudge_x       = -.2,
                   direction     = "y",
                   hjust         = 1,
                   segment.size  = 0.2,
                   box.padding   = 0.2, 
                   point.padding = 0.1,
                   segment.color = 'grey50') +
  geom_label_repel(data = subset(g.fam.mean.a, treatment %in% c("Grading")),
                   nudge_x       = .2,
                   direction     = "y",
                   hjust         = 1,
                   segment.size  = 0.2,
                   box.padding   = 0.2, 
                   point.padding = 0.1,
                   segment.color = 'grey50')


# ## Create family means for adjusted.t
fam.mean.group=aggregate(grade.dat.fam[c("adjusted.t")],
                    by= grade.dat.fam[c("family","grade")], mean)
fam.mean.group
# 
# # plot family mean lengths comparing control and groups of graded (small, medium, large)
# install.packages("viridis")  # Install
# library("viridis")           # Load
# 
# famplot.group <- ggplot(data = fam.mean.group, aes(x = grade, y = adjusted.t, label = family, group = family, color = family)) +
#   geom_point() + geom_line() + scale_y_continuous(limits=c(17,21)) + theme(panel.grid = element_line()) + labs(title="Mean family length in control and graded (length adjusted per size)") + theme(plot.title = element_text(hjust = 0.5)) + scale_color_viridis(discrete = TRUE)
# famplot.group
# 
# famplot.group + 
#   geom_label_repel(data = subset(fam.mean.group, grade %in% c("Control")),
#                    nudge_x       = -.2,
#                    direction     = "y",
#                    hjust         = 1,
#                    segment.size  = 0.2,
#                    box.padding   = 0.2, 
#                    point.padding = 0.1,
#                    segment.color = 'grey50') +
#   geom_label_repel(data = subset(fam.mean.group, grade %in% c("Small")),
#                    nudge_x       = .2,
#                    direction     = "y",
#                    hjust         = 1,
#                    segment.size  = 0.2,
#                    box.padding   = 0.2, 
#                    point.padding = 0.1,
#                    segment.color = 'grey50')


##### Boxplots for length
# box plot of length for control and grading (adjust per group)
ggplot(subset(grade.dat.fam, treatment %in% "Control"), aes(family, adjusted)) +
  geom_boxplot() + labs(y= "Length", x = "Family") + ggtitle("Lengths for Control (length adjusted per grade)") + theme(plot.title = element_text(hjust = 0.5)) + ylim(5, 29)

ggplot(subset(grade.dat.fam, treatment %in% "Grading"), aes(family, adjusted)) +
  geom_boxplot() + labs(y= "Length", x = "Family") + ggtitle("Lengths for Grading (length adjusted per grade)") + theme(plot.title = element_text(hjust = 0.5)) + ylim(5, 29)

# Boxplots ----
## control (NOT adjusted) ----

# Create data with reordered group levels
g.control_ordered <- g.control    
# Order boxes by median
fam_ordered.cg <- with(g.control,reorder(family, length, median))
g.control_ordered$family <- factor(g.control_ordered$family,
                                   levels = levels(fam_ordered.cg))
# Draw ggplot2 boxplot ordered by median
box.g.control.na <- ggplot(g.control_ordered,                              
                           aes(x = family,
                               y = length)) +
  geom_boxplot() + 
  labs(y= "Length", x = "Family") + 
  ggtitle("Lengths for Grading Control (not adjusted)") + 
  theme(plot.title = element_text(hjust = 0.5))
box.g.control.na

## graded (NOT adjusted) ----
# Create data with reordered group levels
grade_ordered <- grade    
# Order boxes by median
fam_ordered.gg <- with(grade,reorder(family, length, median))
grade_ordered$family <- factor(grade_ordered$family,
                              levels = levels(fam_ordered.gg))
# Draw ggplot2 boxplot ordered by median
box.grade.na <- ggplot(grade_ordered,                              
                      aes(x = family,
                          y = length)) +
  geom_boxplot() + 
  labs(y= "Length", x = "Family") + 
  ggtitle("Lengths for Graded tanks (not adjusted)") + 
  theme(plot.title = element_text(hjust = 0.5))
box.grade.na

## control (adjusted) ----

# Create data with reordered group levels
g.control_ordered.a <- g.control    
# Order boxes by median
fam_ordered.cga <- with(g.control,reorder(family, adjusted, median))
g.control_ordered.a$family <- factor(g.control_ordered.a$family,
                                     levels = levels(fam_ordered.cga))

# Draw ggplot2 boxplot ordered by median
box.g.control.a <- ggplot(g.control_ordered.a,                              
                          aes(x = family,
                              y = adjusted)) +
  geom_boxplot() + 
  labs(y= "Length (adjusted)", x = "Family") + 
  ggtitle("Lengths (adjusted) for Grade Control") + 
  theme(plot.title = element_text(hjust = 0.5))
box.g.control.a

## graded (adjusted)----
# Create data with reordered group levels
grade_ordered.a <- grade    
# Order boxes by median
fam_ordered.gga <- with(grade,reorder(family, adjusted, median))
grade_ordered.a$family <- factor(grade_ordered.a$family,
                                levels = levels(fam_ordered.gga))

# Draw ggplot2 boxplot ordered by median
box.grade.a <- ggplot(grade_ordered.a,                              
                     aes(x = family,
                         y = adjusted)) +
  geom_boxplot() + 
  labs(y= "Length (adjusted)", x = "Family") + 
  ggtitle("Lengths (adjusted) for Graded tanks") + 
  theme(plot.title = element_text(hjust = 0.5))    
box.grade.a





# Histograms ----

ggplot(grade.dat.fam, aes(x=length, color=treatment)) +
  geom_histogram(fill="white", alpha=0.5, position="identity") + labs(x="Length (cm)", y = "Count") +
  ggtitle("Lengths for Grade Control Distribution") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  expand_limits(x = 0, y = 0) +
  theme(axis.text=element_text(size=15),
        title=element_text(size=20),
        legend.text=element_text(size=15))
g.ICCplot


ggplot(grade.dat.fam, aes(x = family, y = length, fill = treatment)) + 
  geom_boxplot() +
  labs(title = "Variability of Treatment vs. Control for Each Family", 
       x = "family", 
       y = "length") +
  theme_minimal()

## control count (NOT adjusted) ----
hist.g.control.na <- ggplot(g.control_ordered, aes(x=length)) + 
  geom_histogram(colour="black", fill="white") +
  labs(x="Length(cm)", y = "Count") +
  ggtitle("Lengths for Grade Control Distribution") + 
  theme(plot.title = element_text(hjust = 0.5)) 
hist.g.control.na

## control density (NOT adjusted) ----
hist.g.control.den.na <- ggplot(g.control_ordered, aes(x=length)) + 
  geom_histogram(aes(y=after_stat(density)), colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  labs(x="Length(cm)", y = "Density") +
  ggtitle("Lengths for Grade Control Distribution") + 
  theme(plot.title = element_text(hjust = 0.5))
hist.g.control.den.na

## grade count (NOT adjusted) ----
hist.grade.na <- ggplot(grade_ordered, aes(x=length)) + 
  geom_histogram(colour="black", fill="white") +
  labs(x="Length(cm)", y = "Count") +
  ggtitle("Lengths for Graded tank Distribution") + 
  theme(plot.title = element_text(hjust = 0.5)) 
hist.grade.na

## grade density (NOT adjusted) ----
hist.grade.den.na <- ggplot(grade_ordered, aes(x=length)) + 
  geom_histogram(aes(y=after_stat(density)), colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  labs(x="Length(cm)", y = "Density") +
  ggtitle("Lengths for Graded tank Distribution") + 
  theme(plot.title = element_text(hjust = 0.5))
hist.grade.den.na



## control count (adjusted) ----
hist.g.control.a <- ggplot(g.control_ordered, aes(x=adjusted)) + 
  geom_histogram(colour="black", fill="white") +
  labs(x="Length(cm)", y = "Count") +
  ggtitle("Lengths for Grade Control Distribution (adjusted)") + 
  theme(plot.title = element_text(hjust = 0.5)) 
hist.g.control.a

## control density (adjusted) ----
hist.g.control.den.a <- ggplot(g.control_ordered, aes(x=adjusted)) + 
  geom_histogram(aes(y=after_stat(density)), colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  labs(x="Length(cm)", y = "Density") +
  ggtitle("Lengths for Grade Control Distribution (adjusted)") + 
  theme(plot.title = element_text(hjust = 0.5))
hist.g.control.den.a

## graded count (adjusted) ----
hist.grade.a <- ggplot(grade_ordered, aes(x=adjusted)) + 
  geom_histogram(colour="black", fill="white") +
  labs(x="Length(cm)", y = "Count") +
  ggtitle("Lengths for Graded tank Distribution (adjusted)") + 
  theme(plot.title = element_text(hjust = 0.5)) 
hist.grade.a

## graded density (adjusted) ----
hist.grade.den.a <- ggplot(grade_ordered, aes(x=adjusted)) + 
  geom_histogram(aes(y=after_stat(density)), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") +
  labs(x="Length(cm)", y = "Density") +
  ggtitle("Lengths for Graded tank Distribution (adjusted)") + 
  theme(plot.title = element_text(hjust = 0.5))
hist.grade.den.a


# Q-Q plots ----
## (NOT adjusted) ----
qq.g.na <- ggplot(grade.dat.fam, aes(sample = length)) +
  stat_qq(aes(color = treatment)) +
  scale_color_manual(values = c("darkgrey", "black"))+
  labs(y = "Length") +
  ggtitle("Q-Q plot for Control and Graded tank Length") + 
  theme(plot.title = element_text(hjust = 0.5))
qq.g.na

## (adjusted) ----
qq.g.a <- ggplot(grade.dat.fam, aes(sample = adjusted)) +
  stat_qq(aes(color = treatment)) +
  scale_color_manual(values = c("darkgrey", "black"))+
  labs(y = "Length") +
  ggtitle("Q-Q plot for Control and Graded tank Length (adjusted)") + 
  theme(plot.title = element_text(hjust = 0.5))
qq.g.a



