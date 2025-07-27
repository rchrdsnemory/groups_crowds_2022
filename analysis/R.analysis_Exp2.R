########################################################################################
##################      Exp 2 Group vs Crowd (5 talking, 50 polled)     ################
########################################################################################
###### Load libraries
library(tidyverse)
library(afex)
library(emmeans)
library(broom)
library(readxl)
WOCExp2 <- read_excel("data/WOC_Exp2OSF.xlsx")

########## Experiment 2
## Data formatting

WOCExp2<-subset(WOCExp2, Exclusion == 0)

WOCExp2<-WOCExp2%>%
  separate(Age,into=c("AgeYears","AgeMonths"),sep=";")

# by questionType
WOCExp2_long<-WOCExp2%>%
  pivot_longer(cols = c("Reason_Avg", "Popular_Avg"),
               names_to = "Question", values_to = "AvgRating")%>%
  separate(Question,into=c("QuestionType","Discard"),sep="_")%>%
  select(-Discard)

WOCExp2_long$AgeYears<-as.numeric(WOCExp2_long$AgeYears)
WOCExp2_long$AgeMonths<-as.numeric(WOCExp2_long$AgeMonths)
WOCExp2_long$AgeMonths = (WOCExp2_long$AgeYears)*12+(WOCExp2_long$AgeMonths)
WOCExp2_long<-WOCExp2_long%>%
  mutate(AgeYears = ifelse(is.na(AgeYears), "Adult", AgeYears))
WOCExp2_long$QuestionType<-factor(WOCExp2_long$QuestionType, levels = c("Popular", "Reason"))
WOCExp2_long$AgeGroup<-factor(WOCExp2_long$AgeGroup, levels = c("Younger", "Older", "Adult"))
WOCExp2_long$CB_Order<-factor(WOCExp2_long$CB_Order, levels = c("1", "2", "3"))
WOCExp2_long$CB_Color<-factor(WOCExp2_long$CB_Color, levels = c("TTBlue", "TTGreen"))

# by_questionItem
WOCExp2_questions<-WOCExp2%>%
  pivot_longer(cols = c("Reason_Nim", "Popular_Color", "Reason_Sudoku", "Popular_Day",
                        "Reason_Mario", "Popular_Fruit", "Reason_Bottle", "Popular_Pizza"),
               names_to = "Question", values_to = "Rating")%>%
  separate(Question,into=c("QuestionType","Question"),sep="_")

WOCExp2_questions$AgeYears<-as.numeric(WOCExp2_questions$AgeYears)
WOCExp2_questions<-WOCExp2_questions%>%
  mutate(AgeYears = ifelse(is.na(AgeYears), "Adult", AgeYears))
WOCExp2_questions$AgeYears<-factor(WOCExp2_questions$AgeYears, levels = c("7", "8", "9", "10", "Adult"))
WOCExp2_questions$QuestionType<-factor(WOCExp2_questions$QuestionType, levels = c("Popular", "Reason"))
WOCExp2_questions$AgeGroup<-factor(WOCExp2_questions$AgeGroup, levels = c("Younger", "Older", "Adult"))
WOCExp2_questions$CB_Order<-factor(WOCExp2_questions$CB_Order, levels = c("1", "2", "3"))
WOCExp2_questions$CB_Color<-factor(WOCExp2_questions$CB_Color, levels = c("TTBlue", "TTGreen"))
WOCExp2_questions$Question<-factor(WOCExp2_questions$Question, levels = c("Color", "Day","Fruit","Pizza",
                                                                     "Bottle","Mario","Nim","Sudoku"))

########################################################################################
###########################         Exp 2 Visualizations        ########################
########################################################################################


# Boxplots & violin plots of results by Question Type 
WOCExp2_long%>%
  ggplot(aes(x = QuestionType, y = AvgRating, fill=QuestionType))+
  geom_jitter(width=.1, height=0)+geom_violin(alpha=.3)+
  geom_line(aes(group=subID))+
  geom_boxplot(alpha=.8)+
  stat_summary(fun ="mean", geom="label", 
               aes(label=round(..y.., 2), group=QuestionType), size=5, fill="red",
               color="black", position=position_dodge(width=.75))+
  scale_y_continuous("AvgRating of 4 items (4=Def TT)")+
  facet_grid(~AgeGroup, labeller=
               labeller(AgeGroup = as_labeller(c('Younger' = "Younger(n=40): Ages 7-8",
                                                 'Older' = "Older(n=40): Ages 9-10", 
                                                 'Adult' = "Adult(n=40): MTurk"))))+
  theme_minimal()+theme(axis.text.x=element_blank(),
                        axis.title.x=element_text(size=13, face="bold", color="black"),
                        axis.title.y=element_text(size=14, face="bold", color="black"),
                        axis.text.y=element_text(size=16, face="bold", color="black"),
                        legend.text = element_text(size=13, face="bold", color="black"),
                        legend.title = element_blank(),
                        legend.position="bottom",
                        plot.title = element_text(colour="black", size=16, face="bold"),
                        strip.text = element_text(size=16, face="bold", color="black"),
                        strip.background = element_rect(fill="gray82", color="black"))+
  scale_fill_manual(values = c("Popular"= "#1C8C42", "Reason"=  "#1B6FB7"))+
  ggtitle("Information-Seeking Preferences by Question Type: Talking Together, or Answering Alone?")

# Boxplots & violin plots of results by Question & Question Type  
WOCExp2_questions%>%
  ggplot(aes(x=QuestionType, y=Rating, fill=Question))+facet_grid(~AgeGroup)+
  geom_hline(aes(yintercept=2.5), color = "black", linetype="dashed", size=1.1)+
  geom_boxplot()+
  stat_summary(fun ="mean", geom="label", aes(label=round(..y.., 2), group=Question), size=5, fill="red",
               color="black", position=position_dodge(width=.75))+
  scale_y_continuous("Rating By Question (1=Alone, 4=Group Talk)")+
  facet_grid(~AgeGroup, labeller=
               labeller(AgeGroup = as_labeller(c('Younger' = "Younger(n=40): Ages 7-8",
                                                 'Older' = "Older(n=40): Ages 9-10",
                                                 'Adult' = "Adult(n=40): MTurk"))))+
  theme_minimal()+theme(axis.text.x=element_blank(),
                        axis.title.x=element_text(size=13, face="bold", color="black"),
                        axis.title.y=element_text(size=14, face="bold", color="black"),
                        axis.text.y=element_text(size=16, face="bold", color="black"),
                        legend.text = element_text(size=13, face="bold", color="black"),
                        legend.title = element_blank(),
                        legend.position="bottom",
                        plot.title = element_text(colour="black", size=16, face="bold"),
                        strip.text = element_text(size=16, face="bold", color="black"),
                        strip.background = element_rect(fill="gray82", color="black"))+
  scale_fill_manual(values = c("#1C8C42", "#71C573", "#B9E5B1", "#EDF8E8",
                               "#1B6FB7", "#68ADD8", "#BCD7E8","#EFF3FF"))+
  ggtitle("Information-Seeking Preferences by Question: Talking Together, or Answering Alone?")

#############################################################################################
##########################     Wrangling & Statistical Analyses #############################
#############################################################################################

# Means and SDs
WOCExp2_long%>%
  group_by(AgeGroup, QuestionType)%>%
  summarise(Avg = mean(AvgRating), SD = sd(AvgRating))

# Repeated Measures ANOVA
rmAOV_WOC2 <- aov_car(AvgRating ~ AgeGroup + Error(subID/QuestionType), WOCExp2_long)
summary(rmAOV_WOC2)

# Comparisons
emm = emmeans(rmAOV_WOC2, ~ AgeGroup * QuestionType)
pairs(emm, adjust = "bonferroni")

##### Popular: 
#Adult vs. Older: not sig #t=-1.995   p=0.7081 
#Adult vs. Younger: sig,  #t=-5.334,  p<0.0001
#Older vs. Younger: sig,  #t=-11.883, p<0.0001 
##### Reason
#Adult vs. Older: not sig #t=-0.911,  p=1.0
#Adult vs. Younger: sig,  #t=-0.911,  p=1.0
#Older vs. Younger: sig,  #t=0.000,  p=1.0

###### Within AgeGroup Popular vs Reason
#Adult: t=-13.057, p<0.0001 
#Older: t=-11.965, p<0.0001
#Younger:t=-8.603, p<0.0001

################## Versus Chance
View(WOC2_Y_R)
WOC2_Y_R<-WOCExp2_long%>%
  filter(AgeGroup == "Younger" & QuestionType == "Reason")
WOC2_Y_P<-WOCExp2_long%>%
  filter(AgeGroup == "Younger" & QuestionType == "Popular")
WOC2_O_R<-WOCExp2_long%>%
  filter(AgeGroup == "Older" & QuestionType == "Reason")
WOC2_O_P<-WOCExp2_long%>%
  filter(AgeGroup == "Older" & QuestionType == "Popular")
WOC2_A_R<-WOCExp2_long%>%
  filter(AgeGroup == "Adult" & QuestionType == "Reason")
WOC2_A_P<-WOCExp2_long%>%
  filter(AgeGroup == "Adult" & QuestionType == "Popular")

############################## T.tests versus chance 
t.test(WOC2_Y_R$AvgRating, m=2.5)# t = 9.2938, df = 39, p =1.952e-11       #YoungReason  p<.0001
t.test(WOC2_Y_P$AvgRating, m=2.5)# t = -3.3801, df = 39, p = 0.001657      #YoungPopular p<.0001
t.test(WOC2_O_R$AvgRating, m=2.5)# t = 11.748, df = 39, p = 2.207e-14      #OlderReason  p<.0001
t.test(WOC2_O_P$AvgRating, m=2.5)# t = -8.5231, df = 39, p = 1.925e-10     #OlderPopular p<.0001
t.test(WOC2_A_R$AvgRating, m=2.5)# t = 6.3736, df = 39, p = 1.569e-07      #AdultReason  p<.0001
t.test(WOC2_A_P$AvgRating, m=2.5)# t = -11.753, df = 39, p = 2.178e-14     #AdultPopular p<.0001

############################## For Comprehension items
# Subset data
WOC_Study2_Y<-WOCExp2%>%
  filter(AgeGroup == "Younger")
WOC_Study2_O<-WOCExp2%>%
  filter(AgeGroup == "Older")
WOC_Study2_A<-WOCExp2%>%
  filter(AgeGroup == "Adult")

# Run binomial tests vs. chance for each age group and both questions
binom.test(sum(WOC_Study2_Y$Comp_TT),
           length(WOC_Study2_Y$Comp_TT), 
           p=.5, alternative = "greater")  #21/40=52.5%, p=0.4373
binom.test(sum(WOC_Study2_Y$Comp_AA),
           length(WOC_Study2_Y$Comp_AA), 
           p=.5, alternative = "greater") #27/40=67.5%, p=0.01924
binom.test(sum(WOC_Study2_O$Comp_TT),
           length(WOC_Study2_O$Comp_TT), 
           p=.5, alternative = "greater") #36/40=90%, p=9.285e-08
binom.test(sum(WOC_Study2_O$Comp_AA),
           length(WOC_Study2_O$Comp_AA), 
           p=.5, alternative = "greater") #37/40=92.5%, p=9.733e-09
binom.test(sum(WOC_Study2_A$Comp_TT),
           length(WOC_Study2_A$Comp_TT), 
           p=.5, alternative = "greater") #36/40=90%, p=9.285e-08
binom.test(sum(WOC_Study2_A$Comp_AA),
           length(WOC_Study2_A$Comp_AA), 
           p=.5, alternative = "greater")  #36/40=90%, p=9.285e-08








