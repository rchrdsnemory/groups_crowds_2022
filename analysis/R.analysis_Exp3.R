########################################################################################
#################            Exp 3 Group vs Crowd / qType               ################ 
#########   (5 talking, 50 polled / hard perception vs easy reasoning)  ################
########################################################################################
###### Load libraries
library(tidyverse)
library(afex)
library(emmeans)
library(broom)
library(readxl)
WOCExp3 <- read_excel("data/WOC_Exp3OSF.xlsx")

########## Experiment 3
## Data formatting
WOCExp3<-subset(WOCExp3, Exclusion == 0)

WOCExp3<-WOCExp3%>%
  filter(Exclusion == 0)%>%
  separate(Age,into=c("AgeYears","AgeMonths"),sep=";")

# by questionType
WOCExp3_long<-WOCExp3%>%
  pivot_longer(cols = c("Reason_Avg", "Percept_Avg"),
               names_to = "Question", values_to = "AvgRating")%>%
  separate(Question,into=c("QuestionType","Discard"),sep="_")%>%
  select(-Discard)

WOCExp3_long$AgeYears<-as.numeric(WOCExp3_long$AgeYears)
WOCExp3_long$AgeMonths<-as.numeric(WOCExp3_long$AgeMonths)
WOCExp3_long$AgeMonths = (WOCExp3_long$AgeYears)*12+(WOCExp3_long$AgeMonths)
WOCExp3_long<-WOCExp3_long%>%
  mutate(AgeYears = ifelse(is.na(AgeYears), "Adult", AgeYears))
WOCExp3_long$QuestionType<-factor(WOCExp3_long$QuestionType, levels = c("Percept", "Reason"))
WOCExp3_long$AgeGroup<-factor(WOCExp3_long$AgeGroup, levels = c("Younger", "Older", "Adult"))
WOCExp3_long$CB_Order<-factor(WOCExp3_long$CB_Order, levels = c("1", "2", "3"))
WOCExp3_long$CB_Color<-factor(WOCExp3_long$CB_Color, levels = c("TTBlue", "TTGreen"))

# by questionItem
WOCExp3_questions<-WOCExp3%>%
  pivot_longer(cols = c("Reason_Bottle","Reason_Mario","Reason_Nim","Reason_Sudoku",
          "Percept_PhotoRealism", "Percept_Superballs","Percept_Stars","Percept_SpinSpeed"),
               names_to = "Question", values_to = "Rating")%>%
  separate(Question,into=c("QuestionType","Question"),sep="_")

WOCExp3_questions$AgeYears<-as.numeric(WOCExp3_questions$AgeYears)
WOCExp3_questions<-WOCExp3_questions%>%
  mutate(AgeYears = ifelse(is.na(AgeYears), "Adult", AgeYears))
WOCExp3_questions$AgeYears<-factor(WOCExp3_questions$AgeYears, levels = c("7", "8", "9", "10", "Adult"))
WOCExp3_questions$QuestionType<-factor(WOCExp3_questions$QuestionType, levels = c("Percept", "Reason"))
WOCExp3_questions$AgeGroup<-factor(WOCExp3_questions$AgeGroup, levels = c("Younger", "Older", "Adult"))
WOCExp3_questions$CB_Order<-factor(WOCExp3_questions$CB_Order, levels = c("1", "2", "3"))
WOCExp3_questions$CB_Color<-factor(WOCExp3_questions$CB_Color, levels = c("TTBlue", "TTGreen"))
WOCExp3_questions$Question<-factor(WOCExp3_questions$Question, levels = c("Bottle","Mario","Nim","Sudoku",
                                                                                "PhotoRealism", "Superballs","Stars","SpinSpeed"))

########################################################################################
###########################         Exp 3 Visualizations        ########################
########################################################################################

# Boxplots & violin plots of results by Question Type 
WOCExp3_long%>%
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
  scale_fill_manual(values = c("Percept"= "#E9E200","Reason"=  "#1B6FB7"))+
  ggtitle("Information-Seeking Preferences by Question Type: Talking Together, or Answering Alone?")

# Boxplots & violin plots of results by Question & Question Type  
WOCExp3_questions%>%
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
  scale_fill_manual(values = c("#1B6FB7", "#68ADD8", "#BCD7E8","#EFF3FF",
                               "#DFBF00", "#ECE200", "#FFF835","#FEFF9C"))+
  ggtitle("Information-Seeking Preferences by Question: Talking Together, or Answering Alone?")


#############################################################################################
##########################     Wrangling & Statistical Analyses #############################
#############################################################################################

# Means and SDs
WOCExp3_long%>%
  group_by(AgeGroup, QuestionType)%>%
  summarise(Avg = mean(AvgRating), SD = sd(AvgRating))


# Repeated Measures ANOVA
rmAOV_WOC3 <- aov_car(AvgRating ~ AgeGroup + Error(subID/QuestionType), WOCExp3_long)
summary(rmAOV_WOC3)


# Comparisons
emm = emmeans(rmAOV_WOC3, ~ AgeGroup * QuestionType)
pairs(emm, adjust = "bonferroni")

##### Percept: 
#Adult vs. Older:   not sig,  #t=-2.156,  p=0.4814  
#Adult vs. Younger: sig,      #t=-4.516,  p<0.0002 
#Older vs. Younger: not sig,  #t=-2.360,  p=0.2868 
##### Reason
#Adult vs. Older:   not sig,  #t=-1.749,  p=1.0
#Adult vs. Younger: not sig,  #t=-0.854,  p=1.0
#Older vs. Younger: not sig,  #t=0.895,   p=1.0

###### Within AgeGroup Percept vs Reason
#Adult: t=-5.743  p<.0001
#Older: t=-5.318  p<.0001 
#Younger:t=-1.914  p=0.8701

################## Versus Chance

WOC3_Y_R<-WOCExp3_long%>%
  filter(AgeGroup == "Younger" & QuestionType == "Reason")
WOC3_Y_P<-WOCExp3_long%>%
  filter(AgeGroup == "Younger" & QuestionType == "Percept")
WOC3_O_R<-WOCExp3_long%>%
  filter(AgeGroup == "Older" & QuestionType == "Reason")
WOC3_O_P<-WOCExp3_long%>%
  filter(AgeGroup == "Older" & QuestionType == "Percept")
WOC3_A_R<-WOCExp3_long%>%
  filter(AgeGroup == "Adult" & QuestionType == "Reason")
WOC3_A_P<-WOCExp3_long%>%
  filter(AgeGroup == "Adult" & QuestionType == "Percept")

############################## T.tests versus chance 
t.test(WOC3_Y_R$AvgRating, m=2.5)# t = 5.3486, df = 39, p= 4.148e-06 M=3.08 #YoungReason  p<.0001
t.test(WOC3_Y_P$AvgRating, m=2.5)# t = 2.6646, df = 39, p= 0.01115   M=2.80 #YoungPercept p=.01115
t.test(WOC3_O_R$AvgRating, m=2.5)# t = 8.9667, df = 39, p= 5.112e-11 M=3.22 #OlderReason  p<.0001
t.test(WOC3_O_P$AvgRating, m=2.5)# t = -0.55415, df = 39, p= 0.5826  M=2.44 #OlderPercept p=ns
t.test(WOC3_A_R$AvgRating, m=2.5)# t = 3.6356, df = 39, p= 0.0008001 M=2.95 #AdultReason  p<.001
t.test(WOC3_A_P$AvgRating, m=2.5)# t = -3.6171, df = 39, p= 0.000844 M=2.11 #AdultPercept p<.001




























