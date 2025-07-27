########################################################################################
##################      Exp 1 Group vs Crowd (5 talking, 5 polled)     #################
########################################################################################
###### Load libraries
library(tidyverse)
library(afex)
library(emmeans)
library(broom)
library(readxl)
WOCExp1 <- read_excel("data/WOC_Exp1OSF.xlsx")


########## Experiment 1
## Data formatting

WOCExp1<-WOCExp1%>%
  separate(Age,into=c("AgeYears","AgeMonths"),sep=";")

# by questionType
WOCExp1_long<-WOCExp1%>%
  pivot_longer(cols = c("Reason_Avg", "Popular_Avg"),
               names_to = "Question", values_to = "AvgRating")%>%
  separate(Question,into=c("QuestionType","Discard"),sep="_")%>%
  select(-Discard)
str(WOCExp1)
WOCExp1_long$AgeYears<-as.numeric(WOCExp1_long$AgeYears)
WOCExp1_long$AgeMonths<-as.numeric(WOCExp1_long$AgeMonths)
WOCExp1_long$AgeMonths = (WOCExp1_long$AgeYears)*12+(WOCExp1_long$AgeMonths)
WOCExp1_long<-WOCExp1_long%>%
  mutate(AgeYears = ifelse(is.na(AgeYears), "Adult", AgeYears))
WOCExp1_long$QuestionType<-factor(WOCExp1_long$QuestionType, levels = c("Popular", "Reason"))
WOCExp1_long$AgeGroup<-factor(WOCExp1_long$AgeGroup, levels = c("Younger", "Older", "Adult"))
WOCExp1_long$CB_Order<-factor(WOCExp1_long$CB_Order, levels = c("1", "2", "3"))
WOCExp1_long$CB_Color<-factor(WOCExp1_long$CB_Color, levels = c("TTBlue", "TTGreen"))

# by questionItem
WOCExp1_questions<-WOCExp1%>%
  pivot_longer(cols = c("Reason_Nim", "Popular_Color", "Reason_Sudoku", "Popular_Day",
                        "Reason_Mario", "Popular_Fruit", "Reason_Bottle", "Popular_Pizza"),
               names_to = "Question", values_to = "Rating")%>%
  separate(Question,into=c("QuestionType","Question"),sep="_")

WOCExp1_questions$AgeYears<-as.numeric(WOCExp1_questions$AgeYears)
WOCExp1_questions<-WOCExp1_questions%>%
  mutate(AgeYears = ifelse(is.na(AgeYears), "Adult", AgeYears))
WOCExp1_questions$AgeYears<-factor(WOCExp1_questions$AgeYears, levels = c("7", "8", "9", "10", "Adult"))
WOCExp1_questions$QuestionType<-factor(WOCExp1_questions$QuestionType, levels = c("Popular", "Reason"))
WOCExp1_questions$AgeGroup<-factor(WOCExp1_questions$AgeGroup, levels = c("Younger", "Older", "Adult"))
WOCExp1_questions$CB_Order<-factor(WOCExp1_questions$CB_Order, levels = c("1", "2", "3"))
WOCExp1_questions$CB_Color<-factor(WOCExp1_questions$CB_Color, levels = c("TTBlue", "TTGreen"))
WOCExp1_questions$Question<-factor(WOCExp1_questions$Question, levels = c("Color", "Day","Fruit","Pizza",
                                                                     "Bottle","Mario","Nim","Sudoku"))


########################################################################################
###########################         Exp 1 Visualizations        ########################
########################################################################################


# Boxplots & violin plots of results by Question Type 
WOCExp1_long%>%
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
  scale_fill_manual(values = c("Popular"= "#1C8C42", "Percept"= "#E9E200","Reason"=  "#1B6FB7"))+
  ggtitle("Information-Seeking Preferences by Question Type: Talking Together, or Answering Alone?")

# Boxplots & violin plots of results by Question & Question Type  
WOCExp1_questions%>%
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
WOCExp1_long%>%
  group_by(AgeGroup, QuestionType)%>%
  summarise(Avg = mean(AvgRating), SD = sd(AvgRating))

# Repeated Measures ANOVA
rmAOV_WOC <- aov_car(AvgRating ~ AgeGroup + Error(subID/QuestionType), WOCExp1_long)
summary(rmAOV_WOC)
# Comparisons
emm = emmeans(rmAOV_WOC, ~ AgeGroup * QuestionType)
pairs(emm, adjust = "bonferroni")

##### Popular: 
#Adult vs. Older: not sig # t= -0.718, p=1.0
#Adult vs. Younger: sig, #t=-4.067, p=.001
#Older vs. Younger: sig, #t=-3.350, p=0.0143 
##### Reason
#Adult vs. Older: not sig # t=1.635,  p=1.0
#Adult vs. Younger: sig, #t=2.153,  p=0.4857
#Older vs. Younger: sig, #t=0.518,  p=1.0

###### Within AgeGroup Popular vs Reason
#Adult: t=-9.201, p<.0001 
#Older: t=-7.105, p<.0001
#Younger:t=-3.659, p<.0.0057

################## Versus Chance

WOC_Y_R<-WOCExp1_long%>%
  filter(AgeGroup == "Younger" & QuestionType == "Reason")
WOC_Y_P<-WOCExp1_long%>%
  filter(AgeGroup == "Younger" & QuestionType == "Popular")
WOC_O_R<-WOCExp1_long%>%
  filter(AgeGroup == "Older" & QuestionType == "Reason")
WOC_O_P<-WOCExp1_long%>%
  filter(AgeGroup == "Older" & QuestionType == "Popular")
WOC_A_R<-WOCExp1_long%>%
  filter(AgeGroup == "Adult" & QuestionType == "Reason")
WOC_A_P<-WOCExp1_long%>%
  filter(AgeGroup == "Adult" & QuestionType == "Popular")

############################## T.tests versus chance
t.test(WOC_Y_R$AvgRating, m=2.5)# t = 7.4239, df = 39, p-value = 5.651e-09     #YoungReason  p<.001
t.test(WOC_Y_P$AvgRating, m=2.5)# t = -0.23161, df = 39, p-value = 0.818       #YoungPopular n.s.
t.test(WOC_O_R$AvgRating, m=2.5)# t = 8.5333, df = 39, p-value = 1.867e-10     #OlderReason  p<.001
t.test(WOC_O_P$AvgRating, m=2.5)# t = -4.0755, df = 39, p-value = 0.0002184    #OlderPopular p<.001
t.test(WOC_A_R$AvgRating, m=2.5)# t = 11.237, df = 39, p-value = 8.509e-14     #AdultReason  p<.001
t.test(WOC_A_P$AvgRating, m=2.5)# t = -5.2385, df = 39, p-value = 5.885e-06    #AdultPopular p<.001

############################## For Comprehension items
# Subset data
WOCExp1_Y<-WOCExp1%>%
  filter(AgeGroup == "Younger")
WOCExp1_O<-WOCExp1%>%
  filter(AgeGroup == "Older")
WOCExp1_A<-WOCExp1%>%
  filter(AgeGroup == "Adult")

# Run binomial tests vs. chance for each age group and both questions
binom.test(sum(WOCExp1_Y$Comp_TT),
           length(WOCExp1_Y$Comp_TT), 
           p=.5, alternative = "greater")  #28/40=70%, p=.008295
binom.test(sum(WOCExp1_Y$Comp_AA),
           length(WOCExp1_Y$Comp_AA), 
           p=.5, alternative = "greater") #26/40=65%, p=0.04035
binom.test(sum(WOCExp1_O$Comp_TT),
           length(WOCExp1_O$Comp_TT), 
           p=.5, alternative = "greater") #34/40, p=4.182e-06
binom.test(sum(WOCExp1_O$Comp_AA),
           length(WOCExp1_O$Comp_AA), 
           p=.5, alternative = "greater") #35/40, p=6.913e-07
binom.test(sum(WOCExp1_A$Comp_TT),
           length(WOCExp1_A$Comp_TT), 
           p=.5, alternative = "greater") #35/40, p=6.913e-07
binom.test(sum(WOCExp1_A$Comp_AA),
           length(WOCExp1_A$Comp_AA), 
           p=.5, alternative = "greater") #37/40, p=9.733e-09



























