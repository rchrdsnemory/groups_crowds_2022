########################################################################################
##################      Exp 3: norm check for stim difficulty          #################
########################################################################################
###### Load libraries
library(tidyverse)
library(readxl)
library(lmerTest)
library(readxl)
WOCDiffNorm4 <- read_excel("data/WOC_NormOSF.xlsx")
str(WOCDiffNorm4)

### Norming Experiment
## Data formatting

WOCDiffNorm4_long<-WOCDiffNorm4%>%
  pivot_longer(cols = c("Reason_Avg", "Percept_Avg"), 
               names_to = "Question", values_to = "AvgRating")%>%
  separate(Question,into=c("QuestionType", "Discard"),sep="_")%>%
  dplyr::select(-Discard)
WOCDiffNorm4_long$QuestionType<-factor(WOCDiffNorm4_long$QuestionType, levels = c("Percept", "Reason"))
WOCDiffNorm4_long$Difficulty<-factor(WOCDiffNorm4_long$Difficulty, levels = c("Easy", "Hard"))
WOCDiffNorm4_questions<-WOCDiffNorm4%>%
  pivot_longer(cols = c("Reason_Nim", "Percept_PhotoRealism", "Reason_Sudoku","Reason_Bottle",
                        "Percept_Superballs", "Reason_Mario", "Percept_SpinSpeed",
                        "Percept_Stars"), 
               names_to = "Question", values_to = "Rating")%>%
  separate(Question,into=c("QuestionType", "Question"),sep="_")
WOCDiffNorm4_questions$QuestionType<-factor(WOCDiffNorm4_questions$QuestionType, levels = c("Percept", "Reason"))
WOCDiffNorm4_questions$Difficulty<-factor(WOCDiffNorm4_questions$Difficulty, levels = c("Easy", "Hard"))
WOCDiffNorm4_questions$Question<-factor(WOCDiffNorm4_questions$Question, levels = c("Bottle","Mario","Nim","Sudoku",
                                                                              "PhotoRealism", "Superballs","Stars","SpinSpeed"))
####################################################################################################

###### Difficulty ratings by QuestionType and Expected Difficulty
WOCDiffNorm4_long%>%
  ggplot(aes(x=QuestionType, y=AvgRating, fill=QuestionType))+
  geom_jitter(width=.1, height=0)+geom_boxplot(alpha=.7)+geom_violin(alpha=.3)+
  stat_summary(fun ="mean", geom="label", aes(label=round(..y.., 2)), size=3, fill="red",
               color="black", position=position_dodge(width=.75))+
  scale_y_continuous("AvgRating on 4 items (7='Extremely Difficult')", breaks = c(1,2,3,4,5,6,7))+
  facet_wrap(~Difficulty)+
  theme_minimal()+theme(axis.text.x=element_text(size=13, face="bold", color="black"),
                        axis.title.x=element_text(size=13, face="bold", color="black"),
                        axis.title.y=element_text(size=14, face="bold", color="black"),
                        axis.text.y=element_text(size=16, face="bold", color="black"),
                        legend.text = element_text(size=13, face="bold", color="black"),
                        legend.title = element_blank(),
                        legend.position="bottom",
                        plot.title = element_text(colour="black", size=16, face="bold"),
                        strip.text = element_text(size=16, face="bold", color="black"),
                        strip.background = element_rect(fill="gray82", color="black"))+
  scale_fill_manual(values = c("darkgoldenrod1", "chartreuse4"))+
  ggtitle("'How difficult would it be to answer the question?'")

###### Difficulty ratings by QuestionType and Expected Difficulty
WOCDiffNorm4_questions%>%
  ggplot(aes(x=QuestionType, y=Rating, fill=Question))+facet_grid(~Difficulty)+
  geom_boxplot()+
  stat_summary(fun ="mean", geom="label", aes(label=round(..y.., 2), group=Question), size=3, fill="red",
               color="black", position=position_dodge(width=.75))+
  scale_y_continuous("AvgRating (1=Extremely Easy, 7=Extremely Difficulty)", breaks = c(1,2,3,4,5,6,7))+
  theme_minimal()+theme(axis.text.x=element_text(size=13, face="bold", color="black"),
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
                               "#1C8C42", "#71C573", "#B9E5B1", "#EDF8E8"))+
  ggtitle("'How difficult would it be to answer the question?'")

##############
############## Norming Stimuli using lmer4
##############
####### Mixed model comparisons for question
# Model 1: Fixed effects of Expected Difficulty and Question Rating.Random slope and 
# intercepts for each participant and QuestionType (Percept vs. Reason) 
mod1<-lmer(Rating ~ Difficulty*Question+(1+QuestionType|SubjectNum), REML=F, data=WOCDiffNorm4_questions)
summary(mod1)
# Model 2: Fixed effects of Expected Difficulty and Question Rating.Random intercepts 
# for each participant
mod2<-lmer(Rating ~ Difficulty*Question+(1|SubjectNum), REML=F, data=WOCDiffNorm4_questions)
summary(mod2)
# Model 3: Fixed effects of Expected Difficulty and QuestionType. Random intercepts 
# for each participant
mod3<-lmer(Rating ~ Difficulty*QuestionType+(1|SubjectNum), REML=F, data=WOCDiffNorm4_questions)
summary(mod3)
## Compare the models
anova(mod2, mod1)#Model 1 has the smaller AIC, 1214.6 vs 1217.5, chisq p=.03
anova(mod3, mod1)#Model 1 has the smaller AIC, 1214.6 vs 1238.5, chisq p<.0001















