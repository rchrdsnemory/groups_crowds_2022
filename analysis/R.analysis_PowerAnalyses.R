### This script contains power analyses for Experiments 2 and 3. Sample for Experiment 1 was
### based on estimated effects from piloting.

library(tidyverse)
library(WebPower)
library(pwr)

##### Power analysis for Experiment 2
## In Exp 1, for QuestionType, partial eta = .532. 
## For AgeGroup*QuestionType, partial eta = .118
## WebPower uses Cohen's f, which for the interaction is sqrt((.118)/(1-.118))=0.3657688

#### WebPower: estimate power for Exp 2 assuming effect sizes similar to Exp 1 
#Observed power for interaction in Exp 1: 95% 
wp.rmanova(n = 120 , ng = 3, nm = 2, f = 0.3657688, nscor = 1, alpha = .05, power = , type = 2)

# Exp 2 required total n for AgeGroup*QuestionType interaction with effect size 
# equal to Exp 1: n=76
wp.rmanova(n =  , ng = 3, nm = 2, f = 0.3657688, nscor = 1, alpha = .05, power = .8, type = 2)

## Exp 2 required total n for AgeGroup*QuestionType interaction with effect size 
## half of Exp 1: n=301
wp.rmanova(n =  , ng = 3, nm = 2, f = .18, nscor = 1, alpha = .05, power = .8, type = 2)

## We use n=120 for Experiment 2, or 40 per age group, which would give us 95% power to 
## detect the same effect size



## Power analysis for Experiment 3
## Our prior studies suggested that n=40 per age group showed large effects of question type. 
# Based on piloting results, we expected the main effect of QuestionType to be be large, 
## while an AgeGroup*QuestionType interaction for the youngest group would be moderate if observed. 
# Cohen (1992) suggested .1, .25, and .40 as small, moderate, and large effect sizes. 


cohens_f<-.3
#### Power to detect f=.3 interaction at n=120: 83.6%
wp.rmanova(n = 120, ng = 3, nm = 2, f = .3, nscor = 1, alpha = .05, power = , type = 2)
#power=0.8358527
#### Main effect detectable at 80% power with n=120:  f=.258
wp.rmanova(n =  120, ng = 3, nm = 2, f =  , nscor = 1, alpha = .05, power =.8 , type = 1)
#0.2578694


























