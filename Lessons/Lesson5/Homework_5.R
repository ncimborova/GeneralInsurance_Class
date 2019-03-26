# ištalacie a načítanie knižnice pre balíčky ktoré potrebujeme
install.packages("dplyr")
install.packages("lubridate")
install.packages("ggplot2")

library(dplyr)
library(lubridate)
library(ggplot2)

# ešte z hodiny , načítanie dát
dt_Policy <- read.csv("C:/GeneralInsurance_Class-master/Data/lesson5_PolicyHistory.csv") %>% distinct(NrPolicy, NrObject, .keep_all = TRUE) 
# dt_Policy %>% nrow()
# dt_Policy %>% distinct(NrPolicy, NrObject) %>% nrow() 

dt_Claims <- read.csv("C:/GeneralInsurance_Class-master/Data/lesson5_Claims.csv") %>% distinct(NrClaim, .keep_all = TRUE)
# dt_Claims %>% nrow()
# dt_Claims %>% distinct(NrClaim) %>% nrow()

# spojenie dát
dt_pol_w_claims <- left_join(dt_Policy, 
                             dt_Claims, 
                             by = c("NrPolicy", "NrObject")
)

# začiatok tabulky
head(dt_pol_w_claims)

dt_pol_w_claims %>% group_by(is.na(Paid)) %>% summarise(cnt = n())

dt_pol_w_claims %>% filter(!is.na(Paid)) %>% select(Paid, Dt_Exp_Start, Dt_Exp_End) %>% arrange(desc(Paid)) %>% head()

dt_pol_w_claims <- 
  dt_pol_w_claims %>% mutate(Time_Exposure = lubridate::dmy(Dt_Exp_End) - lubridate::dmy(Dt_Exp_Start))

dt_pol_w_claims %>% filter(!is.na(Paid)) %>% select(Paid, Dt_Exp_Start, Dt_Exp_End, Time_Exposure)  %>% arrange(desc(Paid)) %>% head()

# Burning_Cost
dt_pol_w_claims <- 
  dt_pol_w_claims %>% 
  mutate(Ult_Loss = Paid + Reserves,
         Burning_Cost = ifelse(is.na(Ult_Loss), 0,  Ult_Loss / as.integer(Time_Exposure))
  )

dt_pol_w_claims %>% 
  filter(!is.na(Paid)) %>% 
  select(Paid, Reserves, Ult_Loss, Burning_Cost) %>% head()
##########################################################################
# One-Way analysis
# miesto Veh_type2 máme D_age
dt_pol_w_claims %>% 
  ggplot(aes(y = Burning_Cost, x = D_age)) + 
  geom_jitter()

dt_pol_w_claims %>% 
  group_by(D_age) %>% 
  summarise(BC_avg = mean(Burning_Cost, na.rm  = TRUE),
            BC_median = median(Burning_Cost, na.rm = TRUE),
            cnt = n()) %>% 
  arrange(desc(BC_avg))


dt_pol_w_claims %>% 
  ggplot(aes(y = Burning_Cost, x = D_age)) + 
  geom_boxplot() +
  ylim(0, 100)

summary(dt_pol_w_claims)

#najvyssi Burning cost je cca pri roku 45 a okolie, čo je asi sposobené z dvoch dovodov, "druha puberta sa blíži" a požičiavania aut potomkom
# ročníky ,kde je malý počet dát ničia našu analýzu
##########################################################################
# miesto Veh_type2 máme D_age_banded

dt_pol_w_claims %>% 
  ggplot(aes(y = Burning_Cost, x = D_age_banded)) + 
  geom_jitter()

dt_pol_w_claims %>% 
  group_by(D_age_banded) %>% 
  summarise(BC_avg = mean(Burning_Cost, na.rm  = TRUE),
            BC_median = median(Burning_Cost, na.rm = TRUE),
            cnt = n()) %>% 
  arrange(desc(BC_avg))


dt_pol_w_claims %>% 
  ggplot(aes(y = Burning_Cost, x = D_age_banded)) + 
  geom_boxplot() +
  ylim(0, 100)

summary(dt_pol_w_claims$D_age_banded)

# tak ako pri D_age hlavne vekove skupiny <40,45> , <45,50> , <50,55> je vyssi Burning_Cost, sposobene pre ten isty dovod ako pri D_age zrejme

##########################################################################
# GLM model 
model_1 <- glm(data = dt_pol_w_claims %>% filter(Burning_Cost != 0, Burning_Cost < 100),
              formula = Burning_Cost ~ D_age,
              family = Gamma())

summary(model_1)

model_2 <- glm(data = dt_pol_w_claims %>% filter(Burning_Cost != 0, Burning_Cost < 100),
              formula = Burning_Cost ~ D_age_banded,
              family = Gamma())

summary(model_2)

model_3 <- glm(data = dt_pol_w_claims %>% filter(Burning_Cost != 0, Burning_Cost < 100),
              formula = Burning_Cost ~ D_age + D_age_banded,
              family = Gamma())

summary(model_3)
##########################################################################

# D_age a D_age_banded su v podstate skoro také isté, ale od oboch závisí Burning_Cost
# vek je statisticky vyznamny, čo som si aj myslela a preto som si vybrala tieto dva stlpce ...Model je potom prediktívny
# však na predikovanie burning cost by sa mohlo nájst asi aj niečo bhodnejšie
# <40,45> je najrizikovejšia vekova skupina
# na grafoch je vela outlieorov