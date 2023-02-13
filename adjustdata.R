#READ THE DATA 

database <- database <- readRDS("Data/database.rds")

#REARRANGING THE DATA 
database <- database %>% arrange(Respondent_ID) %>% drop_na(PINC) %>% mutate(IncomeMC = PINC - mean(PINC)) 

database <- database %>% arrange(Respondent_ID) %>% drop_na(f006) %>% mutate(NatDayMC = f006 - mean(f006))

summary(database$IncomeMC)

### the parameters will be the same 


# INSPECTING THE DATABASE

#View(database)
#names(database)
#summary(database)

#choice is the choice variable
#table(database$choice)

#  1    2    3 
#2450 2669 6284 how many times each choice has been selected

#Respondent_ID is the identifier variable
#length(unique(database$Respondent_ID))

#1267 the number of respondents


#summary(database$prei_2)
#table(database$wald_1)
#table(database$wald_3)



#RECODING VARIABLES

#Recoding wald_:
# 1 is 10% less 
# 2 is 10% more 
# the function is not linear - is going up and down 

database <- database %>% mutate(wald_1 = case_when(wald_1 == 0 ~ 1, wald_1 == 1 ~ 0,
                                                   wald_1 == 2 ~ 2),
                                wald_2 = case_when(wald_2 == 0 ~ 1, wald_2 == 1 ~ 0,
                                                   wald_2 == 2 ~ 2),
                                wald_3 = 1)
#Recode groe_: 
# 0 --> like today
# 1 --> half as much, 
# 2 --> twice as much

database <- database %>% mutate(groe_1 = case_when(groe_1 == 0 ~ 1, groe_1 == 1 ~ 0,
                                                   groe_1 == 2 ~ 2),
                                groe_2 = case_when(groe_2 == 0 ~ 1, groe_2 == 1 ~ 0,
                                                   groe_2 == 2 ~ 2),
                                groe_3 = 1)

#Recode Understorey in forests - FoUnt_
# 0 --> like today
# 1 --> half as much, 
# 2 --> twice as much

database <- database %>% mutate(FoUnt_1 = case_when(FoUnt_1 == 0 ~ 1, FoUnt_1 == 1 ~ 0,
                                                    FoUnt_1 == 2 ~ 2),
                                FoUnt_2 = case_when(FoUnt_2 == 0 ~ 1, FoUnt_2 == 1 ~ 0,
                                                    FoUnt_2 == 2 ~ 2),
                                FoUnt_3 = 1)

#Recode Coniferous trees - ConSh_
# 1 --> 30% coniferous trees
# 2 --> 70% coniferous trees

database <- database %>% mutate(ConSh_1 = case_when(ConSh_1 == 0 ~ 1, ConSh_1 == 1 ~ 0,
                                                    ConSh_1 == 2 ~ 2),
                                ConSh_2 = case_when(ConSh_2 == 0 ~ 1, ConSh_2 == 1 ~ 0,
                                                    ConSh_2 == 2 ~ 2),
                                ConSh_3 = 1)

#Can be now used to estimate the Model with interaction terms such that it (WTP) is comparable with non-interacting models.
