# death cause codes
# death_cause_conditions = list("MI_index==1" = c("I21"),
#              "chd==1" = c("I20","I21","I22","I23","I24","I25"),
#              "cerebro_dis==1" = c("I6"),
#              "stroke_index==1" = c("I635","I634","I633"),
#              "db==1" = c("E10","E11","E12","E13","E14"),
#              "other_heart_disea==1|MI_index==1|chd==1"=
#                c("I110","I130","I132","I200","I209","I208","I253","I255","I256","I259",
#                  "I21","I250","I251","I258","I50","I254","I260","I461"),
#              "pneu==1"=c("J12","J13","J14","J15","J16","J17","J18")
#              )
# death_cause_conditionTextual = c("MI w/ death causes",
#                                  "CHD w/ death causes",
#                                  "CVD w/ death causes",
#                                  "Stroke w/ death causes",
#                                  "Diabetes w/ death causes",
#                                  "Katsoulis' death causes",
#                                  "Pneumonia w/ death causes")

# multiple diagnosis codes
mul_conditions = c("cardiac_arrest diag poi_index dv_index su_index mental_index other_heart_disea pneumonia_influenza down_syn oltd ckd  db bronch
                      chd cerebro_dis dementia parkinson epilepsy cerebral_palsy cancer airway MI_index stroke_index pneu")
mul_conditions = scan(text = mul_conditions, what = "")
mul_conditions = mul_conditions
for (i in 1:length(mul_conditions)){
  cond = mul_conditions[i]
  mul_conditions[i] = paste(cond,"==1")
}
mul_conditionsTextual = c("Cardiac Arrest", "Sepsis", "Poisoning", "Domestic Violence", "Suicide", "Mental illness", "Other heart diseases", 
                          "Pneumonia and Influenza", "Down Syndrome", "Other Learning ", "Chronic Kidney Diseases", "Diabetes", "Bronchitis",
                          "Coronary Heart Disease","Cerebrovascular Diseases","Dementia","Parkinson's","Epilepsy","Cerebral Palsy","Cancer",
                          "Airway","Myocardial Infarction","Stroke","Pneumonia")
# demographics
demo_conditions = c("age>= 65 & Sex_num == 1","age >= 65 & Sex_num == 0",
                    "age>=55 & age<65 & Sex_num==1","age>=55 & age<65 & Sex_num==0",
                    "age>=45 & age <55 & Sex_num==1","age>=45 & age <55 & Sex_num==0",
                  "age>=35 & age <45 & Sex_num==1","age>=35 & age <45 & Sex_num==0",
                    "age>=18 & age <35 & Sex_num==1","age>=18 & age <35 & Sex_num==0",
                    "age>=0 & age <18 & Sex_num==1","age>=0 & age <18 & Sex_num==0")
demo_conditionsTextual = c("65+ & M","65+ & F",
                           "55-64 & M","55-64 & F",
                           "45-54 & M","45-54 & F",
                           "35-44 & M","35-44 & F",
                           "18-34 & M","18-34 & F",
                           "0-17 & M","0-17 & F")
conditions = paste(paste0(demo_conditions,"~\"",demo_conditionsTextual,"\";"),collapse ='')

# elderly
elderly_conditions =  c("age>= 65 & old_home == 1","age >= 65 & old_home == 0")
elderly_conditionsTextual = c("65+ RCHE","65+ non-RCHE")


# heart conditions
# heart_conditions = c("cardiac_arrest == 1", "chd == 1", "MI_index == 1", "other_heart_disea == 1",
#                "other_heart_disea == 1 | MI_index == 1 | chd == 1", "other_heart_disea == 1 | MI_index == 1 | chd == 1 | cardiac_arrest == 1")
# heart_conditionsTextual = c("cardiac_arrest", "chd", "MI", "other heart diseases", "chd, MI, other heart diseases", "chd, MI, other heart diseases and cardiac_arrest")
# 
# elix_conditions = c("CHF_dx","Arrhythmia_dx","Valvular_dx","PHTN_dx","PVD_dx",
# "HTN_dx","HTNcx_dx","Paralysis_dx","NeuroOther_dx", 
# "Pulmonary_dx","DM_dx","DMcx_dx","Hypothyroid_dx" ,
# "Renal_dx","Liver_dx","PUD_dx","HIV_dx",        
# "Lymphoma_dx","Mets_dx","Tumor_dx","Rheumatic_dx",   
# "Coagulopathy_dx","Obesity_dx","WeightLoss_dx","FluidsLytes_dx",
# "BloodLoss_dx","Anemia_dx","Alcohol_dx","Drugs_dx",      
# "Psychoses_dx","Depression_dx")
# for (i in 1:length(elix_conditions)){
#   cond = elix_conditions[i]
#   elix_conditions[i] = paste(cond,"==1")
# }
# 
# deyo_conditions = c("AENumber", "MI_dx", "CHF_dx", "PVD_dx", "Stroke_dx", "Dementia_dx", 
#                     "Pulmonary_dx", "Rheumatic_dx", "PUD_dx", "LiverMild_dx", "DM_dx", 
#                     "DMcx_dx", "Paralysis_dx", "Renal_dx", "Cancer_dx", "LiverSevere_dx", 
#                     "Mets_dx", "HIV_dx")
# for (i in 1:length(deyo_conditions)){
#   cond = deyo_conditions[i]
#   deyo_conditions[i] = paste(cond,"==1")
# }

