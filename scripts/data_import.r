import_stata <- function(dir, up_to = c("2020", "2021")) {
        #' Import ED records from stata files
        #' 
        #' Syntax: stata <- import_stata(dir = dir_where_stata_is_stored, up_to = "2021") # "2021" in quotes
        #' @param up_to. Character. Either "2020" or "2021", in quotes.
        
        # con.df <- tibble(
        #         num  = c(0:25, NA, NA, NA, NA, NA),
        #         conS = c("Feb29", "diag", "poi_index", "dv_index", "su_index", "mental_index", "other_heart_disea", "pneumonia_influenza", "down_syn", "oltd", "ckd",  "db", "bronch", "chd", "cerebro_dis", "dementia", "parkinson", "epilepsy", "cerebral_palsy", "cancer", "airway", "MI_index", "stroke_index", "tra_index", "uncap", "no_dis", "cardiac_arrest", "blood_cancer", "resp_cancer", "co_lung_dis", "asthma"),
        #         conL = c("February 29", "sepsis", "poisoning", "domestic violence", "suicide", "mental diseases", "other heart diseases", "pneumonia and influenza", "Down syndrome", "OLTD", "CKD", "diabetes", "bronchitis", "CHD", "cerebrovascular diseases", "dementia", "Parkinson's", "epilepsy", "cerebral palsy", "cancer", "airway diseases", "MI", "stroke", "trauma", "uncaptured disease groups", "no diagnosis", "cardiac arrest", "blood cancers", "respiratory cancers", "co_lung_dis", "asthma")
        # )
        
        ### file names
        up_to      <- match.arg(up_to)
        dta_all    <- c("2020" = "2019_2020_all_20210312.dta", 
                        "2021" = "2019_2020_2021_all_20210727.dta")
        dta_pridis <- c("2020" = "AE_pri_dis_all.dta",
                        "2021" = "AE_pri_dis_20210728.dta")
        dta_all    <- dta_all[up_to]
        dta_pridis <- dta_pridis[up_to]
        
        ### imports
        stata.T1_5.new <- haven::read_dta(file.path(dir,dta_pridis))
        
        stata <- haven::read_dta(file.path(dir,dta_all),
                          col_select = c(AENumber,ReferenceKey,eventdate,pneu,diag))
        
        

        ### joining
        stata <- left_join(
                stata, 
                stata.T1_5.new %>% select(-diag), 
                by = "AENumber"
        )
        
        ### cleaning
        # stata <- stata %>% 
        #         # annotate primary dx
        #         left_join(select(con.df, num, conL), by = c("pri_dis" = "num")) %>% 
        #         rename(pri_disL = conL) %>% 
        #         mutate(tra_index = ifelse(tra_gp == 0, 0, 1))
                
        # unwanted dates
        stata <- stata %>%
                filter(eventdate != "2020-02-29") 
        
        # de-dup censor and reassign death 
        stata <- stata %>% annotate_last_record()
        
        # COVID +ve AENumber list
        covid_cols = c("AENumber","AY","AEtoIPWardDiagnosisHAMDCT","BA","BB","BC",
                       "PrincipalDiagnosisDescription","DiagnosisHAMDCTDescriptionra","BK","BL","BM")
        covid_stata <- read_dta(file.path(dir,dta_all),
                          col_select = all_of(covid_cols))
        covid_code = "519.8:8"
        covid_AENumber_vec = c()
        for(colname in covid_cols){
                covid_AENumber = covid_stata %>% filter(str_detect(eval(parse(text=colname)),covid_code)) %>% pull(AENumber)
                covid_AENumber_vec = c(covid_AENumber_vec,covid_AENumber)
        }
        covid_AENumber_vec = unique(covid_AENumber_vec)
        # number of covid positive entries
        print(length(covid_AENumber_vec))
        # remove COVID +ve entries
        stata = stata %>% filter(!AENumber %in% covid_AENumber_vec)
        
        # remove last visit before death in 2021 Jan entries if only 2020 data is concerned
        if(up_to == "2020"){
                last.visit.Jan21 = read.csv(file.path(dir,"Dec20_censor.csv")) %>%
                        filter(JanLastAtt==1) %>%
                        pull(AENumber)
                stata = stata %>% mutate(death=ifelse(AENumber %in% last.visit.Jan21 & death==1,0,death))
        }
       
        return(stata)
}


annotate_last_record <- function(stata) {
        # find the last record of each patient that died 
        patientDieList <- stata %>%
                
                filter(as.Date(dead_date)-as.Date(eventdate)<=28,as.Date(dead_date)-as.Date(eventdate)>=0) %>%
                group_by(ReferenceKey) %>%
                arrange(eventdate) %>% 
                
                summarise(AENumber = dplyr::last(AENumber), .groups = "drop") %>% 
                pull(AENumber)
        

        stata <- stata %>% 
                mutate(death = ifelse(AENumber %in% patientDieList, 1, 0))
        
        stata
}

import_csv <- function(dir) {
        #' Import ED records from csv files
        #' version 2 of import data
        #' 
        #' Syntax: stata <- import_stata(dir = dir_where_stata_is_stored, up_to = "2021") # "2021" in quotes
        #' @param dir. directory to the .csv
        
        # con.df <- tibble(
        #         num  = c(0:25, NA, NA, NA, NA, NA),
        #         conS = c("Feb29", "diag", "poi_index", "dv_index", "su_index", "mental_index", "other_heart_disea", "pneumonia_influenza", "down_syn", "oltd", "ckd",  "db", "bronch", "chd", "cerebro_dis", "dementia", "parkinson", "epilepsy", "cerebral_palsy", "cancer", "airway", "MI_index", "stroke_index", "tra_index", "uncap", "no_dis", "cardiac_arrest", "blood_cancer", "resp_cancer", "co_lung_dis", "asthma"),
        #         conL = c("February 29", "sepsis", "poisoning", "domestic violence", "suicide", "mental diseases", "other heart diseases", "pneumonia and influenza", "Down syndrome", "OLTD", "CKD", "diabetes", "bronchitis", "CHD", "cerebrovascular diseases", "dementia", "Parkinson's", "epilepsy", "cerebral palsy", "cancer", "airway diseases", "MI", "stroke", "trauma", "uncaptured disease groups", "no diagnosis", "cardiac arrest", "blood cancers", "respiratory cancers", "co_lung_dis", "asthma")
        # )
        
        ### file names
        stata = read.csv(dir)
        
        # unwanted dates
        # stata <- stata %>%
        #         filter(eventdate != "2020-02-29") 
        
        # de-dup censor and reassign death 
        stata <- stata %>% annotate_last_record2()
        
        # # COVID +ve AENumber list
        # covid_cols = c("DR1","DR2","DR3","DR4","DR5",
        #                "DR6","DR7","DR8","DR9","DR10")
        # covid_code = "519.8:8"
        # covid_AENumber_vec = c()
        # for(colname in covid_cols){
        #         covid_AENumber = stata %>% filter(str_detect(eval(parse(text=colname)),covid_code)) %>% pull(AENumber)
        #         covid_AENumber_vec = c(covid_AENumber_vec,covid_AENumber)
        # }
        # covid_AENumber_vec = unique(covid_AENumber_vec)
        # # number of covid positive entries
        # print(paste("total covid positive entires:",length(covid_AENumber_vec)))
        # # remove COVID +ve entries
        # stata = stata %>% filter(!AENumber %in% covid_AENumber_vec)
        
        return(stata)
}


annotate_last_record2 <- function(stata) {
        # find the last record of each patient that died 
        patientDieList <- stata %>%
                
                filter(as.Date(DateofRegisteredDeath)-as.Date(eventdate)<=28,
                       as.Date(DateofRegisteredDeath)-as.Date(eventdate)>=0) %>%
                group_by(ReferenceKey) %>%
                arrange(eventdate) %>% 
                
                summarise(AENumber = dplyr::last(AENumber), .groups = "drop") %>% 
                pull(AENumber)
        
        
        stata <- stata %>% 
                mutate(death = ifelse(AENumber %in% patientDieList, 1, 0))
        
        stata
}