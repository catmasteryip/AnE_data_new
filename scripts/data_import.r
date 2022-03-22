
import_csv <- function(dir) {
        #' Import ED records from csv files
        #' 
        #' Syntax: df <- import_csv(dir = dir_where_csv_is_stored) 
        #' @param dir. directory to the .csv
        #' @return a dataframe object that only transforms death col, 
        #' such that death == 1 only for dead patients' last visits
        
        ### file names
        stata = read.csv(dir)
        
        # de-dup censor and reassign death 
        stata <- stata %>% annotate_last_record2()
        
        return(stata)
}


annotate_last_record2 <- function(stata) {
        #' Find the last record of each patient that died 
        #' 
        #' @param stata. a dataframe object that contains death col, 
        #' i.e. death status of patients
        #' 
        #' @return a dataframe object that only transforms death col, 
        #' such that death == 1 only for dead patients' last visits
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