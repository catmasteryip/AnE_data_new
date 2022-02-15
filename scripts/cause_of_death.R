import_cause_of_death <- function(dir, stata) {
        
        ### import AE death causes
        filelist <- list.files(path = dir, pattern = "*AE_Attendance_Mortality.*.csv")
        filelist <- file.path(dir, filelist)
        
        # save the files in a for loop
        out <- list()
        for (i in 1:8) {
                out[[length(out)+1]] <- read_csv(filelist[i], col_types = cols()) %>% mutate(Q = i)
        }
        AEcod <- bind_rows(out) %>%
                setNames(c("ReferenceKey", "AE.cod", "AE.cod_supp", "Q")) %>% 
                filter(!is.na(ReferenceKey)) %>% 
                # duplicate rows are just from different quarters
                distinct(ReferenceKey, .keep_all = TRUE)
        
        ### repeat for IP death causes
        filelist <- list.files(path = dir, pattern = "*IP_Cause_of_Death.*.csv")
        filelist <- file.path(dir, filelist)
        
        out <- list()
        for (i in 1:8) {
                out[[length(out)+1]] <- read_csv(filelist[i], col_types = cols()) %>% mutate(Q = i)
        }
        IPcod <- bind_rows(out) %>%
                setNames(c("ReferenceKey", "IP.cod", "IP.cod_supp", "IP.nEpisode", "Q")) %>% 
                filter(!is.na(ReferenceKey)) %>% 
                distinct(ReferenceKey, .keep_all = TRUE)
        
        ### all unique reference keys for patients that are censor == 1
        RefKeyDead <- stata %>% 
                filter(censor == 1) %>% 
                pull(ReferenceKey) %>% 
                unique()
        
        ### create the death_cause table
        COD <- tibble(ReferenceKey = RefKeyDead) %>% 
                left_join(select(AEcod, -Q), by = "ReferenceKey") %>% 
                left_join(select(IPcod, -Q), by = "ReferenceKey") %>% 
                ## annotation
                # a reference key can either AE or IP death cause, or neither, but not both
                mutate(cod = dplyr::coalesce(AE.cod, IP.cod)) %>% 
                mutate(cod.supp = dplyr::coalesce(AE.cod_supp, IP.cod_supp)) %>% 
                mutate(source = dplyr::case_when(
                        is.na(AE.cod) & !is.na(IP.cod) ~ "IP",
                        is.na(IP.cod) & !is.na(AE.cod) ~ "AE"
                ))
        
        COD
}


get_cod_comorbidites <- function(cod, icd10map, death_refkey) {
        ### a. setup
        cod <- cod %>% 
                # using mutate(NoCode = is.na(cod) & is.na(cod.supp)) does not work,
                # because all ReferenceKeys without death codes are filtered away in the first place
                mutate(ReferenceKey = as.numeric(ReferenceKey))
        
        ### b. mapping comorbidities
        print("Mapping ...")
        cod_comorb <- cod %>% 
                # select relevant columns
                select(ReferenceKey, cod, cod.supp) %>% 
                filter(ReferenceKey %in% death_refkey) %>%
                # mapping
                icd10_comorbid(map = icd10map, visit_name = "ReferenceKey", icd_name = c("cod", "cod.supp")) %>% 
                as_tibble(rownames = "ReferenceKey")
        
        ### c. annotation
        print("Annotating ...")
        cod_comorb <- cod_comorb %>% 
                # add ReferenceKey
                mutate(ReferenceKey = as.numeric(ReferenceKey)) %>%
                left_join(cod %>% select(ReferenceKey), by = "ReferenceKey") %>% 
                # add a NoneMapped (T/F) column
                mutate(NoneMapped = rowSums(across(-ReferenceKey)) == 0) %>% 
                # add a NoCode (empty for now, as a place-holder)
                mutate(NoCode = NA)
        
        cod_comorb
}


###  generating the comorbidity dataframe
get_dx_cod_comorbidities <- function(df, cod,
                                     icd9map = icd::icd9_map_quan_deyo,
                                     icd10map = icd::icd10_map_quan_deyo) {
        
        ### 1. Comorbidities for AE/ward diagnosis
        print("Mapping comorbidities for AE/ward diagnosis ...")
        dx_comorb <- get_dx_comorbidities(df, icd9map = icd9map)
        
        ### 2. get a list of reference keys that require cause-of-death comorbidities
        # to save computation time for step 3b
        death_refkey <- dx_comorb %>% pull(ReferenceKey)
        death_refkey <- death_refkey[!is.na(death_refkey)] %>% unique()
        
        ### 3. Cause-of-death comorbidities
        print("Mapping comorbidities for death causes ...")
        cod_comorb <- get_cod_comorbidites(cod, icd10map = icd10map, death_refkey = death_refkey)
        
        print("Joining diagnosis and death cause comorbidities ...") 
        ### 4. final output
        out <- left_join(dx_comorb, cod_comorb, by = "ReferenceKey", suffix = c("_dx", "_cod")) %>% 
                # mutate(NoCode_cod = ifelse(death == 0, NA, is.na(NoneMapped_cod))) %>% 
                # mutate(NoneMapped_cod = ifelse(death == 1 & is.na(NoneMapped_cod), FALSE, NoneMapped_cod)) %>% 
                
                mutate(NoCode_cod = case_when(
                        death == 1 & is.na(NoneMapped_cod) ~ TRUE,
                        death == 1 & !is.na(NoneMapped_cod) ~ FALSE
                )) %>%
                select(AENumber, ReferenceKey, death, everything())
        out
}


