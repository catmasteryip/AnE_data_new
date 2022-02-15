# ///// Append features to records ----------------------------------------------------------
# deprecated
#' import_and_append <- function(df, features, dir, up_to = c("2020", "2021")) {
#'         #' Import and append features to the original records stata file
#'         #'
#'         #' @param up_to. Character. Either "2020" or "2021", in quotes.
#'         
#'         up_to = match.arg(up_to)
#'         file <- c("2020" = "2019_2020_all_20210312.dta", 
#'                   "2021" = "2019_2020_2021_all_20210727.dta")
#'         file <- file[up_to]
#'         features_to_append <- haven::read_dta(file = file.path(dir, file), 
#'                                               col_select = all_of(c("AENumber", features)))
#'         
#'         df.appended <- left_join(df, features_to_append, by = "AENumber")
#'         
#'         return(df.appended)
#' }


# ///// covid and covid deaths ------------------------------------------------

import_covid <- function(dir = NULL) {
        covid <- read_csv(file.path(dir, "daily_cases_hk.csv"), col_types = "Dii") %>% 
                rename(n = hk)
        covid
}

import_covid_deaths <- function(dir = NULL) {
        #' Import COVID deaths in Hong Kong by day
        #' 
        #' Data source: John Hopkins
        #' The default is to retrieve data from: https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series
        #' Local file: "time_series_covid19_deaths_global.csv"
        
        if (is.null(dir)) {
                github_repo <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series"
                file <- url(paste0(github_repo, "/time_series_covid19_deaths_global.csv"))
        } else {
                
                file <- file.path(dir, "/time_series_covid19_deaths_global_retrieved_20210712.csv")
        }
        
        covid_deaths <- read_csv(file = file, col_types = cols())
        covid_deaths_HK <- covid_deaths %>% extract_HK_counts_from_JohnHopkins_cumulative_counts()
        return(covid_deaths_HK)
}

import_HK_covid_data_from_JohnHopkins <- function(data_type = c("deaths", "cases")) {
        #' Extract Hong Kong counts from a spreadsheet of cumulative counts from John Hopkin's github repo
        #' 
        #' @param data_type Character. Either "cases" or "deaths"
        
        # construct url
        data_type <- match.arg(data_type)
        github_repo <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series"
        filenames <- c(
                cases  = "/time_series_covid19_confirmed_global.csv",
                deaths = "/time_series_covid19_deaths_global.csv"
        )
        url <- url(paste0( github_repo, filenames[data_type] ))
        
        # import and clean
        covid_data <- read_csv(file = url, col_types = cols())
        covid_data_HK <- covid_data %>% extract_HK_counts_from_JohnHopkins_cumulative_counts()
        return(covid_data_HK)
}

extract_HK_counts_from_JohnHopkins_cumulative_counts <- function(df) {
        #' Extract Hong Kong counts from cumulative counts spreadsheet from John Hopkins
        
        filtered <- df %>%
                filter(`Province/State` == "Hong Kong") %>% 
                summarise(across(.cols = -c(1:4), # omit the 4 cols on geographical region
                                 .fns = sum)) %>% 
                pivot_longer(cols = everything(), names_to = "date", values_to = "n") %>% 
                mutate(
                        date = lubridate::mdy(date),
                        n = cumsum_to_count(n)
                )
        
        
        abnormal.dates <- filtered %>% filter(n < 0) %>% pull(date)
        if(length(abnormal.dates)>0) warning(paste0("The following dates have abnormal counts: ", 
                                                    paste(abnormal.dates, collapse = "")))
        return(filtered)
}


# ///// Importing conditions defined by team --------------------------------------------

import_condf <- function() {
        con.df <- tribble(
                # num = pri_dis coding
                # conS = condition name, short
                # conL = condition name, long
                # valid = whether it is ok for death analysis (e.g. sufficient deaths, etc)
                # orig  = whether it belongs to the original 25 categories used in old analysis
                ~num, ~conS, ~conL, ~valid, ~orig,
                0, "Feb29", "February 29", FALSE, FALSE,
                1, "diag", "sepsis", TRUE, TRUE,
                2, "poi_index", "poisoning", TRUE, TRUE,
                3, "dv_index", "domestic violence", TRUE, TRUE,
                4, "su_index", "suicide", TRUE, TRUE,
                5, "mental_index", "mental diseases", TRUE, TRUE,
                6, "other_heart_disea", "other heart diseases", TRUE, TRUE,
                7, "pneumonia_influenza", "pneumonia and influenza", TRUE, TRUE,
                8, "down_syn", "Down syndrome", FALSE, TRUE,
                9, "oltd", "OLTD", FALSE, TRUE,
                10, "ckd", "CKD", TRUE, TRUE,
                11, "db", "diabetes", TRUE, TRUE,
                12, "bronch", "bronchitis", TRUE, TRUE,
                13, "chd", "CHD", TRUE, TRUE,
                14, "cerebro_dis", "cerebrovascular diseases", TRUE, TRUE,
                15, "dementia", "dementia", TRUE, TRUE,
                16, "parkinson", "Parkinson's", TRUE, TRUE,
                17, "epilepsy", "epilepsy", TRUE, TRUE,
                18, "cerebral_palsy", "cerebral palsy", FALSE, TRUE,
                19, "cancer", "cancer", TRUE, TRUE,
                20, "airway", "airway diseases", TRUE, TRUE,
                21, "MI_index", "myocardial infarction", TRUE, TRUE,
                22, "stroke_index", "stroke", TRUE, TRUE,
                23, "tra_index", "trauma", TRUE, TRUE,
                24, "uncap", "uncaptured disease groups/others", FALSE, FALSE,
                25, "no_dis", "no diagnosis", FALSE, FALSE,
                NA, "cardiac_arrest", "cardiac arrest", TRUE, TRUE,
                NA, "blood_cancer", "blood cancers", TRUE, FALSE,
                NA, "resp_cancer", "respiratory cancers", TRUE, FALSE,
                NA, "co_lung_dis", "chronic airway obstruction", TRUE, FALSE,
                NA, "asthma", "asthma", TRUE, FALSE,
                NA, "ill_defined_n_unk", "ill-defined and unknown causes of morbidity and mortality", TRUE, FALSE,
                NA, "influenza", "influenza", TRUE, FALSE,
                NA, "pneu", "pneumonia", TRUE, FALSE,
                NA, "drug_over_index", "drug overdose", TRUE, FALSE,
                NA, "NoCode_1d_death", "missing code, DOA", NA, FALSE,
                NA, "NoCode_non1d_death", "missing code, non-DOA", NA, FALSE,
                NA, "death", "death", FALSE, FALSE
        )
        return(con.df)
}


# ///// Importing waves and dates ----------------------------------------------------

import_dates <- function(wave.df = NULL, 
                         start = lubridate::ymd('2019-01-01'), 
                         end =   lubridate::ymd('2021-05-06')
                         ) {
        if (is.null(wave.df)) wave.df <- import_waves()
        
        # period_vector <- rep(1:53, each = 7)[1:365]
                # a vector denoting periods (1 Jan is always the 1st day of period 1)
        
        date.df <- tibble(
                        date = seq(start, end, by = '1 day')
                ) %>% 
                filter(date != "2020-02-29") %>% 
                mutate(wave_num = sapply(date, in_wave, waves = wave.df)) %>% 
                mutate(year = year(date)) %>% 
                mutate(month = month(date)) %>% 
                # mutate(period = rep(period_vector, 2)) %>% 
                mutate(yday = yday(date)) 
        date.df
}


import_waves <- function(day.lag = 0, twosls=F) {
        #' A function to import the COVID waves in Hong Kong.
        # one more wave, 2021 wave 5
        if(!twosls){
                wave.df <- tribble(
                        ~ year, ~ wave, ~ start, ~ end,
                        2019, 1, "2019-01-25", "2019-03-02",
                        2019, 2, "2019-03-17", "2019-05-04",
                        2019, 3, "2019-07-09", "2019-08-28",
                        2019, 4, "2019-11-14", "2019-12-31",
                        2019, 5, "2019-01-01", "2019-02-18",
                        2020, 1, "2020-01-25", "2020-03-02",
                        2020, 2, "2020-03-17", "2020-05-04",
                        2020, 3, "2020-07-09", "2020-08-28",
                        2020, 4, "2020-11-14", "2020-12-31",
                        2021, 5, "2021-01-01", "2021-02-18"
                )  
        }else if(twosls==T){
                wave.df <- tribble(
                        ~ year, ~ wave, ~ start, ~ end,
                        2020, 1, "2020-01-25", "2020-03-02",
                        2020, 2, "2020-03-17", "2020-05-04",
                        2020, 3, "2020-07-09", "2020-08-28",
                        2020, 4, "2020-11-14", "2020-12-31",
                        2021, 4, "2021-01-01", "2021-02-18"
                )
        }
         
        wave.df = wave.df %>% 
                mutate(across(c(start, end), lubridate::ymd)) %>% 
                mutate(end = end + lubridate::days(day.lag)) %>%
                mutate(interval = lubridate::interval(start = start, end = end)) %>% 
                mutate(duration = count_interval_days(interval))
        
        return(wave.df)
}
import_wavedates2021 = function(df.lastday, out_of_wave = 'non-wave'){
        # impute wave
        wave.df <- import_waves(twosls = T)
        date.df <- import_dates(wave.df=wave.df, end=df.lastday)
        
        # # impute 2021 after-wave periods
        date.df = date.df %>%
                rowwise() %>%
                unnest(wave_num) 
        if(out_of_wave == "non-wave"){
                date.df = date.df %>%
                        mutate(wave_num = ifelse(is.na(wave_num), "non-wave",as.character(wave_num))) 
        }else if(out_of_wave == "after-wave"){
                date.df = date.df %>%
                        mutate(wave_num = ifelse(date>= "2020-1-25" & is.na(wave_num), 
                                                 "after-wave",as.character(wave_num)))
        }
        
        date.df.19 = date.df %>%
                filter(year==2019)
        date.df.beyond19 = date.df %>%
                filter(year!=2019) %>%
                mutate(yday = ifelse(year==2020 & date>"2020-2-29",yday-1,yday))
        
        
        date.df = right_join(date.df.19, date.df.beyond19 %>% 
                                     select(yday,wave_num), by="yday") %>% 
                select(-wave_num.x) %>%
                rename(wave_num = wave_num.y) %>%
                bind_rows(., date.df.beyond19) %>%
                drop_na()
}
## helper functions for importing waves and dates --------------------------

in_wave <- function(date, waves = NULL) {
        #' Return the wave number of a date.
        #' 
        #' A helper function to return the wave number. Returns NA if the date is not within a wave.
        
        if (is.null(waves)) waves <- import_waves()
        
        x <- date %within% waves$interval
        if (sum(x) == 0) return(NA)
        waves$wave[x]
}

cumsum_to_count <- function(cumsum) {
        #' Helper function to convert cumulative counts to normal counts
        
        count <- c(cumsum[1],diff(cumsum))
        count
}

count_interval_days <- function(interval) {
        #' Calculate the number of days in an interval
        #'
        #' A helper function to calculate the number of days in a lubridate interval
        
        as.double(lubridate::as.duration(interval) / (3600*24)) + 1
}

convert_to_facet_label <- function(var) {
        paste0(deparse(substitute(var)), "(", var, ")")
}


# ///// Not used ----------------------------------------------------------------

if (FALSE) {
        
        import_covid_AE <- function(dir = NULL) {
                AE.covid <- read_csv(file.path(dir, "covid_positive_AENumber_list.csv"), col_types = "c") %>% 
                        pull(1)
                return(AE.covid)
        }
        
}

import_waves3 <- function(day.lag = 0) {
        #' A function to import the COVID waves in Hong Kong.
        #' Called by import_calendar
        
        waves <- tribble(
                ~ year, ~ wave, ~ start, ~ end,
                2019, "P1", "2019-01-01", "2019-01-24",
                2019, "W1", "2019-01-25", "2019-03-02",
                2019, "P2", "2019-03-03", "2019-03-16",
                2019, "W2", "2019-03-17", "2019-05-04",
                2019, "P3", "2019-05-05", "2019-07-08",
                2019, "W3", "2019-07-09", "2019-08-28",
                2019, "P4", "2019-08-29", "2019-11-13",
                2019, "W4", "2019-11-14", "2019-12-31",
                2020, "P1", "2020-01-01", "2020-01-24",
                2020, "W1", "2020-01-25", "2020-03-02",
                2020, "P2", "2020-03-03", "2020-03-16",
                2020, "W2", "2020-03-17", "2020-05-04",
                2020, "P3", "2020-05-05", "2020-07-08",
                2020, "W3", "2020-07-09", "2020-08-28",
                2020, "P4", "2020-08-29", "2020-11-13",
                2020, "W4", "2020-11-14", "2020-12-31",
        ) %>% 
                mutate(across(c(start, end), lubridate::ymd)) %>% 
                mutate(end = end + lubridate::days(day.lag)) %>%
                mutate(interval = lubridate::interval(start = start, end = end)) %>% 
                mutate(duration = count_interval_days(interval)) %>% 
                mutate(wave = factor(wave, levels = c("P1",
                                                      "W1",
                                                      "P2",
                                                      "W2",
                                                      "P3",
                                                      "W3",
                                                      "P4",
                                                      "W4")))
        return(waves)
}



import_waves2 <- function(day.lag = 0) {
        #' A function to import the COVID waves in Hong Kong.
        
        waves <- tribble(
                ~ year, ~ wave, ~ start, ~ end,
                2019, "A",    "2019-01-01", "2019-01-24",
                2019, "B(1)", "2019-01-25", "2019-03-02",
                2019, "C",    "2019-03-03", "2019-03-16",
                2019, "D(2)", "2019-03-17", "2019-05-04",
                2019, "E",    "2019-05-05", "2019-07-08",
                2019, "F(3)", "2019-07-09", "2019-08-28",
                2019, "G",    "2019-08-29", "2019-11-13",
                2019, "H(4)", "2019-11-14", "2019-12-31",
                2020, "A",    "2020-01-01", "2020-01-24",
                2020, "B(1)", "2020-01-25", "2020-03-02",
                2020, "C",    "2020-03-03", "2020-03-16",
                2020, "D(2)", "2020-03-17", "2020-05-04",
                2020, "E",    "2020-05-05", "2020-07-08",
                2020, "F(3)", "2020-07-09", "2020-08-28",
                2020, "G",    "2020-08-29", "2020-11-13",
                2020, "H(4)", "2020-11-14", "2020-12-31",
        ) %>% 
                mutate(across(c(start, end), lubridate::ymd)) %>% 
                mutate(end = end + lubridate::days(day.lag)) %>%
                mutate(interval = lubridate::interval(start = start, end = end)) %>% 
                mutate(duration = count_interval_days(interval)) %>% 
                mutate(wave = factor(wave, c("A",
                                             "B(1)",
                                             "C",
                                             "D(2)",
                                             "E",
                                             "F(3)",
                                             "G",
                                             "H(4)"
                )))
        return(waves)
}

import_calendar <- function() {
        wave.df <- import_waves3()
        calendar <- import_dates(wave.df) %>% 
                left_join(wave.df %>% 
                                  rename(wave_num = wave) %>% 
                                  select(year, wave_num, start, end, duration), 
                          by = c("year", "wave_num"))
        return(calendar)
}

add_features_to_calendar <- function(df) {
        out <- df %>% 
                mutate(year2020 = year == 2020) %>%
                mutate(in_wave  = year == 2020 & wave_num %in% c("W1", "W2", "W3", "W4")) %>% 
                mutate(wave1 = year == 2020 & wave_num == "W1") %>% 
                mutate(wave2 = year == 2020 & wave_num == "W2") %>% 
                mutate(wave3 = year == 2020 & wave_num == "W3") %>% 
                mutate(wave4 = year == 2020 & wave_num == "W4") %>%
                mutate(daySincePandemic = as.numeric(date - ymd("2020-01-25")) + 1) %>% 
                mutate(daySincePandemic = ifelse(daySincePandemic < 0, 0, daySincePandemic)) 
        return(out)
}

import_icd <- function() {
        icd9 <-  icd.data::icd9cm_hierarchy %>% mutate(code = as.character(code))
        icd9
}


# Deprecated routines
# Replaced by import_stata in data_import.R
if (FALSE) {
        import_stata <- function(dir = NULL) {
                #' Imports the stata files
                
                if (is.null(dir)) dir <- "/storage/holab/wongyh/AnE/data"
                
                con.df <- import_condf()
                
                logtime("Importing AE_pri_dis_all.dta ...")
                ### imports
                stata.T1_5.new <- haven::read_dta(file.path(dir, "AE_pri_dis_all.dta"),
                                                  col_select = c(AENumber, pri_dis, tra_gp))
                
                logtime("Importing 2019_2020_all_20210312.dta ...")
                # this file does not contain 2021 records, the date specifies when it was exported
                stata <- haven::read_dta(file.path(dir, "2019_2020_all_20210312.dta"))
                
                ### joining
                logtime("Joining ...")
                stata <- left_join(stata, stata.T1_5.new, by = "AENumber")
                
                ### cleaning
                logtime("Cleaning ...")
                stata <- stata %>% 
                        update_sepsis_records(dir) %>% 
                        # annotate primary dx
                        left_join(select(con.df, num, conL), by = c("pri_dis" = "num")) %>% 
                        rename(pri_disL = conL) %>% 
                        mutate(tra_index = ifelse(tra_gp == 0, 0, 1)) %>%
                        # parse dates
                        mutate(DateofRegisteredDeath = lubridate::ymd(DateofRegisteredDeath)) %>% 
                        mutate(eventdate             = lubridate::ymd(eventdate))
                
                # Drop Feb 29
                stata <- stata %>%
                        # filter(year != 2021) ### unnecessary because this file does not contain 2021 records
                        filter(eventdate != "2020-02-29")
                
                # de-dup death counts
                logtime("Deduplicating ... ")
                stata <- stata %>% annotate_last_record()
                
                stata <- stata %>% remove_covid_records()
                
                AE.last.visit.Jan21 <- import_last_visit_Jan21(csv_dir)
                stata <- stata %>% remove_dup_Dec20_death(AE.last.visit.Jan21)
                
                logtime("Done")
                return(stata)
        }
        
        
        
        ## helper functions for import_stata ---------------------------------------------
        update_sepsis_records <- function(stata, dir) {
                records_sepsis <- haven::read_dta(file.path(dir, "updated_25dis.dta"))
                stata <- stata %>% 
                        select(-c(pri_dis, diag)) %>% 
                        left_join(records_sepsis, by = "AENumber")
                return(stata)
        }
        
        
        
        annotate_last_record <- function(stata, var_name = NULL) {
                if (is.null(var_name)) var_name <- "death"
                
                patientDieList <- stata %>%
                        ### filter for censor == 1 patients, i.e. they either have
                        # i. death date within 28-days of their admission OR
                        # ii. destination_num == 4
                        filter(censor == 1) %>%
                        filter(DateofRegisteredDeath - eventdate <= 28 | destination_num == 4) %>%  
                        ### for each patient,
                        group_by(ReferenceKey) %>%
                        ### pick the last record
                        arrange(eventdate) %>%
                        summarise(AENumber = dplyr::last(AENumber)) %>% 
                        ### pull out the AENumber
                        pull(AENumber)
                
                ### For these set of AENumbers, annotate them as "death == 1"
                stata <- stata %>% 
                        mutate({{ var_name }} := ifelse(AENumber %in% patientDieList, 1, 0))
                
                stata
        }
        
        remove_covid_records <- function(stata) {
                #' Remove COVID-positive records
                #' 
                #' Remove records that have "519.8:8" in the 10 diagnosis columns
                
                covid_cols = c("AENumber","AY","AEtoIPWardDiagnosisHAMDCT","BA","BB","BC",
                               "PrincipalDiagnosisDescription","DiagnosisHAMDCTDescriptionra","BK","BL","BM")
                covid_subset = stata %>% select(all_of(covid_cols))
                covid_code = "519.8:8"
                covid_AENumber_vec = c()
                for(colname in covid_cols){
                        covid_AENumber = covid_subset %>% filter(str_detect(eval(parse(text=colname)),covid_code)) %>% pull(AENumber)
                        covid_AENumber_vec = c(covid_AENumber_vec,covid_AENumber)
                }
                covid_AENumber_vec = unique(covid_AENumber_vec)
                
                stata_without_covid_records <- stata %>% 
                        filter(!AENumber %in% covid_AENumber_vec)
                
                return(stata_without_covid_records)
        }
        
        import_last_visit_Jan21 <- function(dir) {
                AE.last.visit.Jan21 <- read.csv(file.path(dir,"Dec20_censor.csv")) %>% 
                        filter(JanLastAtt==1) %>%
                        pull(AENumber)
                return(AE.last.visit.Jan21)
        }
        
        
        remove_dup_Dec20_death <- function(stata, AE.last.visit.Jan21) {
                stata <- stata %>% 
                        mutate(death = ifelse(AENumber %in% AE.last.visit.Jan21 & death==1, 0, death))
                return(stata)
        }

}

# yearly comparison of 2020, 2021 against non-covid years avg


compare_yearly = function(df, group.by = c("cond"), trtyrs = c(2020,2021), dates.2021 = NA){
  
  stata = df
  
  if(is.na(dates.2021)){
    dates.2021 = stata %>% 
      select(eventdate) %>% unique() %>% 
      filter(year(eventdate)==2021) %>%
      select(eventdate) %>%
      mutate(year = year(eventdate),
             yday = yday(eventdate)) %>%
      pull(yday) 
  }
  
  # browser()
  # daily count data
  cts.overall <- df  %>%
    filter(across(group.by, ~ !is.na(.x))) %>%
    # filter(!is.na(!!!syms(group.by))) %>%
    count(!!!syms(c("eventdate", "death", group.by))) %>%
    mutate(year = factor(year(eventdate)),
           yday = yday(eventdate),
           ctrltrt = "trt") 
  
  
  # daily count data of trt/covid years
  cts.covid = cts.overall %>%
    filter(year %in% trtyrs) %>%
    select(-eventdate)
  
  # browser()
  cts.noncovid = c()
  if(2020 %in% trtyrs){
    # daily count data of control/non-covid year
    cts.noncovid= cts.overall %>% 
      filter(!(year %in% trtyrs)) %>%
      
      group_by(!!!syms( c("yday", "death", group.by))) %>%
      
      summarise(n = mean(n)) %>%
      mutate(year = as.factor(2020),
             ctrltrt = "ctrl")
    
  }
  
  cts.sameperiod = c()
  if(2021 %in% trtyrs){
    # daily count data of control/non-covid years, same-period as 2021 the incomplete year
    cts.sameperiod = cts.overall %>% 
      filter(!(year %in% trtyrs),
             yday %in% dates.2021) %>%
      
      group_by(!!!syms( c("yday", "death", group.by))) %>%
      
      summarise(n = mean(n)) %>%
      mutate(year = as.factor(2021),
             ctrltrt = "ctrl")
  }
  
  # bind tables vertically
  cts.overall = bind_rows(cts.covid, 
                          cts.noncovid, 
                          cts.sameperiod) %>%
    group_by(!!!syms(c("year", "ctrltrt", "death", group.by))) %>%
    summarise(n = sum(n)) %>%
    mutate(
      death = factor(death),
      year = factor(year, levels = trtyrs)
    )
  
  # convert death==0 survivors to death==-1 attn
  cts.overall = bind_rows(cts.overall %>% 
                            filter(death == 1),
                          aggregate(formula(paste(c("n ~ year + ctrltrt", group.by),collapse="+")), data = cts.overall , FUN = sum) %>%
                            mutate(death = as.factor(-1))) 
  
  # browser()
  # diff between ctrl and trt
  diff.overall = cts.overall %>%
    ungroup() %>%
    group_by(!!!syms(c("year", "death", group.by))) %>%
    # mutate(n[ctrltrt == "trt"] = ifelse(is.na(n[ctrltrt == "trt"]), 0, n[ctrltrt == "trt"]),
    #        n[ctrltrt == "ctrl"] = ifelse(is.na(n[ctrltrt == "ctrl"]), 0, n[ctrltrt == "ctrl"])) %>%
    mutate(n = round(n)) %>%
    mutate(abs_diff = round(n[ctrltrt == "trt"] - n[ctrltrt == "ctrl"]),
           percent_diff = signif((n[ctrltrt == "trt"] / n[ctrltrt == "ctrl"] - 1) * 100,3)) %>%
    ungroup()
  
  return(list(cts.overall, diff.overall))
}

compare_waves = function(df, date.df, group.by = c("cond")){
  #' Import and append features to the original records stata file
  #'    
  #' @param df. Dataframe. dataframe with cond, eventdate, death 
  #' @param date.df. Dataframe. dataframe with wave_num and eventdate columns
  
  selected = df
  # produce daily table
  daily = selected %>% 
    group_by(!!!syms(group.by)) %>% 
    count(eventdate) %>% 
    rename(attn=n) %>%
    # mutate year and t, number of calendar days since 1st Jan
    mutate(year=year(eventdate),
           yday = yday(eventdate),
           ctrltrt = "trt") %>%
    drop_na()
  
  # daily count table in covid years
  daily.covid = daily %>%
    filter(year > 2019) %>%
    left_join(x=., y=date.df %>% select(eventdate=date,wave_num), by="eventdate") 
  
  # daily count table in non-covid years
  daily.noncovid = daily %>%
    filter(year < 2020) %>%
    group_by(!!!syms(c("yday", group.by))) %>%
    summarise(attn = mean(attn)) %>%
    mutate(ctrltrt = "ctrl")
  
  attn.reduction =
    right_join(x=daily.noncovid,
               y=daily.covid,
               by=c("yday",group.by)) %>%
    group_by(cond,wave_num) %>%
    summarise(attn.x = round(sum(attn.x)),
              attn.y = round(sum(attn.y)),
              abs_diff = round(attn.y - attn.x),
              percent_diff = signif(abs_diff / attn.x  * 100,3))
  
  attn.reduction
}


