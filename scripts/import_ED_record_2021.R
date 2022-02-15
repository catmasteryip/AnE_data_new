import_ED_record_2021 <- function(dir) {
        #' A special use-case of import_csv_from_dir
        
        # setup: specific to attendance_2021
        col_types <- readr::cols(
                `Reference Key` = col_double(),
                `Institution (IPAS)` = col_character(),
                `AE Number` = col_character(),
                Sex = col_character(),
                `Admission Age (Year) (episode based)` = col_double(),
                `Paycode (at discharge)` = col_character(),
                `District of Residence (system code)` = col_character(),
                `District of Residence Description` = col_character(),
                `Race Description` = col_character(),
                `Date of Registered Death` = col_date(format = ""),
                `Exact date of death` = col_character(),
                `Death Cause (Main Cause)` = col_character(),
                `Death Cause (Supplementary Cause)` = col_character(),
                `Attendance Date (yyyy-mm-dd)` = col_date(format = ""),
                `Attendance Date (Hour)` = col_character(),
                `Admission from OAH (Y/N)` = col_character(),
                `Ambulance Case (Y/N)` = col_character(),
                `Attendance Specialty (EIS)` = col_character(),
                `Discharge Date (yyyy-mm-dd)` = col_date(format = ""),
                `Discharge Hour (00-23)` = col_character(),
                `Traumatic Type` = col_character(),
                `Domestic Type` = col_logical(),
                `Domestic Nature` = col_logical(),
                `Animal Bite Type Description` = col_character(),
                `Poison Nature Description` = col_character(),
                `Poison Type Description` = col_character(),
                `Poison Description` = col_character(),
                `Triage Category` = col_double(),
                `Mobility Status` = col_character(),
                `Conscious Level` = col_character(),
                `GCS Total Score` = col_logical(),
                `Waiting Time (to cubicle)(Minute)` = col_double(),
                `Waiting Time (to triage)(Minute)` = col_double(),
                `Consultation Start Time (Hour, 00-23)` = col_character(),
                `Observation Room Case (Y/N)` = col_character(),
                `Observation Room Staying Time (Minute)` = col_double(),
                `Trauma Team Activation (Y/N)` = col_character(),
                `Episode Death (Y/N)` = col_character(),
                `Total Staying Time (Minute)` = col_double(),
                `Discharge Status (EIS)` = col_double(),
                `Discharge Destination (AEIS)` = col_character(),
                `A&E to IP Ward: Admission Decision Time (yyyy-mm-dd HH:MM)` = col_datetime(format = ""),
                `A&E to IP Ward: Waiting Time for Admission (Min)` = col_double(),
                `A&E to IP Ward: Length of Stay` = col_double(),
                `A&E to IP Ward: Admission Date` = col_date(format = ""),
                `A&E to IP Ward: Principal Diagnosis Code` = col_character(),
                `A&E to IP Ward: Diagnosis (rank 2)` = col_character(),
                `A&E to IP Ward: Diagnosis (rank 3)` = col_character(),
                `A&E to IP Ward: Diagnosis (rank 4)` = col_character(),
                `A&E to IP Ward: Diagnosis (rank 5)` = col_character(),
                `A&E to IP Ward: Principal Diagnosis Description (HAMDCT)` = col_character(),
                `A&E to IP Ward: Diagnosis HAMDCT Description (rank 2)` = col_character(),
                `A&E to IP Ward: Diagnosis HAMDCT Description (rank 3)` = col_character(),
                `A&E to IP Ward: Diagnosis HAMDCT Description (rank 4)` = col_character(),
                `A&E to IP Ward: Diagnosis HAMDCT Description (rank 5)` = col_character(),
                `Principal Diagnosis Code` = col_character(),
                `Diagnosis (rank 2)` = col_character(),
                `Diagnosis (rank 3)` = col_character(),
                `Diagnosis (rank 4)` = col_character(),
                `Diagnosis (rank 5)` = col_character(),
                `Principal Diagnosis Description (HAMDCT)` = col_character(),
                `Diagnosis HAMDCT Description (rank 2)` = col_character(),
                `Diagnosis HAMDCT Description (rank 3)` = col_character(),
                `Diagnosis HAMDCT Description (rank 4)` = col_character(),
                `Diagnosis HAMDCT Description (rank 5)` = col_character(),
                `A&E to IP Ward: Institution` = col_character(),
                `A&E to IP Ward: HN Number` = col_character(),
                `A&E to IP Ward: Admission Specialty (IPAS)` = col_character()
        )
        
        df <- import_csv_from_dir(
                dir = dir, 
                file_pattern = ".*AE_Attendance_2021.*.csv", # pattern specific to 2021 attendance
                footer_lines = 6,
                skip = 34,
                col_types = col_types
        )
        return(df)
}


import_csv_from_dir <- function(dir, file_pattern = ".csv", footer_lines, ...) {
        #' Import all csv files that match a pattern from a directory
        #'
        #' @param footer_lines integer. Number of lines of footer, which is not imported
        #' @param ... further arguments passed into read_csv
        
        .import_one_file <- function(file, footer_lines, ...) {
                df <- 
                        # first read as lines, 
                        # then remove the last 6 rows which are footer information
                        read_lines(file) %>% 
                        head(n = -footer_lines) %>% 
                        # import
                        read_csv(...)
                return(df)
        }
        
        files <- list.files(dir, pattern = file_pattern, full.names = T)
        dfs <- files %>% map(.import_one_file, footer_lines = footer_lines, ...)
        
        # Check if colnames are same across all files
        .allSame <- function(x) length(unique(x)) == 1
        colnames_all_same <- dfs %>% 
                map(colnames) %>% 
                .allSame()
        if (!colnames_all_same) stop("Column names not the same.")

        # if pass check, bind rows
        out <- dfs %>% bind_rows(.id = NULL)
        return(out)
}

rename_ED_record_2021 <- function(df) {
        renamed <- df %>% 
                rename(
                        ReferenceKey = `Reference Key`,
                        InstitutionIPAS = `Institution (IPAS)`,
                        AENumber = `AE Number`,
                        # Sex
                        AdmissionAgeYearepisodeba = `Admission Age (Year) (episode based)`,
                        # `Paycode (at discharge)`
                        # `District of Residence (system code)`
                        # `District of Residence Description`
                        # `Race Description`
                        DateofRegisteredDeath = `Date of Registered Death`,
                        Exactdateofdeath = `Exact date of death`,
                        # `Death Cause (Main Cause)`
                        # `Death Cause (Supplementary Cause)`
                        eventdate = `Attendance Date (yyyy-mm-dd)`,
                        # `Attendance Date (Hour)`
                        # `Admission from OAH (Y/N)`
                        amb = `Ambulance Case (Y/N)`,
                        # `Attendance Specialty (EIS)`
                        # `Discharge Date (yyyy-mm-dd)`
                        # `Discharge Hour (00-23)`
                        # `Traumatic Type`
                        # `Domestic Type`
                        # `Domestic Nature`
                        # `Animal Bite Type Description`
                        # `Poison Nature Description`
                        # `Poison Type Description`
                        # `Poison Description`
                        TriageCategory = `Triage Category`,
                        MobilityStatus = `Mobility Status`,
                        ConsciousLevel = `Conscious Level` ,
                        # `GCS Total Score` 
                        WaitingTimetocubicleMinute = `Waiting Time (to cubicle)(Minute)`,
                        WaitingTimetotriageMinute = `Waiting Time (to triage)(Minute)`,
                        # `Consultation Start Time (Hour, 00-23)`
                        # `Observation Room Case (Y/N)`
                        # `Observation Room Staying Time (Minute)`
                        # `Trauma Team Activation (Y/N)` = 
                        EpisodeDeathYN = `Episode Death (Y/N)`,
                        # `Total Staying Time (Minute)`
                        # `Discharge Status (EIS)`
                        # `Discharge Destination (AEIS)`,
                        # `A&E to IP Ward: Admission Decision Time (yyyy-mm-dd HH:MM)`
                        AEtoIPWardWaitingTimefor = `A&E to IP Ward: Waiting Time for Admission (Min)`,
                        AEtoIPWardLengthofStay = `A&E to IP Ward: Length of Stay`,
                        # `A&E to IP Ward: Admission Date`
                        DR1 = `A&E to IP Ward: Principal Diagnosis Code`,
                        DR2 = `A&E to IP Ward: Diagnosis (rank 2)`,
                        DR3 = `A&E to IP Ward: Diagnosis (rank 3)`,
                        DR4 = `A&E to IP Ward: Diagnosis (rank 4)`,
                        DR5 = `A&E to IP Ward: Diagnosis (rank 5)`,
                        AY = `A&E to IP Ward: Principal Diagnosis Description (HAMDCT)`,
                        AEtoIPWardDiagnosisHAMDCT = `A&E to IP Ward: Diagnosis HAMDCT Description (rank 2)`,
                        BA = `A&E to IP Ward: Diagnosis HAMDCT Description (rank 3)`,
                        BB = `A&E to IP Ward: Diagnosis HAMDCT Description (rank 4)`,
                        BC = `A&E to IP Ward: Diagnosis HAMDCT Description (rank 5)`,
                        DR6 = `Principal Diagnosis Code`,
                        DR7 = `Diagnosis (rank 2)`,
                        DR8 = `Diagnosis (rank 3)`,
                        DR9 = `Diagnosis (rank 4)`,
                        DR10 = `Diagnosis (rank 5)`,
                        PrincipalDiagnosisDescription = `Principal Diagnosis Description (HAMDCT)`,
                        DiagnosisHAMDCTDescriptionra = `Diagnosis HAMDCT Description (rank 2)`,
                        BK = `Diagnosis HAMDCT Description (rank 3)`,
                        BL = `Diagnosis HAMDCT Description (rank 4)`,
                        BM = `Diagnosis HAMDCT Description (rank 5)`
                        # `A&E to IP Ward: Institution`
                        # `A&E to IP Ward: HN Number` 
                        # `A&E to IP Ward: Admission Specialty (IPAS)`
                )
        return(renamed)
}

