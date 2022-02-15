# comorbidities -----------------------------------------------------------

get_dx_comorbidities <- function(df, icd9map) {
        
        # input args
        # icd9 map
        icd9map = icd::icd9_map_quan_deyo
        
        ### a. select relevant columns
        df <- df %>% 
                # select(AENumber, ReferenceKey, DR1:DR10, death) %>%
                # impute NoCode col
                mutate(NoCode = ifelse(if_any(DR1:DR10, ~!is.na(.)), 0, 1)) 
        
        ### b. comorbidities for AE/ward diagnosis
        print("Mapping ...")
        dx_comorb <- df %>% 
                # select relevant columns
                select(AENumber, DR1:DR10) %>%
                icd::icd9_comorbid(map = icd9map, visit_name = "AENumber", icd_name = paste0("DR", 1:10)) %>% 
                as_tibble(rownames = "AENumber")
        
        ### c. annotation
        print("Annotating ...")
        dx_comorb <- dx_comorb %>% 
          
                # add back death (1/0) and NoCode (T/F) for all records
                left_join(df %>% select(AENumber, NoCode, death, ReferenceKey), by = "AENumber") %>%
          
                # add a NoneMapped (T/F) column
                mutate(NoneMapped = (rowSums(across(-AENumber)) == 0 & !NoCode)) 
                
                # # add back reference key for death == 1 records
                # left_join(df %>% filter(death == 1) %>% select(AENumber, ReferenceKey), by = "AENumber")
        
        dx_comorb
}

# formatting --------------------------------------------------------------

expand_icd_to_5d <- function(x) {
        icd::expand_range(start = x, end = x)
}


# annotation --------------------------------------------------------------

explain_code_wrapper <- function(code) {
        out <- icd::explain_code(as.character(code), warn = FALSE)[1]
        # if (is.na(out)) out <- ""
        out
}

in_subchapter <- function(x, map) {
        out <- map %>% 
                # for each chapter
                map(function(code) x %in% code) %>% 
                unlist() %>% 
                .[.==T] %>% 
                names()
        if (length(out) <= 0) return(NA)
        return(out)
}

get_icd_subchapter_map <- function() {
        icd::chapters_to_map(icd::icd9_sub_chapters)
}


get_icd_subchapter_label <- function() {
        subchapters <- icd::icd9_sub_chapters
        
        start <- subchapters %>% map_chr("start")
        end   <- subchapters %>% map_chr("end")
        label <- names(subchapters)
        
        new_label <- paste0(start, "-", end, ": ", label)
        return(new_label)
}


# onehot encoding ---------------------------------------------------------------

onehot_encode_icd_group <- function(df = NULL, dir = NULL, icd.group, strip_decimal = T) {
        
        # defaults
        if (!inherits(icd.group, "list")) stop("icd.group should be a named list. For one character vector only, use onehot_encode_icd instead.")
        
        if (is.null(df)) df <- import_stata(dir) %>% 
                        import_and_append(features = paste0("DR", 1:10), dir = dir)
        
        missing <- setdiff(paste0("DR", 1:10), colnames(df))
        if(length(missing)>0) {
                cat(paste0("The following expected columns are missing: ", paste0(missing, collapse = ", "), '. Importing and appending these columns...'))
                df <- df %>% import_and_append(features = missing, dir = dir)
        }
        
        # encode
        DRcolumns <- df %>% select(DR1, DR2, DR3, DR4, DR5, DR6, DR7, DR8, DR9, DR10)
        if (strip_decimal) {
                DRcolumns <- DRcolumns %>% 
                        mutate(across(everything(),
                                      str_replace_all, "\\.", ""))
        }
        
        onehot <- icd.group %>% 
                # for each icd group
                map(function(codes) DRcolumns %>% 
                            mutate(across(everything(),
                                          .fns = function(x) x %in% codes)
                            ) %>% 
                            mutate( onehot := as.numeric(rowSums(.) > 0)) %>% 
                            pull( onehot )
                ) %>% 
                setNames(nm = names(icd.group)) %>% 
                do.call(bind_cols, .)
        
        
        out <- bind_cols(AENumber = df$AENumber, onehot)
        return(out)
}

onehot_encode_icd_group_by_regex <- function(df = NULL, dir = NULL, icd.group, strip_decimal = T) {
        
        # defaults
        if (!inherits(icd.group, "list")) stop("icd.group should be a named list. For one character vector only, use onehot_encode_icd instead.")
        
        if (is.null(df)) df <- import_stata(dir) %>% 
                        import_and_append(features = paste0("DR", 1:10), dir = dir)
        
        missing <- setdiff(paste0("DR", 1:10), colnames(df))
        if(length(missing)>0) {
                cat(paste0("The following expected columns are missing: ", paste0(missing, collapse = ", "), '. Importing and appending these columns...'))
                df <- df %>% import_and_append(features = missing, dir = dir)
        }
        
        # encode
        DRcolumns <- df %>% select(DR1, DR2, DR3, DR4, DR5, DR6, DR7, DR8, DR9, DR10)
        if (strip_decimal) {
                DRcolumns <- DRcolumns %>% 
                        mutate(across(everything(),
                                      str_replace_all, "\\.", ""))
        }
        
        group.regex <- icd.group %>% 
                map_chr(construct_regex_starts_with)
        
        print(group.regex)
        onehot <- group.regex %>% 
                # for each icd group
                map(function(code_regex) DRcolumns %>% 
                            mutate(across(everything(),
                                          str_detect, pattern = code_regex)
                            ) %>% 
                            mutate( onehot := as.numeric(rowSums(.) > 0)) %>% 
                            pull( onehot )
                ) %>% 
                setNames(nm = names(icd.group)) %>% 
                do.call(bind_cols, .)
        
        
        out <- bind_cols(AENumber = df$AENumber, onehot)
        return(out)
}

construct_regex_starts_with <- function(x) {
        paste(paste0("^", x), collapse = "|")
}

onehot_encode_icd <- function(df = NULL, dir = NULL, icd.codes, strip_decimal = T) {
        
        # defaults
        if (!inherits(icd.group, "list")) stop("icd.group should be a named list. For one character vector only, use onehot_encode_icd instead.")
        
        if (is.null(df)) df <- import_stata(dir) %>% 
                        import_and_append(features = paste0("DR", 1:10), dir = dir)
        
        missing <- setdiff(paste0("DR", 1:10), colnames(df))
        if(length(missing)>0) {
                cat(paste0("The following expected columns are missing: ", paste0(missing, collapse = ", "), '. Importing and appending these columns...'))
                df <- df %>% import_and_append(features = missing, dir = dir)
        }
        
        # encode
        DRcolumns <- df %>% select(DR1:DR10)
        if (strip_decimal) {
                DRcolumns <- DRcolumns %>% 
                        mutate(across(everything(),
                                      str_replace_all, "\\.", ""))
        }
        
        onehot <- icd.codes %>% 
                map(function(code) DRcolumns %>% 
                            mutate(across(everything(),
                                          .fns = function(x) x == code)
                            ) %>% 
                            mutate( {{code}} := as.numeric(rowSums(.) > 0)) %>% 
                            select( .data[[code]])
                ) %>% 
                setNames(nm = icd.codes) %>% 
                do.call(bind_cols, .)
        
        out <- bind_cols(AENumber = df$AENumber, onehot)
        return(out)
        
}


# wrangling ---------------------------------------------------------------

filter_icd <- function(df, codesL = NULL, regex = NULL, icd.codes = NULL, 
                       three_digit = FALSE, whitelist = c()) {
        
        #' @param codesL. Tibble. 
        #' @param three_digit. Logical. Whether the codes are reduced to three-digit level
        #' @param whitelist. Character. If three_digit is set to TRUE, then this is a whitelist such that 5digit is preserved.
        
        # defaults
        if (is.null(codesL)) {
                codesL <- df %>% 
                        extract_icd_codes_long_format(three_digit = three_digit, whitelist = whitelist)
        }
        
        # regex or icd codes?
        annotation.df <- 
                if (!is.null(regex)) {
                        codesL %>% filter(str_detect(code, regex))
                } else if (!is.null(icd.codes)) {
                        codesL %>% filter(code %in% icd.codes) 
                }
        
        unique.codes <- annotation.df %>% pull(code) %>% unique()
        print(paste0(length(unique.codes), " unique codes found:"))
        print(unique.codes)
        
        annotation.df.2 <- annotation.df %>% 
                mutate(pos = as.numeric(str_extract(pos, "\\d"))) %>% 
                pivot_wider(values_from = pos, names_from = code)
        
        filtered <- inner_join(df, annotation.df.2, by = "AENumber")
        return(filtered)
}


extract_icd_codes_long_format <- function(df, three_digit = FALSE, whitelist = c()) {
        #' Extract a long format of icd codes, along with AENumber as primary ID 
        #' (time-consuming step)
        #' 
        #' @param df. Tibble. Input data.
        #' @param three_digit. Logical. Whether the codes are reduced to three-digit level
        #' @param whitelist. Character. If three_digit is set to TRUE, then this is a whitelist such that 5digit is preserved.
        
        icd_codes_long_format <- df %>%
                select(AENumber, starts_with("DR", ignore.case = F)) %>% 
                pivot_codes_long(three_digit = three_digit) %>% 
                mutate(code = str_replace_all(code, "\\.", ""))
        
        return(icd_codes_long_format)
}

# see also: network.R
pivot_codes_long <- function(df, three_digit = FALSE, whitelist = c()) {
        #' Pivot icd codes to long format, preserve position
        #' Preserve all remaining columns other than those starting with capital "DR"
        #' 
        #' @param df. Tibble. Input data.
        #' @param whitelist. Character. If three_digit is set to TRUE, then this is a whitelist such that 5digit is preserved.
        #' @param three_digit. Logical. Whether the codes are reduced to three-digit level
        
        long <- df %>% 
                ### pivot codes long
                pivot_longer(cols = starts_with("DR", ignore.case = FALSE),
                             names_to = "pos",
                             values_to = "code") %>% 
                ### drop na codes
                filter(!is.na(code)) %>% 
                filter(code != "")
        
        # conversion to 3-digit ICD code, strip everything after the decimal point
        # except if code is in whitelist
        if (three_digit) {
                long <- long %>% mutate(code = ifelse(code %in% whitelist, 
                                                      code, sub("\\..+", "", code)))
        }
        
        long <- long %>% 
                ### drop duplicate codes within each record
                group_by(AENumber) %>% 
                distinct(code, .keep_all = T) %>% 
                ungroup()
        long
}


