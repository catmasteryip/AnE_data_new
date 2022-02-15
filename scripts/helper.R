library(icd)
uniqueIcd_pivotLonger = function(df){
  
  #' find unique set of icd9 codes per patient, transform from wide to long format
  #' 
  #' @param df The wide dataframe object with AENumber and DR1-10 icd9 codes

  df = df %>% 
    pivot_longer(cols = -AENumber, names_to = "diagnosis", values_to = "icd9") %>% 
    group_by(AENumber) %>% 
    distinct(icd9,.keep_all=T) %>% 
    ungroup() %>% 
    mutate(icd9 = ifelse(icd9== "", NA, icd9)) %>% 
    drop_na()
  
  return(df)
}
compare_ctrlvstrt = function(daily, vs=2020){
  # mutate year and assign 2019 same-period 
  # 2021 dates
  dates.2021 = daily %>% 
    filter(year == 2021) %>%
    select(yday) %>%
    unique() %>%
    pull(yday)
  # make 2019 same-period dataframe
  # bind_rows to original daily dataframe
  # count by year 
  # return
}