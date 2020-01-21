library(dplyr)

preprocess <- function(df) {
  df %>% 
    mutate(Price = case_when(
      IsFree == TRUE ~ 0,
      TRUE ~ Price
      )
    ) %>%
    mutate(MaxPrice = case_when(
      IsFree == TRUE ~ 0,
      TRUE ~ MaxPrice
      )
    ) %>% 
    mutate(StartDttm = lubridate::ymd_hms(StartDttm)) %>% 
    mutate(EndDttm = lubridate::ymd_hms(EndDttm)) %>% 
    mutate(Duration = EndDttm - StartDttm) %>% 
    mutate(AgeRestriction = as.factor(AgeRestriction)) %>% 
    mutate(Category = as.factor(Category)) %>% 
    select(-c(IsFree, Status))
  
}
