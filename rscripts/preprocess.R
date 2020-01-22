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
    # чаще всего большая цена - ошибка ввода данных
    # за предыдущие года данных слишком мало, 
    # в будущее нет смысла смотреть, так как данные неполные
    filter(StartDttm >= as.POSIXct('2017-01-01 00:00:00') & StartDttm < as.POSIXct('2020-01-01 00:00:00') & Price < 100000) %>% 
    
    mutate(StartDttm = lubridate::ymd_hms(StartDttm)) %>% 
    mutate(EndDttm = lubridate::ymd_hms(EndDttm)) %>% 
    mutate(Duration = difftime(EndDttm, StartDttm, units='hours')) %>% 
    mutate(AgeRestriction = as.factor(AgeRestriction)) %>% 
    mutate(Category = as.factor(Category)) %>% 
    mutate(Lat = as.numeric(CoordinateY)) %>% 
    mutate(Lng = as.numeric(CoordinateX)) %>% 

    select(-c(IsFree, Status, CoordinateX, CoordinateY))

}
