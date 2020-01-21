####### Contatnts #######

PATH_TO_TEMP = '.\\data\\temp'
PATH_TO_RESULT = '.\\data\\prepared'

####### Useful variable #######

init_df <- data.frame(
  Name = character(),
  ShortDescription = character(),
  AgeRestriction = character(),
  IsFree = logical(),
  Price = numeric(),
  MaxPrice = numeric(),
  Category = character(),
  Status = character(),
  StartDttm = character(),
  EndDttm = character(),
  PlaceName = character(),
  LocaleName = character(),
  CoordinateX = character(),
  CoordinateY = character(),
  stringsAsFactors = FALSE
)

####### Working with JSON #######

load_json_file <- function(path_to_file) {
  RJSONIO::fromJSON(path_to_file, encoding = 'UTF-8')
}


grab_info <- function(One_row) {
  ret <- list(
    "Name" = One_row$data$general$name,
    "ShortDescription" = One_row$data$general$shortDescription,
    "AgeRestriction" = One_row$data$general$ageRestriction,
    "IsFree" = One_row$data$general$isFree,
    "Price" = One_row$data$general$price,
    "MaxPrice" = One_row$data$general$maxPrice,
    "Category" = (One_row$data$general$category['name'])[[1]],
    "Status" = One_row$data$general$status,
    "StartDtm" = One_row$data$general$start,
    "EndDttm" = One_row$data$general$end,
    "PlaceName" = (One_row$data$general$places[[1]])$name,
    "LocaleName" = (One_row$data$general$places[[1]])$locale$name,
    "CoordinateX" = (One_row$data$general$places[[1]])$address$mapPosition$coordinates[[1]],
    "CoordinateY" = (One_row$data$general$places[[1]])$address$mapPosition$coordinates[[2]]
  )
  ret[sapply(ret, is.null)] <- NA
  ret
}

extract <- function(json_file, df) {
  for (obs in json_file) {
    df[nrow(df) + 1, ] <- grab_info(obs)
  }
  df
}

####### Auxiliary functions #######

# creates directory for intermediate dataframes, returns path to this one
create_temp_dir <- function(save.parts = TRUE) {
  if (save.parts) {
    tm <- as.character(Sys.time(), format = "%Y%m%d%H%M%S")
    curr_temp_dir <- file.path(PATH_TO_TEMP, tm)
    dir.create(curr_temp_dir)
    curr_temp_dir
  }
}

# saves part of data from one file, resets empty dataframe
save_part <- function(resul_df, curr_temp_dir, file, save.parts = TRUE) {
    if (save.parts) {
      saveRDS(resul_df, file =  file.path(curr_temp_dir, gsub("json", "rds", basename(file))))
      resul_df <- init_df
    }
    resul_df
  }

get_df_from_temp_parts <- function(curr_temp_dir) {
  files_pathes <- list.files(curr_temp_dir, full.names = TRUE)
  result_df <- init_df
  for (file in files_pathes) {
    print(file)
    df <- readRDS(file)
    result_df <- dplyr::union(result_df, df)
  }
  result_df
}
    
    ####### Base function #######
get_data_from_dir <-  function(path_to_dir, save.parts = TRUE, save.result = TRUE, save.result.file = NA) {
  curr_temp_dir <- create_temp_dir(save.parts)
  files_pathes <- list.files(path_to_dir, full.names = TRUE)
  resul_df <- init_df
  for (file in files_pathes) {
    json_file <- load_json_file(file)
    resul_df <- extract(json_file, resul_df)
    resul_df <- save_part(resul_df, curr_temp_dir, file, save.parts)
  }
  if (save.parts) {
    resul_df <- get_df_from_temp_parts(curr_temp_dir)
  }
  if (save.result) {
    resul_filename <- dplyr::coalesce(c(as.character(save.result.file)), as.character(Sys.time(), format = "%Y%m%d%H%M%S"))
    saveRDS(resul_df, file =  file.path(PATH_TO_RESULT, paste0(resul_filename, ".rds")))
  }
  resul_df
}




