library(httr2)

# getting comtrade data ---------------------------------------------------

## getting list of reference tables
response <- httr2::request('https://comtradeapi.un.org/files/v1/app/reference/ListofReferences.json') |>
  httr2::req_perform()

## getting data from response of list of tables
list_of_datasets <- response |>
  httr2::resp_body_json(simplifyVector = T) |>
  purrr::pluck(1)

## getting date of last modification from list of tables
last_modified <- httr2::resp_header(header = "Last-Modified", resp = response) |>
  stringr::str_extract(pattern = '(\\d{2} [a-zA-Z]+ \\d{4})') |>
  as.Date(format = "%d %b %Y")

## writing last modification to data
list_of_datasets$last_modified <- last_modified

## changing colon to underscore in dataset names
list_of_datasets$category <- stringr::str_replace_all(list_of_datasets$category,':',"_") |>
  tolower()

save(list_of_datasets, file = 'inst/extdata/list_of_datasets.rda')

for(i in 1:nrow(list_of_datasets)){

    response <- httr2::request(list_of_datasets$fileuri[i]) |>
    httr2::req_perform()

    data <- response |>
    httr2::resp_body_json(simplifyVector = T)

    last_modified <- httr2::resp_header(header = "Last-Modified", resp = response) |>
      stringr::str_extract(pattern = '(\\d{2} [a-zA-Z]+ \\d{4})') |>
      as.Date(format = "%d %b %Y")

  print(length(data))
  if(length(data)==1){
    result <- data[[1]]
  } else if(length(data)==2) {
    result <- data[[2]]
  } else {
    result <- data[[5]]
  }

  result$last_modified <- last_modified

  readr::write_rds(result,
       file = paste0('inst/extdata/',list_of_datasets$category[i],'.rds'))
}



# -------------------------------------------------------------------------
switch <- list_of_datasets |>
  mutate(class_code = stringr::str_split_i(fileuri, '/',8) |> stringr::str_remove('.json')) |>
  filter(variable=='Product') |>
  select(class_code,category) |>
  readr::write_delim(file = 'data-raw/switch_cmd_code.csv',delim = ' - ')
