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

## changing colon to underscore in dataset names and adding prefix 'ref'
list_of_datasets$category <- stringr::str_replace_all(list_of_datasets$category,':',"_") |>
  tolower() |>
  stringr::str_c('ref_',y =_)

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

  assign(list_of_datasets$category[i],result)

  save(list = list_of_datasets$category[i],
       file = paste0('inst/extdata/',list_of_datasets$category[i],'.rda'))
}
