library(httr2)

# getting comtrade data ---------------------------------------------------
list_of_datasets <- httr2::request('https://comtradeapi.un.org/files/v1/app/reference/ListofReferences.json') |>
  httr2::req_perform() |>
  httr2::resp_body_json(simplifyVector = T) |>
  purrr::pluck(1)

list_of_datasets$category <- stringr::str_replace_all(list_of_datasets$category,':',"_") |>  tolower()
save(list_of_datasets, file = 'inst/extdata/list_of_datasets.rda')

for(i in 1:nrow(list_of_datasets)){

    response <- httr2::request(list_of_datasets$fileuri[i]) |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = T)
  print(length(response))
  if(length(response)==1){
    result <- response[[1]]
  } else if(length(response)==2) {
    result <- response[[2]]
  } else {
    result <- response[[5]]
  }
  assign(list_of_datasets$category[i],result)

  save(list = list_of_datasets$category[i],
       file = paste0('inst/extdata/',list_of_datasets$category[i],'.rda'))

}

# test <- load("~/Desktop/code_projekte/comtradr/inst/extdata/cmd_h1.rda")
