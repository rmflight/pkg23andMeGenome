#' read rsid file
#' 
#' given a genome file, read it into a data.frame that can be queried and compared
#' with others.
#' 
#' @param rsid_file the file to read
#' 
#' @importFrom readr read_delim col_character col_double
#' 
#' @export
read_rsid = function(rsid_file){
  tmp = readr::read_delim(rsid_file, delim = "\t", skip = 20,
                          col_names = c("rsid", "chromosome", "position", "genotype"),
                          col_types = 
                            readr::cols(rsid = readr::col_character(),
                                 chromosome = readr::col_character(),
                                 position = readr::col_double(),
                                 genotype = readr::col_character()))
  tmp
}

#' query entrez rsid
#' 
#' given an rsid, query entrez for it and save the results
#' 
#' @param rsid the rsid to query
#' 
#' @import rentrez
#' 
#' @export
query_rsid = function(rsid){
  #message(rsid)
  Sys.sleep(10)
  new_id = gsub("rs", "", rsid) %>% gsub("i", "", .)
  
  id_summary = try(rentrez::entrez_summary(db = "snp", id = new_id))
  id_summary
}

#' get a bunch of rsid
#' 
#' given a list of rsid, politely get the data from entrez
#' 
#' @param rsid_list the list of rsids to query
#' @param n_query how many to query at each time (default = 100)
#' @param start which item in the list to start with (allows for restarts)
#' 
#' @import purrr
#' 
#' @export
query_rsidlist = function(rsid_list, n_query = 250, start = 1){
  
  if (length(start) > 1) {
    start_indices = start
  } else {
    start_indices = seq(1, length(rsid_list), by = n_query)
    start_indices = start_indices[start_indices >= start]
  }
  
  out_list = purrr::map(start_indices, function(start_loc){
    end_loc = start_loc + n_query - 1
    if (end_loc > length(rsid_list)) {
      end_loc = length(rsid_list)
    }
    #message(c(start_loc, end_loc))
    query_list = rsid_list[start_loc:end_loc]
    out_rsid = query_rsid(query_list)
    filename = paste0(start_loc, "_", end_loc, ".rds")
    saveRDS(out_rsid, file = filename)
    out_rsid
  })
  
  tmp = unlist(out_list, recursive = FALSE, use.names = FALSE)
  tmp
}

#' read rsid files
#' 
#' Reads in a bunch of RSID queries from a set of files and saves it to a single
#' object that can be queried.
#' 
#' @param rsid_files the set of files to read in
#' @param save_file where to save the full set
#' 
#' @export
#' @return NULL
read_rsid_files = function(rsid_files, save_file){
  rsid_list = purrr::map(rsid_files, ~ readRDS(.x))
  tmp = unlist(rsid_list, recursive = FALSE, use.names = FALSE)
  saveRDS(tmp, file = save_file)
}