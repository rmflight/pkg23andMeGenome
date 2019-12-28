#' load rsid
#' 
#' Loads in the RSID file and names the entries so they can be accessed.
#' 
#' @param rsid_file the file to use to load
#' 
#' @export
#' @return list
load_rsid = function(rsid_file = system.file("extdata/rsid.rds", package = "pkg23andMeGenome")){
  if (nchar(rsid_file) == 0) {
    stop("rsid file not found!")
  }
  
  rsid_data = readRDS(rsid_file)
  names(rsid_data) = purrr::map_chr(rsid_data, ~ .x$uid)
  rsid_data
}

#' table of rsid to variables
#' 
#' Create a table of RSIDs to selected variables
#' 
#' @param rsids the list of named rsids to use
#' @param ... the variables (quoted) to map
#' 
#' @export
#' @return data.frame
#' 
rsid_2_vars = function(rsids, ...){
  if (inherits(rsids, "character")) {
    rsids = readRDS(rsids)
  } else if (!inherits(rsids, "list")) {
    stop("rsids are not a list!")
  }
  
  vars = list(...)
  vars = unlist(vars)
  
  rs2vars = purrr::map_df(rsids, function(in_rs){
    #message(in_rs$uid)
    null_vars = purrr::map_lgl(vars, ~ is.null(in_rs[[.x]]))
    if (any(null_vars)) {
      return(NULL)
    } else {
      tmp_rs = in_rs[vars]
      zero_len_var = vars[purrr::map_lgl(tmp_rs, ~ length(.x) == 0)]
      for (ivar in zero_len_var) {
        tmp_rs[[ivar]] = ""
      }
      return(as.data.frame(tmp_rs, stringsAsFactors = FALSE))
    }
    
  })
  
  rs2vars
}