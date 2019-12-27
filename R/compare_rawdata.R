#' compare rawdata
#' 
#' Given multiple rawdata files, compares the basecalls at each position and
#' returns a data.frame of differences.
#' 
#' @param ... named list of rawdata files
#' 
#' @export
#' @return data.frame
#' 
#' @example 
#' \dontrun{
#' compare_rawdata(mom = "path_to_rawdata.txt", me = "path_to_rawdata2.txt")
#' }
#' 
compare_rawdata = function(...){
  in_files = list(...)
  
  file_rsid = purrr::map(in_files, ~ read_rsid(.x))
  
  ref_set = file_rsid[[1]]
  
  diff_raw = purrr::map(file_rsid[-1], function(.x){
    .x$genotype != ref_set$genotype
  })
  
  for (inames in names(file_rsid)) {
    ref_set[[inames]] = file_rsid[[inames]]$genotype
  }
  
  ref_set$diff = ""
  ref_set = as.data.frame(ref_set)
  for (inames in names(diff_raw)) {
    use_diff = diff_raw[[inames]]
    ref_set[use_diff, "diff"] = paste(ref_set[use_diff, "diff"], inames, sep = ",")
  }
  ref_set
}
