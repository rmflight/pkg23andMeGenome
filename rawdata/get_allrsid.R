library(pkg23andMeGenome)
rf_rsid = read_rsid("inst/extdata/genome_Robert_Flight_v5_Full_20190305102955.txt")

all_results = query_rsidlist(rf_rsid$rsid, n_query = 250)
saveRDS(all_results, "all_rsid.rds")

all_files = dir(".", pattern = ".rds")
file_sizes = file.size(all_files)
small_files = all_files[file_sizes <= 400]

start_locs = strsplit(small_files, "_")
start_locs = purrr::map_dbl(start_locs, ~ as.double(.x[1]))

missing_results = query_rsidlist(rf_rsid$rsid, start = start_locs)


library(pkg23andMeGenome)
rsid_files = dir("rsid_files", full.names = TRUE)
read_rsid_files(rsid_files, "inst/extdata/all_rsid.rds")
