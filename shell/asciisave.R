

saveImageInAsciiRDS <- function(env = .GlobalEnv) {
  vars <- setdiff(ls(pos = env, all.names = TRUE), lsf.str(pos = env, all.names = TRUE))
  for (n in vars) {
    file <- paste0(n, ".rds")
    obj <- get(n, pos = env)
    saveRDS(obj, file = file, ascii = TRUE, compress = FALSE)
  }
}
