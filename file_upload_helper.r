df_from_file <- function(file, func) {
  req(file)
  tryCatch(
    {
      df <- func(file$datapath)
    },
    error = function(e){
      stop(safeError(e))
    })
  df
}