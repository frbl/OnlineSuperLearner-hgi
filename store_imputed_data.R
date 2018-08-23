store_imputed_data = function() {
  setwd('csv_imputed')
  files <- list.files()

  imputed_data <- lapply(files, fread)

  key <- 'number-of-hnd-participants'
  OnlineSuperLearner::OutputPlotGenerator.export_key_value(key = key, value = length(files))
  # Only use complete cases for now, i.e., 90 measurements
  imputed_data <- lapply(imputed_data, function(entry) {
    if(dim(entry)[[1]]==90 && !any(is.na(entry))) entry
  })


  # Remove NULL
  imputed_data <- Filter(Negate(is.null), imputed_data) 
  i = 1
  for(df in imputed_data) {
    df[, id := i]
    i = i + 1
  }

  error_files <- c()
  #error_files <- error_files + 3

  # Remove the files that throw an error
  imputed_data_copy <- list()
  for (df in imputed_data) {
    if(!(unique(df[,id]) %in% error_files)) {
      imputed_data_copy  <- append(imputed_data_copy, list(df))
    }
  }
  
  imputed_data <- rbindlist(imputed_data_copy)

  print('Loading 100 %')
  print(paste('Loaded', length(files),'files'))

  # Add two trend columns

  personal_trend <- ((seq(nrow(imputed_data)) - 1) %% 90) + 1
  imputed_data[, trend := personal_trend]
  imputed_data[, trend_sq := personal_trend^2]

  setwd('..')
  write.csv(imputed_data, file = 'full_imputed_data.csv')
}
