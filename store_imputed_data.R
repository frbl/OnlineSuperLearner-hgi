store_imputed_data = function() {
  setwd('csv_imputed')
  files <- list.files()
  i <- 0
  imputed_data <- lapply(files, fread)

  # Only use complete cases for now, i.e., 90 measurements
  imputed_data <- lapply(imputed_data, function(x) if(dim(x)[[1]]==90) x)
  imputed_data <- rbindlist(imputed_data)

  print('Loading 100 %')
  print(paste('Loaded', length(files),'files'))

  # Add two trend columns
  personal_trend <- ((seq(nrow(imputed_data)) - 1) %% 90) + 1
  imputed_data[, trend := personal_trend]
  imputed_data[, trend_sq := personal_trend^2]

  setwd('..')
  write.csv(imputed_data, file = 'full_imputed_data.csv')
}
