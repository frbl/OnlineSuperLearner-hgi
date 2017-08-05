#install_github('roqua/autovar')

impute_all_files <- function(measurements_per_day = 3,
                           removed_columns = c('time', 'X', 'date')){

  setwd('csv')
  files <- list.files()
  file_list <- list()
  i <- 0
  for(file in files){
    print(paste('Loading', file, '(', (i / length(files)) * 100,'%)'))
    file_list[[file]] <- autovar::load_file(file,log_level = 3)
    i <- i + 1
  }
  print('Loading 100 %')
  print(paste('Loaded', length(files),'files'))

  set.seed(12345)
  i<- 0
  #file_list <- mclapply(files, function(file_name) {
  setwd('../')
  setwd('csv_imputed')
  for(file_name in files) {
    print(paste('Imputing', file_name, '(', (i / length(files)) * 100,'%)'))
    if (file.exists(file_name)) {
      next 
    }
    file <- file_list[[file_name]]

    first_date <- file$data$multiple$date[1]

    # Remove unused columns
    print(paste('Removing', removed_columns))
    raw_data <-file$raw_data[,!(names(file$raw_data) %in% removed_columns)]

    imputed_data <- autovar::impute_dataframe(raw_data,
                                                measurements_per_day = measurements_per_day, 
                                                repetitions = 150)
    #file <- autovar::set_timestamps(file, date_of_first_measurement=first_date,
                          #measurements_per_day = measurements_per_day,
                          #log_level=3)

    write.csv(imputed_data, file = file_name)

    i <- i + 1
  }
  #}, mc.cores = 8)
  print('Imputing 100 %')
  setwd("../")
  file_list
}
