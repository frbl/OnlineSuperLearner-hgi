#!/usr/bin/env Rscript
# Ade4 is needed for the one hot encoding
library('ade4')
library('data.table')

source('create_seperate_hgi_csvs.R')
source('impute_files.R')
source('store_imputed_data.R')
source('run_osl.R')

set.seed(12345)
data_file <- "mad_diary_update19feb2015_merge.csv"
categorical_columns <- c('activity', 'special_event')

# Create the separate CSVs for each individual
create_csvs(data_file=data_file)

# Load and impute the separate data 

# Store all imputed people in a new datatable
impute_all_files()
store_imputed_data()

# Recode the activity values to 0 / 1
imputed_data <- fread('full_imputed_data.csv')
imputed_data[,'V1'] <- NULL


imputed_data[,(categorical_columns) := round(.SD,0), .SDcols=categorical_columns]
# TODO: Fix this:
imputed_data
imputed_data[ , 'special_event' := lapply(.SD, min, 4), .SDcols = 'special_event',  by = 1:nrow(imputed_data)]
imputed_data[ , 'activity' := lapply(.SD, min, 13), .SDcols = 'activity',  by = 1:nrow(imputed_data)]

for (col in categorical_columns) {
  imputed_data_dummy = acm.disjonctif(imputed_data[,col, with=FALSE])
  imputed_data[,col] = NULL
  imputed_data = cbind(imputed_data, imputed_data_dummy)
}

run_osl(imputed_data)

print('Done!')
