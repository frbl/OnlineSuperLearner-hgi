
algos <- list()


algos <- append(algos, list(list(algorithm = 'ML.XGBoost',
                        algorithm_params = list(alpha = 0),
                        params = list(nbins = nbins, online = TRUE))))

#algos <- append(algos, list(list(algorithm = 'ML.H2O.gbm',
                        #algorithm_params = list(ntrees=c(10,20), min_rows=1),
                        #params = list(nbins = c(6), online = TRUE))))

#algos <- append(algos, list(list(algorithm = 'ML.H2O.randomForest',
                        #algorithm_params = list(ntrees=c(10,20)),
                        #params = list(nbins = nbins, online = TRUE))))

#algos <- append(algos, list(list(algorithm = 'condensier::speedglmR6',
                        ##algorithm_params = list(),
                        #params = list(nbins = c(39, 40), online = FALSE))))

algos <- append(algos, list(list(algorithm = 'ML.Local.Speedlm',
                        #algorithm_params = list(),
                        params = list(nbins = nbins, online = FALSE))))

#algos <- append(algos, list(list(algorithm = 'ML.GLMnet',
                        #algorithm_params = list(alpha = c(0.3,0.8)),
                        #params = list(nbins = nbins, online = FALSE))))

#algos <- append(algos, list(list(algorithm = 'condensier::glmR6',
                        ##algorithm_params = list(),
                        #params = list(nbins = c(39, 40), online = FALSE))))


run_osl <- function(data.train) {

  set.seed(12345)

  # We'd like to use the following features in our estimation:
  #W1 <- RandomVariable$new(formula = W ~ Y_lag_1 + A_lag_1 +  W_lag_1 + Y_lag_2, family = 'gaussian')
  #A <- RandomVariable$new(formula = A ~ W + Y_lag_1 + A_lag_1 + W_lag_1, family = 'binomial')

  # W
  here_and_now    <- RandomVariable$new(formula = here_and_now ~    , family = 'gaussian')
  busy            <- RandomVariable$new(formula = busy ~            , family = 'gaussian')
  special_event_A <- RandomVariable$new(formula = special_event_A ~ , family = 'binomial')
  special_event_B <- RandomVariable$new(formula = special_event_A ~ , family = 'binomial')
  special_event_C <- RandomVariable$new(formula = special_event_A ~ , family = 'binomial')
  special_event_D <- RandomVariable$new(formula = special_event_A ~ , family = 'binomial')
  humor           <- RandomVariable$new(formula = humor ~           , family = 'gaussian')
  make_difference <- RandomVariable$new(formula = make_difference ~ , family = 'gaussian')
  be_outside      <- RandomVariable$new(formula = be_outside ~      , family = 'gaussian')
  activity_level  <- RandomVariable$new(formula = activity_level ~  , family = 'gaussian')
  aware           <- RandomVariable$new(formula = aware ~           , family = 'gaussian')

  # A
  activity_A      <- RandomVariable$new(formula = activity_A ~ , family = 'binomial')
  activity_B      <- RandomVariable$new(formula = activity_B ~ , family = 'binomial')
  activity_C      <- RandomVariable$new(formula = activity_C ~ , family = 'binomial')
  activity_D      <- RandomVariable$new(formula = activity_D ~ , family = 'binomial')
  activity_E      <- RandomVariable$new(formula = activity_E ~ , family = 'binomial')
  activity_F      <- RandomVariable$new(formula = activity_F ~ , family = 'binomial')
  activity_G      <- RandomVariable$new(formula = activity_G ~ , family = 'binomial')
  activity_H      <- RandomVariable$new(formula = activity_H ~ , family = 'binomial')
  activity_I      <- RandomVariable$new(formula = activity_I ~ , family = 'binomial')
  activity_J      <- RandomVariable$new(formula = activity_J ~ , family = 'binomial')
  activity_K      <- RandomVariable$new(formula = activity_K ~ , family = 'binomial')
  activity_L      <- RandomVariable$new(formula = activity_L ~ , family = 'binomial')
  activity_M      <- RandomVariable$new(formula = activity_M ~ , family = 'binomial')

  # Y
  PA              <- RandomVariable$new(formula = PA ~ , family = 'gaussian')

  randomVariables <- c(here_and_now, busy, special_event_A, special_event_B, special_event_C,
                       special_event_D, humor, make_difference, be_outside, activity_level, aware,
                       activity_A, activity_B, activity_C, activity_D, activity_E, activity_F,
                       activity_G, activity_H, activity_I, activity_J, activity_K, activity_L,
                       activity_M,
                       PA) 

  # Create the bounds
  bounds <- OnlineSuperLearner::PreProcessor.generate_bounds(data.train)

  # Create the measures we'd like to include in our model
  # In this simulation we will include 2 lags and the latest data (non lagged)
  # Define the variables in the initial dataset we'd like to use
  #private$train(data.test, data.train, bounds, randomVariables, 2)
  train(data.train, bounds, randomVariables, PA,  max_iterations = 2)
}

train = function(data.train, bounds, randomVariables, variable_of_interest, max_iterations) {
          data.train.static <- Data.Static$new(data.train)

          outcome.variables <- sapply(randomVariables, function(rv) rv$getY)
          smg_factory <- SMGFactory$new()

          pre_processor <- PreProcessor$new(bounds = bounds)
          summaryMeasureGenerator <- smg_factory$fabricate(randomVariables, 
                                                           pre_processor = pre_processor,
                                                           data = data.train.static)

          print('Initializing OSL')
          osl <- OnlineSuperLearner$new(algos,
                                        summaryMeasureGenerator = summaryMeasureGenerator,
                                        pre_processor = pre_processor,
                                        verbose = FALSE)

          print('Running OSL')

          mini_batch_size <- (private$training_set_size / 2) / max_iterations
          mini_batch_size <- ifelse(is.na(mini_batch_size) || is.infinite(mini_batch_size), 1, mini_batch_size)

          # Divide by two here just so the initial size is a lot larger then each iteration, not really important
          risk <- osl$fit(data.train.static, randomVariables = randomVariables,
                          initial_data_size = private$training_set_size / 2,
                          max_iterations = max_iterations,
                          mini_batch_size = mini_batch_size) %T>%
            print


          private$log && cat(private$log, 'Predicting using all estimators')

          # Calculate prediction quality
          observed.outcome <- data.test[, outcome.variables, with=FALSE]
          predicted.outcome <- osl$predict(data = copy(data.test), randomVariables, plot= TRUE)

          #performance <- 
            #private$cv_risk_calculator$calculate_evaluation(predicted.outcome = predicted.outcome,
                                                            #observed.outcome = observed.outcome,
                                                            #randomVariables = randomVariables) 

          #OutputPlotGenerator.create_risk_plot(performance, 'performance', '/tmp/osl/')

          #})
          #performances <- do.call(rbind, lapply(performances, data.frame)) %T>%
          #print

          #plot(x=performances$iterations, y=performances$performance)
          #performances
          # TODO:!!!!!!!!!!!!!!!!!!!!!!!!! THIS SHOULD BE AN INTERVENTION OVER ALL A !!!!!!!!!!!!!!!!
          intervention <- list(variable = 'A', when = c(2), what = c(1))
          tau = 2
          B <- 100

          pre <- options('warn')$warn

          # Note that this won't work when we have an H2O estimator in the set. The parallelization will fail.
          O_0 <- data.train[1,]
          cat('Sampling from Pn* (for approximation)...\n')
          outcome <- variable_of_interest$getY


          result.dosl <- foreach(i=seq(B), .combine=rbind) %dopar% {
            print(i)
            osl$sample_iteratively(data = O_0,
                                   randomVariables = randomVariables,
                                   intervention = intervention,
                                   discrete = TRUE,
                                   tau = tau)[tau, outcome, with=FALSE]
          } %>%
            unlist

          result.osl <- foreach(i=seq(B), .combine=rbind) %dopar% {
            print(i)
            osl$sample_iteratively(data = O_0,
                                   randomVariables = randomVariables,
                                   intervention = intervention,
                                   return_type = 'observations',
                                   discrete = FALSE,
                                   tau = tau)[tau, outcome, with=FALSE]
          } %>%
            unlist


          options(warn=pre)

          result.dosl.mean <- result.dosl %>% mean
          result.osl.mean <- result.osl %>% mean

          # Plot the convergence
          data <- list(dosl = result.dosl, osl = result.osl)
          OnlineSuperLearner::OutputPlotGenerator.create_convergence_plot(data = data,
                                                                          output = paste('convergence_configuration',
                                                                                         1,sep='_'))

          #browser()
          osl$info

          #lapply(performance, function(x) {lapply(x,mean)})

          # Now, the fimal step is to apply the OneStepEstimator
          OOS <- OnlineSuperLearner::OneStepEstimator$new(osl = osl, 
                                                          randomVariables = randomVariables, 
                                                          discrete = TRUE,
                                                          N = 90, 
                                                          B = B,
                                                          pre_processor = pre_processor
                                                          )

          result.approx.mean.updated <- OOS$perform(initial_estimate = result.dosl.mean,
                                                   data = data.test,
                                                   variable_of_interest = variable_of_interest,
                                                   intervention = intervention,
                                                   tau = tau)

          print(paste('The difference between the estimate and approximation (after oos) for dosl is: ',
                      abs(result.approx.mean - result.approx.mean.updated$oos_estimate)))
          differences
        }

