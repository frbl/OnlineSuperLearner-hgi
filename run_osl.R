suppressWarnings(devtools::load_all('../OnlineSuperLearner'))
library('magrittr')
library('R.oo')
library('R.utils')
library('parallel')
library('foreach')
library('doParallel')

nbins <- c(20, 30, 40, 50)#, 60, 70)
algos <- list()


nbins <- c(40, 50, 60, 70)
algos <- list()


alphas <- runif(3,0,1)
alphas <- c(0, alphas)
algos <- append(algos, list(list(algorithm = 'ML.XGBoost',
                        algorithm_params = list(alpha = alphas), 
                        params = list(nbins = nbins, online = FALSE))))

#algos <- append(algos, list(list(algorithm = 'ML.H2O.gbm',
                        #algorithm_params = list(ntrees=c(10,20), min_rows=1),
                        #params = list(nbins = c(6), online = TRUE))))

#algos <- append(algos, list(list(algorithm = 'ML.H2O.randomForest',
                        #algorithm_params = list(ntrees=c(10,20)),
                        #params = list(nbins = nbins, online = TRUE))))

#algos <- append(algos, list(list(algorithm = 'ML.SVM',
                        ##algorithm_params = list(),
                        #params = list(nbins = nbins, online = FALSE))))

#algos <- append(algos, list(list(algorithm = 'ML.NeuralNet',
                        ###algorithm_params = list(),
                        #params = list(nbins = nbins, online = TRUE))))

algos <- append(algos, list(list(algorithm = 'ML.randomForest',
                        algorithm_params = list(ntrees=c(500,1000)),
                        params = list(nbins = nbins, online = FALSE))))

algos <- append(algos, list(list(algorithm = 'ML.Local.Speedlm',
                        #algorithm_params = list(),
                        params = list(nbins = nbins, online = FALSE))))

#algos <- append(algos, list(list(algorithm = 'ML.GLMnet',
                        ##algorithm_params = list(alpha = alphas),
                        #params = list(nbins = nbins, online = FALSE))))

#algos <- append(algos, list(list(algorithm = 'condensier::glmR6',
                        ##algorithm_params = list(),
                        #params = list(nbins = c(39, 40), online = FALSE))))


run_osl <- function(data.train) {

  set.seed(12345)

  # We'd like to use the following features in our estimation:
  W <- c(
  'here_and_now',
  'age',
  'gender',
  ## 'busy.1'          ,
  'busy.2',
  'busy.3',
  'busy.4',
  'busy.5',
  ## 'special_event.1' ,
  'special_event.2',
  'special_event.3',
  'special_event.4',
  'humor',
  'make_difference',
  'be_outside',
  'activity_level',
  'aware', 'id') 

  A <- c(
  ## 'activity.0', 
  'activity.1', 
  'activity.2', 
  'activity.3', 
  'activity.4', 
  'activity.5', 
  'activity.6', 
  'activity.7', 
  'activity.8', 
  'activity.9', 
  'activity.10', 
  'activity.11', 
  'activity.12')

  Y <- c('pa')

  ## Note that we removed special_event.1 and activity.0, as including these in
  ## the model would introduce a multicolliniar system. These are loaded on the
  ## intercept term.
  ## TODO: Check if indeed all algorithms include an intercept. If not, we should include it manually here.


  formulae <- generate_formulae(W,A,Y)

  age             <- RandomVariable$new(formula = formulae$W$age             , family = 'gaussian')
  gender          <- RandomVariable$new(formula = formulae$W$gender          , family = 'binomial')
  here_and_now    <- RandomVariable$new(formula = formulae$W$here_and_now    , family = 'gaussian')
  #busy.1          <- RandomVariable$new(formula = formulae$W$busy.1            , family = 'binomial')
  busy.2          <- RandomVariable$new(formula = formulae$W$busy.2          , family = 'binomial')
  busy.3          <- RandomVariable$new(formula = formulae$W$busy.3          , family = 'binomial')
  busy.4          <- RandomVariable$new(formula = formulae$W$busy.4          , family = 'binomial')
  busy.5          <- RandomVariable$new(formula = formulae$W$busy.5          , family = 'binomial')
  #special_event.1 <- RandomVariable$new(formula = formulae$W$special_event.1 , family = 'binomial')
  special_event.2 <- RandomVariable$new(formula = formulae$W$special_event.2 , family = 'binomial')
  special_event.3 <- RandomVariable$new(formula = formulae$W$special_event.3 , family = 'binomial')
  special_event.4 <- RandomVariable$new(formula = formulae$W$special_event.4 , family = 'binomial')
  humor           <- RandomVariable$new(formula = formulae$W$humor           , family = 'gaussian')
  make_difference <- RandomVariable$new(formula = formulae$W$make_difference , family = 'gaussian')
  be_outside      <- RandomVariable$new(formula = formulae$W$be_outside      , family = 'gaussian')
  activity_level  <- RandomVariable$new(formula = formulae$W$activity_level  , family = 'gaussian')
  aware           <- RandomVariable$new(formula = formulae$W$aware           , family = 'gaussian')

  # A
  #activity.0       <- RandomVariable$new(formula = formulae$A$activity.0      , family = 'binomial')
  activity.1       <- RandomVariable$new(formula = formulae$A$activity.1      , family = 'binomial')
  activity.2       <- RandomVariable$new(formula = formulae$A$activity.2      , family = 'binomial')
  activity.3       <- RandomVariable$new(formula = formulae$A$activity.3      , family = 'binomial')
  activity.4       <- RandomVariable$new(formula = formulae$A$activity.4      , family = 'binomial')
  activity.5       <- RandomVariable$new(formula = formulae$A$activity.5      , family = 'binomial')
  activity.6       <- RandomVariable$new(formula = formulae$A$activity.6      , family = 'binomial')
  activity.7       <- RandomVariable$new(formula = formulae$A$activity.7      , family = 'binomial')
  activity.8       <- RandomVariable$new(formula = formulae$A$activity.8      , family = 'binomial')
  activity.9       <- RandomVariable$new(formula = formulae$A$activity.9      , family = 'binomial')
  activity.10      <- RandomVariable$new(formula = formulae$A$activity.10     , family = 'binomial')
  activity.11      <- RandomVariable$new(formula = formulae$A$activity.11     , family = 'binomial')
  activity.12      <- RandomVariable$new(formula = formulae$A$activity.12     , family = 'binomial')

  # Y
  pa              <- RandomVariable$new(formula = formulae$Y$pa              , family = 'gaussian')

  randomVariables <- c(here_and_now, 
                       gender,
                       age,
                       #busy.1, 
                       busy.2, 
                       busy.3, 
                       busy.4, 
                       busy.5, 
                       #special_event.1, 
                       special_event.2, 
                       special_event.3,
                       special_event.4, 
                       humor, make_difference, be_outside, activity_level, aware,
                       #activity.0, 
                       activity.1, 
                       activity.2,
                       activity.3, 
                       activity.4,
                       activity.5,
                       activity.6, 
                       activity.7, 
                       activity.8, 
                       activity.9,
                       activity.10, 
                       activity.11,
                       activity.12,
                       pa) 

  # Create the bounds
  bounds <- OnlineSuperLearner::PreProcessor.generate_bounds(data.train)

  # Create the measures we'd like to include in our model
  # In this simulation we will include 2 lags and the latest data (non lagged)
  # Define the variables in the initial dataset we'd like to use
  #private$train(data.test, data.train, bounds, randomVariables, 2)
  train(data.train, bounds, randomVariables, pa,  max_iterations = 0, A)
}

train = function(data.train, bounds, randomVariables, variable_of_interest, max_iterations, A) {


          log <- Arguments$getVerbose(-8, timestamp=TRUE)
          data.train.static <- Data.Static$new(data.train)

          outcome.variables <- sapply(randomVariables, function(rv) rv$getY)
          smg_factory <- SMGFactory$new()

          pre_processor <- PreProcessor$new(bounds = bounds)
          summaryMeasureGenerator <- smg_factory$fabricate(randomVariables, 
                                                           pre_processor = pre_processor,
                                                           data = data.train.static,
                                                           number_of_observations_per_timeseries = 90
                                                           )

          print('Initializing OSL')
          osl <- OnlineSuperLearner$new(algos,
                                        summaryMeasureGenerator = summaryMeasureGenerator,
                                        pre_processor = pre_processor,
                                        verbose = log)

          print('Running OSL')

          # Divide by two here just so the initial size is a lot larger then each iteration, not really important
          risk <- osl$fit(data.train.static, randomVariables = randomVariables,
                          #initial_data_size = 89,
                          initial_data_size = 500,
                          max_iterations = (nrow(data.train) / 90),
                          mini_batch_size = 500) %T>%
                          #mini_batch_size = 89) %T>%
            print

          # Calculate prediction quality
          data.train.static$reset
          summaryMeasureGenerator$reset()
          data.train.augmented <- summaryMeasureGenerator$getNext(nrow(data.train) - 1)
          observed.outcome <- data.train.augmented[, outcome.variables, with=FALSE]
          predicted.outcome <- osl$predict(data = copy(data.train.augmented), randomVariables, plot= TRUE)

          cv_risk_calculator <- OnlineSuperLearner::CrossValidationRiskCalculator$new()

          performance <- 
            cv_risk_calculator$calculate_evaluation(predicted.outcome = predicted.outcome,
                                                            observed.outcome = observed.outcome,
                                                            randomVariables = randomVariables) 

          #OutputPlotGenerator.create_risk_plot(performance, 'performance', '/tmp/osl/')

          #})
          #performances <- do.call(rbind, lapply(performances, data.frame)) %T>%
          #print

          #plot(x=performances$iterations, y=performances$performance)
          #performances
          A <- append(A, 'activity.0')
          
          final_result <- lapply(A, function(a) {
            if (a == 'activity.0') {
              intervention <- OnlineSuperLearner::InterventionParser.generate_intervention(A, NULL, when = 2, what = 1)
            } else {
              intervention <- OnlineSuperLearner::InterventionParser.generate_intervention(A, a, when = 2, what = 1)
            }
            tau <- 3
            B <- 100

            pre <- options('warn')$warn

            # Note that this won't work when we have an H2O estimator in the set. The parallelization will fail.
            O_0 <- data.train.augmented[1,]
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
            print(data)
            OnlineSuperLearner::OutputPlotGenerator.create_convergence_plot(data = data,
                                                                            dir = '~/tmp/hgi',
                                                                            output = paste('convergence_configuration',
                                                                            a,sep='_'))

            list(dosl.mean = result.dosl.mean, osl.mean = result.osl.mean)
          })

          names(final_result) <- A

          for (act in A) {
            key <- paste(act,'dosl-pre-oos', sep='-')
            OnlineSuperLearner::OutputPlotGenerator.export_key_value(key = key, 
                                                                     dir = '~/tmp/hgi',
                                                                     value = round(as.numeric(final_result[[act]]$dosl.mean),2))
            key <- paste(act,'osl-pre-oos', sep='-')
            OnlineSuperLearner::OutputPlotGenerator.export_key_value(key = key, 
                                                                     dir = '~/tmp/hgi',
                                                                     value = round(as.numeric(final_result[[act]]$osl.mean),2))
          }

          key_performance = 'risk_cv_summary_cfg_act'
          OutputPlotGenerator.create_risk_plot(performance=osl$get_cv_risk, 
                                               output=key_performance, make_summary=TRUE, 
                                               dir = '~/tmp/hgi',
                                               label='total.risk')

          

          #lapply(performance, function(x) {lapply(x,mean)})

          # Now, the fimal step is to apply the OneStepEstimator
          #OOS <- OnlineSuperLearner::OneStepEstimator$new(osl = osl, 
                                                          #randomVariables = randomVariables, 
                                                          #discrete = TRUE,
                                                          #N = 90, 
                                                          #B = B,
                                                          #pre_processor = pre_processor
                                                          #)

          #result.approx.mean.updated <- OOS$perform(initial_estimate = result.dosl.mean,
                                                   #data = data.test,
                                                   #variable_of_interest = variable_of_interest,
                                                   #intervention = intervention,
                                                   #tau = tau)

          #print(paste('The difference between the estimate and approximation (after oos) for dosl is: ',
                      #abs(result.approx.mean - result.approx.mean.updated$oos_estimate)))
          #differences
        }

generate_formulae <- function(W, A, Y){
  # Generate W Formulae
  W_form <- lapply(W, function(w) {
    s <- w
    first <- TRUE
    for (w_in in c(W, A, Y)) {
      lagged <- paste(w_in, '_lag_1', sep='')
      if(first) s <- paste(s, lagged, sep = ' ~ ')
      else s <- paste(s, lagged, sep = ' + ')
      first <- FALSE
    }
    first <- TRUE
    formula(s)
  })
  names(W_form) <- W

  A_form <- lapply(A, function(a) {
    s <- a
    first <- TRUE
    for (a_in in c(W, A, Y)) {
      lagged <- paste(a_in, '_lag_1', sep='')
      if(first) s <- paste(s, lagged, sep = ' ~ ')
      else s <- paste(s, lagged, sep = ' + ')
      first <- FALSE
    }

    for (a_in in c(W)) {
      if(first) s <- paste(s, lagged, sep = ' ~ ')
      else s <- paste(s, a_in, sep = ' + ')
      first <- FALSE
    }
    first <- TRUE
    formula(s)
  })
  names(A_form) <- A

  Y_form <- lapply(Y, function(y) {
    s <- y
    first <- TRUE
    for (y_in in c(W, A, Y)) {
      lagged <- paste(y_in, '_lag_1', sep='')
      if(first) s <- paste(s, lagged, sep = ' ~ ')
      else s <- paste(s, lagged, sep = ' + ')
      first <- FALSE
    }

    #for (y_in in c(W,A)) {
      #if(first) s <- paste(s, lagged, sep = ' ~ ')
      #else s <- paste(s, y_in, sep = ' + ')
      #first <- FALSE
    #}
    first <- TRUE
    formula(s)
  })
  names(Y_form) <- Y

  list(W=W_form, A=A_form, Y=Y_form)
}
