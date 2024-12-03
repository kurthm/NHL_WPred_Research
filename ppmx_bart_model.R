library(tidyverse)
library(ppmSuite)

# Prepping Data -----------
all_data <- read.csv('pbp_w_covar.csv')

# Taking out overtime games
game_ends <- all_data %>% 
  filter(event == 'Game End') %>% 
  filter(period != 5) %>% # no shootouts
  filter(period != 4) # no OT (3 on 3)
  # end up with 1000 games

game_ids <- unique(game_ends$game_id)

game_ends$H_Win <- factor(game_ends$H_Win)

iters <- 15 # repeat the model because the sampling will result in slightly different draws

correct_perc <- rep(0, iters) # vector to store prediction %

# PPMx model-----
for(i in 1:iters){
  # Divide into test and train --------
  set.seed(i)
  samp_games <- sample(game_ids, size = 500, replace = FALSE)
  samp_game_ends <- game_ends %>% filter(game_id %in% samp_games)
  
  game_ends <- game_ends %>% mutate(train = case_when(
    game_id %in% samp_games ~ 1, TRUE ~ 0
  ))
  
  # Standardize the covariates-----------------
  # Kept the code for all the variables from earlier models so it is easily accessible
  # H_SV_PM--
  mu_sv_pm <- mean(game_ends$H_SV_PM)
  sd_sv_pm <- sd(game_ends$H_SV_PM)
  
  game_ends <- game_ends %>% mutate(H_SV_PM_Stand = ((H_SV_PM - mu_sv_pm)/sd_sv_pm))
  
  
  #H_TA_PM--
  mu_ta_pm <- mean(game_ends$H_TA_PM)
  sd_ta_pm <- sd(game_ends$H_TA_PM)
  
  game_ends <- game_ends %>% mutate(H_TA_PM_Stand = ((H_TA_PM - mu_ta_pm)/sd_ta_pm))
  
  
  # #H_Hit_PM--
  # mu_hit_pm <- mean(game_ends$H_Hit_PM)
  # sd_hit_pm <- sd(game_ends$H_Hit_PM)
  # 
  # game_ends <- game_ends %>% mutate(H_Hit_PM_Stand = ((H_Hit_PM - mu_hit_pm)/sd_hit_pm))
  # 
  
  #H_SAtt_PM--
  # mu_satt_pm <- mean(game_ends$H_SAtt_PM)
  # sd_satt_pm <- sd(game_ends$H_SAtt_PM)
  # 
  # game_ends <- game_ends %>% mutate(H_SAtt_PM_Stand = ((H_SAtt_PM - mu_satt_pm)/sd_satt_pm))
  # 
  # 
  # #H_PIM_PM--
  # mu_pim_pm <- mean(game_ends$H_PIM_PM)
  # sd_pim_pm <- sd(game_ends$H_PIM_PM)
  # 
  # game_ends <- game_ends %>% mutate(H_PIM_PM_Stand = ((H_PIM_PM - mu_pim_pm)/sd_pim_pm))
  # 
  # 
  # #H_GvA_PM--
  # mu_gva_pm <- mean(game_ends$H_GvA_PM)
  # sd_gva_pm <- sd(game_ends$H_GvA_PM)
  # 
  # game_ends <- game_ends %>% mutate(H_GvA_PM_Stand = ((H_GvA_PM - mu_gva_pm)/sd_gva_pm))
  # 
  
  #H_BS_PM
  mu_bs_pm <- mean(game_ends$H_BS_PM)
  sd_bs_pm <- sd(game_ends$H_BS_PM)
  
  game_ends <- game_ends %>% mutate(H_BS_PM_Stand = ((H_BS_PM - mu_bs_pm)/sd_bs_pm))
  
  # # H_FO_PM--
  # mu_fo_pm <- mean(game_ends$H_FO_PM)
  # sd_fo_pm <- sd(game_ends$H_FO_PM)
  # 
  # game_ends <- game_ends %>% mutate(H_FO_PM_Stand = ((H_FO_PM - mu_fo_pm)/sd_fo_pm))
  # 
  # # H_SC_PM--
  mu_sc_pm <- mean(game_ends$H_SC_PM)
  sd_sc_pm <- sd(game_ends$H_SC_PM)
  
  game_ends <- game_ends %>% mutate(H_SC_PM_Stand = ((H_SC_PM - mu_sc_pm)/sd_sc_pm))
  

  
  # Create a logical index to remove outlier games
  rows_to_keep <- (
    #(game_ends$H_PIM_PM_Stand <= 3) & (game_ends$H_PIM_PM_Stand >= -3) &
    (game_ends$H_SV_PM_Stand <= 3) & (game_ends$H_SV_PM_Stand >= -3) &
      (game_ends$H_TA_PM_Stand <= 3) & (game_ends$H_TA_PM_Stand >= -3) &
      # (game_ends$H_Hit_PM_Stand <= 3) & (game_ends$H_Hit_PM_Stand >= -3) &
      # (game_ends$H_SAtt_PM_Stand <= 3) & (game_ends$H_SAtt_PM_Stand >= -3) &
      #(game_ends$H_GvA_PM_Stand <= 3) & (game_ends$H_GvA_PM_Stand >= -3) &
      (game_ends$H_BS_PM_Stand <= 3) & (game_ends$H_BS_PM_Stand >= -3) &
      #(game_ends$H_FO_PM_Stand <= 3) & (game_ends$H_FO_PM_Stand >= -3) &
      (game_ends$H_SC_PM_Stand <= 3) & (game_ends$H_SC_PM_Stand >= -3)
  )
  
  
  game_ends <- game_ends[rows_to_keep,]
  
  # Get all the training data in one df
  train <- game_ends %>% filter(train == 1) 
  
  
  # Testing data into one df
  test <- game_ends %>% filter(train == 0)
  
  # Prepping the model ---------
  # Covariate data for the model
  x_train <- train %>% select(
    H_SV_PM_Stand
    ,
    H_TA_PM_Stand
    ,
    #H_Hit_PM_Stand
    #,
    #H_SAtt_PM_Stand
    #,
    #H_PIM_PM_Stand
    #,
    #H_GvA_PM_Stand
    #,
    H_BS_PM_Stand
    ,
    #H_FO_PM_Stand
    #,
    H_SC_PM_Stand
  )
  
  # Response for each training game
  y_train <- train %>% select(H_Win)
  y_train <- as.double(y_train$H_Win) - 1 
  
  
  # Covariates of test data
  x_test <- test %>%
    select(
      H_SV_PM_Stand
      ,
      H_TA_PM_Stand
      ,
      #H_Hit_PM_Stand
      #,
      #H_SAtt_PM_Stand
      #,
      #H_PIM_PM_Stand
      #,
      # H_GvA_PM_Stand
      #,
      H_BS_PM_Stand
      ,
      #H_FO_PM_Stand
      #,
      H_SC_PM_Stand
    )
  
  # Response for each test game
  y_test <- as.numeric(test$H_Win)-1
  
  
  # Set values for model
  v <- 0.5 # represents within-cluster variance
  simParms <- c(0., 1.0, v, 1.0, 2.0, 0.1) # standard parameter values
  co <- c(-100000, 0, 100000)
  
  # Running the model ---------------
  fit_3 <- ordinal_ppmx(y = y_train,
                        co = co,
                        X = x_train,
                        M = 1,
                        meanModel = 2,
                        Xpred = x_test,
                        similarity_function = 1,
                        consim = 1,
                        calibrate = 0,
                        simParms = simParms,
                        draws = 11000,
                        burn = 1001,
                        thin = 10,
                        verbose = TRUE)
  
  
  
  zero_zero_ppm <- table(y_test, apply(fit_3$ord.ppred, 2, median))[1,1]
  zero_one_ppm <- table(y_test, apply(fit_3$ord.ppred, 2, median))[1,2]
  one_zero_ppm <- table(y_test, apply(fit_3$ord.ppred, 2, median))[2,1]
  one_one_ppm  <- table(y_test, apply(fit_3$ord.ppred, 2, median))[2,2]
  
  correct_per <- (zero_zero_ppm + one_one_ppm)/
    (zero_zero_ppm + zero_one_ppm + one_zero_ppm + one_one_ppm)
  
  correct_perc[i] <- correct_per
  
  print(i)
}


# Save workspace ---------------
save.image("sv_bs_ta_sc_ppmx.RData")



# BART (benchmark)--------------------------------------------------------------
library(BayesTree)

bart_corr_per <- rep(0, iters)

# iters <- 15

for(i in 1:iters){
  
  set.seed(i) # same seed as used for ppmx model (same data)
  samp_games <- sample(game_ids, size = 500, replace = FALSE) 
  
  samp_game_ends <- game_ends %>% filter(game_id %in% samp_games) 
  
  game_ends <- game_ends %>% mutate(train = case_when(
    game_id %in% samp_games ~ 1, TRUE ~ 0
  ))
  
  #standardize the covariates
  # H_SV_PM--
  mu_sv_pm <- mean(game_ends$H_SV_PM)
  sd_sv_pm <- sd(game_ends$H_SV_PM)
  
  game_ends <- game_ends %>% mutate(H_SV_PM_Stand = ((H_SV_PM - mu_sv_pm)/sd_sv_pm))
  
  #H_TA_PM--
  mu_ta_pm <- mean(game_ends$H_TA_PM)
  sd_ta_pm <- sd(game_ends$H_TA_PM)
  
  game_ends <- game_ends %>% mutate(H_TA_PM_Stand = ((H_TA_PM - mu_ta_pm)/sd_ta_pm))
  
  #H_Hit_PM--
  # mu_hit_pm <- mean(game_ends$H_Hit_PM)
  # sd_hit_pm <- sd(game_ends$H_Hit_PM)
  # 
  # game_ends <- game_ends %>% mutate(H_Hit_PM_Stand = ((H_Hit_PM - mu_hit_pm)/sd_hit_pm))
  
  #H_SAtt_PM--
  # mu_satt_pm <- mean(game_ends$H_SAtt_PM)
  # sd_satt_pm <- sd(game_ends$H_SAtt_PM)
  # 
  # game_ends <- game_ends %>% mutate(H_SAtt_PM_Stand = ((H_SAtt_PM - mu_satt_pm)/sd_satt_pm))
  # 
  #H_S_PM-- Don't need anymore because taking this covariate out (not informative)
  # mu_s_pm <- mean(game_ends$H_S_PM)
  # sd_s_pm <- sd(game_ends$H_S_PM)
  #
  # game_ends <- game_ends %>% mutate(H_S_PM_Stand = ((H_S_PM - mu_s_pm)/sd_s_pm))
  
  #H_PIM_PM--
  # mu_pim_pm <- mean(game_ends$H_PIM_PM)
  # sd_pim_pm <- sd(game_ends$H_PIM_PM)
  # 
  # game_ends <- game_ends %>% mutate(H_PIM_PM_Stand = ((H_PIM_PM - mu_pim_pm)/sd_pim_pm))
  # 
  
  #H_GvA_PM--
  # mu_gva_pm <- mean(game_ends$H_GvA_PM)
  # sd_gva_pm <- sd(game_ends$H_GvA_PM)
  # 
  # game_ends <- game_ends %>% mutate(H_GvA_PM_Stand = ((H_GvA_PM - mu_gva_pm)/sd_gva_pm))
  
  #H_BS_PM--
  mu_bs_pm <- mean(game_ends$H_BS_PM)
  sd_bs_pm <- sd(game_ends$H_BS_PM)
  
  game_ends <- game_ends %>% mutate(H_BS_PM_Stand = ((H_BS_PM - mu_bs_pm)/sd_bs_pm))
  
  # H_FO_PM--
  # mu_fo_pm <- mean(game_ends$H_FO_PM)
  # sd_fo_pm <- sd(game_ends$H_FO_PM)
  # 
  # game_ends <- game_ends %>% mutate(H_FO_PM_Stand = ((H_FO_PM - mu_fo_pm)/sd_fo_pm))
  
  # H_SC_PM--
  mu_sc_pm <- mean(game_ends$H_SC_PM)
  sd_sc_pm <- sd(game_ends$H_SC_PM)
  
  game_ends <- game_ends %>% mutate(H_SC_PM_Stand = ((H_SC_PM - mu_sc_pm)/sd_sc_pm))
  
  
  
  # Create a logical index to remove outlier games
  rows_to_keep <- (
    #(game_ends$H_PIM_PM_Stand <= 3) & (game_ends$H_PIM_PM_Stand >= -3) &
    (game_ends$H_SV_PM_Stand <= 3) & (game_ends$H_SV_PM_Stand >= -3) &
      (game_ends$H_TA_PM_Stand <= 3) & (game_ends$H_TA_PM_Stand >= -3) &
      #(game_ends$H_Hit_PM_Stand <= 3) & (game_ends$H_Hit_PM_Stand >= -3) &
      #(game_ends$H_SAtt_PM_Stand <= 3) & (game_ends$H_SAtt_PM_Stand >= -3) &
      #(game_ends$H_GvA_PM_Stand <= 3) & (game_ends$H_GvA_PM_Stand >= -3) &
      (game_ends$H_BS_PM_Stand <= 3) & (game_ends$H_BS_PM_Stand >= -3) &
      #(game_ends$H_FO_PM_Stand <= 3) & (game_ends$H_FO_PM_Stand >= -3) &
      (game_ends$H_SC_PM_Stand <= 3) & (game_ends$H_SC_PM_Stand >= -3)
  )
  
  
  game_ends <- game_ends[rows_to_keep,]
  
  # Get all the training data in one df
  train <- game_ends %>% filter(train == 1)
  
  
  # Testing data into one df
  test <- game_ends %>% filter(train == 0)
  
  
  # Get the covariates to train the model on
  x_train <- train %>% select(
    H_SV_PM_Stand,
    H_TA_PM_Stand,
    #H_Hit_PM_Stand,
    #H_SAtt_PM_Stand,
    #H_PIM_PM_Stand,
    #H_GvA_PM_Stand,
    H_BS_PM_Stand,
    #H_FO_PM_Stand,
    H_SC_PM_Stand
  )
  
  # Response for each training game
  y_train <- train %>% select(H_Win)
  y_train <- as.double(y_train$H_Win) - 1 # turn into a vector
  
  
  
  # Covariates of test data
  x_test <- test %>%
    select(
      H_SV_PM_Stand,
      H_TA_PM_Stand,
      #H_Hit_PM_Stand,
      #H_SAtt_PM_Stand,
      #H_PIM_PM_Stand,
      #H_GvA_PM_Stand,
      H_BS_PM_Stand,
      #H_FO_PM_Stand,
      H_SC_PM_Stand
    )
  
  # Response for each test game
  y_test <- as.numeric(test$H_Win)-1
  
  
  bart_model <- bart(y.train = y_train,
                     x.train = x_train,
                     x.test = x_test)
  
  
  # function to determine if the predicted value is predicting a win or loss
  t <- function(x){
    out <- rep(1, length(x))
    out[x<0] <- 0
    out
  }
  
  
  zero_zero <- table(apply(apply(bart_model$yhat.test, 2, t), 2, median), y_test)[1,1]
  zero_one <- table(apply(apply(bart_model$yhat.test, 2, t), 2, median), y_test)[1,2]
  one_zero <- table(apply(apply(bart_model$yhat.test, 2, t), 2, median), y_test)[2,1]
  one_one <- table(apply(apply(bart_model$yhat.test, 2, t), 2, median), y_test)[2,2]
  
  correct_per <- (zero_zero + one_one)/
    (zero_zero + zero_one + one_zero + one_one)
  
  bart_corr_per[i] <- correct_per
  
  print(i)
  
  
}

# Printing Results and Comparison-----
print_pred <- function(ppmx_pred, bart_pred){
  print(paste("Mean PPMx prediction proportion: ", round(mean(correct_perc), 3)))
  print(paste("Mean BART prediction proportion: ", round(mean(bart_corr_per), 3)))
}
print_pred()
  # Note: PPMx performs as well as BART, on average

# Saving workspace------------
save.image("sv_bs_ta_sc_comparison.RData")

