
library(readxl)
library(torch)
library(dplyr)


#get alll the data
folder_path <- "SMD houry-March2026-2017"  
xlsx_files <- list.files(folder_path, pattern = "\\.xlsx$", full.names = TRUE)# pulls all excel files
results <- list()  # empty list to collect results
for (file in xlsx_files) {
  print(file)
  sheets <- excel_sheets(file)
  if ("ISO NE CA" %in% sheets) {
    df <- read_excel(file, sheet = "ISO NE CA") 
    results[[length(results) + 1]] <- df
  } else {
    message("Skipping (tab not found): ", basename(file))
  }
}
df_combined <- do.call(rbind, results)
rm(results,df) # Remove the results and temp variable ( keep ma memmory up )
gc() 


#clean 
# ISO NE convention for the extra hour during the daylight saving time fallback (the clock "falls back" and hour 2 occurs twice).
df_combined |>pull(Hr_End) |>unique() 
#02X - is the daylight saveings time makes things odd at times 
df_combined <- df_combined %>%
  mutate(Hr_End = ifelse(Hr_End == "02X", "02", Hr_End))
df_combined <- df_combined %>%
  mutate(Hr_End = as.integer(Hr_End))
df_combined <- df_combined %>%
  mutate(Date = as.POSIXct(Date, format = "%Y-%m-%d")) %>%
  arrange(Date)  # make sure it is sorted chronologically

summary(df_combined)
df_combined_DA <- df_combined[ , !(names(df_combined) %in% c("RT_LMP", "RT_EC", "RT_CC", "RT_MLC", "System_Load","Min_5min_RSP","Max_5min_RSP","Min_5min_RCP","Max_5min_RCP"))]
numeric_cols_DA    <- which(sapply(df_combined_DA, is.numeric))
df_combined_DA    <- df_combined_DA[, numeric_cols_DA]


df_combined_tight <- df_combined[ , !(names(df_combined) %in% c("RT_LMP", "RT_EC", "RT_CC", "RT_MLC", "System_Load","Min_5min_RSP","Max_5min_RSP","Min_5min_RCP","Max_5min_RCP","DA_LMP", "DA_EC", "DA_CC", "DA_MLC","DA_Demand"))]

#dates are cyclical and 1-12 does not show that converting to sin cos to transform 
# in to a cyclical data that can alow the model to pick up dec and january are very close
library(lubridate)
df_cycle <- df_combined_tight %>%
  mutate(
    
    # ── Hour of day cycle (period = 24) ──────────────────
    hour_sin     = sin(2 * pi * Hr_End / 24),
    hour_cos     = cos(2 * pi * Hr_End / 24),
    
    # ── Day of week cycle (period = 7) ───────────────────
    dow_sin      = sin(2 * pi * wday(Date, week_start = 1) / 7),
    dow_cos      = cos(2 * pi * wday(Date, week_start = 1) / 7),
    
    # ── Month cycle (period = 12) ────────────────────────
    month_sin    = sin(2 * pi * month(Date) / 12),
    month_cos    = cos(2 * pi * month(Date) / 12),
    
    # ── Day of year cycle (period = 365) ───────────────── maybe excessive 
    doy_sin      = sin(2 * pi * yday(Date) / 365),
    doy_cos      = cos(2 * pi * yday(Date) / 365),
    
  )
summary(df_cycle)

numeric_cols_tight    <- which(sapply(df_combined_tight, is.numeric))
df_combined_tight    <- df_combined_tight[, numeric_cols_tight]


#FUNCTIONS FOR THE MODEL 

# ── model architecture ────── builds the basic structure with the additional layers to estimate hiden node distrobutions 
make_lstm_model <- function(input_size, hidden_size, output_size = 1L) {
  list(
    lstm      = nn_lstm(input_size, hidden_size, batch_first = TRUE),
    fc_mu     = nn_linear(hidden_size, hidden_size),
    fc_logvar = nn_linear(hidden_size, hidden_size),
    fc_out    = nn_linear(hidden_size, output_size)
  )
}

lstm_forward <- function(model, x, training = TRUE) {
  
  lstm_out <- model$lstm(x)#get models state 
  h_last   <- lstm_out[[2]][[1]]$squeeze(1L)
  if (h_last$dim() == 1L) h_last <- h_last$unsqueeze(1L)
  mu       <- model$fc_mu(h_last)
  log_var  <- model$fc_logvar(h_last)
  h_sample <- if (training) {
    mu + torch_exp(0.5 * log_var) * torch_randn_like(log_var) #generate a potential out put from the predicted distribution 
  } else {
    mu
  }
  list(y_pred = model$fc_out(h_sample), mu = mu, log_var = log_var)
}

get_params <- function(model) {
  c(model$lstm$parameters,
    model$fc_mu$parameters,
    model$fc_logvar$parameters,
    model$fc_out$parameters)
}


# ── loss function ───────── sets up the modiffied loss function to inclued the penalities + the loss for the hiden node
elbo_loss <- function(y_pred, y_true, mu, log_var,
                      model,
                      alpha1 = 0.01,
                      alpha2 = 0.01,
                      alpha3 = 0.001) {
  
  recon_loss <- nn_mse_loss()(y_pred, y_true)  # mean
  kl_loss    <- -0.5 * torch_mean(1 + log_var - mu$pow(2) - log_var$exp())
  elbo       <- recon_loss + kl_loss # estimat of probability
  
  
  Wi <- model$lstm$parameters[["weight_ih_l1"]]
  Ug <- model$lstm$parameters[["weight_hh_l1"]]
  bg <- model$lstm$parameters[["bias_ih_l1"]]
  q  <- model$lstm$hidden_size
  
  Wi_i <- Wi[1:q, ];          Wi_f <- Wi[(q+1):(2*q), ]
  Wi_c <- Wi[(2*q+1):(3*q), ]; Wi_o <- Wi[(3*q+1):(4*q), ]
  Ug_i <- Ug[1:q, ];          Ug_f <- Ug[(q+1):(2*q), ]
  Ug_c <- Ug[(2*q+1):(3*q), ]; Ug_o <- Ug[(3*q+1):(4*q), ]
  bg_i <- bg[1:q];             bg_f <- bg[(q+1):(2*q)]
  bg_c <- bg[(2*q+1):(3*q)];   bg_o <- bg[(3*q+1):(4*q)]
  
  
  l1_input <- alpha1 * (
    torch_sum(torch_abs(Wi_i)) + torch_sum(torch_abs(Wi_f)) +
      torch_sum(torch_abs(Wi_c)) + torch_sum(torch_abs(Wi_o))
  )
  l1_recurrent <- alpha2 * (
    torch_sum(torch_abs(Ug_i)) + torch_sum(torch_abs(Ug_f)) +
      torch_sum(torch_abs(Ug_c)) + torch_sum(torch_abs(Ug_o))
  )
  l1_bias <- alpha3 * (
    torch_sum(torch_abs(bg_i)) + torch_sum(torch_abs(bg_f)) +
      torch_sum(torch_abs(bg_c)) + torch_sum(torch_abs(bg_o))
  )
  
  list(
    full_loss = elbo + l1_input + l1_recurrent + l1_bias,
    elbo      = elbo
  )
}

# ── BIC ───────────────── basic BIC expects the ELbo to estimate the likely hood 
compute_bic <- function(elbo, n_samples, n_params) {
  n_samples * log(as.numeric(elbo) / n_samples) + n_params * log(n_samples)
}

# ── fit one candidate model ──────────────── Out puts the best model of the ninit run ( prevents bad random start from confusing) , BIC , # of non zero param  , loss 
fit_candidate_model <- function(X_train, y_train,
                                input_size,
                                hidden_size,
                                ninit    = 3,
                                epochs   = 100,
                                lr       = 0.01,
                                alpha1   = 0.01,
                                alpha2   = 0.01,
                                alpha3   = 0.001,
                                patience = 15) {
  
  best_model     <- NULL
  best_full_loss <- Inf
  best_elbo      <- Inf
  
  X_t <- torch_tensor(X_train, dtype = torch_float())
  y_t <- torch_tensor(y_train, dtype = torch_float())$unsqueeze(2) # add a dimension because the output layer cant reduce to one d  
  
  for (init in 1:ninit) {
    cat("  init:", init, "/", ninit, "\n")
    
    model     <- make_lstm_model(input_size, hidden_size)
    optimizer <- optim_adam(get_params(model), lr = lr)
    
    
    best_init_full_loss <- Inf
    best_init_elbo      <- Inf
    patience_count      <- 0
    
    for (epoch in 1:epochs) {
      optimizer$zero_grad()
      output<- lstm_forward(model, X_t, training = TRUE)
      losses <- elbo_loss(
        y_pred  = output$y_pred,
        y_true  = y_t,
        mu      = output$mu,
        log_var = output$log_var,
        model   = model,
        alpha1  = alpha1,
        alpha2  = alpha2,
        alpha3  = alpha3
      )
      losses$full_loss$backward()
      optimizer$step()
      
      current_full_loss <- losses$full_loss$item()
      current_elbo      <- losses$elbo$item()
      
      if (epoch %% 10 == 0) {
        cat("    epoch:", epoch,
            "full loss:", round(current_full_loss, 4),
            "elbo:",      round(current_elbo, 4), "\n")
      }
      
      #STOPs early if no improvement for a few epochs ( set to 5 for speed :p)
      if (current_full_loss < best_init_full_loss) {
        best_init_full_loss <- current_full_loss
        best_init_elbo      <- current_elbo
        patience_count      <- 0
      } else {
        patience_count <- patience_count + 1
        if (patience_count >= patience) {
          cat("    early stopping at epoch", epoch, "\n")
          break
        }
      }
    }
    # stores best Init 
    if (best_init_full_loss < best_full_loss) {
      best_full_loss <- best_init_full_loss
      best_elbo      <- best_init_elbo
      best_model     <- model
    }
  }
  
  # number of non zero parameters ( the degree of freedom for BIC )
  n_params <- sum(sapply(get_params(best_model), function(p) {
    p_vals <- as.numeric(p$detach())
    sum(abs(p_vals) > 1e-6)# in practice hard to actualy get the 0 so 1e-6 used as partical replace ment  
  }))
  
  # bic with elbo as likelyhood estimate 
  bic <- compute_bic(
    elbo      = best_elbo,
    n_samples = nrow(X_train),
    n_params  = n_params
  )
  
  cat("  best elbo:", round(best_elbo, 4),
      "non-zero params:", n_params,
      "BIC:", round(bic, 4), "\n")
  
  list(
    model    = best_model,
    bic      = bic,
    elbo     = best_elbo,
    loss     = best_full_loss,
    n_params = n_params
  )
}

# ── hidden node selection ---- Outputs best number of hidden nodes (q) based on BIC 
hidden_node_selection <- function(X_train, y_train,
                                  qmax,
                                  ninit  = 3,
                                  epochs = 75,
                                  lr     = 0.01,
                                  alpha1 = 0.01,
                                  alpha2 = 0.01,
                                  alpha3 = 0.001) {
  best_bic <- Inf
  best_q   <- NULL
  # kind of works like a stepwise function in linear reggretion running through the difrent model size calculating the bic and keeping the best 
  # tests the number of hided nodes size from 1 to max  
  for (q in 1:qmax) {
    cat("testing q =", q, "\n")
    result <- fit_candidate_model(
      X_train     = X_train,
      y_train     = y_train,
      input_size  = dim(X_train)[3],
      hidden_size = q,
      ninit       = ninit,
      epochs      = epochs,
      lr          = lr,
      alpha1      = alpha1,
      alpha2      = alpha2,
      alpha3      = alpha3
    )
    if (result$bic < best_bic) {
      best_bic <- result$bic
      best_q   <- q
    }
  }
  cat("best q:", best_q, "BIC:", best_bic, "\n")
  #returns the best q 
  return(best_q)
}

# ── input node selection  ---- Outputs best set of inputs (p) based on BIC
input_node_selection <- function(X_train, y_train,
                                 feature_names = feature_names,
                                 selected_inputs,
                                 hidden_size,
                                 ninit  = 3,
                                 epochs = 75,
                                 lr     = 0.01,
                                 alpha1 = 0.01,
                                 alpha2 = 0.01,
                                 alpha3 = 0.001) {
  improved    <- TRUE
  X_current   <- X_train[,,selected_inputs, drop = FALSE]
  fit_current <- fit_candidate_model(
    X_train     = X_current,
    y_train     = y_train,
    input_size  = length(selected_inputs),
    hidden_size = hidden_size,
    ninit       = ninit,
    epochs      = epochs,
    lr          = lr,
    alpha1      = alpha1,
    alpha2      = alpha2,
    alpha3      = alpha3
  )
  current_bic <- fit_current$bic
  
  while (improved) {
    improved <- FALSE
    for (i in selected_inputs) {
      trial_inputs <- selected_inputs[selected_inputs != i]
      X_trial      <- X_train[,,trial_inputs, drop = FALSE]
      result <- fit_candidate_model(
        X_train     = X_trial,
        y_train     = y_train,
        input_size  = length(trial_inputs),
        hidden_size = hidden_size,
        ninit       = ninit,
        epochs      = epochs,
        lr          = lr,
        alpha1      = alpha1,
        alpha2      = alpha2,
        alpha3      = alpha3
      )
      if (result$bic < current_bic) {
        dropped_name <- feature_names[i]
        cat("dropping", dropped_name,
            'Total number of inputs: ',length(trial_inputs),
            "BIC improved from", current_bic,
            "to", result$bic, "\n")
        selected_inputs <- trial_inputs
        current_bic     <- result$bic
        improved        <- TRUE
        break
      }
    }
  }
  return(selected_inputs)
}

# ── fine tune --given the best inputs set and hidden variables  related in tandom breaks when change stops improvement 
fine_tune <- function(X_train, y_train,
                      feature_names = feature_names,
                      selected_inputs,
                      q, qmax,
                      ninit  = 3,
                      epochs = 100,
                      lr     = 0.01,
                      alpha1 = 0.01,
                      alpha2 = 0.01,
                      alpha3 = 0.001) {
  improved <- TRUE
  pass_count<-0
  while (improved) {
    improved <- FALSE
    pass_count  <- pass_count + 1
    
    cat("──────────────────────────────────────\n")
    cat("  PASS", pass_count, "\n")
    cat("  current q:", q, 
        "| current inputs:", length(selected_inputs), "\n")
    cat("──────────────────────────────────────\n")
    # test one less q and one more q then the initial best with full data 
    cat("\n  Testing q adjustments\n")
    for (delta in c(-1, 1)) {
      q_trial <- q + delta
      if (q_trial < 1 || q_trial > qmax) next
      X_sub       <- X_train[,,selected_inputs, drop = FALSE]
      fit_current <- fit_candidate_model(
        X_train     = X_sub,
        y_train     = y_train,
        input_size  = length(selected_inputs),
        hidden_size = q,
        ninit       = ninit,
        epochs      = epochs,
        lr          = lr,
        alpha1      = alpha1,
        alpha2      = alpha2,
        alpha3      = alpha3
      )
      fit_trial <- fit_candidate_model(
        X_train     = X_sub,
        y_train     = y_train,
        input_size  = length(selected_inputs),
        hidden_size = q_trial,
        ninit       = ninit,
        epochs      = epochs,
        lr          = lr,
        alpha1      = alpha1,
        alpha2      = alpha2,
        alpha3      = alpha3
      )
      if (fit_trial$bic < fit_current$bic) {
        cat("q adjusted from", q, "to", q_trial, "\n")
        q        <- q_trial
        improved <- TRUE
        break
      }
    }
    # Tests on the new q for new candate inputs
    cat("\n  [Step 2] Testing input adjustments\n")
    new_inputs <- input_node_selection(
      X_train         = X_train,
      y_train         = y_train,
      feature_names = feature_names,
      selected_inputs = selected_inputs,
      hidden_size     = q,
      ninit           = ninit,
      epochs          = epochs,
      lr              = lr,
      alpha1          = alpha1,
      alpha2          = alpha2,
      alpha3          = alpha3
    )
    if (!setequal(new_inputs, selected_inputs)) {
      selected_inputs <- new_inputs
      improved        <- TRUE
    }
  }
  list(q = q, selected_inputs = selected_inputs)
}

# ── master model selection ──────────────────────Out puts the final best q and p values based on BIC 
model_selection <- function(X_train, y_train,
                            feature_names = feature_names,
                            qmax   = 20,
                            ninit  = 3,
                            epochs = 75,
                            lr     = 0.01,
                            alpha1 = 0.01,
                            alpha2 = 0.01,
                            alpha3 = 0.001) {
  
  n_feat          <- dim(X_train)[3]
  selected_inputs <- 1:n_feat
  
  cat("=== Step 1: Hidden Node Selection ===\n") # tests how many hidden nodes given bic as the indicatore ( basicaly step wise )
  best_q <- hidden_node_selection(
    X_train = X_train,
    y_train = y_train,
    qmax    = qmax,
    ninit   = ninit,
    epochs  = epochs,
    lr      = lr,
    alpha1  = alpha1,
    alpha2  = alpha2,
    alpha3  = alpha3
  )
  
  cat("=== Step 2: Input Node Selection ===\n")  # tests which inputs given bic as the indicatore ( basicaly step wise )
  selected_inputs <- input_node_selection(
    X_train         = X_train,
    y_train         = y_train,
    feature_names = feature_names,
    selected_inputs = selected_inputs,
    hidden_size     = best_q,
    ninit           = ninit,
    epochs          = epochs,
    lr              = lr,
    alpha1          = alpha1,
    alpha2          = alpha2,
    alpha3          = alpha3
  )
  
  cat("=== Step 3: Fine Tuning ===\n") # runs through combos given the preliminary best from the above as starting points 
  final <- fine_tune(
    X_train         = X_train,
    y_train         = y_train,
    feature_names = feature_names,
    selected_inputs = selected_inputs,
    q               = best_q,
    qmax            = qmax,
    ninit           = ninit,
    epochs          = epochs,
    lr              = lr,
    alpha1          = alpha1,
    alpha2          = alpha2,
    alpha3          = alpha3
  )
  
  list(
    best_q          = final$q,
    selected_inputs = final$selected_inputs
  )
}

#setting up the test train splits for diffent look backs

#rolling window - 2 years training 1 year test 
hours_per_year  <- 365 * 24   # 8,760
train_hours     <- 2 * hours_per_year   # 17,520 hours = 2 years
test_hours      <- 1 * hours_per_year   #  8,760 hours = 1 year
window_hours    <- train_hours + test_hours  # 26,280 hours = 3 years
step_hours      <- test_hours   # roll forward 1 year at a time

lookbacks<-24 # testinng on this wayyyy to late 


df_combined<-df_combined_tight
n <- nrow(df_combined)
n_folds <- floor((n - window_hours) / step_hours) + 1
#five folds
resultgroups <- list()   # initialize before the loop



for (fold in 1:n_folds) {
  
  train_start <- (fold - 1) * step_hours + 1
  train_end   <- train_start + train_hours - 1
  test_start  <- train_end + 1
  test_end    <- test_start + test_hours - 1
  
  train_data <- df_combined[train_start:train_end, ]
  test_data  <- df_combined[test_start:test_end, ]
  
  
  train_mean   <- apply(train_data, 2, mean)
  train_sd     <- apply(train_data, 2, sd)
  
  # scaled data  so that the error is not screwing my loss function 
  train_scaled <- scale(train_data,
                        center = train_mean,
                        scale  = train_sd)
  #scaled on the training data to mirror real usage 
  test_scaled  <- scale(test_data,
                        center = train_mean, 
                        scale  = train_sd)   
  
  for (t in lookbacks) {
    cat("\n=== Fold:", fold, "| Lookback:", t, "===\n")
    target_col <- which(colnames(train_scaled) == "RT_Demand")
    feature_names <- colnames(train_scaled)[-target_col]
    cat("target_col index:", target_col, "\n")
    cat("target_col name:", colnames(train_scaled)[target_col], "\n")
    
    n_sets_train <- nrow(train_scaled) - t
    n_sets_test  <- nrow(test_scaled)  - t
    n_feat       <- ncol(train_scaled) - 1
    
    X_train <- array(NA, dim = c(n_sets_train, t, n_feat))
    y_train <- vector("numeric", n_sets_train)
    X_test  <- array(NA, dim = c(n_sets_test,  t, n_feat))
    y_test  <- vector("numeric", n_sets_test)

    for (i in 1:n_sets_train) {
      X_train[i,,] <- as.matrix(
        train_scaled[i:(i+t-1), -target_col])
      y_train[i]   <- train_scaled[i+t, target_col]
    }
    for (i in 1:n_sets_test) {
      X_test[i,,]  <- as.matrix(
        test_scaled[i:(i+t-1), -target_col])
      y_test[i]    <- test_scaled[i+t, target_col]
    }
    
    #  model selection (hidden nodes + input nodes + fine tune) 
    cat("--- Running model selection ---\n")
    selection <- model_selection(
      X_train = X_train,
      y_train = y_train,
      feature_names = feature_names,
      qmax    = 10,
      ninit   = 3,
      epochs  = 150,
      lr      = 0.01,
      alpha1  = 0.01,
      alpha2  = 0.01,
      alpha3  = 0.001
    )
    best_q          <- selection$best_q
    selected_inputs <- selection$selected_inputs
    
    # Refit final model with best architecture with more epochs for final run 
    cat("--- Refitting final model | q =", best_q,
        "| n_inputs =", length(selected_inputs), "---\n")
    cat("--- Selected Features ---\n")
    cat(feature_names[selected_inputs], sep = "\n")
    
    X_train_sel <- X_train[, , selected_inputs, drop = FALSE]
    final_fit <- fit_candidate_model(
      X_train     = X_train_sel,
      y_train     = y_train,
      input_size  = length(selected_inputs),
      hidden_size = best_q,
      ninit       = 3,
      epochs      = 200,   # more epochs 
      lr          = 0.01,
      alpha1      = 0.01,
      alpha2      = 0.01,
      alpha3      = 0.001
    )
    final_model <- final_fit$model
   
    # predict on test set
    X_test_sel <- X_test[, , selected_inputs, drop = FALSE] # modify so inputs are only the final selected set 
    X_test_t   <- torch_tensor(X_test_sel, dtype = torch_float())
    
    test_output <- with_no_grad(lstm_forward(final_model, X_test_t, training = FALSE))# this pulls the output with out trying to update weights 
    
    predictions_scaled <- as.numeric(test_output$y_pred$squeeze(2))
    
    # ── unscale back to original scale (Mega Wats ) 
    predictions_mw <- predictions_scaled * train_sd[target_col] + train_mean[target_col]
    y_test_mw      <- y_test* train_sd[target_col] + train_mean[target_col]
    
    # ── metrics ──────────────────────────────────────────────────────────────
    test_rmse <- sqrt(mean((predictions_scaled - y_test)^2))
    rmse_mw   <- sqrt(mean((predictions_mw - y_test_mw)^2))
    
    ss_res <- sum((predictions_scaled - y_test)^2)
    ss_tot <- sum((y_test - mean(y_test))^2)
    r2     <- 1 - (ss_res / ss_tot)
    
    
    cat("  Test RMSE (scaled):", round(test_rmse, 4), "\n")
    cat("  Test RMSE (MW):    ", round(rmse_mw, 2), "\n")
    cat("  R²:                ", round(r2, 4), "\n")

    
    # ── plot true vs predicted ────────────────────────────────────────────────
    # create time index for x axis
    n_test_pts <- length(predictions_mw)
    time_index <- seq_len(n_test_pts)
    
    # ploting first 30 days(720hrs) because full year could look crazy 
    plot_n <-n_test_pts
    
    # set up plot area
    par(mfrow = c(2, 1),           # 2 rows 1 column
        mar   = c(4, 4, 3, 1))    # margins
    
    # first 30 days
    plot(time_index[1:plot_n],
         y_test_mw[1:plot_n],
         type = "l",
         col  = "steelblue",
         lwd  = 1.5,
         main = paste0("Fold ", fold,
                       " | Lookback ", t, "hrs",
                       " | q=", best_q,
                       " | First 30 Days"),
         xlab = "Hour",
         ylab = "RT Demand (MW)",
         ylim = range(c(y_test_mw[1:plot_n],
                        predictions_mw[1:plot_n])))
    
    lines(time_index[1:plot_n],
          predictions_mw[1:plot_n],
          col = "firebrick",
          lwd = 1.5,
          lty = 2)
    
    legend("topright",
           legend = c("Actual", "Predicted"),
           col    = c("steelblue", "firebrick"),
           lwd    = 2,
           lty    = c(1, 2),
           bty    = "n")
    
    # add RMSE and R² to plot
    mtext(paste0("RMSE: ", round(rmse_mw, 1), " MW",
                 " | R²: ", round(r2, 3)),
          side = 3,
          line = 0,
          cex  = 0.8)
    
    # ── plot 2: scatter actual vs predicted of all 
    plot(y_test_mw,
         predictions_mw,
         pch  = 16,
         cex  = 0.3,
         col  = adjustcolor("steelblue", alpha.f = 0.3),
         main = paste0("Fold ", fold,
                       " | Actual vs Predicted"),
         xlab = "Actual RT Demand (MW)",
         ylab = "Predicted RT Demand (MW)")
    
    # perfect prediction line
    abline(a   = 0,
           b   = 1,
           col = "firebrick",
           lwd = 2,
           lty = 2)
    
    # add R² to scatter
    mtext(paste0("R² = ", round(r2, 3)),
          side = 3,
          line = 0,
          cex  = 0.8)
    
    # reset plot layout
    par(mfrow = c(1, 1))
    
    # ── save plot ─────────────────────────────────────────────────────────────
    plot_filename <- paste0("cleaned_fold_", fold, "_lookback_", t, ".png")
    dev.copy(png,
             filename = plot_filename,
             width    = 1200,
             height   = 800,
             res      = 120)
    dev.off()
    cat("  Plot saved:", plot_filename, "\n")
  
    
    #  save fold details 
    key <- paste(fold, t, sep = "_")
    resultgroups[[key]] <- list(
      fold            = fold,
      t               = t,
      bic             = final_fit$bic,
      best_q          = best_q,
      selected_inputs = selected_inputs,
      test_rmse       = test_rmse,
      test_rmse_mw         = rmse_mw,
      predictions_scaled = predictions_scaled,
      predictions_mw     = predictions_mw
    )
    
    rm(X_train, y_train, X_test, y_test, X_train_sel, X_test_sel, X_test_t)
    gc()
  }
}


# ── Summary table after all folds finish ─────────────────────────────────────
results_df <- do.call(rbind, lapply(resultgroups, function(r) {
  data.frame(
    fold            = r$fold,
    lookback        = r$t,
    best_q          = r$best_q,
    n_inputs        = length(r$selected_inputs),
    selected_features = paste(feature_names[r$selected_inputs], collapse = ", "),
    bic             = round(r$bic, 2),
    test_rmse       = round(r$test_rmse_mw, 4)
  )
}))

print(results_df)

# Mean RMSE across folds per lookback (the "true" out-of-sample error estimate)
aggregate(test_rmse ~ lookback, data = results_df, FUN = mean)










