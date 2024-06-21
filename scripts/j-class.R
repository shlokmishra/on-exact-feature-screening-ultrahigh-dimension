  rm(list = ls())
  #------------------------------------------------------------------------------
  set.seed(123)
  REP = 100 # number of iterations to run.
  trn.sz.perClass = 100 # size of training sample from each class in all simulated examples Ex 1- 16 except Ex 8
  tst.sz.perClass = 250 # size of test sample from each class in all simulated examples
  # OS.var = readline(prompt = 'If you are on Windows, please type "W". Otherwise type "O": ')
  # cluster.type = ifelse(OS.var == 'W', 'PSOCK', 'FORK')
  cluster.type = 'FORK'
  #------------------------------------------------------------------------------
  EXNAMES = paste('ex', c(8), sep = '')
  #------------------------------------------------------------------------------
  PCKG = c(
    # 'energy',
    'proxy',
    'parallelDist',
    'nbpMatching',
    'MASS',
    'doParallel',
    'plot.matrix',
    'RcppArmadillo',
    'RcppXPtrUtils',
    'inline',
    # 'VariableScreening',
    'utils',
    'e1071',
    'Peacock.test',
    'ks',
    'nprobust'
  )
  
  # install.packages(setdiff(PCKG, rownames(installed.packages())))
  lapply(PCKG, library, character.only = T)
  
  #------------------------------------------------------------------------------
  setwd("~/code/on-exact-feature-screening-ultrahigh-dimension/scripts")
  source('../relevant_functions/relevant_functions.R')
  source('../relevant_functions/modifications.R')
  
  #main script starts here.
  
  no.cores = round(detectCores() - 1)
  cl = makeCluster(no.cores, type = cluster.type)
  registerDoParallel(cl)
  clusterExport(cl, as.character(lsf.str())) # exporting all user defined functions
  i = 1
  flg = 1
  RESULT = list(0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0)
  names(RESULT) = EXNAMES
  out = NULL
  pb = ERR = NULL
  gamfuns = c('gexp', 'gsqrt', 'glog') #choice of gamma functions
  d = 10 #dimension
  revmu = 0.75 # the difference in mean for the marginal signal in examples 9-16
  
  # exID = EXNAMES[1]
  for (exID in EXNAMES) {
    switch (
      exID,
      ex1 = { # Extended example with three classes for a location problem with marginal features.
        block.size = 2
        s = 2
        no.s = 4 # 4 marginals
        sig.pos = c(1, 3) # seq(1, length.out = s, by = block.size)
        
        n1train = n2train = n3train = trn.sz.perClass
        ntrain = n1train + n2train + n3train
        n1test = n2test = n3test = tst.sz.perClass
        n1 = n1train + n1test
        n2 = n2train + n2test
        n3 = n3train + n3test
        n = n1 + n2 + n3 # Adjust total sample size to account for the third class
        prms = t(rperm(m = 50 * ntrain, size = ntrain))
      },
      
      ex2 = { # Extended example 2 from the manuscript to include a third class. A scale problem with paired features.
        block.size = 2
        s = 2
        no.s = 4 # 2 pairs
        sig.pos = c(1, 3)
        
        n1train = n2train = n3train = trn.sz.perClass
        ntrain = n1train + n2train + n3train
        n1test = n2test = n3test = tst.sz.perClass
        n1 = n1train + n1test
        n2 = n2train + n2test
        n3 = n3train + n3test
        n = n1 + n2 + n3 # Adjusted for three classes
        prms = t(rperm(m = 50 * ntrain, size = ntrain))
        
      },
      
      ex8 = { # Extended example 8 for a three-class scenario with marginal features
        block.size = 2
        s = 2
        no.s = 2 # 2 pairs, assuming marginal features are handled similarly
        sig.pos = c(1, 3) # Positions of significant features, assuming this remains unchanged
        
        n1train = n2train = n3train = 100 # Training sizes for three classes
        ntrain = n1train + n2train + n3train # Total training size for all classes
        
        n1test = n2test = n3test = tst.sz.perClass # Test sizes for three classes, assuming tst.sz.perClass is defined elsewhere
        n1 = n1train + n1test
        n2 = n2train + n2test
        n3 = n3train + n3test
        n = n1 + n2 + n3 # Total size for all classes
        
        prms = t(rperm(m = 50 * ntrain, size = ntrain)) # Assuming this is for permutation, and its usage remains the same
      }
      
    )
    
    
    clusterExport(
      cl,
      c(
        'revmu',
        'n1train',
        'n2train',
        'n',
        'ntrain',
        'n1test',
        'n2test',
        'n1',
        'n2',
        'd',
        'block.size'
      )
    )
    ptm = proc.time()
    pb <-
      txtProgressBar(
        initial = 1,
        min = 1,
        max = REP ,
        style = 3,
        width = REP,
        char = '='
      )
    
    out2 <- data.frame(listS = character(REP), stringsAsFactors = FALSE)
    # test.out <- data.frame(margE = character(REP), stringsAsFactors = FALSE)
    for (iter in 1:REP) {
      switch (
        exID,
        ex1 = {
          pop1 = matrix(rnorm(n1 * d, mean = 0, sd = sqrt(1)), ncol = d)
          
          pop2 = cbind(matrix(rnorm(n2 * 4, mean = 5, sd = sqrt(1)), ncol = 4), matrix(rnorm(n2 * (d-4), mean = 0, sd = sqrt(1)), ncol = d-4))
          
          pop3 = cbind(matrix(rnorm(n3 * 4, mean = -5, sd = sqrt(1)), ncol = 4), matrix(rnorm(n3 * (d-4), mean = 0, sd = sqrt(1)), ncol = d-4))
          
        },
        
        ex2 = {
          # Population 1
          pop1 = matrix(rnorm(n1 * d, mean = 0, sd = 1), ncol = d)
          # Generate correlated features for pop1
          for (k in 1:n1) {
            for (len in 1:length(sig.pos)) {
              pop1[k, c(sig.pos[len] + (0:(block.size - 1)))] = mvrnorm(
                n = 1,
                mu = rep(0, block.size),
                Sigma = constcor.mat(d = block.size, rho = 0.9)
              )
            }
          }
          
          # Population 2
          pop2 = matrix(rnorm(n2 * d, mean = 0, sd = 1), ncol = d)
          # Generate correlated features for pop2
          for (k in 1:n2) {
            for (len in 1:length(sig.pos)) {
              pop2[k, c(sig.pos[len] + (0:(block.size - 1)))] = mvrnorm(
                n = 1,
                mu = rep(0, block.size),
                Sigma = constcor.mat(d = block.size, rho = -0.9)
              )
            }
          }
          
          # Population 3
          pop3 = matrix(rnorm(n3 * d, mean = 0, sd = 1), ncol = d)
          # Generate correlated features for pop3
          for (k in 1:n3) {
            for (len in 1:length(sig.pos)) {
              pop3[k, c(sig.pos[len] + (0:(block.size - 1)))] = mvrnorm(
                n = 1,
                mu = rep(0, block.size),
                Sigma = constcor.mat(d = block.size, rho = 0.5)
              )
            }
          }
        },
        
        ex8 = {
          # d = 10 # Assuming the dimensionality of the data is defined somewhere
          # n1 = 100 # Assuming the size for pop1
          # n2 = 100 # Assuming the size for pop2
          # n3 = 100 # Size for the third class, pop3
          
          # Generate pop1
          pop1 = matrix(rnorm(n1 * d, mean = 0, sd = sqrt(1)), ncol = d)
          
          # Generate pop2 and then apply a specific transformation to features 1:4
          pop2 = matrix(rnorm(n2 * d, mean = 0, sd = sqrt(1)), ncol = d)
          tmp = foreach (k = 1:n2, .combine = rbind) %dopar% {
            c(cycl(pop2[k, 1:2]), cycl(pop2[k, 3:4]))
          }
          pop2[, 1:4] = tmp
          rm(tmp)
          
          # Generate pop3 similarly and apply a potentially different transformation to features 1:4
          pop3 = matrix(rnorm(n3 * d, mean = 0, sd = sqrt(1)), ncol = d)
          tmp3 = foreach (k = 1:n3, .combine = rbind) %dopar% {
            # Assuming a transformation for pop3; using cycl as a placeholder
            # Replace or modify the transformation as per the requirements for pop3
            c(cycl(pop3[k, 1:2]), cycl(pop3[k, 3:4]))
          }
          pop3[, 1:4] = tmp3
          rm(tmp3)
        }
        
      )
      
      
      ########################### DATA GENERATION IS COMPLETE ##################
      train1 = pop1[1:n1train,]
      train2 = pop2[1:n2train,]
      train3 = pop3[1:n3train,]  # New line for third class
      
      # Combine training data from three classes
      train.set  = rbind(train1, train2, train3)
      
      # Combine testing data from three classes
      test.set = rbind(pop1[n1train + (1:n1test),], pop2[n2train + (1:n2test),], pop3[n3train + (1:n3test),])
      rm(pop1)
      rm(pop2)
      rm(pop3)  # Remove pop3 after creating test set
      
      # Create labels for training and testing data
      train.lab = rep(1:3, c(n1train, n2train, n3train))
      test.lab = rep(1:3, c(n1test, n2test, n3test))
      tmp = NULL
      
      clusterExport(cl, c('train.set', 'test.set'))
      
      # iter <- 1
      if(exID == "ex2" | exID == "ex8"){
        result <- ks_test_2d_multi(train.set, train.lab)
        selected_pairs <- result$selected_pairs
        sig.pos.est <- result$sig_positions_est
        Dpairs <- result$Dpairs
        
        # Assuming train.set is your dataset and class.labels is a vector of class labels
        pair_strings <- apply(selected_pairs, 1, function(x) paste(x, collapse = "-"))
        # Add it to the out2 dataframe
        out2$pair[iter] <- paste(pair_strings, collapse = ", ")
        
        epsilon <- 1e-10 # Define a small constant to prevent log(0)
        
        pairclass <- system.time({
          sig.pos.est <- sig.pos.est[sig.pos.est <= nrow(Dpairs)]
          
          # Extract pairs for each class
          train_class1_pairs <- lapply(sig.pos.est, function(pair_idx) {
            train.set[train.lab == 1, Dpairs[pair_idx, ]]
          })
          
          train_class2_pairs <- lapply(sig.pos.est, function(pair_idx) {
            train.set[train.lab == 2, Dpairs[pair_idx, ]]
          })
          
          train_class3_pairs <- lapply(sig.pos.est, function(pair_idx) {
            train.set[train.lab == 3, Dpairs[pair_idx, ]]
          })
          
          # Compute the number of observations in each class
          n_1 <- length(train_class1_pairs[[1]][,1])
          n_2 <- length(train_class2_pairs[[1]][,1])
          n_3 <- length(train_class3_pairs[[1]][,1])
          total_n <- n_1 + n_2 + n_3
          
          # Compute bivariate KDEs for each pair for each class
          f_hats_class1 <- lapply(train_class1_pairs, function(pair_data) {
            kde(as.matrix(pair_data))
          })
          
          f_hats_class2 <- lapply(train_class2_pairs, function(pair_data) {
            kde(as.matrix(pair_data))
          })
          
          f_hats_class3 <- lapply(train_class3_pairs, function(pair_data) {
            kde(as.matrix(pair_data))
          })
          
          compute_bivariate_density_multi <- function(x, kde_list_class1, kde_list_class2, kde_list_class3) {
            log_densities_class1 <- sapply(1:length(kde_list_class1), function(i) {
              input_matrix <- matrix(x[((i-1)*2+1):(i*2)], nrow = 1, byrow = TRUE)
              pred_val_class1 <- predict(kde_list_class1[[i]], x = input_matrix)
              log(ifelse(is.atomic(pred_val_class1), pred_val_class1, pred_val_class1$estimate) + epsilon)
            })
            
            log_densities_class2 <- sapply(1:length(kde_list_class2), function(i) {
              input_matrix <- matrix(x[((i-1)*2+1):(i*2)], nrow = 1, byrow = TRUE)
              pred_val_class2 <- predict(kde_list_class2[[i]], x = input_matrix)
              log(ifelse(is.atomic(pred_val_class2), pred_val_class2, pred_val_class2$estimate) + epsilon)
            })
            
            log_densities_class3 <- sapply(1:length(kde_list_class3), function(i) {
              input_matrix <- matrix(x[((i-1)*2+1):(i*2)], nrow = 1, byrow = TRUE)
              pred_val_class3 <- predict(kde_list_class3[[i]], x = input_matrix)
              log(ifelse(is.atomic(pred_val_class3), pred_val_class3, pred_val_class3$estimate) + epsilon)
            })
            
            log_F_hat_class1 <- sum(log_densities_class1) + log(n_1 / total_n)
            log_F_hat_class2 <- sum(log_densities_class2) + log(n_2 / total_n)
            log_F_hat_class3 <- sum(log_densities_class3) + log(n_3 / total_n)
            
            c(log_F_hat_class1, log_F_hat_class2, log_F_hat_class3)
          }
          
          classify_test_obs_multi <- function(x) {
            log_F_hats <- compute_bivariate_density_multi(x, f_hats_class1, f_hats_class2, f_hats_class3)
            which.max(log_F_hats)
          }
          
          # Similar to the binary case, populate the test_data_pairs matrix and classify
          num_test_obs <- nrow(test.set)
          test_data_pairs <- matrix(nrow = num_test_obs, ncol = length(sig.pos.est) * 2)
          
          for (i in seq_along(sig.pos.est)) {
            pair_indices <- Dpairs[sig.pos.est[i], ]
            test_data_pairs[, ((i-1)*2+1):(i*2)] <- test.set[, pair_indices, drop = FALSE]
          }      
          
          test_predictions <- apply(test_data_pairs, 1, classify_test_obs_multi)
          
          EPairClass = mean(test_predictions != test.lab) # Error rate for three-class classification
          EPairClass <- ifelse(is.na(EPairClass), 1, EPairClass)
        })
        out2$EPairClass[iter] <- EPairClass
        
        
        } 
      
      else {
       
        marg.var <- multiClassKS(train.set, train.lab)
        marg_string <- paste(marg.var, collapse = ", ")
        out2$marg[iter] <- marg_string

        
        #classification based on marginal signals begins here
        marclass <- system.time({
          # Step 1: Extract Marginal Data
          train_class1 <- as.matrix(train.set[train.lab == 1, marg.var])
          train_class2 <- as.matrix(train.set[train.lab == 2, marg.var])
          
          n <- nrow(train_class1)
          m <- nrow(train_class2)
          
          # Step 2: Compute KDEs
          f_hats <- lapply(1:ncol(train_class1), function(i) {
            kde(train_class1[, i])
          })
          
          g_hats <- lapply(1:ncol(train_class2), function(i) {
            kde(train_class2[, i])
          })
          
          
          test_data_subset <- test.set[, marg.var, drop = FALSE]
          test_predictions <- apply(test_data_subset, 1, classify_test_obs)
          
          EMarclass = mean(test_predictions != test.lab) # Error rate
          # Ensure iter is defined outside this snippet as part of a larger loop
          out2$EmarClass[iter] <- EMarclass
        })
        
        marclassRobust <- system.time({
          # Step 1: Extract Marginal Data
          train_class1 <- as.matrix(train.set[train.lab == 1, marg.var])
          train_class2 <- as.matrix(train.set[train.lab == 2, marg.var])
          
          n <- nrow(train_class1)
          m <- nrow(train_class2)
          
          # Step 2: Compute KDEs with kdrobust
          f_hats <- lapply(1:ncol(train_class1), function(i) {
            kdrobust(x = train_class1[, i])
          })
          
          g_hats <- lapply(1:ncol(train_class2), function(i) {
            kdrobust(x = train_class2[, i])
          })
          
          # Step 3: Classify Test Observations
          test_data_subset <- test.set[, marg.var, drop = FALSE]
          test_predictions <- apply(test_data_subset, 1, function(x) classify_test_obs_robust(x, f_hats, g_hats))
          
          # Step 4: Calculate Error Rate
          EMarclassRob <- mean(test_predictions != test.lab) # Error rate
          
          # Ensure iter is defined outside this snippet as part of a larger loop
          out2$EmarClass[iter] <- EMarclassRob
        })
        
        
      }
      
      
      
      
      switch(
        exID,
        ex1 = {
          listS = list(1,2,3,4)
        },
        ex2 = {
          listS = list(1:2, 3:4)
        },
        ex3 = {
          listS = list(1:2, 3)
        },
        ex4 = {
          listS = list(1,2,3,4)
        },
        ex5 = {
          listS = list(1,2,3,4)
        },
        ex6 = {
          listS = list(1,2,3,4)
        },
        ex7 = {
          listS = list(1,2,3,4)
        },
        ex8 = {
          listS = list(1:2, 3:4)
        }
      )
      
      out2$listS[iter] <- paste(format_list(listS), collapse = ", ")
      # out2$listShat[iter] <- paste(format_list(listShat), collapse = ", ")
      setTxtProgressBar(pb, iter)
      cat(
        paste('//', iter * 100 / REP, '% complete', sep = ''),
        paste("Time Elapsed ", round((
          proc.time()[3] - ptm[3]
        ) / 60), " Minutes"),
        " "
      )
    }
    
    
    gc()
    
    out2.name = switch(
      EXPR = exID,
      ex1 = '1.csv',
      ex2 = '2.csv',
      ex3 = '3.csv',
      ex4 = '4.csv',
      ex5 = '5.csv',
      ex6 = '6.csv',
      ex7 = '7.csv',
      ex8 = '8.csv'
    )
    
 
    
    write.csv(out2, out2.name, row.names = F)
    # write.csv(test.out, test.out.name, row.names = F)
    
    close(pb)
    print(paste(exID, 'is done.', sep = ' '))
  }
  
  stopCluster(cl)
  gc()
  