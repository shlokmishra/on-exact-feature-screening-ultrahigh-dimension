rm(list = ls())
#------------------------------------------------------------------------------
set.seed(123)
REP = 100 # number of iterations to run.
trn.sz.perClass = 100 # size of training sample from each class in all simulated examples Ex 1- 16 except Ex 8
tst.sz.perClass = 250 # size of test sample from each class in all simulated examples
# OS.var = readline(prompt = 'If you are on Windows, please type "W". Otherwise type "O": ')
# cluster.type = ifelse(OS.var == 'W', 'PSOCK', 'FORK')
cluster.type = 'PSOCK'
#------------------------------------------------------------------------------
EXNAMES = paste('ex', c(1,2,8), sep = '')
#------------------------------------------------------------------------------
PCKG = c(
  'energy',
  'proxy',
  'parallelDist',
  'nbpMatching',
  'MASS',
  'doParallel',
  'plot.matrix',
  'RcppArmadillo',
  'RcppXPtrUtils',
  'inline',
  'VariableScreening',
  'utils',
  'e1071',
  'Peacock.test',
  'ks'
)

install.packages(setdiff(PCKG, rownames(installed.packages())))
lapply(PCKG, library, character.only = T)

#------------------------------------------------------------------------------
source('relevant_functions.R')
source('modifications.R')
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
d = 1000 #dimension
revmu = 0.75 # the difference in mean for the marginal signal in examples 9-16

# exID = EXNAMES[1]
for (exID in EXNAMES) {
  switch (
    exID,
    ex1 = { #example 1 from the manuscript. A location problem with marginal features.
      block.size = 2
      s = 2
      no.s = 4 #4 marginals
      sig.pos = c(1, 3) #seq(1, length.out = s, by = block.size)
      
      n1train = n2train = trn.sz.perClass
      ntrain = n1train + n2train
      n1test = n2test = tst.sz.perClass
      n1 = n1train + n1test
      n2 = n2train + n2test
      n = n1 + n2
      prms = t(rperm(m = 50 * ntrain, size = ntrain))
    },
    
    ex2 = { #example 2 from the manuscript. A scale problem with paired features
      block.size = 2
      s = 2
      no.s = 4 #2 pairs
      sig.pos = c(1, 3) 
      
      n1train = n2train = trn.sz.perClass
      ntrain = n1train + n2train
      n1test = n2test = tst.sz.perClass
      n1 = n1train + n1test
      n2 = n2train + n2test
      n = n1 + n2
      prms = t(rperm(m = 50 * ntrain, size = ntrain))
    },
    
    ex3 = { #example 3 from the manuscript. A location-scale problem with marginal and paired features
      block.size = 2
      s = 2
      no.s = 3 # 1 pair 1 marginal
      sig.pos = c(1, 3) 
      
      n1train = n2train = trn.sz.perClass
      ntrain = n1train + n2train
      n1test = n2test = tst.sz.perClass
      n1 = n1train + n1test
      n2 = n2train + n2test
      n = n1 + n2
      prms = t(rperm(m = 50 * ntrain, size = ntrain))
    },
    
    ex4 = { #example 4 from the manuscript. A scale problem with marginal features
      block.size = 2
      s = 2
      no.s = 4 # 4 marginals
      sig.pos = c(1, 3) #seq(1, length.out = s, by = block.size)
      
      n1train = n2train = trn.sz.perClass
      ntrain = n1train + n2train
      n1test = n2test = tst.sz.perClass
      n1 = n1train + n1test
      n2 = n2train + n2test
      n = n1 + n2
      prms = t(rperm(m = 50 * ntrain, size = ntrain))
    },
    
    ex5 = { #example 5 from the manuscript. A heavy-tailed location problem with marginal features
      block.size = 2
      s = 2
      no.s = 4 # 4 marginals
      sig.pos = c(1, 3) 
      
      n1train = n2train = trn.sz.perClass
      ntrain = n1train + n2train
      n1test = n2test = tst.sz.perClass
      n1 = n1train + n1test
      n2 = n2train + n2test
      n = n1 + n2
      prms = t(rperm(m = 50 * ntrain, size = ntrain))
    },
    
    ex6 = { #example 6 from the manuscript. A heavy-tailed scale problem with marginal features
      block.size = 2
      s = 2
      no.s = 4 # 4 marginals
      sig.pos = c(1, 3) #seq(1, length.out = s, by = block.size)
      
      n1train = n2train = trn.sz.perClass
      ntrain = n1train + n2train
      n1test = n2test = tst.sz.perClass
      n1 = n1train + n1test
      n2 = n2train + n2test
      n = n1 + n2
      prms = t(rperm(m = 50 * ntrain, size = ntrain))
    },
    
    ex7 = { #example 7 from the manuscript. A location-scale problem with mixture distributions having marginal features 
      block.size = 2
      s = 2
      no.s = 4 # 4 marginals
      sig.pos = c(1, 3) #seq(1, length.out = s, by = block.size)
      
      n1train = n2train = trn.sz.perClass
      ntrain = n1train + n2train
      n1test = n2test = tst.sz.perClass
      n1 = n1train + n1test
      n2 = n2train + n2test
      n = n1 + n2
      prms = t(rperm(m = 50 * ntrain, size = ntrain))
    },
    
    ex8 = { #example 8 from the manuscript. A scale problem with marginal features 
      block.size = 2
      s = 2
      no.s = 2 # 2 pairs
      sig.pos = c(1, 3) #seq(1, length.out = s, by = block.size)
      
      n1train = n2train = 20
      ntrain = n1train + n2train
      n1test = n2test = tst.sz.perClass
      n1 = n1train + n1test
      n2 = n2train + n2test
      n = n1 + n2
      prms = t(rperm(m = 50 * ntrain, size = ntrain))
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

for (iter in 1:REP) {
  switch (
    exID,
    ex1 = {
      pop1 = matrix(rnorm(n1 * d, mean = 0, sd = sqrt(1)), ncol = d)
      
      pop2 = cbind(matrix(rnorm(n2 * 4, mean = 1, sd = sqrt(1)), ncol = 4), matrix(rnorm(n2 * (d-4), mean = 0, sd = sqrt(1)), ncol = d-4))
      
    },
    
    ex2 = {
      pop1 = matrix(rnorm(n1 * d, mean = 0, sd = sqrt(1)), ncol = d)
      
      pop2 = matrix(rnorm(n2 * d, mean = 0, sd = sqrt(1)), ncol = d)
      
      for (k in 1:n1) {
        for (len in 1:length(sig.pos)) {
          pop1[k, c(sig.pos[len] + (0:(block.size - 1)))] = mvrnorm(
            n = 1,
            mu = c(rep(0, block.size - 1), 0) ,
            Sigma = constcor.mat(d = block.size, rho = 0.9)
          )
        }
      }
      
      for (k in 1:n2) {
        for (len in 1:length(sig.pos)) {
          pop2[k, c(sig.pos[len] + (0:(block.size - 1)))] = mvrnorm(
            n = 1,
            mu = c(rep(0, block.size - 1), 0) ,
            Sigma = constcor.mat(d = block.size, rho = -0.9)
          )
        }
      }
    },
    
    ex3 = {
      pop1 = matrix(rnorm(n1 * d, mean = 0, sd = sqrt(1)), ncol = d)
      
      pop2 = matrix(rnorm(n2 * d, mean = 0, sd = sqrt(1)), ncol = d)
      
      pop1[, 1:3] = foreach(k = 1:n1, .combine = rbind) %dopar% {
        c(mvrnorm(
          n = 1,
          mu = rep(0, block.size) ,
          Sigma = constcor.mat(d = block.size, rho = 0.9)
        ),
        rnorm(
          n = 1,
          mean = 1,
          sd = 1
        ))
      }
      
      pop2[, 1:2] = foreach(k = 1:n2, .combine = rbind) %dopar% {
        mvrnorm(
          n = 1,
          mu = rep(0, block.size) ,
          Sigma = constcor.mat(d = block.size, rho = -0.9)
        )
      }
    },
    
    ex4 = {
      pop1 = matrix(rnorm(n1 * d, mean = 0, sd = sqrt(1)), ncol = d)
      
      pop2 = cbind(matrix(rnorm(n2 * 4, mean = 0, sd = sqrt(1/3)), ncol = 4), matrix(rnorm(n2 * (d-4), mean = 0, sd = sqrt(1)), ncol = d-4))
    },
    
    ex5 = {
      pop1 = matrix(rcauchy(
        n = n1 * d,
        location = 0,
        scale = sqrt(1)
      ), ncol = d)
      
      pop2 = cbind(matrix(rcauchy(
        n = n2 * 4,
        location = 2,
        scale = sqrt(1)
      ), ncol = 4),
      matrix(rcauchy(
        n = n2 * (d-4),
        location = 0,
        scale = sqrt(1)
      ), ncol = d-4)
      )
    },
    
    ex6 = {
      pop1 = matrix(rcauchy(
        n = n1 * d,
        location = 0,
        scale = sqrt(1)
      ), ncol = d)
      
      pop2 = cbind(matrix(rcauchy(
        n = n2 * 4,
        location = 0,
        scale = 5
      ), ncol = 4),
      matrix(rcauchy(
        n = n2 * (d-4),
        location = 0,
        scale = 1
      ), ncol = d-4)
      )
    },
    
    ex7 = {
      pop1 = cbind(matrix(rnorm(
        n1 * 4, mean = 0, sd = sqrt(4)
      ), ncol = 4), matrix(rnorm(
        n1 * (d - 4), mean = 0, sd = sqrt(1)
      ), ncol = d - 4))
      
      pop2  = cbind(matrix(
        mixdist(n = n2 * 4, mu = 1.95),
        nrow = n2,
        ncol = 4
      ),
      matrix(rnorm(
        n2 * (d-4), mean = 0, sd = sqrt(1)
      ), ncol = d-4))
    },
    
    ex8 = {
      pop1 = matrix(rnorm(n1 * d, mean = 0, sd = sqrt(1)), ncol = d)
      
      pop2 = matrix(rnorm(n2 * d, mean = 0, sd = sqrt(1)), ncol = d)
      
      tmp = foreach (k = 1:n2, .combine = rbind) %dopar% {
        c(cycl(pop2[k, 1:2]), cycl(pop2[k, 3:4]))
      }
      pop2[, 1:4] = tmp
      rm(tmp)
    }
  )
  
  
  ########################### DATA GENERATION IS COMPLETE ##################
  train1 = pop1[1:n1train,]
  train2 = pop2[1:n2train,]
  
  train.set  = rbind(train1, train2)
  
  test.set = rbind(pop1[n1train + (1:n1test),], pop2[n2train + (1:n2test),])
  rm(pop1)
  rm(pop2)
  train.lab = rep(1:2, c(n1train, n2train))
  test.lab = rep(1:2, c(n1test, n2test))
  tmp = NULL
  
  clusterExport(cl, c('train.set', 'test.set'))
  
  if(exID == "ex2" | exID == "ex8"){
    tpair = system.time({
      pairE = ks_test_2d(train.set, n1train, n2train)
    })
    
    tnbp = system.time({
      #time required for the non-bipartite matching algorithm to find the appropriate pairs of components
      pairE = as.matrix(pairE)
      dissimMAT =  max(pairE) - pairE #measure of dissimilarity
      diag(dissimMAT) = 0 # diagonals of the dissimilarity matrix are set to zero
      obj3 = distancematrix(x = as.matrix(dissimMAT))
      obj4 = nonbimatch(obj3)
      
      pairs.tmp = cbind(obj4$matches$Group1.Row, obj4$matches$Group2.Row) #- obj4$matches$Distance + max(as.matrix(pairE))
      pairs.tmp1 = unique(apply(pairs.tmp, 1, function(vec)
        paste(sort(vec), collapse = '-'))) #removing repeated entries
      Dpairs = do.call('rbind.data.frame',
                       strsplit(pairs.tmp1, split = '-', fixed = T))
      colnames(Dpairs) = c('p1', 'p2')
      rm(obj3)
      rm(obj4)
      rm(pairs.tmp)
      rm(pairs.tmp1)
      
      Dpairs = (apply(Dpairs, 2, as.numeric))
    }) # pairing complete
    
    tdecoupling = system.time({
      clusterExport(cl, 'pairE')
      
      blockE = parApply(cl, Dpairs, 1, function(vec) {
        pairE[vec[1], vec[2]]
      })
      
      sort.blockE = sort(blockE)
      
      b.rat = abs(sort.blockE[-1] / sort.blockE[-length(sort.blockE)])
      
      Ecut = (floor(length(b.rat) / 2)) + which.max(b.rat[-(1:(floor(length(b.rat) /
                                                                       2)))])
      
      sig.pos.est = which(blockE %in% sort.blockE[1:(length(sort.blockE)) > Ecut]) # selected pairs
      # Convert matrix rows into strings of pairs
      selected_pairs <- Dpairs[sig.pos.est, , drop=FALSE]
      pair_strings <- apply(selected_pairs, 1, function(x) paste(x, collapse = "-"))
      # Add it to the out2 dataframe
      out2$pair[iter] <- paste(pair_strings, collapse = ", ")
      
    })

    epsilon <- 1e-10 # Define a small constant to prevent log(0)
    
    pairclass <- system.time({
      # Assuming train.set is your training dataset and train.lab are your labels
      
      # Extract pairs for class 1 and class 2
      train_class1_pairs <- lapply(sig.pos.est, function(pair_idx) {
        train.set[train.lab == 1, Dpairs[pair_idx, ]]
      })
      
      train_class2_pairs <- lapply(sig.pos.est, function(pair_idx) {
        train.set[train.lab == 2, Dpairs[pair_idx, ]]
      })
      
      n <- length(train_class1_pairs[[1]][,1])
      m <- length(train_class2_pairs[[1]][,1])
      
      # Compute bivariate KDEs for each pair
      f_hats <- lapply(train_class1_pairs, function(pair_data) {
        kde(as.matrix(pair_data))
      })
      
      g_hats <- lapply(train_class2_pairs, function(pair_data) {
        kde(as.matrix(pair_data))
      })
      
      compute_bivariate_density <- function(x, kde_list) {
        log_densities <- sapply(1:length(kde_list), function(i) {
          input_matrix <- matrix(x[((i-1)*2+1):(i*2)], nrow = 1, byrow = TRUE)
          pred_val <- predict(kde_list[[i]], x = input_matrix)
          
          # Ensure density estimates are non-zero by adding epsilon
          density_estimate <- ifelse(is.atomic(pred_val), pred_val, pred_val$estimate) + epsilon
          
          log(density_estimate)
        })
        return(sum(log_densities))
      }
      
      classify_test_obs <- function(x) {
        log_F_hat <- compute_bivariate_density(x, f_hats) + log(n / (n + m))
        log_G_hat <- compute_bivariate_density(x, g_hats) + log(m / (n + m))
        
        DM <- log_F_hat - log_G_hat
        ifelse(DM >= 0, 1, 2)
      }
      
      # Initialize the test_data_pairs matrix with the correct dimensions
      num_test_obs <- nrow(test.set)
      test_data_pairs <- matrix(nrow = num_test_obs, ncol = length(sig.pos.est) * 2)
      
      # Populate the test_data_pairs matrix
      for (i in seq_along(sig.pos.est)) {
        pair_indices <- Dpairs[sig.pos.est[i], ]
        test_data_pairs[, ((i-1)*2+1):(i*2)] <- test.set[, pair_indices, drop = FALSE]
      }      
      test_predictions <- apply(test_data_pairs, 1, classify_test_obs)
      
      EPairClass = mean(test_predictions != test.lab) # Error rate
      # Handle NA values in error rate calculation
      EPairClass <- ifelse(is.na(EPairClass), 1, EPairClass) # Assign 100% error if NA
      # Store the error rate similarly to the previous example
      out2$EPairClass[iter] <- EPairClass
    })
    

  } else {
    tmar = system.time({
      # time required to identify the marginal signals
      margE = ks_test_1d(train.set, n1train, n2train)
      
      sort.margE = sort(margE)
      
      b.rat = abs(sort.margE[-1] / sort.margE[-length(sort.margE)])
      
      Ecut = (floor(length(b.rat) / 2)) + which.max(b.rat[-(1:(floor(length(b.rat) /
                                                                       2)))])
      sig.pos.est = which(margE %in% sort.margE[1:(length(sort.margE)) > Ecut]) # selected components
      marg.var = sig.pos.est
    }) #identification of marginal signals ends here
    marg_string <- paste(marg.var, collapse = ", ")
    out2$marg[iter] <- marg_string
    plot(sort.margE, type='o', col='blue', xlab='Index', ylab='Values', main='Plot of Sorted KS Statistics')
    
    
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
      
      # Step 3: Compute Aggregate Densities
      compute_density <- function(x, kde_list) {
        epsilon <- .Machine$double.eps^0.5  # A small positive number close to zero
        
        density_logs <- sapply(1:length(kde_list), function(i) {
          pred_val <- predict(kde_list[[i]], x = x[i])
          
          # Use the estimate or the atomic value
          density_estimate <- if(is.atomic(pred_val)) {
            pred_val[1]
          } else {
            pred_val$estimate
          }
          
          # Ensure the density estimate is strictly positive before taking log
          density_estimate <- max(density_estimate, epsilon)
          
          log_val <- log(density_estimate)
          
          return(log_val)
        })
        return(sum(density_logs))
      }
      
      # Step 4: Classify Test Data
      classify_test_obs <- function(x) {
        log_F_hat <- compute_density(x, f_hats) + log(n / (n + m))
        log_G_hat <- compute_density(x, g_hats) + log(m / (n + m))
        
        DM <- log_F_hat - log_G_hat
        ifelse(DM >= 0, 1, 2)
      }
      
      test_data_subset <- test.set[, marg.var, drop = FALSE]
      test_predictions <- apply(test_data_subset, 1, classify_test_obs)
      
      EMarclass = mean(test_predictions != test.lab) # Error rate
      # Ensure iter is defined outside this snippet as part of a larger loop
      out2$EmarClass[iter] <- EMarclass
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
}

gc()

  out2.name = switch(
    EXPR = exID,
    ex1 = 'signals_1.csv',
    ex2 = 'signals_2.csv',
    ex3 = 'signals_3.csv',
    ex4 = 'signals_4.csv',
    ex5 = 'signals_5.csv',
    ex6 = 'signals_6.csv',
    ex7 = 'signals_7.csv',
    ex8 = 'signals_8.csv'
  )
write.csv(out2, out2.name, row.names = F)

close(pb)
print(paste(exID, 'is done.', sep = ' '))
}

stopCluster(cl)
gc()
