# This file contains all the necessary functions that are required to execute the main file : SimulationStudy_ReviewResponse_JCGS_optimizedParallel.R
###############################################################################
cycl <-
  function(vec)
    # function is required to generate data from signed multivariate normal distribution
  {
    sgn <- sign(vec)
    return(c(vec[-length(vec)] * sgn[-1], vec[length(vec)] * sgn[1]))
  }

#########################################################################
mixdist = function(n, mu) {
  #function to generate observations for Ex 8
  sapply(1:n, function(val) {
    if (rbinom(n = 1, size = 1, prob = 0.5) == 1) {
      rnorm(n = 1,
            mean = mu,
            sd = sqrt(4 - mu * 2))
    } else{
      rnorm(n = 1,
            mean = -mu,
            sd = sqrt(4 - mu * 2))
    }
  })
}




#########################################################################
# the following two functions are required for computing the energy distances using gamma_1
custom.dist.exp = function(v1, v2) {
  #generalized euclidean distance
  tmp = 1 - exp(-((v1 - v2) ^ 2))
  # tmp = abs(v1-v2)
  mean(tmp)
}
#______________________________________________________________________
gexpFuncPtr <-
  cppXPtr(
    "double customDist(const arma::mat &A, const arma::mat &B) {
    double tmp;
    tmp = arma::accu(1 - exp(-(arma::square(A - B))));
 return  tmp;}",
 depends = c("RcppArmadillo")
  )





#######################################################################
# the following two functions are required for computing the energy distances using gamma_2
custom.dist.log = function(v1, v2) {
  #generalized euclidean distance
  tmp = log(1 + ((v1 - v2) ^ 2))
  mean(tmp)
}
#______________________________________________________________________
glogFuncPtr <-
  cppXPtr(
    "double customDist(const arma::mat &A, const arma::mat &B) {
    double tmp;
    tmp = arma::accu( log(1 + arma::square(A - B)));
 return  tmp;}",
 depends = c("RcppArmadillo")
  )




#######################################################################
# the following two functions are required for computing the energy distances using gamma_3
custom.dist.sqrt = function(v1, v2) {
  #generalized euclidean distance
  # tmp = 1 - exp(-(v1 - v2) ^ 2)
  tmp = abs(v1 - v2)
  mean(tmp)
}
#______________________________________________________________________
gsqrtFuncPtr <-
  cppXPtr(
    "double customDist(const arma::mat &A, const arma::mat &B) {
    double tmp;
    tmp = arma::accu((arma::abs(A - B)));
 return  tmp;}",
 depends = c("RcppArmadillo")
  )
#######################################################################


#the generalized scale adjusted average distance classifier
gSAVG = function(Z, train1.df, train2.df, dxx, dyy, dist.fun) {
  #gSAVG classifier
  # Z = test.set[1,]
  dm1 = apply(train1.df, 1, dist.fun, v2 = Z)
  dm2 = apply(train2.df, 1, dist.fun, v2 = Z)
  Tx = mean(dm1)
  Ty = mean(dm2)
  
  
  Tx = Tx - 0.5 * dxx
  Ty = Ty - 0.5 * dyy
  return(which.min(c(Tx, Ty)))
}



#######################################################################
#the average distance classifier
# AVG = function(Z, train1.df, train2.df) {
#   #AVG classifier
#   # Z = test.set[1,]
#   dm1 = apply(train1.df, 1, function(vec) {
#     mean((vec - Z) ^ 2)
#   })
#   dm2 = apply(train2.df, 1, function(vec) {
#     mean((vec - Z) ^ 2)
#   })
#   Tx = mean(dm1)
#   Ty = mean(dm2)
#   
#   return(which.min(c(Tx, Ty)))
# }




#######################################################################
#the block-generalized scale adjusted average distance classifier
bgSAVG <-  function(z,
                    blocked.train.SW,
                    n1train,
                    n2train,
                    SWg1,
                    SWg2,
                    SWg3,
                    SWg4) {
  n.train <- nrow(blocked.train.SW[[1]])
  tmp.tmp <- blocked.train.SW
  repz <-
    lapply(z, function(u) {
      return(matrix(rep(u, n.train), nrow = n.train, byrow = T))
    })
  temprep <-
    lapply(1:length(repz), function(u) {
      return(rowMeans(tmp.tmp[[u]] - repz[[u]]) ^ 2)
    })
  t1 <- do.call('cbind', temprep)
  if (ncol(t1) > 1) {
    gamma.exp <- rowMeans(1 - exp(-t1))
    gamma.sqrt <- rowMeans(0.5 * sqrt(t1))
    gamma.log <- rowMeans(log(1 + t1))
    gamma.rat <- rowMeans(t1 / (1 + t1))
  } else{
    gamma.exp <- 1 - exp(-t1)
    gamma.sqrt <- 0.5 * sqrt(t1)
    gamma.log <- log(1 + t1)
    gamma.rat <- t1 / (1 + t1)
  }
  rm(list = c('tmp.tmp', 'repz', 't1', 'temprep'))
  
  obj <- cbind.data.frame(gamma.exp, gamma.sqrt, gamma.rat)
  T1 <- colMeans(obj[1:n1train,])
  T2 <- colMeans(obj[n1train + (1:n2train),])
  T3 <- c(sum(SWg1[1:n1train, 1:n1train]),
          sum(SWg2[1:n1train, 1:n1train]),
          sum(SWg3[1:n1train, 1:n1train]),
          sum(SWg4[1:n1train, 1:n1train])) / (2 * n1train * (n1train - 1))
  T4 <- c(sum(SWg1[n1train + (1:n2train), n1train + (1:n2train)]),
          sum(SWg2[n1train + (1:n2train), n1train + (1:n2train)]),
          sum(SWg3[n1train + (1:n2train), n1train + (1:n2train)]),
          sum(SWg4[n1train + (1:n2train), n1train + (1:n2train)])) /    (2 * n2train * (n2train - 1))
  return(c(ifelse(T2 - T1 + T3 - T4 > 0, 1, 2)))
}



#######################################################################

# dcor1 = function(X1, Y1) {
#   n = nrow(X1)
#   A <- as.matrix(dist(X1))
#   B <- as.matrix(dist(Y1))
#   
#   mu <- sum(A) / (n * n)
#   mucol <- matrix(rep(colSums(A) / n, n), ncol = n, byrow = T)
#   mufilas <- matrix(rep(colSums(A) / n, n), ncol = n, byrow = F)
#   A <- A - mufilas - mucol + mu
#   
#   mu <- sum(B) / (n * n)
#   mucol <- matrix(rep(colSums(B) / n, n), ncol = n, byrow = T)
#   mufilas <- matrix(rep(colSums(B) / n, n), ncol = n, byrow = F)
#   B <- B - mufilas - mucol + mu
#   
#   
#   #Calculo de la correlacion
#   res <- nu2(A, B)
#   if (res != 0) {
#     res <- nu2(A, B) / sqrt(nu2(A, A) * nu2(B, B))
#   }
#   return(res)
# }
# ###########################################################################
# nu2 = function(A, B) {
#   n = nrow(A)
#   
#   
#   v2 = sum(diag(A %*% B)) / (n * n)
#   
#   return(v2)
# }



###########################################################################
# the Robust Rank Screening method proposed by Cheng et al. (2017)
screenRRS =  function(train.set, n1train, n2train) {
  ntrain = n1train + n2train
  s.hat = floor(ntrain / log(ntrain))
  order.train = apply(train.set, 2, order)
  RRS = apply(order.train, 2, function(vec) {
    tmp = c(mean(vec[1:n1train]), mean(vec[n1train + (1:n2train)]))
    tmp1 = (2 * tmp / (ntrain + 1) - 1) ^ 2
    tmp2 = c(n1train, n2train) * tmp1 / ntrain
    return(sum(tmp2))
  })
  RRSrank = order(RRS, decreasing = T)
  RRS.var = RRSrank[1:s.hat]
  return(sort(setdiff(RRS.var, NA)))
}




#######################################################################
# the Screening method based on marginal classifiers proposed by Sheng and Wang (2020)
# here SVM with linear and RBF kernels are used. 

MCS.SVM = function(train.set, train.lab) {
  s.hat = floor(nrow(train.set) / log(nrow(train.set)))
  
  acc.lin = acc.rbf = NULL
  acc.lin = foreach::foreach(
    j = 1:ncol(train.set),
    .combine = 'rbind',
    .packages = 'e1071'
  ) %dopar% {
    k.den1 = svm(x = train.set[, j],
                 y = factor(train.lab),
                 kernel = 'linear')
    k.den2 = svm(x = train.set[, j],
                 y = factor(train.lab),
                 kernel = 'radial')
    svm.lin.lbl = predict(k.den1, newdata = train.set[, j])
    # acc.lin[j] = mean(svm.lin.lbl == test.lab)
    
    svm.rbf.lbl = predict(k.den2, newdata = train.set[, j])
    # acc.rbf[j] = mean(svm.rbf.lbl == test.lab)
    c(mean(svm.lin.lbl == train.lab),
      mean(svm.rbf.lbl == train.lab))
  }
  
  acc.rbf = acc.lin[, 2]
  MCS.svmlin.rank = order(acc.lin[, 1], decreasing = T)
  MCS.svmrbf.rank = order(acc.rbf, decreasing = T)
  
  MCS.svmlin.var = MCS.svmlin.rank[1:s.hat]
  MCS.svmlin.var = sort(setdiff(MCS.svmlin.var, NA))
  
  MCS.svmrbf.var = MCS.svmrbf.rank[1:s.hat]# MV-SIS
  MCS.svmrbf.var = sort(setdiff(MCS.svmrbf.var, NA))
  
  return(list(MCS.svmlin.var, MCS.svmrbf.var))
}

#######################################################################
# MV_info = function(train.cord.i, train.lab) {
#   #Runze Li
#   tmp = ecdf(train.cord.i)
#   F0 = tmp(train.cord.i)
#   Yr <- unique(train.lab)
#   tmp2 = sapply(Yr, function(val) {
#     indr  = which(train.lab == val)
#     tmp1 = ecdf(train.cord.i[indr])
#     Fr = tmp1(train.cord.i)
#     return(length(indr) * max((Fr - F0) ^ 2) / length(train.lab))
#   })
#   return(sum(tmp2))
# }
# #######################################################################
# KS_MVSIS = function (train.df, train.lab)
# {
#   results <- NULL
#   
#   v1 <- abs(apply(train.df, 2, MV_info, train.lab = train.lab))
#   MWord <- order(v1, decreasing = T)
#   rank <- match(v1, v1[MWord])
#   results$measurement = v1
#   results$rank <- rank
#   
#   return(results)
# }
# 


#######################################################################
fun2.on.indices = function(indx,
                           z,
                           trainingset,
                           distmat.gamma.exp,
                           n.train) {
  # indx <- 1
  tmp.tmp <- trainingset
  rm(trainingset)
  tmp.tmp[indx,] <- z
  repz <- matrix(rep(z, n.train), nrow = n.train, byrow = T)
  temprep <- (tmp.tmp - repz) ^ 2
  rm(list = c('tmp.tmp', 'repz'))
  
  gamma.exp <- rowMeans(1 - exp(-temprep))
  
  rm(temprep)
  gamma.exp.dist.temp.L2 <-
    sum(abs(gamma.exp - distmat.gamma.exp[, indx])) #dissimarity between training samples and z
  
  rm(list = c('gamma.exp', 'distmat.gamma.exp'))
  return(gamma.exp.dist.temp.L2)
  
}




# ###########################################################
# #returns predicted class label for z. classifier: NN-MADD-gamma
# # z: test example |  trainingset : training sample with class labels in last column |  distmat.block.gamma : n.train X n.train matrix with (i,j)th entry as the dissimilarity(based on gamma) between ith and jth training obs. | class.change: size of training sample corresponding to class 1 |   n.train : size of training set | alph : value of alpha in gamma.log
# NN.gMADD.lbl =  function(z,
#                          trainingset,
#                          distmat.gamma.exp,
#                          n.train,
#                          lbl) {
#   #NN-gMADD1
#   # lbl <- trainingset[,ncol(trainingset)]
#   # z <- test.new[1,]
#   # z <- Z
#   indices <- 1:n.train
#   new.temp <-
#     do.call(
#       'rbind',
#       lapply(
#         indices,
#         fun2.on.indices,
#         z,
#         trainingset,
#         distmat.gamma.exp,
#         n.train
#       )
#     )
#   rm(trainingset)
#   
#   obj.gamma.exp <- which.min(new.temp[, 1])
#   rm(new.temp)
#   
#   lbl.gamma.exp.1 <- lbl[obj.gamma.exp]
#   rm(list = c('indices', 'obj.gamma.exp'))
#   
#   return(lbl.gamma.exp.1)
#   
# }





###########################################################
# a function that returns d X d autocorrelation matrix with correlation coefficient rho 
autocor.mat = function(d, rho) {
  indic = expand.grid(1:d, 1:d)
  mat.el = apply(indic, 1, function(vec)
    rho ^ abs(diff(vec)))
  return(matrix(mat.el, nrow = d, byrow = T))
}




###########################################################
# a function that returns d X d equicorrelation positive definite matrix with correlation coefficient rho
constcor.mat = function(d, rho) {
  if (rho <= -1 / (d - 1)) {
    print(paste('rho must be larger than',-1 / (d - 1), sep = ' '))
  } else{
    mat.el = matrix(rho, ncol = d, nrow = d)
    mat.el = mat.el + diag(1 - rho, d)
    return(mat.el)
  }
}




###########################################################
# fun1.block <- function(x, blocked.train.SW)
# {
#   n.train <- nrow(blocked.train.SW[[1]])
#   repx <-
#     lapply(x, function(u) {
#       return(matrix(rep(u, n.train), nrow = n.train, byrow = T))
#     })
#   temprep <-
#     lapply(1:length(repx), function(u) {
#       return(rowMeans(blocked.train.SW[[u]] - repx[[u]]) ^ 2)
#     })
#   t1 <- do.call('cbind', temprep)
#   if (ncol(t1) > 1) {
#     gamma.exp <- rowMeans(1 - exp(-t1))
#     gamma.sqrt <- rowMeans(sqrt(t1))
#     gamma.log <- rowMeans(log(1 + t1))
#     gamma.rat <- rowMeans(t1 / (1 + t1))
#   } else{
#     gamma.exp <- 1 - exp(-t1)
#     gamma.sqrt <- sqrt(t1)
#     gamma.log <- log(1 + t1)
#     gamma.rat <- t1 / (1 + t1)
#   }
#   # proc.time()-ptm
#   rm(list = c('repx', 'temprep', 't1'))
#   
#   return(cbind.data.frame(gamma.exp, gamma.sqrt, gamma.log, gamma.rat))
# }




#############################################################################
# a function that returns the sample energy distance between two distributions for a given clustering of covairates.
fun2.block <- function(x, blocked.train.SW, gamfun)
{
  n.train <- nrow(blocked.train.SW[[1]])
  repx <-
    lapply(x, function(u) {
      return(matrix(rep(u, n.train), nrow = n.train, byrow = T))
    })
  temprep <-
    lapply(1:length(repx), function(u) {
      return(rowMeans(blocked.train.SW[[u]] - repx[[u]]) ^ 2)
    })
  t1 <- do.call('cbind', temprep)
  if (ncol(t1) > 1) {
    out = switch(
      gamfun,
      gexp = rowMeans(1 - exp(-t1)),
      gsqrt = rowMeans(sqrt(t1)),
      glog = rowMeans(log(1 + t1)),
      grat = rowMeans(t1 / (1 + t1))
    )
  } else{
    out = switch(
      gamfun,
      gexp = 1 - exp(-t1),
      gsqrt = sqrt(t1),
      glog = log(1 + t1),
      grat = t1 / (1 + t1)
    )
  }
  # proc.time()-ptm
  rm(list = c('repx', 'temprep', 't1'))
  
  return(out)
}
###############################################
# avg_dist_cls.block <-
#   function(z,
#            blocked.train.SW,
#            n1train,
#            n2train,
#            SW.gamma.exp.distmat,
#            SW.gamma.sqrt.distmat,
#            SW.gamma.log.distmat,
#            SW.gamma.rat.distmat) {
#     n.train <- nrow(blocked.train.SW[[1]])
#     tmp.tmp <- blocked.train.SW
#     # tmp.tmp <- lapply(1:length(blocked.train.SW),function(u){tmp.tmp[[u]][indx,]<- z[[u]];return(tmp.tmp[[u]])})
#     # rm(train0)
#     repz <-
#       lapply(z, function(u) {
#         return(matrix(rep(u, n.train), nrow = n.train, byrow = T))
#       })
#     temprep <-
#       lapply(1:length(repz), function(u) {
#         return(rowMeans(tmp.tmp[[u]] - repz[[u]]) ^ 2)
#       })
#     t1 <- do.call('cbind', temprep)
#     if (ncol(t1) > 1) {
#       gamma.exp <- rowMeans(1 - exp(-t1))
#       gamma.sqrt <- rowMeans(sqrt(t1))
#       gamma.log <- rowMeans(log(1 + t1))
#       gamma.rat <- rowMeans(t1 / (1 + t1))
#     } else{
#       gamma.exp <- 1 - exp(-t1)
#       gamma.sqrt <- sqrt(t1)
#       gamma.log <- log(1 + t1)
#       gamma.rat <- t1 / (1 + t1)
#     }
#     rm(list = c('tmp.tmp', 'repz', 't1', 'temprep'))
#     
#     obj <-
#       cbind.data.frame(gamma.exp, gamma.sqrt, gamma.log, gamma.rat)
#     T1 <- colMeans(obj[1:n1train,])
#     T2 <- colMeans(obj[n1train + (1:n2train),])
#     T3 <-
#       c(
#         sum(SW.gamma.exp.distmat[1:n1train, 1:n1train]),
#         sum(SW.gamma.sqrt.distmat[1:n1train, 1:n1train]),
#         sum(SW.gamma.log.distmat[1:n1train, 1:n1train]),
#         sum(SW.gamma.rat.distmat[1:n1train, 1:n1train])
#       ) / (2 * n1train * (n1train - 1))
#     T4 <-
#       c(
#         sum(SW.gamma.exp.distmat[n1train + (1:n2train), n1train + (1:n2train)]),
#         sum(SW.gamma.sqrt.distmat[n1train + (1:n2train), n1train + (1:n2train)]),
#         sum(SW.gamma.log.distmat[n1train + (1:n2train), n1train + (1:n2train)]),
#         sum(SW.gamma.rat.distmat[n1train + (1:n2train), n1train + (1:n2train)])
#       ) / (2 * n2train * (n2train - 1))
#     return(c(ifelse(T2 - T1 + T3 - T4 > 0, 1, 2)))
#   }






###################################################
# block generalized scale adjusted average distance classifier for two-class problems.
bgSAVG.real <-
  function(z,
           blocked.train.SW,
           n1train,
           n2train,
           SW.distmat,
           gamfun) {
    n.train <- nrow(blocked.train.SW[[1]])
    tmp.tmp <- blocked.train.SW
    repz <-
      lapply(z, function(u) {
        return(matrix(rep(u, n.train), nrow = n.train, byrow = T))
      })
    temprep <-
      lapply(1:length(repz), function(u) {
        return(rowMeans(tmp.tmp[[u]] - repz[[u]]) ^ 2)
      })
    t1 <- do.call('cbind', temprep)
    if (ncol(t1) > 1) {
      obj = switch(
        gamfun,
        gexp = rowMeans(1 - exp(-t1)),
        gsqrt = rowMeans(sqrt(t1)),
        glog = rowMeans(log(1 + t1)),
        grat = rowMeans(t1 / (1 + t1))
      )
    } else{
      obj = switch(
        gamfun,
        gexp = 1 - exp(-t1),
        gsqrt = sqrt(t1),
        glog = log(1 + t1),
        grat = t1 / (1 + t1)
      )
    }
    rm(list = c('tmp.tmp', 'repz', 't1', 'temprep'))
    
    T1 <- mean(obj[1:n1train])
    T2 <- mean(obj[n1train + (1:n2train)])
    T3 <-
      sum(SW.distmat[1:n1train, 1:n1train]) / (2 * n1train * (n1train - 1))
    T4 <-
      sum(SW.distmat[n1train + (1:n2train), n1train + (1:n2train)]) / (2 * n2train * (n2train - 1))
    return(c(ifelse(T2 - T1 + T3 - T4 > 0, 1, 2)))
  }





###############################################################################
# screening method based on Kolmogorov filtering
k.filter <- function(x, y, response.type, method) {
  lbl = unique(y)
  obj = apply(x, 2, function(vec) {
    tmp = ks.test(x = vec[y == lbl[1]],
                  y = vec[y == lbl[2]],
                  alternative = 'two.sided')
    return(tmp$statistic)
  })
  return(list(k.stat = obj))
}




###############################################################################
#function that return computing univariate energy distances for different choices of gamma.
marginalenergy <- function(train.set, n1train, n2train, gamfun) {
  codeC = switch(
    gamfun,
    gexp = paste(
      "
    int i, j, k;
    int d = dimen[0];

    int n1train = ",
    n1train,
    ";
    int n2train = ",
    n2train,
    ";
    int ntrain = n1train + n2train;

    double **dataMAT;

    dataMAT = malloc(sizeof(double*) * ntrain); // ntrain X d training data

    for(i = 0; i < ntrain; i++) {
        dataMAT[i] = malloc(sizeof(double*) * d);
    }

    for(i=0; i < ntrain ; i++){
        for(j = 0; j < d ; j++){
            dataMAT[i][j] = datavec[i*d +j];
        }
    }

    for(i = 0 ; i < d ; i++){
        double h11 = 0;
        double h22 = 0;
        double h12 = 0;
        double tmp = 0;

        for(j = 0 ; j < n1train ; j++){
            for(k =0; k < n1train ; k++){
                tmp = pow(dataMAT[j][i] - dataMAT[k][i],2);
                h11 = h11 +  1 - exp(-tmp);
                //h11 = h11 +  sqrt(tmp);
                //h11 = h11 +  log(1 + tmp);
                //h11 = h11 +  tmp/(1 + tmp);
            }

            for(k =n1train; k < ntrain ; k++){
                tmp = pow(dataMAT[j][i] - dataMAT[k][i],2);
                h12 = h12 +  1 - exp(-tmp);
                //h12 = h12 +  sqrt(tmp);
                //h12 = h12 + log(1 + tmp);
                //h12 = h12 + tmp/(1 + tmp);
            }
        }
        h11 = h11 / (n1train * (n1train));
        h12 = h12 / (n1train * n2train);

        for(j = n1train ; j < ntrain ; j++){
            for(k = n1train; k < ntrain ; k++){
                tmp = (pow(dataMAT[j][i] - dataMAT[k][i],2));
                h22 = h22 + 1 - exp(-tmp);
                //h22 = h22 +  sqrt(tmp);
                //h22 = h22 + log(1 + tmp);
                //h22 = h22 + tmp/(1 + tmp);
            }
        }
        h22 = h22 / (n2train * (n2train));

        margE[i] = h12 - 0.5 * ( h11 + h22);

    } // the marginal energies are computed.

",
sep = ''
    ),
gsqrt = paste(
  "
    int i, j, k;
    int d = dimen[0];

    int n1train = ",
  n1train,
  ";
    int n2train = ",
  n2train,
  ";
    int ntrain = n1train + n2train;

    double **dataMAT;

    dataMAT = malloc(sizeof(double*) * ntrain); // ntrain X d training data

    for(i = 0; i < ntrain; i++) {
        dataMAT[i] = malloc(sizeof(double*) * d);
    }

    for(i=0; i < ntrain ; i++){
        for(j = 0; j < d ; j++){
            dataMAT[i][j] = datavec[i*d +j];
        }
    }

    for(i = 0 ; i < d ; i++){
        double h11 = 0;
        double h22 = 0;
        double h12 = 0;
        double tmp = 0;

        for(j = 0 ; j < n1train ; j++){
            for(k =0; k < n1train ; k++){
                tmp = pow(dataMAT[j][i] - dataMAT[k][i],2);
                //h11 = h11 +  1 - exp(-tmp);
                h11 = h11 +  sqrt(tmp);
                //h11 = h11 +  log(1 + tmp);
                //h11 = h11 +  tmp/(1 + tmp);
            }

            for(k =n1train; k < ntrain ; k++){
                tmp = pow(dataMAT[j][i] - dataMAT[k][i],2);
                //h12 = h12 +  1 - exp(-tmp);
                h12 = h12 +  sqrt(tmp);
                //h12 = h12 + log(1 + tmp);
                //h12 = h12 + tmp/(1 + tmp);
            }
        }
        h11 = h11 / (n1train * (n1train));
        h12 = h12 / (n1train * n2train);

        for(j = n1train ; j < ntrain ; j++){
            for(k = n1train; k < ntrain ; k++){
                tmp = (pow(dataMAT[j][i] - dataMAT[k][i],2));
                //h22 = h22 + 1 - exp(-tmp);
                h22 = h22 +  sqrt(tmp);
                //h22 = h22 + log(1 + tmp);
                //h22 = h22 + tmp/(1 + tmp);
            }
        }
        h22 = h22 / (n2train * (n2train));

        margE[i] = h12 - 0.5 * ( h11 + h22);

    } // the marginal energies are computed.

",
sep = ''
),
glog = paste(
  "
    int i, j, k;
    int d = dimen[0];

    int n1train = ",
  n1train,
  ";
    int n2train = ",
  n2train,
  ";
    int ntrain = n1train + n2train;

    double **dataMAT;

    dataMAT = malloc(sizeof(double*) * ntrain); // ntrain X d training data

    for(i = 0; i < ntrain; i++) {
        dataMAT[i] = malloc(sizeof(double*) * d);
    }

    for(i=0; i < ntrain ; i++){
        for(j = 0; j < d ; j++){
            dataMAT[i][j] = datavec[i*d +j];
        }
    }

    for(i = 0 ; i < d ; i++){
        double h11 = 0;
        double h22 = 0;
        double h12 = 0;
        double tmp = 0;

        for(j = 0 ; j < n1train ; j++){
            for(k =0; k < n1train ; k++){
                tmp = pow(dataMAT[j][i] - dataMAT[k][i],2);
                //h11 = h11 +  1 - exp(-tmp);
                //h11 = h11 +  sqrt(tmp);
                h11 = h11 +  log(1 + tmp);
                //h11 = h11 +  tmp/(1 + tmp);
            }

            for(k =n1train; k < ntrain ; k++){
                tmp = pow(dataMAT[j][i] - dataMAT[k][i],2);
                //h12 = h12 +  1 - exp(-tmp);
                //h12 = h12 +  sqrt(tmp);
                h12 = h12 + log(1 + tmp);
                //h12 = h12 + tmp/(1 + tmp);
            }
        }
        h11 = h11 / (n1train * (n1train));
        h12 = h12 / (n1train * n2train);

        for(j = n1train ; j < ntrain ; j++){
            for(k = n1train; k < ntrain ; k++){
                tmp = (pow(dataMAT[j][i] - dataMAT[k][i],2));
                //h22 = h22 + 1 - exp(-tmp);
                //h22 = h22 +  sqrt(tmp);
                h22 = h22 + log(1 + tmp);
                //h22 = h22 + tmp/(1 + tmp);
            }
        }
        h22 = h22 / (n2train * (n2train));

        margE[i] = h12 - 0.5 * ( h11 + h22);

    } // the marginal energies are computed.

",
sep = ''
),
grat = paste(
  "
    int i, j, k;
    int d = dimen[0];

    int n1train = ",
  n1train,
  ";
    int n2train = ",
  n2train,
  ";
    int ntrain = n1train + n2train;

    double **dataMAT;

    dataMAT = malloc(sizeof(double*) * ntrain); // ntrain X d training data

    for(i = 0; i < ntrain; i++) {
        dataMAT[i] = malloc(sizeof(double*) * d);
    }

    for(i=0; i < ntrain ; i++){
        for(j = 0; j < d ; j++){
            dataMAT[i][j] = datavec[i*d +j];
        }
    }

    for(i = 0 ; i < d ; i++){
        double h11 = 0;
        double h22 = 0;
        double h12 = 0;
        double tmp = 0;

        for(j = 0 ; j < n1train ; j++){
            for(k =0; k < n1train ; k++){
                tmp = pow(dataMAT[j][i] - dataMAT[k][i],2);
                //h11 = h11 +  1 - exp(-tmp);
                //h11 = h11 +  sqrt(tmp);
                //h11 = h11 +  log(1 + tmp);
                h11 = h11 +  tmp/(1 + tmp);
            }

            for(k =n1train; k < ntrain ; k++){
                tmp = pow(dataMAT[j][i] - dataMAT[k][i],2);
                //h12 = h12 +  1 - exp(-tmp);
                //h12 = h12 +  sqrt(tmp);
                //h12 = h12 + log(1 + tmp);
                h12 = h12 + tmp/(1 + tmp);
            }
        }
        h11 = h11 / (n1train * (n1train));
        h12 = h12 / (n1train * n2train);

        for(j = n1train ; j < ntrain ; j++){
            for(k = n1train; k < ntrain ; k++){
                tmp = (pow(dataMAT[j][i] - dataMAT[k][i],2));
                //h22 = h22 + 1 - exp(-tmp);
                //h22 = h22 +  sqrt(tmp);
                //h22 = h22 + log(1 + tmp);
                h22 = h22 + tmp/(1 + tmp);
            }
        }
        h22 = h22 / (n2train * (n2train));

        margE[i] = h12 - 0.5 * ( h11 + h22);

    } // the marginal energies are computed.

",
sep = ''
)
  )
  
  
  
  eg = cfunction(
    signature(
      margE = 'double',
      datavec = 'double',
      train1size = 'integer',
      train2size = 'integer',
      dimen = 'integer'
    ),
    codeC,
    language = 'C',
    convention = '.C'
  )
  
  d = ncol(train.set)
  ntrain = n1train + n2train
  
  ccor = eg(
    margE = rep(0, d),
    datavec = c(t(train.set)),
    train1size = n1train,
    train2size = n2train,
    dimen = d
  )
  
  margE = ccor$margE * n1train * n2train / ntrain
  
  return(margE)
}




###############################################################################
#function that return a dXd matrix of pairwise energy distances for different choices of gamma.
pairenergy <-
  function(train.set,
           n1train,
           n2train,
           no.cores,
           gamfun) {
    cstring = switch(
      gamfun,
      gexp = paste(
        "double pairEDist(const arma::mat &A, const arma::mat &B) {

            int k,l;
            double h11=0;
            double h12=0;
            double h22=0;

            int n1train = ",
        n1train,
        "; // DON'T FORGET TO CHANGE THIS
            int n2train = ",
        n2train,
        "; // DON'T FORGET TO CHANGE THIS
            int ntrain = n1train + n2train;

            double tmp = 0;

            for(k =0; k <n1train; k++){
                for(l=0; l<n1train; l++){
                    tmp = 0.5*(pow(A[k] - A[l],2) + pow(B[k] - B[l],2));
                    h11 = h11 +  1 - exp(-tmp);
                    //h11 = h11 +  sqrt(tmp);
                    //h11 = h11 +  log(1 + tmp);
                    //h11 = h11 +  tmp/(1 + tmp);
                }

                for(l=n1train; l<ntrain; l++){
                    tmp = 0.5*(pow(A[k] - A[l],2) + pow(B[k] - B[l],2));
                    h12 = h12 + 1 - exp(-tmp);
                    //h12 = h12 +  sqrt(tmp);
                    //h12 = h12 + log(1 + tmp);
                    //h12 = h12 + tmp/(1 + tmp);
                }
            }
            h11 = h11/(n1train * n1train);
            h12 = h12/(n1train * n2train);

            for(k =n1train; k <ntrain; k++){
                for(l=n1train; l<ntrain; l++){
                    tmp = 0.5*(pow(A[k] - A[l],2) + pow(B[k] - B[l],2));
                    h22 = h22 +  1 - exp(-tmp);
                    //h22 = h22 +  sqrt(tmp);
                    //h22 = h22 + log(1 + tmp);
                    //h22 = h22 + tmp/(1 + tmp);
                }
            }
            h22 = h22/(n2train * n2train);

            return h12 - 0.5*(h11 + h22);
            }",
        sep = ''
      ),
      gsqrt = paste(
        "double pairEDist(const arma::mat &A, const arma::mat &B) {

            int k,l;
            double h11=0;
            double h12=0;
            double h22=0;

            int n1train = ",
        n1train,
        "; // DON'T FORGET TO CHANGE THIS
            int n2train = ",
        n2train,
        "; // DON'T FORGET TO CHANGE THIS
            int ntrain = n1train + n2train;

            double tmp = 0;

            for(k =0; k <n1train; k++){
                for(l=0; l<n1train; l++){
                    tmp = 0.5*(pow(A[k] - A[l],2) + pow(B[k] - B[l],2));
                    //h11 = h11 +  1 - exp(-tmp);
                    h11 = h11 +  sqrt(tmp);
                    //h11 = h11 +  log(1 + tmp);
                    //h11 = h11 +  tmp/(1 + tmp);
                }

                for(l=n1train; l<ntrain; l++){
                    tmp = 0.5*(pow(A[k] - A[l],2) + pow(B[k] - B[l],2));
                    //h12 = h12 + 1 - exp(-tmp);
                    h12 = h12 +  sqrt(tmp);
                    //h12 = h12 + log(1 + tmp);
                    //h12 = h12 + tmp/(1 + tmp);
                }
            }
            h11 = h11/(n1train * n1train);
            h12 = h12/(n1train * n2train);

            for(k =n1train; k <ntrain; k++){
                for(l=n1train; l<ntrain; l++){
                    tmp = 0.5*(pow(A[k] - A[l],2) + pow(B[k] - B[l],2));
                    //h22 = h22 +  1 - exp(-tmp);
                    h22 = h22 +  sqrt(tmp);
                    //h22 = h22 + log(1 + tmp);
                    //h22 = h22 + tmp/(1 + tmp);
                }
            }
            h22 = h22/(n2train * n2train);

            return h12 - 0.5*(h11 + h22);
            }",
        sep = ''
      ),
      glog = paste(
        "double pairEDist(const arma::mat &A, const arma::mat &B) {

            int k,l;
            double h11=0;
            double h12=0;
            double h22=0;

            int n1train = ",
        n1train,
        "; // DON'T FORGET TO CHANGE THIS
            int n2train = ",
        n2train,
        "; // DON'T FORGET TO CHANGE THIS
            int ntrain = n1train + n2train;

            double tmp = 0;

            for(k =0; k <n1train; k++){
                for(l=0; l<n1train; l++){
                    tmp = 0.5*(pow(A[k] - A[l],2) + pow(B[k] - B[l],2));
                    //h11 = h11 +  1 - exp(-tmp);
                    //h11 = h11 +  sqrt(tmp);
                    h11 = h11 +  log(1 + tmp);
                    //h11 = h11 +  tmp/(1 + tmp);
                }

                for(l=n1train; l<ntrain; l++){
                    tmp = 0.5*(pow(A[k] - A[l],2) + pow(B[k] - B[l],2));
                    //h12 = h12 + 1 - exp(-tmp);
                    //h12 = h12 +  sqrt(tmp);
                    h12 = h12 + log(1 + tmp);
                    //h12 = h12 + tmp/(1 + tmp);
                }
            }
            h11 = h11/(n1train * n1train);
            h12 = h12/(n1train * n2train);

            for(k =n1train; k <ntrain; k++){
                for(l=n1train; l<ntrain; l++){
                    tmp = 0.5*(pow(A[k] - A[l],2) + pow(B[k] - B[l],2));
                    //h22 = h22 +  1 - exp(-tmp);
                    //h22 = h22 +  sqrt(tmp);
                    h22 = h22 + log(1 + tmp);
                    //h22 = h22 + tmp/(1 + tmp);
                }
            }
            h22 = h22/(n2train * n2train);

            return h12 - 0.5*(h11 + h22);
            }",
        sep = ''
      ),
      grat = paste(
        "double pairEDist(const arma::mat &A, const arma::mat &B) {

            int k,l;
            double h11=0;
            double h12=0;
            double h22=0;

            int n1train = ",
        n1train,
        "; // DON'T FORGET TO CHANGE THIS
            int n2train = ",
        n2train,
        "; // DON'T FORGET TO CHANGE THIS
            int ntrain = n1train + n2train;

            double tmp = 0;

            for(k =0; k <n1train; k++){
                for(l=0; l<n1train; l++){
                    tmp = 0.5*(pow(A[k] - A[l],2) + pow(B[k] - B[l],2));
                    //h11 = h11 +  1 - exp(-tmp);
                    //h11 = h11 +  sqrt(tmp);
                    //h11 = h11 +  log(1 + tmp);
                    h11 = h11 +  tmp/(1 + tmp);
                }

                for(l=n1train; l<ntrain; l++){
                    tmp = 0.5*(pow(A[k] - A[l],2) + pow(B[k] - B[l],2));
                    //h12 = h12 + 1 - exp(-tmp);
                    //h12 = h12 +  sqrt(tmp);
                    //h12 = h12 + log(1 + tmp);
                    h12 = h12 + tmp/(1 + tmp);
                }
            }
            h11 = h11/(n1train * n1train);
            h12 = h12/(n1train * n2train);

            for(k =n1train; k <ntrain; k++){
                for(l=n1train; l<ntrain; l++){
                    tmp = 0.5*(pow(A[k] - A[l],2) + pow(B[k] - B[l],2));
                    //h22 = h22 +  1 - exp(-tmp);
                    //h22 = h22 +  sqrt(tmp);
                    //h22 = h22 + log(1 + tmp);
                    h22 = h22 + tmp/(1 + tmp);
                }
            }
            h22 = h22/(n2train * n2train);

            return h12 - 0.5*(h11 + h22);
            }",
        sep = ''
      )
    )
    pairEnergyFuncPtr <-
      cppXPtr(cstring,
              depends = c("RcppArmadillo"))
    
    ntrain = n1train + n2train
    
    pairE = parDist(t(train.set),
                    method = "custom",
                    func = pairEnergyFuncPtr,
                    threads = no.cores)
    
    pairE = pairE * n1train * n2train / ntrain
    
    return(pairE)
  }




###############################################################################
#a function that obtains m unique permutations of 1:size
rperm <-
  function(m, size) {
    # Obtain m unique permutations of 1:size
    
    # Function to obtain a new permutation.
    newperm <- function() {
      count <- 0                # Protects against infinite loops
      repeat {
        # Generate a permutation and check against previous ones.
        p <- sample(1:size)
        hash.p <- paste(p, collapse = "")
        if (is.null(cache[[hash.p]]))
          break
        
        # Prepare to try again.
        count <- count + 1
        if (count > 1000) {
          # 10000 is arbitrary; adjust to taste
          p <-
            NA           # NA indicates a new permutation wasn't found
          hash.p <- ""
          break
        }
      }
      cache[[hash.p]] <<-
        TRUE  # Update the list of permutations found
      p                         # Return this (new) permutation
    }
    
    # Obtain m unique permutations.
    cache <- list()
    replicate(m, newperm())
  } # Returns a `size` by `m` matrix; each column is a permutation of 1:size.
###############################################################################