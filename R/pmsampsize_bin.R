pmsampsize_bin <- function(rsquared,parameters,prevalence,shrinkage) {
  
  r2a <- rsquared
  n1 <- n2 <- n3 <- parameters
  
  # criteria 1 - shrinkage
  n1 <- ceiling((parameters/((shrinkage-1)*(log(1-(r2a/shrinkage))))))
  shrinkage_1 <- shrinkage
  E1 <- n1*prevalence
  epp1 <- E1/parameters
  EPP_1 <- round(epp1,digits=2)
  
  # criteria 2 - small absolute difference in r-sq adj
  lnLnull <- (E1*(log(E1/n1)))+((n1-E1)*(log(1-(E1/n1))))
  max_r2a <- round((1- exp((2*lnLnull)/n1)),digits=2)
  
  if (max_r2a < r2a) {
    error_msg <- paste0("User specified R-squared adjusted is larger than the maximum possible R-squared (", max_r2a, ") as defined by equation 23 (Riley et al. 2018)")
    stop(error_msg)
  }
  
  s_4_small_diff <- round((r2a/(r2a+(0.05*max_r2a))),digits=3)
  
  n2 <- ceiling((parameters/((s_4_small_diff-1)*(log(1-(r2a/s_4_small_diff))))))
  shrinkage_2 <- s_4_small_diff
  
  E2 <- n2*prevalence
  epp2 <- E2/parameters
  EPP_2 <- round(epp2,digits=2)
  
  
  # criteria 3 - precise estimation of the intercept
  n3 <- ceiling((((1.96/0.05)^2)*(prevalence*(1-prevalence))))
  
  E3 <- n3*prevalence 
  epp3 <- E3/parameters
  EPP_3 <- round(epp3,digits=2)
  
  if (shrinkage_2 > shrinkage) {
      shrinkage_3 <- shrinkage_2
      } else {
        shrinkage_3 <- shrinkage
        }
  
  # minimum n 
  nfinal <- max(n1,n2,n3)
  shrinkage_final <- shrinkage_3
  E_final <- nfinal*prevalence
  epp_final <- E_final/parameters
  EPP_final <- round(epp_final,digits=2)
  
  # create output table 
  res <- matrix(NA,4,6)
  colnames(res) <- c("Samp_size","Shrinkage","Parameter","Rsq","Max_Rsq", "EPP")
  rownames(res) <- c("Criteria 1","Criteria 2","Criteria 3","Final")
  res[,1] <- c(n1,n2,n3,nfinal)
  res[,2] <- c(shrinkage_1,shrinkage_2,shrinkage_3,shrinkage_final)
  res[,3] <- parameters
  res[,4] <- rsquared
  res[,5] <- max_r2a
  res[,6] <- c(EPP_1,EPP_2,EPP_3,EPP_final)
  
  out <- list(results_table = res,
              final_shrinkage = shrinkage_final,
              sample_size = nfinal,
              parameters = parameters,
              r2a = r2a,
              max_r2a = max_r2a,
              events = E_final,
              EPP = EPP_final,
              prevalence = prevalence,
              type = "binary")
  
  
}