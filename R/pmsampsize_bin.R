pmsampsize_bin <- function(rsquared,parameters,prevalence,shrinkage,cstatistic) {

  r2a <- rsquared
  n1 <- n2 <- n3 <- parameters

  if (shrinkage < r2a) {
    error_msg <- paste0("User specified shrinkage is lower than R-squared adjusted. Error in log(1 - (r2a/shrinkage)) : NaNs produced")
    stop(error_msg)
  }

  # criteria 1 - shrinkage
  n1 <- ceiling((parameters/((shrinkage-1)*(log(1-(r2a/shrinkage))))))
  shrinkage_1 <- shrinkage
  E1 <- n1*prevalence
  epp1 <- E1/parameters
  EPP_1 <- round(epp1,digits=2)

  # criteria 2 - small absolute difference in r-sq adj
  lnLnull <- (E1*(log(E1/n1)))+((n1-E1)*(log(1-(E1/n1))))
  max_r2a <- (1- exp((2*lnLnull)/n1))
  nag_r2 <- r2a/max_r2a

  if (max_r2a < r2a) {
    error_msg <- paste0("User specified R-squared adjusted is larger than the maximum possible R-squared (", max_r2a, ") as defined by equation 23 (Riley et al. 2018)")
    stop(error_msg)
  }

  s_4_small_diff <- (r2a/(r2a+(0.05*max_r2a)))

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
  res <- matrix(NA,4,7)
  colnames(res) <- c("Samp_size","Shrinkage","Parameter","CS_Rsq","Max_Rsq","Nag_Rsq", "EPP")
  rownames(res) <- c("Criteria 1","Criteria 2","Criteria 3","Final")
  res[,1] <- c(n1,n2,n3,nfinal)
  res[,2] <- round(c(shrinkage_1,shrinkage_2,shrinkage_3,shrinkage_final),digits = 3)
  res[,3] <- parameters
  res[,4] <- rsquared
  res[,5] <- round(max_r2a, digits = 3)
  res[,6] <- round(nag_r2, digits = 3)
  res[,7] <- c(EPP_1,EPP_2,EPP_3,EPP_final)

  out <- list(results_table = res,
              final_shrinkage = shrinkage_final,
              sample_size = nfinal,
              parameters = parameters,
              rsquared = r2a,
              max_r2a = max_r2a,
              nag_r2 = nag_r2,
              events = E_final,
              EPP = EPP_final,
              prevalence = prevalence,
              type = "binary",
              cstatistic = cstatistic)


}
