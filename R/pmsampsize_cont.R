pmsampsize_cont <- function(rsquared,parameters,intercept,sd,shrinkage,mmoe) {

  r2a   <- rsquared
  n     <- parameters + 2
  n1    <- parameters + 2


  # criteria 1
  es    <-  1 + ((parameters-2)/(n1*(log(1-((r2a*(n1-parameters-1))+parameters)/(n1-1)))))
  if (es > shrinkage) {
    shrinkage_1 <- round(es,digits=3)
    n1          <- n1
    spp_1       <- n1/parameters
    SPP_1       <- round(spp_1,digits=2)
  } else {
    while (es < shrinkage) {
      n1 <- n1 + 1
      es <- 1 + ((parameters-2)/(n1*(log(1-((r2a*(n1-parameters-1))+parameters)/(n1-1)))))
      if (!is.na(es) & es >= shrinkage) {
        shrinkage_1 <- round(es,digits=3)
        n1          <- n1
        spp_1       <- n1/parameters
        SPP_1       <- round(spp_1,digits=2)
      }
    }
  }



  # criteria 2 - small absolute difference in r-sq adj & r-sq app
  n2          <- ceiling(1+((parameters*(1-r2a))/0.05))
  shrinkage_2 <- 1 + ((parameters-2)/(n2*(log(1-((r2a*(n2-parameters-1))+parameters)/(n2-1)))))
  shrinkage_2 <- round(shrinkage_2,digits=3)
  spp_2       <- n2/parameters
  SPP_2       <- round(spp_2,digits=2)


  # criteria 3 - precise estimate of residual variance
  n3      <- 234 + parameters

  shrinkage_3 <- 1 + ((parameters-2)/(n3*(log(1-((r2a*(n3-parameters-1))+parameters)/(n3-1)))))
  shrinkage_3 <- round(shrinkage_3,digits=3)
  spp_3 <- n3 / parameters
  SPP_3 <- round(spp_3,digits=2)

    # criteria 4 - precise estimation of intercept
  n4    <- max(n1,n2,n3)
  df    <- n4 - parameters - 1
  uci   <- intercept+((((sd^2*(1-r2a))/n4)^.5)*(-stats::qt(0.025,df)))
  lci   <- intercept-((((sd^2*(1-r2a))/n4)^.5)*(-stats::qt(0.025,df)))
  int_mmoe <- uci / intercept
  if (int_mmoe > mmoe) {
    while (int_mmoe > mmoe) {
      n4 <- n4 + 1
      df <- n4 - parameters - 1
      uci <- intercept + ((((sd^2*(1-r2a))/n4)^.5)*(stats::qt(0.025,df)))
      lci <- intercept - ((((sd^2*(1-r2a))/n4)^.5)*(stats::qt(0.025,df)))
      int_mmoe <- uci / intercept
        if (int_mmoe <= mmoe) {
          shrinkage_4 <- 1 + ((parameters-2)/(n4*(log(1-((r2a*(n4-parameters-1))+parameters)/(n4-1)))))
          shrinkage_4 <- round(shrinkage_4,digits=3)
          spp_4 <- n4 / parameters
          SPP_4 <- round(spp_4,digits=2)
          int_uci <- round(uci,digits=2)
          int_lci <- round(lci,digits=2)
        }
      }
    } else {
      shrinkage_4 <- 1 + ((parameters-2)/(n4*(log(1-((r2a*(n4-parameters-1))+parameters)/(n4-1)))))
      shrinkage_4 <- round(shrinkage_4,digits=3)
      spp_4 <- n4 / parameters
      SPP_4 <- round(spp_4,digits=2)
      int_uci <- round(uci,digits=2)
      int_lci <- round(lci,digits=2)
    }


  # minimum n
  nfinal <- max(n1,n2,n3,n4)
  spp_final <- nfinal / parameters
  SPP_final <- round(spp_final,digits=2)
  shrinkage_final <- 1 + ((parameters-2)/(nfinal*(log(1-((r2a*(nfinal-parameters-1))+parameters)/(nfinal-1)))))
  shrinkage_final <- round(shrinkage_final,digits=3)


  # create output table
  res <- matrix(NA,5,5)
  colnames(res) <- c("Samp_size","Shrinkage","Parameter","Rsq","SPP")
  rownames(res) <- c("Criteria 1","Criteria 2","Criteria 3","Criteria 4*","Final")
  res[,1] <- c(n1,n2,n3,n4,nfinal)
  res[,2] <- c(shrinkage_1,shrinkage_2,shrinkage_3,shrinkage_4,shrinkage_final)
  res[,3] <- parameters
  res[,4] <- rsquared
  res[,5] <- c(SPP_1,SPP_2,SPP_3,SPP_4,SPP_final)

  out <- list(results_table = res,
              final_shrinkage = shrinkage_final,
              sample_size = nfinal,
              parameters = parameters,
              r2a = r2a,
              SPP = SPP_final,
              int_mmoe = int_mmoe,
              intercept = intercept,
              int_uci = int_uci,
              int_lci = int_lci,
              type = "continuous")

}
