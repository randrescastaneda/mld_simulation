new_mld <- function(welfare, weight,  sh, sc, rp) {

  ln <- length(welfare)
  wf <- welfare
  wf <- wf*sc

  if (rp == "ignore") {

    wf[1:round(ln*sh)] <- 0


  } else if (rp == "1") {

    wf[1:round(ln*sh)] <- 1


  } else if (rp == "min") {

    wf_min <- fmin(wf[wf>0])

    wf[1:round(ln*sh)] <- wf_min

  } else if (rp == "shr") {

    wf_min1 <- fmin(wf[wf>0])
    wf_min2 <- fmin(wf[wf>wf_min1])

    wf[1:round(ln*sh)] <- wf_min1*(wf_min1 / wf_min2)

  } else {
    stop("replace not valid")
  }


  mld <- fmld(wf, weight = weight)
  dt <- data.table(share   = sh,
                   scale   = sc,
                   replace = rp,
                   mld     = mld)
  return(dt)

}

fmld <- function (welfare, weight) {
  weight  <- weight[welfare > 0]
  welfare <- welfare[welfare > 0]

  mean_welfare <- collapse::fmean(x = welfare, w = weight)
  deviation <- log(mean_welfare/welfare)
  mld <- collapse::fmean(x = deviation, w = weight)
  return(mld)
}




