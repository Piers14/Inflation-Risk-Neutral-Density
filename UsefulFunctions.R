ReadOptions1Y <- function(fileName){
  inf1 <- read.csv(fileName, colClasses = c("character", rep("numeric", 21)), header = FALSE)
  strikes <- as.numeric(inf1[1,2:ncol(inf1)])
  inf1 <- inf1[2:nrow(inf1),]
  colnames(inf1) <- c("Date", as.character(strikes))
  rownames(inf1) <- as.character(as.numeric(rownames(inf1))-1)
  
  inf1$Date <- as.Date(inf1$Date, format = "%d/%m/%Y")
  inf1$Date <- as.character(inf1$Date)
  return(inf1)
}

ReadOptions <- function(fileNames){
  n <- length(fileNames)
  test <- lapply(fileNames, read.csv, colClasses = c("character", rep("numeric", 21)), header = FALSE)
  strikes <- as.numeric(test[[1]][1,2:ncol(test[[1]])])
  for(i in 1:n){
    test[[i]] <- test[[i]][2:nrow(test[[i]]), ]
    rownames(test[[i]]) <- as.character(as.numeric(rownames(test[[i]]))-1)
  }
  colNames <- c("Date", as.character(strikes))
  test <- lapply(test, setNames, colNames)
  for(i in 1:n){
    test[[i]]$Date <- as.Date(test[[i]]$Date, format = "%d/%m/%Y")
    test[[i]]$Date <- as.character(test[[i]]$Date)
  }
  return(test)
}
ReadOIS <- function(fileName){
  OISData <- read.csv(fileName, colClasses = c("character", rep("numeric", 30)), header = FALSE)[, 1:31]
  OISYears <- as.numeric(OISData[1,2:ncol(OISData)])
  OISData <- OISData[2:nrow(OISData),]
  colnames(OISData) <- c("Date", as.character(OISYears))
  rownames(OISData) <- as.character(as.numeric(rownames(OISData))-1)
  OISData$Date <- as.Date(OISData$Date, format = "%d/%m/%Y")
  OISData$Date <- as.character(OISData$Date)
  return(OISData)
}

# Reads OIS converts into matrix on bonds up to 10 year maturity
Get10YBonds <- function(fileName){
  OISData <- ReadOIS(fileName)
  Bonds <- matrix(NA, nrow = nrow(OISData), ncol = 10)
  for(i in 1:nrow(OISData)){
    for(j in 2:11){
      Bonds[i, (j-1)] <- 1 / ((j-1) * OISData[i, j] / 100 + 1)
    }
  }
  return(Bonds)
}

GetSWIL <- function(fileName){
  SWILData <- read.csv(fileName, colClasses = c("character", rep("numeric", 15)), header = FALSE)[, 1:16]
  SWILYears <- as.numeric(SWILData[1,2:ncol(SWILData)])
  SWILData <- SWILData[2:nrow(SWILData),]
  colnames(SWILData) <- c("Date", as.character(SWILYears))
  rownames(SWILData) <- as.character(as.numeric(rownames(SWILData))-1)
  SWILData$Date <- as.Date(SWILData$Date, format = "%d/%m/%Y")
  SWILData$Date <- as.character(SWILData$Date)
  return(SWILData)
}

BS <- function(x, K, sig, r, T){
  d1 <- ( log(x/K) + (r+0.5*sig^2)*T ) / (sig*sqrt(T))
  d2 <- d1 - sig * sqrt(T)
  price <- x * pnorm(d1) - K * exp(-r*T) * pnorm(d2)
  price
}

ImpVol <- function(val, x, K, r, T, initGuess){
  sig_old <- initGuess
  sig_new <- 0
  iterate <- TRUE
  counter <- 0
  while(iterate){
    d1 <- ( log(x/K) + (r+0.5*sig_old^2)*T ) / (sig_old*sqrt(T))
    vega <- x * dnorm(d1) * sqrt(T)
    sig_new = sig_old - (BS(x, K, sig_old, r, T) - val) / vega
    counter = counter + 1
    if(abs(sig_new - sig_old) < 10e-9){
      iterate = FALSE
    }
    if(counter >= 100){
      iterate = FALSE
      sig_new = NA
    }
    sig_old = sig_new
  }
  sig_new
}
FloorIndices <- function(k){
  if(k < -0.005){
    return(c(1,2,3))
  } else if(k < 0){
    return(c(1,2,3,4))
  } else if(k < 0.005){
    return(c(1,2,3,4,5))
  } else if(k < 0.01){
    return(c(1,2,3,4,5,6))
  } else if(k < 0.015){
    return(c(1,2,3,4,5,6,7))
  } else if(k < 0.02){
    return(c(1,2,3,4,5,6,7,8))
  } else if(k < 0.025){
    return(c(1,2,3,4,5,6,7,8,9))
  } else{
    return(c(1,2,3,4,5,6,7,8,9,10))
  }
}

CapIndices <- function(k){
  if(k < -0.005){
    return(seq(1,10,1))
  } else if(k < 0){
    return(seq(1,10,1))
  } else if(k < 0.005){
    return(seq(1,10,1))
  } else if(k < 0.01){
    return(seq(1,10,1))
  } else if(k < 0.015){
    return(seq(2,10,1))
  } else if(k < 0.02){
    return(seq(3,10,1))
  } else if(k < 0.025){
    return(seq(4,10,1))
  } else{
    return(seq(5,10,1))
  }
}

areaOfTriangle <- function(x){
  x1 <- x[1,]
  x2 <- x[2,]
  x3 <- x[3,]
  areaSum <- x1[1] * (x3[2] - x2[2]) + x2[1] * (x1[2] - x3[2]) + x3[1] * (x2[2] - x1[2])
  return(areaSum)
}
convexTest <- function(x){
  if(areaOfTriangle(x) < 0){
    return(TRUE)
  } else {
    return(FALSE)
  }
}
convexCall <- function(combCap){
  combCap <- combCap[!rowSums(!is.finite(combCap)),]
  output <- TRUE
  k <- nrow(combCap)
  for(i in 1:(k-2)){
    x <- combCap[i:(i+2), ]
    if(!convexTest(x)){
      output <- FALSE
    }
  }
  return(output)
}

RemoveConvex <- function(combCap){
  k <- nrow(combCap)
  inds <- numeric()
  for(i in 1:(k-2)){
    x <- combCap[i:(i+2), ]
    if(!convexTest(x)){
      inds <- c(inds, i+1)
    }
  }
  if(length(inds) > 0){
    return(combCap[-inds, ])
  } else {
    return(combCap)
  }
}


convexConditionTransformed <- function(K, v, fPart, sPart, tau, F_price){
  v_imp <- sqrt(log(v))
  d1 <- (log(F_price/K) + 0.5*v_imp^2 * tau) / (v_imp * sqrt(tau))
  d2 <- d1 - v_imp * sqrt(tau)
  
  x1 <- dnorm(d2) / (K * sqrt(log(v)) * sqrt(tau))
  x2 <- (d1 * dnorm(d2) * fPart) / (v * log(v))
  x3 <- (K * dnorm(d2) * sqrt(tau) * (d1 * d2 - 2*log(v) - 1) * fPart^2) / (4 * v^2 * (log(v))^1.5)
  x4 <- K * dnorm(d2) * sqrt(tau) * sPart / (2 * v * sqrt(log(v)))
  
  output <- (x1 + x2 + x3 + x4) 
  return(output)
}

DensityPlot <- function(options, SWIL, Bonds, date_in, nCap, nFlo, mats, currentMats){
  nMats <- length(options)
  k <- 1
  allDensities <- list()
  for(i in mats){
  CapList <- as.matrix(options[[k]][ ,2:(nCap+1)])
  FloList <- as.matrix(options[[k]][ ,(nCap+2):(nCap+1+nFlo)])
  i15Apr <- which(options[[k]]$Date == date_in)
  # Getting cap strikes and floor strikes
  capStrikes <- as.numeric(colnames(CapList))
  floStrikes <- as.numeric(colnames(FloList))
  
  # Volatility surface matrix
  h <- 0.0001
  OverallDensity <- matrix(NA, 10, 3001)
  
  kswap <- SWIL[i15Apr, i+1]/100
  x <- CapList[i15Apr, ][CapIndices(kswap)]
  y <- FloList[i15Apr,][FloorIndices(kswap)]
  n <- length(y)
  if(n==0){y <- numeric()}
  
  # Put-Call parity
  newCap <- array(NA, n)
  for(j in 1:n){
    newCap[j] <- (y[j] / 10000 + (Bonds[i15Apr, i] * (1 + SWIL[i15Apr, i+1]/100)^i) - (1+floStrikes[j])^i * Bonds[i15Apr, i]) * 10000
  }
  testStrikes <- c(floStrikes[FloorIndices(kswap)], capStrikes[CapIndices(kswap)])
  combCap <- cbind(testStrikes, c(newCap, x))
  colnames(combCap) <- rownames(combCap) <- NULL
  combCap <- combCap[which(combCap[,2] != 0),]
  combCap <- combCap[!is.na(combCap[,2]), ]
  
  combCap <- RemoveConvex(combCap)
  combCapVols <- array(NA, nrow(combCap))
  for(j in 1:nrow(combCap)){
    skip_to_next <- FALSE
    tryCatch(ImpVol(combCap[j, 2]/(Bonds[i15Apr, i]*10000),(1 + SWIL[i15Apr, i+1]/100)^i,
                    (1+combCap[j, 1])^i, 0,
                    i, 0.1), 
             error=function(e){skip_to_next <<- TRUE})
    if(skip_to_next){next}
    else{
      combCapVols[j] <- ImpVol(combCap[j, 2]/(Bonds[i15Apr, i]*10000),(1 + SWIL[i15Apr, i+1]/100)^i,
                               (1+combCap[j, 1])^i, 0,
                               i, 0.1)
    }
  }
  # Fitting Spline
  capMoneyness <- (combCap[,1] + 1)^i
  expCapVol <- exp(combCapVols^2)
  capMoneyness <- capMoneyness[!is.na(expCapVol)]
  expCapVol <- expCapVol[!is.na(expCapVol)]
  
  CapVolSpline_mness <- smooth.spline(capMoneyness, expCapVol, spar = 0.5)
  smoothCapVolSpline_mness <- predict(CapVolSpline_mness, seq(0.9, 1.2, 0.0001))
  
  
  # Calculating density
  K <- smoothCapVolSpline_mness$x
  sigma_tilde <- smoothCapVolSpline_mness$y
  fpart_tilde <- predict(CapVolSpline_mness, seq(0.9, 1.2, 0.0001), deriv = 1)$y
  spart_tilde <- predict(CapVolSpline_mness, seq(0.9, 1.2, 0.0001), deriv = 2)$y
  predPDF <- array(NA, length(K))
  for(j in 1:length(fpart_tilde)){
    predPDF[j] <- convexConditionTransformed(K[j], sigma_tilde[j], fpart_tilde[j], spart_tilde[j], i, (1+SWIL[i15Apr, i+1]/100)^i)
  }
  predPDF <- cbind(K, predPDF)
  
  
  oneYearPDF <- predPDF
  oneYearPDF[,1] <- (predPDF[,1])^(1/i) - 1
  oneYearPDF[,2] <- predPDF[,2] * i * (predPDF[,1])^((i-1)/i)
  
  allDensities[[k]] <- oneYearPDF[oneYearPDF[,2] > 0.1, ]
  k = k + 1
  }
  ncMats <- length(currentMats)
  inds <- array(NA, length(currentMats))
  for(i in 1:ncMats){
    inds[i] = which(mats == currentMats[i])
  }
  min_x <- 10
  max_x <- -10
  max_y <- 0
  for(i in inds){
    min_x <- min(min_x, allDensities[[i]][,1])
    max_x <- max(max_x, allDensities[[i]][,1])
    max_y <- max(max_y, allDensities[[i]][,2])
  }
  plot(allDensities[[inds[1]]], typ = "l", col = inds[1], lwd = 2, xlim = c(min_x, max_x), ylim = c(0, max_y), 
       xlab = "Inflation rate", ylab = "Density")
  abline(h = 0, col = "darkgrey", lwd = 1.1)
  abline(v = 0, col = "darkgrey", lwd = 1.1)
  zgrid = seq(-0.10, 0.12, 0.01)
  for(z in zgrid){
    abline(v = z, col = "darkgrey", lty = 2)
  }
  if(ncMats > 1){
    for(i in inds[-1]){
      lines(allDensities[[i]], col = i, lwd = 2)
    }
  }
  yearNames <- array(NA, ncMats)
  for(i in 1:ncMats){
    yearNames[i] <- paste(currentMats[i], "Y")
  }
  legend(max_x-0.018, max_y, legend = yearNames, col = inds, lwd = 2)
}

DensityWrite <- function(options, SWIL, Bonds, date_in, nCap, nFlo, mats, currentMats){
  nMats <- length(options)
  k <- 1
  allDensities <- list()
  for(i in mats){
    CapList <- as.matrix(options[[k]][ ,2:(nCap+1)])
    FloList <- as.matrix(options[[k]][ ,(nCap+2):(nCap+1+nFlo)])
    i15Apr <- which(options[[k]]$Date == date_in)
    # Getting cap strikes and floor strikes
    capStrikes <- as.numeric(colnames(CapList))
    floStrikes <- as.numeric(colnames(FloList))
    
    # Volatility surface matrix
    h <- 0.0001
    OverallDensity <- matrix(NA, 10, 3001)
    
    kswap <- SWIL[i15Apr, i+1]/100
    x <- CapList[i15Apr, ][CapIndices(kswap)]
    y <- FloList[i15Apr,][FloorIndices(kswap)]
    n <- length(y)
    if(n==0){y <- numeric()}
    
    # Put-Call parity
    newCap <- array(NA, n)
    for(j in 1:n){
      newCap[j] <- (y[j] / 10000 + (Bonds[i15Apr, i] * (1 + SWIL[i15Apr, i+1]/100)^i) - (1+floStrikes[j])^i * Bonds[i15Apr, i]) * 10000
    }
    testStrikes <- c(floStrikes[FloorIndices(kswap)], capStrikes[CapIndices(kswap)])
    combCap <- cbind(testStrikes, c(newCap, x))
    colnames(combCap) <- rownames(combCap) <- NULL
    combCap <- combCap[which(combCap[,2] != 0),]
    combCap <- combCap[!is.na(combCap[,2]), ]
    
    combCap <- RemoveConvex(combCap)
    combCapVols <- array(NA, nrow(combCap))
    for(j in 1:nrow(combCap)){
      skip_to_next <- FALSE
      tryCatch(ImpVol(combCap[j, 2]/(Bonds[i15Apr, i]*10000),(1 + SWIL[i15Apr, i+1]/100)^i,
                      (1+combCap[j, 1])^i, 0,
                      i, 0.1), 
               error=function(e){skip_to_next <<- TRUE})
      if(skip_to_next){next}
      else{
        combCapVols[j] <- ImpVol(combCap[j, 2]/(Bonds[i15Apr, i]*10000),(1 + SWIL[i15Apr, i+1]/100)^i,
                                 (1+combCap[j, 1])^i, 0,
                                 i, 0.1)
      }
    }
    # Fitting Spline
    capMoneyness <- (combCap[,1] + 1)^i
    expCapVol <- exp(combCapVols^2)
    capMoneyness <- capMoneyness[!is.na(expCapVol)]
    expCapVol <- expCapVol[!is.na(expCapVol)]
    
    CapVolSpline_mness <- smooth.spline(capMoneyness, expCapVol, spar = 0.5)
    smoothCapVolSpline_mness <- predict(CapVolSpline_mness, seq(0.9, 1.2, 0.0001))
    
    
    # Calculating density
    K <- smoothCapVolSpline_mness$x
    sigma_tilde <- smoothCapVolSpline_mness$y
    fpart_tilde <- predict(CapVolSpline_mness, seq(0.9, 1.2, 0.0001), deriv = 1)$y
    spart_tilde <- predict(CapVolSpline_mness, seq(0.9, 1.2, 0.0001), deriv = 2)$y
    predPDF <- array(NA, length(K))
    for(j in 1:length(fpart_tilde)){
      predPDF[j] <- convexConditionTransformed(K[j], sigma_tilde[j], fpart_tilde[j], spart_tilde[j], i, (1+SWIL[i15Apr, i+1]/100)^i)
    }
    predPDF <- cbind(K, predPDF)
    
    
    oneYearPDF <- predPDF
    oneYearPDF[,1] <- (predPDF[,1])^(1/i) - 1
    oneYearPDF[,2] <- predPDF[,2] * i * (predPDF[,1])^((i-1)/i)
    
    allDensities[[k]] <- oneYearPDF[oneYearPDF[,2] > 0.1, ]
    k = k + 1
  }
  return(allDensities)
}








intro1 <- function(){
  "This app estimates the \\( T \\)-forward density of the inflation rate from quoted zero-coupon inflation cap and floor prices.
  An example dataset is pre-loaded which enables the calculation of the denisty across multiple dates for a range of maturities. However, 
  you can choose to upload new data. This can be done on the Data Input tab. Please read the data formatting section below if you choose to do this. 
  The \"Update Data\" button will read in the user uploaded data or reset to the example data. Plots of the density can be seen in the Plot tab. 
  You can select the date and maturity and download the computed density."
}

intro2 <- function(){
  "User uploaded data must be a .csv file with the same format as the example dataset. There must also be of notional amount of 10e4. To see the required format, you can download the example dataset. 
  Note that this dataset is not real data, and is used only as an example."
}

intro4 <- function(){
  "Multiple .csv files containing option prices can be uploaded, with each file corresponding to a different maturity."
}

intro3 <- function(){
  "The example dataset contains option prices from 11/12/2009 to 18/02/2012 for 1-year and 3-year maturities. 
  If \"missing or infinites values\" error, it is because the data is synthetically generated and does not always satisfy no-arbitrage conditions. 
  Try another date instead."
}

















