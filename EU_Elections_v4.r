library(electoral)
library(dplyr)
library(reshape2)

roundUp <- function(x, to = 10000) {
  to * (x%/%to + as.logical(x%%to))
}

dta <- read.csv(file = "C:\\Users\\user\\Documents\\R\\EU\\EU Elections_YouGov_April28_exc_WlesScot.csv",
  header = TRUE, sep = ",", row.names = 1)
swingdata <- read.csv(file = "C:\\Users\\user\\Documents\\R\\EU\\swing_data_exc_WlsScot.csv", header = TRUE,
  sep = ",", row.names = 1)

moddata <- dta[-NROW(dta), ]
regns <- colnames(dta)
parts <- head(rownames(dta), -1)
numregn <- length(regns)
numparts <- length(parts)

# Loop below matches swingdata column names with dta row names to get dta row number for raw vote for
# swinging party
swparts <- rep(NA, NCOL(swingdata))
for (i in 1:NCOL(swingdata)) {
  colnme <- colnames(swingdata)[i]
  if (length(which(colnme == rownames(dta))) == 0) {
    print("Error! Column Name/ Row Name Mismatch", i)
    browser()
  } else {
    swparts[i] <- which(colnme == rownames(dta))
  }
}

# Edit next variable rmnparts variable to produce results for individual swings to rather than all. 2
# for just CHUK, 4 for just Greens, 6 for just LibDems, 7 for just SNPPLD, or any other combination
# desired rmnparts <- c(2, 4, 6, 7)
rmnparts <- 2
rmparts2 <- c(2, 4, 6)


output <- data.frame(matrix(nrow = numparts, ncol = numregn, 0))
rownames(output) <- head(rownames(dta), -1)
colnames(output) <- colnames(dta)

modoutput <- data.frame(matrix(nrow = numparts, ncol = numregn, 0))
rownames(modoutput) <- head(rownames(dta), -1)
colnames(modoutput) <- colnames(dta)

for (i in 1:numregn) {
  output[, i] <- seats_ha(parties = parts, votes = dta[-NROW(dta), i], n_seats = dta[NROW(dta), i],
    method = "dhondt")
}


tmpdta <- dta
prevdta <- matrix(nrow = NROW(dta), ncol = NCOL(dta))
for (i in 1:numregn) {
  n_seats <- dta[NROW(dta), i]

  mx <- vector(length = n_seats)
  for (j in 1:n_seats) {
    mx[j] <- which(tmpdta[, i] == max(tmpdta[, i]))
    modoutput[mx[j], i] <- modoutput[mx[j], i] + 1

    prevdta[mx[j], i] <- tmpdta[mx[j], i]

    tmpdta[mx[j], i] <- dta[mx[j], i]/(modoutput[mx[j], i] + 1)

  }

  maxrmnidx <- which(tmpdta[rmparts2, i] == max(tmpdta[rmparts2, i]))
  maxrmnvtes <- tmpdta[rmparts2[maxrmnidx], i]

  if (mx[j] %in% rmparts2) {
    lstwin <- prevdta[mx[j - 1], i]
    mgn <- round(lstwin - maxrmnvtes + 1)

    cat(colnames(dta)[i], " ", rownames(dta)[rmparts2[maxrmnidx]], " require ", roundUp(mgn), " votes to take extra seat off ",
      rownames(dta)[mx[j - 1]], "\n")
  } else {

    lstwin <- prevdta[mx[j], i]
    mgn <- round(lstwin - maxrmnvtes + 1)

    cat(colnames(dta)[i], " ", rownames(dta)[rmparts2[maxrmnidx]], " require ", roundUp(mgn), " votes to take extra seat off ",
      rownames(dta)[mx[j]], "\n")
  }
}

