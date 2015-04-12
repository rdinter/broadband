#Robert Dinterman, NCSU Economics PhD Student


# Load Data ---------------------------------------------------------------

load("1-data.RData")

keep <- data$FIPS %in% as.numeric(row.names(W))
data <- data[keep,]

NAICS.2008l <- grepl("employA", names(data)) & grepl("2008", names(data))
NAICS.2008  <- names(data[NAICS.2008l])

NAICS.2010l <- grepl("employA", names(data)) & grepl("2010", names(data))
NAICS.2010  <- names(data[NAICS.2010l])


source("4-naics_2eq.R")

alpha      <- mapply(function(x, y) naics_2eq_alpha(x, y, data = data, W = W),
                     x = NAICS.2008, y = NAICS.2010, SIMPLIFY = F)

beta       <- mapply(function(x, y) naics_2eq_beta(x, y, data = data, W = W),
                     x = NAICS.2008, y = NAICS.2010, SIMPLIFY = F)

dalpha      <- data.frame(t(sapply(alpha, c)))
dbeta       <- data.frame(t(sapply(beta, c)))
regressions <- cbind(dalpha, dbeta)

library(xtable)
xtable(regressions)