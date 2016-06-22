#Robert Dinterman, NCSU Economics PhD Student


# Load Data ---------------------------------------------------------------

load("1-data.RData")

keep <- data$FIPS %in% as.numeric(row.names(W))
data <- data[keep,]

# Employees ---------------------------------------------------------------

NAICS.2008l <- grepl("employA", names(data)) & grepl("2008", names(data))
NAICS.2008  <- names(data[NAICS.2008l])

NAICS.2010l <- grepl("employA", names(data)) & grepl("2010", names(data))
NAICS.2010  <- names(data[NAICS.2010l])

source("4-naics_2eq.R")

alpha1      <- mapply(function(x, y) naics_2eq_alpha_new(x, y, data = data, W = W),
                      x = NAICS.2008, y = NAICS.2010, SIMPLIFY = F)

beta1       <- mapply(function(x, y) naics_2eq_beta_new(x, y, data = data, W = W),
                      x = NAICS.2008, y = NAICS.2010, SIMPLIFY = F)

dalpha1      <- data.frame(t(sapply(alpha1, c)))
dbeta1       <- data.frame(t(sapply(beta1, c)))

# Establishments ----------------------------------------------------------

NAICS.2008l <- grepl("establishmentsA", names(data)) & grepl("2008", names(data))
NAICS.2008  <- names(data[NAICS.2008l])

NAICS.2010l <- grepl("establishmentsA", names(data)) & grepl("2010", names(data))
NAICS.2010  <- names(data[NAICS.2010l])

alpha2      <- mapply(function(x, y) naics_2eq_alpha_new(x, y, data = data, W = W),
                      x = NAICS.2008, y = NAICS.2010, SIMPLIFY = F)

beta2       <- mapply(function(x, y) naics_2eq_beta_new(x, y, data = data, W = W),
                      x = NAICS.2008, y = NAICS.2010, SIMPLIFY = F)

dalpha2      <- data.frame(t(sapply(alpha2, c)))
dbeta2       <- data.frame(t(sapply(beta2, c)))


# Tables ------------------------------------------------------------------

alpha <- cbind(dalpha1, dalpha2)
beta  <- cbind(dbeta1, dbeta2)

library(xtable)
xtable(alpha)
xtable(beta)