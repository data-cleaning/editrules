library(editrules)

#
E_set <- editset(c(
  "local >= 1",
  "local <= 10000",
  "nace %in% c('461', '462')",
  "turnover < -1",                                            # contradictionary range check
  "turnover > 1e6",                    # contradictionary range check
  "pers >= 1",
  "pers <= 99999",
  "pers >= local",
  "if (nace == '461') pers <= 3 * local",
  "if (nace == '461') turnover <= 10000 * pers",
  "turnover <= 1e6 * pers"))
is.editset(E_set)
E_set
summary(E_set)
#
isFeasible(E_set, warn = TRUE)
#
rawdata <- data.frame(
  id = c("1","2"),
  nace = c("461", "462"),
  local = c(1,2),
  pers = c(100001,100001),
  turnover = c(1e6 + 1, 1e6 + 1)
)
rawdata <- rawdata[,-1]
rawdata
#
recwghts <- data.frame(
  id = c("1","2"),
  nace = c(1000,1000),
  local = c(90,90),
  pers = c(10,10),
  turnover = c(95,95)
)
recwghts <- as.matrix(recwghts[,-1])
recwghts
is.matrix(recwghts)
#
#
localizeErrors(E_set,rawdata,weight=rep(1, ncol(rawdata)))

#
localizeErrors(E_set,rawdata,weight=recwghts, method = "mip")
errorLocalizer.mip(E_set, rawdata[1,], weight=recwghts[1,])
#
