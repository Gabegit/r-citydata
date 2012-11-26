library(XLConnect)

setwd("E:/data/1995_2011_city/city_convergence")


# read and append data from excel files -----------------------------------

## read excel files================================================================
  # wb11=loadWorkbook("2011.xls", create = TRUE)
  # data11=readWorksheet(wb11, sheet = "data", startRow = 6,
  #                    endRow = 321,startCol = 1, endCol = 240)
  # wb10=loadWorkbook("2010.xls", create = TRUE)
  # data10=readWorksheet(wb10, sheet = "data", startRow = 5,
  #                     endRow = 320,startCol = 1, endCol = 239)
  # wb09=loadWorkbook("2009.xls", create = TRUE)
  # data09=readWorksheet(wb09, sheet = "data", startRow = 4,
  #                      endRow = 319,startCol = 1, endCol = 239)
  # wb08=loadWorkbook("2008.xls", create = TRUE)
  # data08=readWorksheet(wb08, sheet = "data", startRow = 6,
#                      endRow = 321,startCol = 1, endCol = 244)
# wb08=loadWorkbook("2008.xls", create = TRUE)
# data08=readWorksheet(wb08, sheet = "data", startRow = 6,
#                      endRow = 321,startCol = 1, endCol = 244)
# wb07=loadWorkbook("2007.xls", create = TRUE)
# data07=readWorksheet(wb07, sheet = "data", startRow =6,
#                      endRow = 321,startCol =1, endCol = 254)
# wb06=loadWorkbook("2006.xls", create = TRUE)
# data06=readWorksheet(wb06, sheet = "data", startRow =4,
#                      endRow = 319,startCol =1, endCol = 254)
# wb05=loadWorkbook("2005.xls", create = TRUE)
# data05=readWorksheet(wb05, sheet = "data", startRow =4,
#                      endRow = 319,startCol =1, endCol = 184)
# wb04=loadWorkbook("2004.xls", create = TRUE)
# data04=readWorksheet(wb04, sheet = "data", startRow =4,
#                      endRow = 319,startCol =2, endCol = 254)
# wb03=loadWorkbook("2003.xls", create = TRUE)
# data03=readWorksheet(wb03, sheet = "data", startRow =4,
#                      endRow = 319,startCol =2, endCol = 210)
# wb02=loadWorkbook("2002.xls", create = TRUE)
# data02=readWorksheet(wb02, sheet = "data", startRow =4,
#                      endRow = 319,startCol =2, endCol = 207)
# wb01=loadWorkbook("2001.xls", create = TRUE)
# data01=readWorksheet(wb01, sheet = "data", startRow =4,
#                      endRow = 319,startCol =2, endCol = 177)
# wb00=loadWorkbook("2000.xls", create = TRUE)
# data00=readWorksheet(wb00, sheet = "data", startRow =4,
#                      endRow =319,startCol =2, endCol=203)
# wb99=loadWorkbook("1999.xls", create = TRUE)
# data99=readWorksheet(wb99, sheet = "data", startRow =4,
#                      endRow = 319,startCol =3, endCol = 182)
# wb98=loadWorkbook("1998.xls", create = TRUE)
# data98=readWorksheet(wb98, sheet = "data", startRow =5,
#                      endRow = 320,startCol =2, endCol = 191)
# wb97=loadWorkbook("1997.xls", create = TRUE)
# data97=readWorksheet(wb97, sheet = "data", startRow =4,
#                      endRow = 319,startCol =2, endCol = 151)
# wb96=loadWorkbook("1996.xls", create = TRUE)
# data96=readWorksheet(wb96, sheet = "data", startRow =4,
#                      endRow =319 ,startCol =2, endCol =177 )
# wb95=loadWorkbook("1995.xls", create = TRUE)
# data95=readWorksheet(wb95, sheet = "data", startRow =6,
#                      endRow = 321,startCol =2, endCol =240 )
# ## append===============================
# library(gtools)
# data=smartbind(data11,data10,data09,data08,data07,data06,data05,data04,data03,data02,data01,data00
#                ,data99,data98,data97,data96,data95)
#
# # wb <- loadWorkbook("city95-10.xlsx", create = TRUE)
# # createSheet(wb, name = "citydata")
# # writeWorksheet(wb, data, sheet = "citydata", startRow = 1, startCol = 1)
# # saveWorkbook(wb)
#
# write.csv(data, "data.csv")


# keep the variables ------------------------------------------------------


data <- read.csv("data.csv")
nomissing <- which(data$cityid != "NA")
data1 <- data[nomissing, ]

needvars <- c("city", "cityid", "year", "pop", "worker", "rat_em_st", "rat_em_nd", "rat_em_rd", "empl_man",
              "area", "area_const", "gr_pop", "gdp", "gdp_per", "gr_gdp", "r_gdp_st", "r_gdp_nd", "r_gdp_rd", "ind_val",
              "total_fix", "fdi", "budget", "pay", "wage", "road_av", "high_col", "nmid_stu", "lit_stu", "gm_ind_hk",
              "sci_pay", "edu_pay", "inst_load", "saving", "n_working", "n_phone", "total_power", "c_huoyun")
datacity <- data1[needvars]
datacity$cityid <- as.character(datacity$cityid)
datacity$isCap <- ifelse(substr(datacity$cityid, 3, 4) == "00" | substr(datacity$cityid, 3, 4) == "01", 1,
                         0) #create capital city dummy
print(datacity$isCap)


eastreg <- c("11", "12", "13", "21", "31", "32", "33", "35", "37", "44", "46")
midreg <- c("14", "22", "23", "34", "36", "41", "42", "43")
westreg <- c("15", "45", "51", "52", "53", "54", "61", "62", "63", "64", "65")
datacity$bigregion <- 0
datacity$bigregion[substr(datacity$cityid, 1, 2) %in% eastreg] <- 1
datacity$bigregion[substr(datacity$cityid, 1, 2) %in% midreg] <- 2
datacity$bigregion[substr(datacity$cityid, 1, 2) %in% westreg] <- 3
datacity$bigregion <- factor(datacity$bigregion, levels = c("1", "2", "3"))
datacity$D_east <- (datacity$bigregion == "Eastern Region") * 1
datacity$D_central <- (datacity$bigregion == "Central Region") * 1
datacity$D_west <- (datacity$bigregion == "Western Region") * 1
datacity <- transform(datacity, deficit = budget - pay)
write.csv(datacity, "citypanel.csv")
write.dta(datacity, "citypanel.dta")


# generate average data ---------------------------------------------------


datacity$is97 <- (datacity$year == 1997) * 1
datacity$is08 <- (datacity$year == 2008) * 1
datacity$fdigdp <- with(datacity, fdi/gdp)
datacity <- transform(datacity, fdigdp = fdi/gdp)
fdiord <- order(datacity$fdigdp, decreasing = TRUE)
datacitytem <- datacity[fdiord, ]
write.csv(datacitytem, "tem.csv")

ccname <- c("pop", "worker", "rat_em_st", "rat_em_nd", "rat_em_rd", "empl_man", "area", "area_const", "gr_pop",
            "gdp", "gdp_per", "gr_gdp", "r_gdp_st", "r_gdp_nd", "r_gdp_rd", "ind_val", "total_fix", "fdi", "budget",
            "pay", "deficit", "wage", "road_av", "high_col", "nmid_stu", "lit_stu", "gm_ind_hk", "sci_pay", "edu_pay",
            "inst_load", "saving", "n_working", "n_phone", "total_power", "c_huoyun")
av <- aggregate(datacity[ccname], by = list(cityid = datacity$cityid), mean, na.rm = TRUE)
av$isCap <- ifelse(substr(av$cityid, 3, 4) == "00" | substr(av$cityid, 3, 4) == "01", 1, 0)
print(av$isCap)
av$fdigdpAv <- with(fdi/gdp * 100, data = av)
av$paygdpAv <- with(pay/gdp * 100, data = av)
av$fixgdpAv <- av$total_fix/av$gdp
av$deficit <- av$deficit/av$gdp
av$gdp2010 <- datacity$gdp[(datacity$year == 2010)]
av$regions <- datacity$bigregion[(datacity$year == 2010)]
write.csv(av, "cityaver.csv")
write.dta(av, "cityaver.dta")

# merge average data with zhang data for threshold model--------------------------------------


dataaver <- read.csv("cityaver.csv")
wbcity <- loadWorkbook("city_old.xls", create = TRUE)
data_old <- readWorksheet(wbcity, sheet = "city-new", startRow = 2, endRow = 246, startCol = 2, endCol = 13)
datamerg <- merge(data_old, dataaver, "cityid")
names(datamerg)
datamerg <- transform(datamerg, r_em_manu = round(empl_man/worker * 100, 2))
threshold <- c("prov", "cityid", "y", "gdp90", "gnAv", "primstu90", "highstu90", "noAgri90", "middstu90",
               "midStuPop", "invgdp", "fdigdpAv", "pop", "rat_em_st", "area", "gdp", "gdp_per", "gr_gdp", "road_av",
               "n_phone", "total_power", "c_huoyun", "deficit", "paygdpAv", "high_col", "gm_ind_hk", "isCap", "regions")
datathresh <- datamerg[threshold]
datathresh$D_east <- (datathresh$regions == "1") * 1
datathresh$D_central <- (datathresh$regions == "2") * 1
datathresh$D_west <- (datathresh$regions == "3") * 1
write.csv(datathresh, "datathresh.csv")
write.csv(datamerg, "datafinall.csv")

te <- read.csv("datathresh.csv")
write.dta(te, "datathresh.dta")


# some plot ---------------------------------------------------------------

library(ggplot2)

qplot(fdi/gdp * 100, gdp_per, data = datacity, colour = factor(bigregion))

p <- ggplot(te, aes(x = fdigdpAv, y = y, size = road_av, color = factor(regions)))
p + geom_point() + geom_smooth()+ theme_bw()


p <- ggplot(te, aes(x = fdigdpAv, y = y, size = road_av))
p + geom_point(aes(color = factor(regions))) + geom_smooth()+ theme_bw()

pc <- c("road_av", "total_power", "n_phone")
datpc <- te[pc]
pairs(datpc)
pcm <- princomp(datpc, scores = TRUE)
summary(pcm)
plot(pcm)
pcm$loading
pcm$scores


# geni coefficent ---------------------------------------------------------

dat.geni <- read.csv("citypanel.csv")
dat.geni <- transform(dat.geni,provid=substr(cityid, 1, 2)) #create province id dummy var
# test to construct the theil index
Theil <- function (x, parameter = 0)
{
  if (is.null(parameter))
    parameter <- 0
  if (parameter == 0) {
    x <- x[!(x == 0)]
    Th <- x/mean(x)
    Th <- sum(x * log(Th))
    Th <- Th/sum(x)
  }
  else {
    Th <- exp(mean(log(x)))/mean(x)
    Th <- -log(Th)
  }
  Th
}

th <- Theil(x)

