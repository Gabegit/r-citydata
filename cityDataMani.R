library(XLConnect)

setwd("E:/data/1995_2011_city/city_convergence")
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
th <- function (x){
  th <- x/mean(x)
}

