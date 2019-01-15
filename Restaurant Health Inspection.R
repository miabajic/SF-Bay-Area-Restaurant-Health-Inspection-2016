# SF Bay Area health inspections of restaurants
# Group project

berkeley = readRDS("berkeley.rds")
County = readRDS("shp_county.rds")
Place = readRDS("shp_place.rds")
library("maps")
library("viridis")
library("lattice")
library("reshape2")
install.packages("stringr")
library("stringr")

# alameda
alamedabiz = read.csv("businesses.csv")
alamedainspection = read.csv("inspections.csv")
alamedaviolation = read.csv("violations.csv")
alameda2 = merge(alamedabiz, alamedainspection, by = "business_id")
alameda = merge(alameda2, alamedaviolation, by = c("business_id","date"))

# yolo
yolobiz = read.csv("businesses.csv")
yoloinspection = read.csv("inspections.csv")
yoloviolation = read.csv("violations.csv")
yolo2 = merge(yolobiz, yoloinspection, by = "business_id")
yolo = merge(yolo2, yoloviolation, by = c("business_id","date"))

# SF
setwd("c:/Users/miabajic/assignment-5-Team17/data/san_francisco")
sfbiz = read.csv("businesses.csv")
sfinspection = read.csv("inspections.csv")
sfviolation = read.csv("violations.csv")
sf2 = merge(sfbiz, sfinspection, by = "business_id")
sanfran = merge(sf2, sfviolation, by = c("business_id","date"))

## Convert Alameda's G, Y, R result column into some kind of score 

##map
pal = magma(4)
map("county","california,san francisco")
points(sanfran$latitude~sanfran$longitude, col = pal[cut_sanfran_score])
cut_sanfran_score = cut(sanfran$Score, breaks = c(0,70,85,90,100))
levels(cut_sanfran_score) = c("Poor","Needs Improvement","Adequate","Good")
legend("topright",levels(cut_sanfran_score),fill = pal,cex = 0.6)


#---- 
# fastfood restaurants chain
#sanfran
sanfran$name = str_to_lower(sanfran$name)
#mcdonalds (90)
grep_result_mc1 = grep("mcdonalds",sanfran$name)
grep_result_mc2 = grep("mcdonald's",sanfran$name)
mcdonalds_sanfran = sanfran[c(grep_result_mc1,grep_result_mc2),]
#subway (244)
grep_result_subway = grep("subway",sanfran$name)
subway_sanfran = sanfran[grep_result_subway,]
#burger king (104)
grep_result_bk = grep("burger king",sanfran$name)
bk_sanfran = sanfran[grep_result_bk,]
#innout (2)
grep_result_innout = grep("in-n-out",sanfran$name)
innout_sanfran = sanfran[grep_result_innout,]
#Wendy's (0)
grep_result_wendys = grep("wendy's",sanfran$name)
wendys_sanfran = sanfran[grep_result_wendys,]
#taco bell (4)
grep_result_tacobell = grep("taco bell",sanfran$name)
tacobell_sanfran = sanfran[grep_result_tacobell,]
#pizza hut (16)
grep_result_pizzahut = grep("pizza hut",sanfran$name)
pizzahut_sanfran = sanfran[grep_result_pizzahut,]
#Arby's (0)
grep_result_arbys = grep("arby's",sanfran$name)
arbys_sanfran = sanfran[grep_result_arbys,]
#KFC (121)
grep_result_kfc1 = grep("kentucky fried chicken",sanfran$name)
grep_result_kfc2 = grep("kfc",sanfran$name)
kfc_sanfran = sanfran[c(grep_result_kfc1,grep_result_kfc2),]
#Jack in the box (28)
grep_result_jack = grep("jack in the box",sanfran$name)
jack_sanfran = sanfran[grep_result_jack,]

#yolo
yolo$name = str_to_lower(yolo$name)
#mcdonalds (242)
yolo_grep_result_mc1 = grep("mcdonalds",yolo$name)
mcdonalds_yolo = yolo[yolo_grep_result_mc1,]
#subway (335)
yolo_grep_result_subway = grep("subway",yolo$name)
subway_yolo = yolo[yolo_grep_result_subway,]
#burger king (91)
yolo_grep_result_bk = grep("burger king",yolo$name)
bk_yolo = yolo[yolo_grep_result_bk,]
#innout (34)
yolo_grep_result_innout = grep("in-n-out",yolo$name)
innout_yolo = yolo[yolo_grep_result_innout,]
#Wendy's (0)
yolo_grep_result_wendys = grep("wendy's",yolo$name)
wendys_yolo = yolo[yolo_grep_result_wendys,]
#taco bell (115)
yolo_grep_result_tacobell = grep("taco bell",yolo$name)
tacobell_yolo = yolo[yolo_grep_result_tacobell,]
#pizza hut (0)
yolo_grep_result_pizzahut = grep("pizza hut",yolo$name)
pizzahut_yolo = yolo[yolo_grep_result_pizzahut,]
#Arby's (0)
yolo_grep_result_arbys = grep("arby's",yolo$name)
arbys_yolo = yolo[yolo_grep_result_arbys,]
#KFC (102)
yolo_grep_result_kfc1 = grep("kentucky fried chicken",yolo$name)
kfc_yolo = yolo[yolo_grep_result_kfc1,]
#Jack in the box (204)
yolo_grep_result_jack = grep("jack in the box",yolo$name)
jack_yolo = yolo[yolo_grep_result_jack,]

#alameda
alameda$name = str_to_lower(alameda$name)

#mcdonalds (1020)
alameda_grep_result_mc1 = grep("mcdonalds",alameda$name)
alameda_grep_result_mc2 = grep("mcdonald's",alameda$name)
mcdonalds_alameda = alameda[c(alameda_grep_result_mc1,alameda_grep_result_mc2),]
#subway (2201)
alameda_grep_result_subway = grep("subway",alameda$name)
subway_alameda = alameda[alameda_grep_result_subway,]
#burger king (514)
alameda_grep_result_bk = grep("burger king",alameda$name)
bk_alameda = alameda[alameda_grep_result_bk,]
#innout (85)
alameda_grep_result_innout = grep("in-n-out",alameda$name)
innout_alameda = alameda[alameda_grep_result_innout,]
#Wendy's (192)
alameda_grep_result_wendys = grep("wendy's",alameda$name)
wendys_alameda = alameda[alameda_grep_result_wendys,]
#taco bell (568)
alameda_grep_result_tacobell = grep("taco bell",alameda$name)
tacobell_alameda = alameda[alameda_grep_result_tacobell,]
#pizza hut (175)
alameda_grep_result_pizzahut = grep("pizza hut",alameda$name)
pizzahut_alameda = alameda[alameda_grep_result_pizzahut,]
#Arby's (52)
alameda_grep_result_arbys = grep("arby's",alameda$name)
arbys_alameda = alameda[alameda_grep_result_arbys,]
#KFC  (646)
alameda_grep_result_kfc1 = grep("kentucky fried chicken",alameda$name)
alameda_grep_result_kfc2 = grep("kfc",alameda$name)
kfc_alameda = alameda[c(alameda_grep_result_kfc1,alameda_grep_result_kfc2),]
#Jack in the box (740)
alameda_grep_result_jack = grep("jack in the box",alameda$name)
jack_alameda = alameda[alameda_grep_result_jack,]

#berkeley
alameda_berkeley$name = str_to_lower(alameda_berkeley$name)
#find the subsets for berkeley
berkeley = x
berkeley$Name = str_to_lower(berkeley$Name)
colnames(berkeley) = str_to_lower(colnames((berkeley)))
#mcdonalds (20)
berkeley_grep_result_mc1 = grep("mcdonald",berkeley$name)
mcdonalds_berkeley = berkeley[berkeley_grep_result_mc1,]
#subway (60)
berkeley_grep_result_subway = grep("subway",berkeley$name)
subway_berkeley = berkeley[berkeley_grep_result_subway,]
#burger king (0)
berkeley_grep_result_bk = grep("burger king",berkeley$name)
bk_berkeley = berkeley[berkeley_grep_result_bk,]
#innout (0)
berkeley_grep_result_innout = grep("in-n-out",berkeley$name)
innout_berkeley = berkeley[berkeley_grep_result_innout,]
#Wendy's (0)
berkeley_grep_result_wendys = grep("wendy",berkeley$name)
wendys_berkeley = berkeley[berkeley_grep_result_wendys,]
#taco bell (0)
berkeley_grep_result_tacobell = grep("taco bell",berkeley$name)
tacobell_berkeley = berkeley[berkeley_grep_result_tacobell,]
#pizza hut (0)
berkeley_grep_result_pizzahut = grep("pizza hut",berkeley$name)
pizzahut_berkeley = berkeley[berkeley_grep_result_pizzahut,]
#Arby's (0)
berkeley_grep_result_arbys = grep("arby",berkeley$name)
arbys_berkeley = berkeley[berkeley_grep_result_arbys,]
#KFC  (0)
berkeley_grep_result_kfc1 = grep("kentucky fried chicken",berkeley$name)
berkeley_grep_result_kfc2 = grep("kfc",berkeley$name)
kfc_berkeley = berkeley[c(berkeley_grep_result_kfc1,berkeley_grep_result_kfc2),]
#Jack in the box (10)
berkeley_grep_result_jack = grep("jack",berkeley$name)
jack_berkeley = berkeley[berkeley_grep_result_jack,]

berkeley_fastfood = rbind(mcdonalds_berkeley,subway_berkeley,jack_berkeley)


alameda_fastfood = rbind(mcdonalds_alameda,subway_alameda,bk_alameda,innout_alameda,wendys_alameda,tacobell_alameda,pizzahut_alameda,arbys_alameda,kfc_alameda,jack_alameda)

yolo_fastfood = rbind(mcdonalds_yolo,subway_yolo,bk_yolo,innout_yolo,wendys_yolo,tacobell_yolo,pizzahut_yolo,arbys_yolo,kfc_yolo,jack_yolo)

sanfran_fastfood = rbind(mcdonalds_sanfran,subway_sanfran,bk_sanfran,innout_sanfran,wendys_yolo,tacobell_sanfran,pizzahut_sanfran,arbys_sanfran,kfc_sanfran,jack_sanfran)



library("ggplot2")
# San Francisco Vizualizations

#Histogram of SF fast food scores
histogram(~Score, sanfran_fastfood, nint = 50, main = "Fast Food Score Distribution for San Francisco")
gg = ggplot(sanfran_fastfood, aes(x = Score, y = risk_category))
gg = gg + geom_point()
gg %+% subset(sanfran_fastfood, Score >= 0)

install.packages("ggmap")
library("ggmap")

library("RgoogleMaps")
library(sp)

summary(sanfran_fastfood$Score)
dim(sanfran_fastfood)

#cut based on SF rules
br = c(-Inf, 70, 85, 90, Inf)
col = cut(sanfran_fastfood$Score, br)
palette = magma(10)[3:6]
sfmap = qmap("San Francisco", zoom = 12, maptype = "hybrid")
sfmap + geom_point(data = sanfran_fastfood, mapping = aes(x = longitude, y = latitude), color = palette[col], lwd = 2) + ggtitle("Fast Food Scores in San Francisco")

library("dplyr")
sanfran_regularfood = anti_join(sanfran, sanfran_fastfood)

#Histogram of SF regular food scores
histogram(~Score, sanfran_regularfood, nint = 50, main = "Regular Food Score Distribution for San Francisco")

#cut based on SF rules
br = c(-Inf, 70, 85, 90, Inf)
col = cut(sanfran_regularfood$Score, br)
palette = magma(10)[3:6]
sfmap = qmap("San Francisco", zoom = 12, maptype = "hybrid")
sfmap + geom_point(data = sanfran_regularfood, mapping = aes(x = longitude, y = latitude), color = palette[col], lwd = 2) + ggtitle("Regular Food Scores in San Francisco") 


#Mia's Visualizations Continued--------------------------
# Berkeley Visualizations


br = c(-Inf, 70, 85, 90, Inf)
col = cut(berkeley_fastfood$score, br)
palette = magma(10)[3:6]
berkmap = qmap("Berkeley", zoom = 13, maptype = "hybrid")
berkmap + geom_point(data = berkeley_fastfood, mapping = aes(x = longitude, y = latitude), color = palette[col], lwd = 2) + ggtitle("Fast Food Scores in Berkeley")

berkeley_regularfood = anti_join(berkeley, berkeley_fastfood)

br = c(-Inf, 70, 85, 90, Inf)
col = cut(berkeley_regularfood$score, br)
palette = magma(10)[3:6]
berkmap = qmap("Berkeley", zoom = 13, maptype = "hybrid")
berkmap + geom_point(data = berkeley_regularfood, mapping = aes(x = longitude, y = latitude), color = palette[col], lwd = 2) + ggtitle("Regular Food Scores in Berkeley")

#
#-------------------
# Alameda with scores
# alameda visualizations
summary(alameda_fastfood$result)
alameda_fastfood$result=str_to_lower(alameda_fastfood$result)

# convert 'g', 'y', and 'r' into scores using the scale given by SF scoring system
# Basically, Green is >90, Red is <=70, Yellow is 71 to 90.

# Alameda's Top10 fast food chains with scores
green = subset(alameda_fastfood, result=='g')
green$score = 90
yellow = subset(alameda_fastfood, result=='y')
yellow$score = 80
red = subset(alameda_fastfood, result=='r')
red$score = 65
summary(alameda_fastfood$score)
# alameda fast food data frame with scores in it
alafastfood_score = rbind(green, yellow, red)

# Alameda's other restaurants
install.packages("dplyr")
library("dplyr")
alameda_regular = anti_join(alameda, alameda_fastfood)
# assigning scores
rgreen = subset(alameda_regular, result=='g')
rgreen$score = 90
ryellow = subset(alameda_regular, result=='y')
ryellow$score = 80
rred = subset(alameda_regular, result=='r')
rred$score = 65
# Alameda's other restaurants with scores
alaregular_score = rbind(rgreen,ryellow, rred)


# Visualization
br = c(0,65, 80, 90)
col = cut(alafastfood_score$score, br)
palette = magma(10)[3:6]
# want to assign certain colors: red, yellow, and green
sfmap = qmap("Alameda", zoom = 12, maptype = "hybrid")
sfmap + geom_point(data = alafastfood_score, mapping = aes(x = longitude, y = latitude), color = palette[col], show.legend = F, lwd = 2) + ggtitle("Fast Food Scores in Alameda")

library("ggplot2")
install.packages("ggmap")
library("ggmap")
library("RgoogleMaps")
library(sp)
summary(alaregular_score$score)
br = c(0,65,80,90)
col = cut(alaregular_score$score, br)
palette = magma(10)[3:6]
sfmap = qmap("Alameda", zoom = 12, maptype = "hybrid")
sfmap + geom_point(data = alaregular_score, mapping = aes(x = longitude, y = latitude), color = palette[col], lwd = 2) + ggtitle("Other Restaurants in Alameda")


## Yolo
followup = subset(yolo, type == "followup")
routine = subset(yolo, type == "routine")
complaint = subset(yolo, type == "complaint")
yolo_vio_id = table(yolo$business_id, yolo$type)
yolo_followups = as.data.frame(table(yolo$business_id, yolo$type))
merge(yolo, yolo_vio_id, by = "business_id")
map("county","california,yolo")
points(yolo$latitude~yolo$longitude, col = c("green","red"))
cut_followups = c(yolo$type, "routine", "complaint", "followup")