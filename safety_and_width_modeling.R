library(tmap); library(tidyverse); library(rgdal); library(sf)
#road 도로구간 데이터  sigungu 행정경계 데이터 
road <- st_read("Z_KAIS_TL_SPRD_MANAGE_11000.shp", options = "ENCODING=euc-kr")
Sigungu <- st_read("Z_NGII_N3A_G0100000.shp", options = "ENCODING=euc-kr")
head(road)
강남<- subset(Sigungu, NAME == "강남구")
child <- st_read(".//safetydata/TG_ACDT19.shp", options = "ENCODING=UTF-8")
old <- read.csv(".//safetydata/전국노인장애인보호구역표준데이터.csv")

crosswalk <- st_read(".//safetydata/A004_A.shp", options = "ENCODING=euc-kr", crs = 5186)
intersection <- st_read(".//safetydata/A008_P.shp", options = "ENCODING=euc-kr")
busstop<- read.csv(".//safetydata/서울시버스정류소좌표데이터(2021.01.14.).csv")
firewater<- read.csv(".//safetydata/서울시 소화용수 위치정보 (좌표계_ ITRF2000).csv")

st_crs(강남)<- st_crs(road)

crosswalk<- st_transform(crosswalk, st_crs(road))
crosswalk<- st_cast(crosswalk, "MULTILINESTRING")
intersection<- st_transform(intersection, st_crs(road))
busstop<- st_as_sf(busstop, coords = c('X좌표', 'Y좌표'),crs = "+init=epsg:4326")
busstop<- st_transform(busstop, st_crs(road))
firewater<- st_as_sf(firewater, coords = c('X좌표', 'Y좌표'),crs = "+proj=tmerc +lat_0=38 +lon_0=127 +k=1 +x_0=200000 +y_0=500000 +ellps=GRS80 +units=m +no_defs") #5181
firewater<- st_transform(firewater, st_crs(road))
#300m 어린이, 실버존
child<- st_transform(child, st_crs(road))
old <- old %>% filter(시도명 == "서울특별시")
old <- st_as_sf(old, coords = c('경도', '위도'), crs = "+init=epsg:4326")
old <- st_transform(old, st_crs(road))

busstop <- st_intersection(busstop, 강남)
firewater <- st_intersection(firewater, 강남)
crosswalk <- st_intersection(crosswalk, 강남)
intersection <- st_intersection(intersection, 강남)
child <- st_intersection(child, 강남)
old <- st_intersection(old, 강남)
head(crosswalk)
bus_10m = st_buffer(busstop, dist = 10)
firewater_5m = st_buffer(firewater, dist = 5)
intersection_5m = st_buffer(intersection, dist = 5)
child_300m = st_buffer(child, dist = 300)
old_300m = st_buffer(old, dist = 300)

도로_ex <- st_intersection(road, 강남)

idx<- grep("자전거길", 도로_ex$RN)
도로<- 도로_ex[-idx,]

도로$도로길이<- st_length(도로$geometry) 

도로 <- subset(도로, !(RN == "양재대로" & ROAD_BT >=50))
도로 <- subset(도로, !(RN == "올림픽대로"))
도로$ROAD_BT <- (max(도로$ROAD_BT) + 1)  - 도로$ROAD_BT
int <- as_tibble(st_intersection(bus_10m, 도로))
busfeature<- int %>% group_by(RBP_CN,REP_CN) %>% summarise(buscount = n()) %>%ungroup()
int <- as_tibble(st_intersection(firewater_5m, 도로))
firefeature<- int %>% group_by(RBP_CN,REP_CN) %>% summarise(firecount = n()) %>%ungroup()
int <- as_tibble(st_intersection(intersection_5m, 도로))
interfeature<- int %>% group_by(RBP_CN,REP_CN) %>% summarise(intercount = n()) %>%ungroup()
int <- as_tibble(st_intersection(crosswalk, 도로))
crossfeature<- int %>% group_by(RBP_CN,REP_CN) %>% summarise(crosscount = n()) %>%ungroup()
도로<- left_join(도로, busfeature)
도로 <- left_join(도로, firefeature)
도로 <- left_join(도로, interfeature)
도로 <- left_join(도로, crossfeature)
#거리별 count를 치환해서 적용해보기.
도로$buscount<- ifelse(is.na(도로$buscount), 0, 도로$buscount)
도로$firecount<- ifelse(is.na(도로$firecount), 0, 도로$firecount)
도로$intercount<- ifelse(is.na(도로$intercount), 0, 도로$intercount)
도로$crosscount<- ifelse(is.na(도로$crosscount), 0, 도로$crosscount)
도로$count <- 도로$buscount + 도로$firecount + 도로$intercount + 도로$crosscount
도로$안전요소 <- 0.1639*도로$count + 0.0605 *도로$ROAD_BT
#공간 일반화 포아송 모형에 대해 4대 금지요소 평균 :0.1639 / 폭(면적) 0.0605
tmap_mode("view")
gn_pred<- st_read(".//res/pred_gn.shp")
gn_pred<- st_transform(gn_pred, crs = st_crs(road))
rd <- st_intersection(도로, gn_pred)
rd <- rd %>% mutate(saferank = ntile(안전요소, 4), demrank = ntile(pred, 4))
rd$tot <- rd$saferank + rd$demrank
#보호구역 제외
tm_shape(rd) + tm_lines(col ="tot", scale = 3, palette = c("deepskyblue", "chartreuse", "orange", "red"), popup.vars = c("RN", "도로길이"))
#보호구역 포함
tm_shape(rd) + tm_lines(col ="tot", scale = 3, palette = c("deepskyblue", "chartreuse", "orange", "red"), popup.vars = c("RN", "도로길이")) +tm_shape(child_300m) +tm_polygons(col = "red", alpha = .2) +tm_layout(frame = FALSE) +tm_shape(old_300m) +tm_polygons(col = "orange", alpha = .2) +tm_layout(frame = FALSE)
