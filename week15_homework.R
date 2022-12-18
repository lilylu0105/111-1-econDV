googlesheets4::gs4_deauth()
googlesheets4::read_sheet(
  ss="https://docs.google.com/spreadsheets/d/1Vk4jo92pUJ6nWRPSRQuYIIgb6lpTPf2pJDqLBgEGEZI/edit?usp=sharing",
  sheet="Sheet3-week15"
) -> population

#資料來源:https://data.gov.tw/dataset/77144

library(dplyr)
library(ggplot2)
library(sf)

population$縣市名<-stringr::str_sub(population$區域別,1,3)
population|>
  mutate(
    遷入合計={`遷入人數_合計_男`+`遷入人數_合計_女`},
    遷出合計={`遷出人數_合計_男`+`遷出人數_合計_女`})|>
  group_by(縣市名)|>
  summarise(
    遷入 = sum(遷入合計),
    遷出 = sum(遷出合計),
    `遷入/遷出` = 遷入 - 遷出)-> populationCity

populationCity$`遷入/遷出`|>cut(c(-Inf,-4000,-1000,0,100,300,Inf))->populationCity$cut

sf_taiwan <- sf::read_sf("C:/Users/chieh/Documents/GitHub/111-1-econDV/mapdata/mapdata202209220943/COUNTY_MOI_1090820.shp")
sf_taiwan %>%
  st_crop(c(xmin=119,xmax=122,ymin=22,ymax=25.8)) -> sf_taiwan
taiwanMap <- sf_taiwan[c("COUNTYNAME", "geometry")]
mapPopulationData <- left_join(taiwanMap, populationCity, by= c("COUNTYNAME" = "縣市名"))

sf_taiwan |> sf::st_centroid(of_largest_polygon = T) -> sf_taiwanPoint
df_label = data.frame(name=sf_taiwan$COUNTYNAME)
sf_taiwanPoint |> sf::st_coordinates() -> xyCoord
df_label$lat = xyCoord[,1]
df_label$long= xyCoord[,2]

plt=Plot()
plt$ggplot = ggplot()
plt$geom1 = list(
  geom_sf(data = mapPopulationData,
          aes(fill=cut),
          colour="white"),
  scale_fill_manual(
    limits=c("(300, Inf]", "(100,300]", "(0,100]"),
    values=c("#FFB200", "#FFCB42", "#FFF4CF"),
    labels=c("高","中","低"), 
    name="遷入趨勢"),
  ggnewscale::new_scale_fill(),
  geom_sf(data = mapPopulationData,
          aes(fill=cut),
          colour="white"),
  scale_fill_manual(
    limits=c("(-Inf,-4e+03]", "(-4e+03,-1e+03]", "(-1e+03,0]"),
    values=c("#6E85B7", "#B2C8DF", "#C4D7E0"),
    labels=c("高","中","低"), 
    name="遷出趨勢", na.value = NA),
  geom_text(data = df_label,
            aes(x = lat,
                y = long,
                label = name))
  )
plt$explain = list(
  labs(
    title="臺灣各縣市人口遷入/遷出趨勢統計",
    subtitle="以2022年1月至10月資料統計",
    caption="資料來源：https://data.gov.tw/dataset/77144"
  )
)
plt$others = list(
  theme_void()
  )
plt$make()

ggsave("week15hw.svg", width = 20, height = 20, units = "cm")

ggdash()
