googlesheets4::gs4_deauth()
googlesheets4::read_sheet(
  ss="https://docs.google.com/spreadsheets/d/1Vk4jo92pUJ6nWRPSRQuYIIgb6lpTPf2pJDqLBgEGEZI/edit?usp=sharing",
  sheet="sheet2-week13"
) -> priceIndex

library(lubridate)
library(dplyr)

priceIndex$`生產者物價年增率(%)`<-as.numeric(priceIndex$`生產者物價年增率(%)`)
priceIndex$統計期<-ym(priceIndex$統計期)

priceIndex |>
#  mutate(markUp= ((外食費物價指數-生產者物價基本總指數)/生產者物價基本總指數)*100)|>
  group_by(統計期) |>
  summarise(
    生產者物價指數增長率={(生產者物價基本總指數 - 97.2)/97.2},
    消費者物價指數增長率={(消費者物價基本分類總指數 - 103.08)/103.08},
    外食費增長率={(外食費物價指數-107)/107}
  )|>
  group_by(統計期) |>
  summarise(
    生產者物價指數增長率=生產者物價指數增長率*100,
    消費者物價指數增長率=消費者物價指數增長率*100,
    外食費增長率=外食費增長率*100
  )|>
  mutate(生產者與外食增長率差距=生產者物價指數增長率-外食費增長率)-> priceIndex_110_1

tidyr::pivot_longer(
  data=priceIndex_110_1,
  cols=2:4,
  names_to = "growthRateType",
  values_to = "Rate"
) -> priceIndex_110_1_long

plt=Plot()
plt$ggplot = ggplot()
plt$geom = list(
  geom_vline(xintercept = c(as.Date("2021-04-01"),as.Date("2021-07-01"),as.Date("2021-10-01"),as.Date("2022-01-01"),as.Date("2022-04-01"),as.Date("2022-07-01")),
             colour = "#F4F3EE"),
  geom_ribbon(data=priceIndex_110_1, aes(
    x=統計期,
    y = 生產者物價指數增長率,
    ymin = 生產者物價指數增長率-生產者與外食增長率差距,
    ymax = 生產者物價指數增長率+生產者與外食增長率差距),
    fill = "#6ECCAF",
    alpha = 0.3
  ),
  geom_line(data=priceIndex_110_1, 
            aes(x=統計期,y = 生產者物價指數增長率),
            linewidth=2, colour = "white"),
  geom_line(data=priceIndex_110_1, 
            aes(x=統計期,y = 消費者物價指數增長率),
            linewidth=2, colour = "white"),
  geom_line(data=priceIndex_110_1, 
            aes(x=統計期,y = 外食費增長率),
            linewidth=2, colour = "white"),
  geom_line(data=priceIndex_110_1_long, 
            aes(x=統計期,y = Rate, colour = growthRateType),
            linewidth=1.5)
#  geom_line(data=priceIndex_110_1, 
#            aes(x=統計期,y = markUp),
#            linewidth=1.5, colour = "pink")
  )

plt$make()

plt$scale = scale_colour_manual(
  limits=c("生產者物價指數增長率", "外食費增長率", "消費者物價指數增長率"),
  values=c("#ADE792", "#F3ECB0", "#344D67"),
  labels=c("生產者","外食費","消費者"), name="物價指數增長率"
)

plt$explain = list(
  labs(
    title="於通貨膨脹萬物齊漲的時候，是老闆賺得少還是消費者付得多?",
    subtitle="以2021年1月為基期之%變化",
    caption="資料來源：https://www.stat.gov.tw//Point.aspx?sid=t.2&n=3581&sms=11480"
  )
)

plt$others = list(
  scale_x_continuous(expand=c(0,0),breaks = seq(as.Date("2021-01-01"),as.Date("2022-10-01-01"), by="3 month"),
                     labels=c("2021-01", "04", "07", "10", "2022-01", "04", "07", "10")),
  scale_y_continuous(expand=c(0,2), breaks = seq(0, 30, by=10),sec.axis = dup_axis()), #, position = "right"
  xlab(NULL),
  ylab(NULL),
  theme(
    axis.line.y=element_blank(),
    axis.ticks.y=element_blank(),
    plot.title=element_text(size = 15),
    plot.subtitle = element_text(size= 12),
    legend.position = c(.1, .8)
  )
)

plt$make()

gg$dash()
