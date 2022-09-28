
# Google sheets
googlesheets4::read_sheet(
  "https://docs.google.com/spreadsheets/d/1lkts4hLkrAFAobONFXiEjgDnuUmXKci6YF--vg1pC1s/edit?usp=sharing",
  sheet="data 3"
) -> data3

ggplot(
  data=data3,
)+
  geom_point(
      aes(
        x=`Total Effect`,
        y=Characteristic,
        color=Country
        ),
      size=0.3
    )+
  geom_point(
    aes(
      x=`Total Effect`,
      y=Characteristic,
      color=Country,
      size = Population,
      fill = Country,
  ),
  shape = 21,
  alpha=0.2,
  stroke = 1
  )

 
