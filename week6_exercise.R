newCompanies = readr::read_csv("https://data.gcis.nat.gov.tw/od/file?oid=8B227133-FFAB-4218-963C-21846226B750")

eachTypeCompanyCapital <- newCompanies[c("縣 市 別", "總計資本額", "無限公司資本額", "兩合公司資本額", "有限公司資本額", "股份有限公司資本額")]

tidyr::pivot_longer(
  data=eachTypeCompanyCapital,
  cols=3:6,
  # column names to What
  names_to = "CompanyType",
  values_to = "CompanyTypeCapital"
) -> eachTypeCompanyCapital_long

library(dplyr)
options(scipen=999)

eachTypeCompanyCapital_long |> 
  mutate(
    佔比 = (eachTypeCompanyCapital_long[[4]]/eachTypeCompanyCapital_long[[2]])*100
  ) -> eachTypeCompanyCapital_long

eachTypeCompanyCapital_long$`縣 市 別` |>
  factor() |>
  levels() -> OrLevels

c(OrLevels[-c(23, 25)],OrLevels[c(23,25)])-> correctLevels

eachTypeCompanyCapital_long$`縣 市 別` |>
  factor(
    levels=correctLevels
  ) -> eachTypeCompanyCapital_long$`縣 市 別`

plot=Plot(eachTypeCompanyCapital_long)
plot$ggplot=ggplot(data=plot$data)
plot$theme = theme(
  axis.text.x = element_text(
    angle=45, hjust=1
  ))
plot$geoms = list(
  geom_line( 
    aes(x=`縣 市 別`,
        y=`佔比`,
        colour = CompanyType,
        group = CompanyType)
    ),
  geom_point(
    aes(x=`縣 市 別`,
        y=`佔比`,
        colour = CompanyType,
  )
  ),geom_text(
    aes(x=`縣 市 別`,
        y=`佔比`,
        label = round(`佔比`, 2)
  ),vjust = -2
  )) -> geom_1

plot$ggplot + plot$theme + geom_1


{plot$geoms = list(
  geom_col(
    aes(x=`縣 市 別`,
        y= CompanyTypeCapital,
        fill = CompanyType), position = "dodge"))} -> geom_2
plot$make()

