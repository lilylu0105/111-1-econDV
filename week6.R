googledrive::drive_deauth()
googledrive::drive_download(
  file=googledrive::as_id("https://drive.google.com/file/d/1K8IWyy7G9LAKewjVXMGD8kUbbvjGoRT7/view?usp=sharing"),
  overwrite = T
)

library(readr)
marriage <- read_csv("marriage.csv")
head(marriage)

library(dplyr)
marriageTaoYuan = {
  marriage |>
    dplyr::filter(
      # 只看桃園市
      stringr::str_detect(site_id,"桃園市")
    ) -> marriageTaoYuan
  marriageTaoYuan |>
    # 創造行政區欄位
    mutate(
      site_id2 = stringr::str_sub(site_id,4,6)
    ) 
  
}

# 結婚年齡
# X：以妻年齡來看
# Y：對數
library(ggplot2)
plt = new.env()
plt$ggplot = ggplot(data=marriageTaoYuan)
plt$geoms = list(
  geom_col(
    mapping=aes(
      x=female_age_or_spouse1,
      y=marry_pair
    ),
    position="stack" # sum across sites within same age group.
  )
)
plt$ggplot + plt$geoms

plt$ggplot + plt$geoms + coord_flip()
