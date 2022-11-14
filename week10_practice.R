googlesheets4::gs4_deauth()
googlesheets4::read_sheet(
  ss="https://docs.google.com/spreadsheets/d/1Vk4jo92pUJ6nWRPSRQuYIIgb6lpTPf2pJDqLBgEGEZI/edit?usp=sharing",
  sheet="Sheet1-week10"
) -> enrollmentRate_department

gsub(enrollmentRate_department$`當學年度各學系境外(新生)學生實際註冊人數 (E)`, pattern = "...", replacement = 0)|>
  as.numeric()->enrollmentRate_department$`當學年度各學系境外(新生)學生實際註冊人數 (E)`

library(dplyr)

enrollmentRate_department |>
  group_by(學年度,學校名稱) |>
  summarise(
    當學年度總量內核定新生招生名額合計={
      `當學年度總量內核定新生招生名額(A)`|>
        sum()
      },
    當學年度新生保留入學資格人數合計={
      `當學年度新生保留入學資格人數(B)`|>
        sum()
    },
    當學年度總量內新生招生核定名額之實際註冊人數合計={
      `當學年度總量內新生招生核定名額之實際註冊人數(C)`|>
        sum()
    },
    當學年度各學系境外.新生.學生實際註冊人數合計={
      `當學年度各學系境外(新生)學生實際註冊人數 (E)`|>
        sum()
    }
  ) |>
  mutate(
    註冊率 = (當學年度總量內新生招生核定名額之實際註冊人數合計+
             當學年度各學系境外.新生.學生實際註冊人數合計)/
      (當學年度總量內核定新生招生名額合計-當學年度新生保留入學資格人數合計+
         當學年度各學系境外.新生.學生實際註冊人數合計)*100
    )|>
  arrange(註冊率)|>
  arrange(desc(學年度)) -> enrollmentRate_university

universityName <- c("稻江科技暨管理學院", "國立陽明大學",enrollmentRate_university$學校名稱[1:68])

enrollmentRate_university$學校名稱 <- factor(enrollmentRate_university$學校名稱, levels = universityName)

plt=Plot()
plt$ggplot = ggplot(data=enrollmentRate_university)
plt$geom = 
  geom_tile(
    aes(
      x=學年度,
      y=學校名稱,
      fill=註冊率
    ))

plt$others = list(
  scale_y_discrete(expand=c(0,0)),
  scale_x_continuous(expand=c(0,0), position="top",breaks = seq(108,110, by=1),labels=c("108學年度", "109學年度", "110學年度")),
  xlab(NULL),
  ylab(NULL),
  theme(
  axis.ticks.x=element_blank(),
  axis.line.y=element_blank(),
  axis.ticks.y=element_blank()
  )
)

plt$explain = list(
  labs(
    title="臺灣大專院校退場",
    subtitle="以一般大學日間部為例",
    caption="資料來源：https://data.gov.tw/dataset/26228"
  )
)

plt$make()

