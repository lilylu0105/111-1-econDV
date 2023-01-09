googlesheets4::read_sheet(
  "https://docs.google.com/spreadsheets/d/1gaG-rA210IhHahQworJuYhlCF9sdXOE48MA8zZ9mlw0/edit?usp=sharing",
  sheet="sheet1"
) -> data2

data{
library(dplyr)

data2|>
  filter(kinds == "Explained by: Perceptions of corruption")|>
  arrange(desc(score_acc))->totalScore

data2|>
  filter(kinds == "Dystopia+ residual")|>
  arrange(desc(scores))|>
  head()->dystopiaandResidual

data2|>
  filter(kinds == "Explained by: GDP per capita")|>
  arrange(desc(scores))|>
  head()->GDPperCapital

data2|>
  filter(kinds == "Explained by: Social support")|>
  arrange(desc(scores))|>
  head()->socialSupport

data2|>
  filter(kinds == "Explained by: Healthy life expectancy")|>
  arrange(desc(scores))|>
  head()->healthyLifeExpectancy

data2|>
  filter(kinds == "Explained by: Freedom to make life choices")|>
  arrange(desc(scores))|>
  head()->freedomToMakeLifeChoices

data2|>
  filter(kinds == "Explained by: Generosity")|>
  arrange(desc(scores))|>
  head()->generosity

data2|>
  filter(kinds == "Explained by: Perceptions of corruption")|>
  arrange(desc(scores))|>
  head()->perceptionsofCorruption

data2$country <- factor(data2$country,
                        levels = factor(totalScore$country[30:1]))

bind_rows(dystopiaandResidual,GDPperCapital,socialSupport,
          healthyLifeExpectancy,freedomToMakeLifeChoices,
          generosity,perceptionsofCorruption)->data3

kinds_status<- c(
  `Dystopia+ residual` = "Dystopia + residual",
  `Explained by: GDP per capita` = "GDP per capita",
  `Explained by: Social support` = "Social support",
  `Explained by: Healthy life expectancy` = "Healthy life expectancy",
  `Explained by: Freedom to make life choices` = "Freedom to make life choices",
  `Explained by: Generosity` = "Generosity",
  `Explained by: Perceptions of corruption` = "Perceptions of corruption"
)
}

plt1{
  plt1=Plot()
  plt1$ggplot = ggplot()
  plt1$geom = list(
    geom_bar(data=data2,
           aes(x=scores, y=country, fill=kinds),
           stat="identity"),
    geom_text(data=totalScore,
              aes(x=score_acc,y=country,label=score_acc),
              colour="white",
              hjust=1.2))
  plt1$others = list(
    scale_y_discrete(expand=c(0,0)),
    scale_x_continuous(expand=c(0,0), position="top"),
    ylab(NULL),
    theme(
      axis.line.x=element_blank(),
      axis.line.y=element_blank(),
      axis.ticks.x=element_blank(),
      axis.text.x=element_blank(),
      legend.position="none"
    )
  )
  plt1$scale = scale_fill_manual(
    values = c("#d87659","#e8a66c","#e3c577","#92b082","#4f9b8f","#3d7070","#2e4552"),
    labels=c("Dystopia + residual","Freedom to make life choices","GDP per capita","Generosity","Healthy life expectancy","Perceptions of corruption","Social support"))
  plt1$explain = list(
    labs(
      title="2022世界30個國家幸福分數排名",
      subtitle="幸福分數以7個細項組成及各細項下排名前6的國家"))
  plt1$make() ->plt1
}

plt2{
  plt2=Plot()
  plt2$ggplot = ggplot()
  plt2$geom = list(
    geom_bar(data=data3,
             aes(x=scores, y=country, fill=kinds),
             stat="identity"),
    facet_wrap(vars(kinds), scales = "free_y",
               labeller = labeller(
                 kinds = kinds_status), nrow = 4),
    geom_text(data=data3,
              aes(x=scores, y=country,label=scores),
              colour="white",
              hjust=1.2)
    )
  plt2$others = list(
    scale_x_continuous(expand=c(0,0)),
    xlab(NULL),
    ylab(NULL),
    theme(
    axis.line.x=element_blank(),
    axis.line.y=element_blank(),
    axis.ticks.x=element_blank(),
    axis.text.x=element_blank(),
    plot.title=element_text(size = 30),
    plot.subtitle = element_text(size= 20),
    legend.position="none"
    )
  )
  plt2$scale = scale_fill_manual(
    values = c("#d87659","#e8a66c","#e3c577","#92b082","#4f9b8f","#3d7070","#2e4552"),
    labels=c("Dystopia + residual","Freedom to make life choices","GDP per capita","Generosity","Healthy life expectancy","Perceptions of corruption","Social support"))
  plt2$explain = list(
    labs(
      caption="資料來源：https://worldhappiness.report/"))
  plt2$make() -> plt2
}

plt1+plt2{
  library(patchwork)
  plt1+plt2+plot_layout(nrow = 2)
}

ggsave("Other.svg", width = 35, height = 40, units = "cm")
