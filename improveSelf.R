googlesheets4::read_sheet(
  ss="https://docs.google.com/spreadsheets/d/1Vk4jo92pUJ6nWRPSRQuYIIgb6lpTPf2pJDqLBgEGEZI/edit?usp=sharing",
  sheet="sheet2-week13-improve")->PPIdata
#data:https://nstatdb.dgbas.gov.tw/dgbasall/webMain.aspx?sys=210&funid=A030701010
data{
  PPIdata[1:23,]->PPIdata
  factor(PPIdata$統計期, levels =
           c("110年1月", "110年2月", "110年3月", "110年4月",
             "110年5月", "110年6月","110年7月", "110年8月",
             "110年9月", "110年10月","110年11月", "110年12月",
             "111年1月", "111年2月", "111年3月", "111年4月",
             "111年5月", "111年6月","111年7月", "111年8月",
             "111年9月", "111年10月", "111年11月")) ->PPIdata$統計期

  library(dplyr)
  PPIdata|>
    group_by(統計期) |>
    summarise(
      總指數={(總指數 - 97.20)/97.20*100},
      農林漁牧業產品={(`一.農林漁牧業產品`-97.90)/97.90*100},
      農產品={(`1.農產品`-93.03)/93.03*100},
      禽畜產品={(`2.禽畜產品`-100.43)/100.43*100},
      林產品={(`3.林產品`-105.71)/105.71*100},
      水產品={(`4.水產品`-97.74)/97.74*100},
      土石及礦產品={(`二.土石及礦產品`-98.41)/98.41*100},
      石油及天然氣={(`1.石油及天然氣`-78.80)/78.80*100},
      土石採取及其他礦產品={(`2.土石採取及其他礦產品`-102.95)/102.95*100},
      製造業產品={(`三.製造業產品`-97.35)/97.35*100},
      食品及飼品={(`1.食品及飼品`-104.92)/104.92*100},
      飲料={(`2.飲料`-98.98)/98.98*100},
      菸類={(`3.菸類`-137.28)/137.28*100},
      紡織品={(`4.紡織品`-92.78)/92.78*100},
      成衣及服飾品={(`5.成衣及服飾品`-97.06)/97.06*100},
      皮革及其製品={(`6.皮革及其製品`-92.62)/92.62*100},
      木竹製品={(`7.木竹製品`-97.13)/97.13*100},
      `紙漿、紙、紙製品及印刷品`={(`8.紙漿、紙、紙製品及印刷品`-106.54)/106.54*100},
      石油及煤製品={(`9.石油及煤製品`-103.84)/103.84*100},
      化學材料及其製品與藥品={(`10.化學材料及其製品與藥品`-100.40)/100.40*100},
      橡膠及塑膠製品={(`11.橡膠及塑膠製品`-97.13)/97.13*100},
      非金屬礦物製品={(`12.非金屬礦物製品`-100.94)/100.94*100},
      基本金屬={(`13.基本金屬`-127.77)/127.77*100},
      金屬製品={(`14.金屬製品`-103.74)/103.74*100},
      電子零組件={(`15.電子零組件`-89.09)/89.09*100},
      `電腦、電子產品及光學製品`={(`16.電腦、電子產品及光學製品`-84.42)/84.42*100},
      電力設備及配備={(`17.電力設備及配備`-102.32)/102.32*100},
      機械設備={(`18.機械設備`-96.17)/96.17*100},
      運輸工具及零件={(`19.運輸工具及零件`-96.49)/96.49*100},
      家具及裝設品={(`20.家具及裝設品`-97.40)/97.40*100},
      雜項工業製品={(`21.雜項工業製品`-93.08)/93.08*100},
      水電燃氣={(`四.水電燃氣`-93.29)/93.29*100},
      水={(`1.水`-96.60)/96.60*100},
      電={(`2.電`-96.54)/96.54*100},
      燃氣={(`3.燃氣`-88.38)/88.38*100},
    )->PPIdata_rate
  tidyr::pivot_longer(
    data=PPIdata_rate,
    cols=4:7,
    # column names to What
    names_to = "subCategoryPPI",
    values_to = "PPIrate")|>
    select("統計期","subCategoryPPI","PPIrate")-> subCategoryPPI1
  tidyr::pivot_longer(
    data=PPIdata_rate,
    cols=9:10,
    # column names to What
    names_to = "subCategoryPPI",
    values_to = "PPIrate")|>
    select("統計期","subCategoryPPI","PPIrate")-> subCategoryPPI2
  tidyr::pivot_longer(
    data=PPIdata_rate,
    cols=12:32,
    # column names to What
    names_to = "subCategoryPPI",
    values_to = "PPIrate")|>
    select("統計期","subCategoryPPI","PPIrate")-> subCategoryPPI3
  tidyr::pivot_longer(
    data=PPIdata_rate,
    cols=34:36,
    # column names to What
    names_to = "subCategoryPPI",
    values_to = "PPIrate")|>
    select("統計期","subCategoryPPI","PPIrate")-> subCategoryPPI4
  tidyr::pivot_longer(
    data=PPIdata_rate,
    cols=c(3,8,11,33),
    # column names to What
    names_to = "mainCategoryPPI",
    values_to = "PPIrate")|>
    select("統計期","mainCategoryPPI","PPIrate")-> mainCategoryPPI
  bind_rows("農林漁牧業產品"=subCategoryPPI1,
            "土石及礦產品"=subCategoryPPI2,
            "製造業產品"=subCategoryPPI3,
            "水電燃氣"=subCategoryPPI4,
            .id="mainCategoryPPI")->bindsubCategoryPPI
  filter(bindsubCategoryPPI,統計期 == "111年11月")->bindsubCategoryPPI_11111
  filter(mainCategoryPPI,統計期 == "111年11月")->mainCategoryPPI_11111
}

plt1{
  plt1=Plot()
  plt1$ggplot = ggplot()
  plt1$geom = list(
    geom_line(data = bindsubCategoryPPI,
              aes(x = 統計期,
                  y = PPIrate,
                  group = subCategoryPPI),
              linewidth=1.8,
              colour="white",
    ),    
    geom_line(data = bindsubCategoryPPI,
               aes(x = 統計期,
                   y = PPIrate,
                   colour=subCategoryPPI,
                   group = subCategoryPPI),
              linewidth=1.2
               ),

    geom_text(data=bindsubCategoryPPI_11111,
              aes(y=PPIrate, label=subCategoryPPI),
              x="111年11月", hjust=1),
    facet_wrap(vars(mainCategoryPPI), nrow = 4,
               scales = "fixed",strip.position = "left")
  )
  plt1$others = list(
    xlab(NULL),
    ylab(c("生產者物價指數(%)")),
    scale_colour_manual(
      na.value = "#f0efeb",
      limits = c("燃氣", "水產品", "石油及煤製品"),
      values = c("#9b9b7d","#c68f67","#d2b097")
  ),
    theme(
      strip.background = element_blank(),
      strip.text = element_blank(),
      legend.position="none",
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank()
  ))
  plt1$explain = 
    labs(
      caption="資料來源：https://nstatdb.dgbas.gov.tw/dgbasall/webMain.aspx?sys=210&funid=A030701010"
    )
  plt1$make()
  plt1$make()->plt1
}

plt2{
  plt2=Plot()
  plt2$ggplot = ggplot()
  plt2$geom = list(
    geom_line(data=PPIdata_rate,
              aes(x=統計期,
                  y=總指數),
              group = 1,
              colour="#7a7d65",
              linewidth=1.5),
    geom_point(data=PPIdata_rate,
               aes(x=統計期,
                   y=總指數),
               colour="#7a7d65",
               size=2)
  )
  plt2$others = list(
    xlab(NULL),
    scale_x_discrete(labels=c("2021年1月", "2月", "3月", "4月", "5月",
                                "6月", "7月", "8月", "9月", "10月", "11月", "12月",
                                "2022年1月", "2月", "3月", "4月", "5月",
                                "6月", "7月", "8月", "9月", "10月", "11月")),
    theme(
      axis.line.y=element_blank(),
      axis.ticks.x=element_blank(),
      #axis.line.x=element_blank(),
      #axis.text.x=element_blank(),
    )
  )
  plt2$explain = 
    labs(
      title="生產者物價指數的變化主要是來自於哪些原物料的變動?",
      subtitle="以2021年1月為基期之%變化",
    )
  plt2$make()
  plt2$make()->plt2
}

plt3{
  plt3=Plot()
  plt3$ggplot = ggplot()
  plt3$geom = list(
    geom_line(data=mainCategoryPPI,
              aes(x = 統計期,
                  y = PPIrate,
                  colour = mainCategoryPPI,
                  group = mainCategoryPPI),
              linewidth=1.8),
    geom_text(data=mainCategoryPPI_11111,
              aes(y=PPIrate,
                  label=mainCategoryPPI),
              x="111年11月", hjust=1),
    facet_wrap(vars(mainCategoryPPI), nrow = 4)
  )
  plt3$others = list(
    xlab(NULL),
    ylab(NULL),
    scale_y_continuous(position = "right"),
    scale_colour_manual(
      values = c("#f0efeb","#9b9b7d","#c68f67","#d2b097")
    ),
    theme(
      strip.background = element_blank(),
      strip.text = element_blank(),
      legend.position="none",
      axis.ticks.x=element_blank(),
      #axis.line.x=element_blank(),
      axis.text.x=element_blank()
    ))
  plt3$make()
  plt3$make()->plt3
}

plt1+plt2{
  library(patchwork)
  design <- c(
    area(1, 1, 1, 4),
    area(2, 1, 3, 1),
    area(2, 2, 3, 4)
  )
  plt2+plt3+plt1+ plot_layout(design = design)
}

ggsave("self.svg", width = 35, height = 40, units = "cm")
