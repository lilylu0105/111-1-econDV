df <- data.frame(
  country=c("a","b","c","d","e","f","g"),
  shareProfitable=c(0.28,0.19,0.6,0.68,0.68,0.77,0.39),
  shareNonProfita=c(-0.72,-0.81,-0.4,-0.32,-0.32,-0.23,-0.61),
  Volumn=c(10293,29384,2923,19280,5029,1299,19220),
  isOPEC=c(T,T,F,T,F,F,T)
)

ggplot(
  data=df,
  aes(
    fill=isOPEC
    ,width = `Volumn`*0.00005
  )
)+
  geom_col(
    aes(
      x=country,
      y=shareProfitable
    )
  )+
  geom_col(
    aes(
      x=country,
      y=shareNonProfita
    ),
    linetype=5,
    alpha=0.2,
    linetypes="longdash"
  )+
  geom_text(
    aes(
      label= country,
      x=country,
      y=shareProfitable
    )
  )+
  geom_hline(yintercept = 0)


