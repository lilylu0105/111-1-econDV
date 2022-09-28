data0=data.frame(
  x=c(1, 2, 3, 4),
  y=c(2, 3, 6, 7),
  alpha_stroke=c("A","B","A","B")
)
split(data0,data0$alpha_stroke) -> data_split
data_split


# data cleaning /manipulation
ggplot()+
  geom_point( # 透明組
    data=data_split$A,
    aes(
      x=x,y=y
    ),
    alpha=0.1,
    color="blue",
    size=3
  )+
  geom_point(
    data=data_split$B,
    aes(
      x=x, y=y
    ),
    shape=21,
    size=3,
    fill="blue", # 填色
    color="black" # stroke顏色
  )
