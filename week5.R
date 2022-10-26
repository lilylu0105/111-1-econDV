plot2 = Plot(data.frame(x=c(1,2), y=c(5, -1)))
plot2$ggplot=ggplot(data=plot2$data)
plot2$geoms <- list(
  geom_point(
    aes(x = x, y = y),
    color = "blue",
    size = 9
  ),
  geom_line(
    aes(x = x, y = y)
  )
)
plot2$make()

