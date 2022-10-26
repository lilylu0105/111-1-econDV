# download data ---------
googlesheets4::gs4_deauth()
googlesheets4::read_sheet(
  ss="https://docs.google.com/spreadsheets/d/1lkts4hLkrAFAobONFXiEjgDnuUmXKci6YF--vg1pC1s/edit#gid=1655384974",
  sheet="data 5"
) -> data5

#data
plot=Plot(data5)
plot$data


#angle <- `high-low satisfaction disparity`*360
#angle
x <- (plot$data[[3]]-sqrt(1-plot$data[[4]]**2))*1000
y <- plot$data[[3]]-(plot$data[[4]]/2)
xend <-(plot$data[[3]]+sqrt(1-plot$data[[4]]**2))*1000
yend <- plot$data[[3]]+(plot$data[[4]]/2)

plot$ggplot=ggplot(data=plot$data)

plot$geoms = list(
  geom_point(
    aes(x=gdppp,
        y=avgLifeSatisfaction,
        fill="#012C3F")),
  geom_segment(
    aes(x = x,
        y = y,
        xend = xend,
        yend = yend,
        colour = "#012C3F"))
)

# make the plot
plot$make() 
