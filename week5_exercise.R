# download data ---------
googlesheets4::gs4_deauth()
googlesheets4::read_sheet(
  ss="https://docs.google.com/spreadsheets/d/1lkts4hLkrAFAobONFXiEjgDnuUmXKci6YF--vg1pC1s/edit#gid=1655384974",
  sheet="data 5"
) -> data5

#data
plot=Plot(data5)
plot$data

library(dplyr)

r=500
data5 |>
  mutate(
    x=gdppp,
    y=avgLifeSatisfaction,
    theta = pi*`high-low satisfaction disparity`
  ) |>
  mutate(
    ystart = y-r*sin(theta)/10000,
    yend= y+r*sin(theta)/10000,
    xend = x+r*cos(theta),
    xstart = x-r*cos(theta)
  ) -> data5a
head(data5a |> select(x,y,xstart, xend, ystart, yend))

plot$ggplot=ggplot(data=data5a)

plot$geoms = list(
  geom_point(
    aes(x=gdppp,
        y=avgLifeSatisfaction,
        fill="#012C3F")),
  geom_segment(
    aes(x = xstart,
        y = ystart,
        xend = xend,
        yend = yend,
        colours = "#012C3F"))
)

# make the plot
plot$make() 



