
# Google sheets
googlesheets4::read_sheet(
  "https://docs.google.com/spreadsheets/d/1lkts4hLkrAFAobONFXiEjgDnuUmXKci6YF--vg1pC1s/edit?usp=sharing",
  sheet="data 1"
) -> data1

  geom_line(
    mapping=aes(
      x=Time,
      y=Index1
    ),
    color="black"
  )+
  geom_line(
    mapping=aes(
      x=Time,
      y=Index2
    ),
    color="red"
  )+
  geom_line(
    mapping=aes(
      
      x=Time,
      y=Index3
    ),
    color="green"
  )

###
# pivot wide data to long data
tidyr::pivot_longer(
  data=data1,
  cols=2:4,
  # column names to What
  names_to = "IndexName",
  values_to = "IndexValue"
) -> data1_long

ggplot(
  data=data1_long
)+
  geom_line(
    aes(
      x=Time,
      y=IndexValue,
      color=IndexName
    )
  )

