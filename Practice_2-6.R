moocs <- read.csv(file.choose(), header=T)

length (moocs$YoB)
range (moocs$YoB)
quantile (moocs$YoB, .10)
#table (moocs)

