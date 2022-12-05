data<-read.table("airqualityuci.csv",header=TRUE,sep=";",stringsAsFactors=TRUE)

attach(data)
summary(data)
library(visdat)
vis_miss(data)
vis_dat(data)
library(naniar)
# https://upset.app/
gg_miss_upset(data)
detach(data)
