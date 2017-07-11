### Fixing dataset ###

library(readr)
matrix <- read_csv("~/Dropbox/A.leari/modelo censos/matrix-censos-CEMAVE_3_one population.csv",col_types = cols(date = col_date(format = "%Y-%m-%d")))


str(matrix)

mid.month.day <- function(month){
  day <- paste(month,"-","15",sep="")
  day <- strptime(day,"%m-%d")
  day <- as.integer(floor(julian(day, origin="2016-12-31")))
  return(day)
}

matrix$julian <- mid.month.day(matrix$month)
matrix <- as.data.frame(matrix)
matrix <- matrix[,c(3:5,7,9)]

matrix
head(matrix)

## prepare data for unmarked #
library(unmarked)


# observation data. this is an MxJ matrix of count data, where M is the number
site.name <- as.factor(unique(paste(matrix[,2],matrix[,3])))
sites <- unique(matrix[,c(2,3)])
rownames(sites) <- seq(length=nrow(sites))

y <- matrix(NA,34,6)
times <- matrix(NA,34,6)
months <- matrix(NA,34,6)

for (i in 1:34){
  counts <- matrix[which(matrix$year==sites[i,1]&matrix$month==sites[i,2]),4]
  time <- matrix[which(matrix$year==sites[i,1]&matrix$month==sites[i,2]),1]
  month <- matrix[which(matrix$year==sites[i,1]&matrix$month==sites[i,2]),3]
  for(t in 1:length(counts)){
    y[i,t] <- counts[t]
    times[i,t] <- time[t]
    months[i,t] <- month[t]
  }
}
y
times 
months 

months <- matrix(as.factor(months),34,6)

obs.covs <- list(times=times,months=months)

site.covs <- data.frame(year=as.factor(sites[[1]]),month=as.factor(sites[[2]]),julian=matrix$julian[as.integer(row.names(unique(matrix[,c(2,3)])))])



umf1<- unmarkedFramePCount(y=y,siteCovs = site.covs,obsCovs = obs.covs)
summary(umf1)
