# Updated 
#install.packages("fitdistrplus")
library(fitdistrplus)

funcMakeDate = function(x, mo = 1, da = 1){
  return (as.numeric(strptime(paste(x,mo,da, sep ="-"), format = "%Y-%m-%d")))
}

setwd("Q:/Projects/FEMA/Utah_Lake/400 - Technical/FreqAnalysis")
daily_csv = read.csv("DailyElev_2004_to_2019.csv")
daily_dts_str = paste(daily_csv$Year,daily_csv$Month, daily_csv$Day,sep = "-")
daily_dts = strptime(daily_dts_str, format = "%Y-%m-%d")

comp_elev = 4489.045

plot(daily_dts, daily_csv$Elev,
	'b', cex = 0.4,
	xlab = "Years",
	ylab = "Lake Elev (ft) [NAVD 88]",
	main = "Daily Data from \nUTAH LAKE STORAGE CONTENT (GAGE READING)")
lines(daily_dts, daily_csv$Elev, col = 'black', lwd = 2)
points(daily_dts, daily_csv$Elev, cex = 0.3)
abline(h = comp_elev, lwd = 2, lty = 2)
for (i in 2004:2019) {abline (v = funcMakeDate(i), col = 'lightgray', lty = 3)}
for (i in 4400:4600) {abline (h = i, col = 'lightgray', lty = 3)}

#install.packages("zoo")
library(zoo)
# used Aggregate functionality of "zoo" package to find yearly / Monthly maximum and averages
daily_df = zoo(daily_csv$Elev, daily_dts)

yr <- function(x)format(x, '%Y')
yrly_max = aggregate(daily_df, by = yr, FUN = max,na.rm=T)# na.action = "na.omit")
yrly_max[is.infinite(yrly_max)] = NA

# To find which month had the maximum lake elevation for each year
# We will use the long term data to find duration curve

#hist(daily_csv$Month[match(yrly_max, daily_csv$Elev)], breaks = 0:12, xlab = "Months", ylab = "Number of max values",
		 main = "Which month had maximum lake elevation \n over years?")
#box()
#grid()
#hist(daily_csv$Month[match(yrly_max, daily_csv$Elev)], breaks = 0:12,add= T, col = 'lightgray')

# Monthly Max
month <- function(x)format(x, '%Y.%m')
mon_max = aggregate(daily_df, by = month, FUN = max,na.rm=T)# na.action = "na.omit")
mon_max[is.infinite(mon_max)] = NA

# Monthly Avg
mon_avg = aggregate(daily_df, by = month, FUN = mean,na.rm=T)# na.action = "na.omit")
mon_avg[is.infinite(mon_avg)] = NA

# plot Monthly avg and Max
par(mar = c(5, 4, 4, 4) + 0.3) 
plot(1:length(mon_max), mon_max, cex = 0.3, 'p',xlab = "", ylab = "Monthly values (ft)", axes = F, lwd = 2,
	ylim = c(4472, 4492))
lines(1:length(mon_max), mon_max, 'l', lwd = 1)
axis(1, at = seq(1,length(mon_max),12), index(mon_max)[seq(1,length(mon_max),12)], las = 2)
axis(2)
box()
for (i in seq(1,length(mon_max),12)){
	abline(v = i, lty = 2, col = 'lightgray')
}
for (i in seq(4482,4600,1)){
	abline(h = i, lty = 2, col = 'lightgray')
}
lines(1:length(mon_avg), mon_avg, cex = 0.3,'l', col = 'blue', lwd =1)
legend('topright', legend = c("Max", "Avg"), lty = c(1,1), col = c("black", "blue"), bg = 'white')
par(new = TRUE)
plot(1:length(mon_max), mon_max - mon_avg,  type = 'l', axes = F, bty = 'n', xlab = '', ylab ='',
	ylim = c(0, 2), col = "darkgray")
axis(side =4, at = c(0, 0.5, 1))
mtext("Max - Avg", side=4, line=3, adj = 0)
abline(h = mean(mon_max - mon_avg), lty = 2, col = 'darkgray', lwd = 2)
abline(h = 0.5, lty =3, col = 'lightgray')


cor_3 = cor(mon_avg[1:(length(mon_avg)-3)], mon_avg[4:length(mon_avg)])

windows(10,7)
acf_ = acf(as.numeric(mon_avg), na.action = na.pass,
	lag.max = 40, main = "Autocorrelation among mean monthly values", xlab = "Lag (in months)")
print (acf_$acf[3])
grid()
lines(x=c(3,3), y = c(0,acf_$acf[3]), col = 'red', lwd = 3)






# plot Monthly max - Monthly Avg
diff_mon = mon_max - mon_avg # This is called "Wind Setup" (USACE report 1990 Pg 3)
plot(1:length(diff_mon), diff_mon, cex = 0.4, 'p',xlab = "",
	 ylab = "Max - Avg Monthly values (ft)",
	 axes = F, lwd = 2,
	 main = "Fluctuations (Wind Setup)")
lines(1:length(diff_mon), diff_mon, col = 'darkgray')
points(1:length(diff_mon), diff_mon, lwd = 2, cex =0.3)
axis(1, at = seq(1,length(mon_max),12*1), index(mon_max)[seq(1,length(mon_max),12*1)], las = 2)
axis(2)
box()
for (i in seq(1,length(mon_max),12)){
	abline(v = i, lty = 2, col = 'lightgray')
}
for (i in seq(0,0.7,0.1)){
	abline(h = i, lty = 2, col = 'lightgray')
}



print (acf(as.numeric(diff_mon), na.action = na.pass, lag.max = 40))

dt_mon = strptime(paste(index(mon_max),".15",sep=""),"%Y.%m.%d")

mon_df = data.frame(dt_mon, format(dt_mon, "%m"),mon_max, diff_mon, mon_avg)
colnames(mon_df) = c("Date","Mon","Mon_Max", "Diff_mon", "Mon_Avg")
mon_df$Mon = as.numeric(levels(mon_df$Mon)[mon_df$Mon])

for (mon in 1:12){
	mon_ind = which(mon_df$Mon == mon)
	if (mon == 1){
		plot(1:length(mon_ind), mon_df$Mon_Max[mon_ind],
			ylim = c(4480, 4491),
			 'l',cex = 0.4, lwd = 2, col = mon)
	} else {
		lines(1:length(mon_ind), mon_df$Mon_Max[mon_ind], 'l',cex = 0.4, lwd = 2, col = mon)
	}
}
#legend('topright', rep("",12), col = 1:12, lwd =2, lty =1 )
grid()

print (acf(as.numeric(mon_df$Diff_mon), na.action = na.pass, lag.max = 100, main = "ACF for all months"))


## Frequency analysis on the "diff" data
diff_mon = mon_df$Diff_mon
plot(1:length(diff_mon), diff_mon, "p", lwd =2, cex = 0.5)
lines(1:length(diff_mon), diff_mon, col = 'gray')
grid()

hist(diff_mon, n = 20)
hist(diff_mon, probability = T, xlim = c(0,1), breaks = seq(0,1,0.02))


plot(density(diff_mon), xlim = c(-0.5,1),
	#ylim = c(0,4),
	lwd =2, xlab = "", main = "Density comparison with Normal")
lines(density(rnorm(1000, mean = mean(diff_mon), sd = sd(diff_mon))), col = 'red')
lines(density(diff_mon), lwd =4)
legend('topright', legend = c("Normal", "Fluc"), lty = c(1,1), lwd = c(1,2), col = c("red", "black"), bg = 'white')
grid()

qqnorm(diff_mon, cex = 0.4,lwd = 2, main = "Normal Q-Q plot of Fluctuations")
qqline(diff_mon, col = 'red', lwd = 2, lty = 2)
grid()

diff_mon = sort(as.numeric(diff_mon))
rank_ = rank(sort(as.numeric(diff_mon), decreasing = T), ties.method = "last")
exc_prob_qi = rank_/(length(diff_mon)+1)
non_exc_prob_pi = 1 - exc_prob_qi
Tp = 1/(1-non_exc_prob_pi)

plot(Tp, diff_mon,'p', lwd =3,
	 xlab = "Return Period (months)",
	 ylab = "Fluctuations (Delta H)",
	 main = "Frequency Analysis of monthly fluctuation \n(Assuming Gumbel Distribution)")
grid()

write.csv(data.frame(exc_prob_qi, diff_mon), file = "Fluc_diff.csv")




# -----------------------------------------------------------

## Create duration curve with full monthly data
monthly_csv = read.csv("UtahLakeHistorical_Distribution.csv")
mon_avg = monthly_csv$Elevation
xx = sort(mon_avg,decreasing = T)
# Plot 1
exc_time = order(sort(xx))*100/length(xx)
plot(exc_time, xx, type = 'b', lwd = 2, cex = 0.6,
	main = "Duration curve for Monthly mean flows",
	 xlab = "Percent of time exceeded", ylab = "Monthly average (ft)")
lines(exc_time, xx, col = 'lightgray')
points(exc_time, xx, lwd = 2, cex = 0.4)
grid()

acf_ = acf(as.numeric(mon_avg), na.action = na.pass,lag.max = 100, main = "ACF for all monthly values")
print (acf_)
lines(x=c(3,3), y = c(0,acf_$acf[3+1]), col = 'red', lwd = 3)

plot(as.numeric(mon_avg[1:(length(mon_avg)-3)]), as.numeric(mon_avg[4:length(mon_avg)]), xlab = "Monthly Avg", ylab = "Monthly Avg (lagged by 3 months)", cex = 0.5, lwd =2)
grid()
abline(0,1, lty = 2, lwd = 2, col = "blue")
text(4480, 4491, round(acf_$acf[3+1],2))


#install.packages("zoo")
library(zoo)
# used Aggregate functionality of "zoo" package to find yearly / Monthly maximum and averages

monthly_dts_str = paste(monthly_csv$Year,monthly_csv$Month, monthly_csv$Day,sep = "-")
monthly_dts = strptime(monthly_dts_str, format = "%Y-%m-%d")
ind = 1:length(monthly_dts)
#ind = 1:40
plot(monthly_dts[ind], monthly_csv$Elev[ind], cex = 0.2, lwd = 1, 'p',
	xlab = "Years",
	ylab = "Elev")
#	xlim = c(as.numeric(monthly_dts[1]), as.numeric(monthly_dts[20])))

lines(monthly_dts, monthly_csv$Elev, col = 'black', lwd = 1)
abline(h = 4493.8385, col = 'red', lty = 2, lwd =2)

for (i in 1930:2019) {abline (v = funcMakeDate(i), col = 'lightgray', lty = 3)}
for (i in 4400:4600) {abline (h = i, col = 'lightgray', lty = 3)}
abline(h = comp_elev, lwd = 2, lty = 2)
lines(monthly_dts, monthly_csv$Elev, col = 'black')

lines(monthly_dts[4:length(monthly_dts)], monthly_csv$Elev[1:(length(monthly_dts)-3)],
	col = 'darkgray',
	type = "l",
	cex = 0.5,
	lwd = 2, 
	lty = 2)

lines(monthly_dts[4:length(monthly_dts)], monthly_csv$Elev[1:(length(monthly_dts)-3)],
	col = 'darkgray',
	type = "b",
	cex = 0.5,
	lwd = 2, 
	lty = 2)
legend('topright', legend = c("Monthly avg", "Lagged by 3 months"),
	lwd = c(2,2),
	#cex = c(0.5, 0.5),
	lty = c(1,1),
	col = c("black", "darkgray"))

monthly_df = zoo(monthly_csv$Elev, monthly_dts)

yr <- function(x)format(x, '%Y')
yrly_max = aggregate(monthly_df, by = yr, FUN = max,na.rm=T)# na.action = "na.omit")
yrly_max[is.infinite(yrly_max)] = NA

hist(monthly_csv$Month[match(yrly_max, monthly_csv$Elev)], breaks = 0:12, xlab = "Months", ylab = "Number of max values",
		 main = "Which month had maximum lake elevation \n over years?")
box()
grid()
hist(monthly_csv$Month[match(yrly_max, monthly_csv$Elev)], breaks = 0:12, add= T, col = 'lightgray')


dt_mon = strptime(paste(monthly_csv$Year, ".", monthly_csv$Month,".15",sep=""),"%Y.%m.%d")
mon_avg = monthly_csv$Elevation
mon_max = monthly_csv$Elevation
diff_mon = mon_max - mon_avg
mon_df = data.frame(dt_mon, format(dt_mon, "%m"), mon_max, diff_mon, mon_avg)

colnames(mon_df) = c("Date","Mon","Mon_Max", "Diff_mon", "Mon_Avg")
mon_df$Mon = as.numeric(levels(mon_df$Mon)[mon_df$Mon])

mon = c(5)
mon_ind = which(mon_df$Mon == mon)
plot(mon_df$Date[mon_ind], mon_df$Mon_Avg[mon_ind], 'l', cex = 0.4, lwd =2,
	xlab = "Years",
	ylab = "Elev (ft)",
	main = paste("Month = ", mon, sep =""))
lines(mon_df$Date[mon_ind], mon_df$Mon_Avg[mon_ind], 'p')
grid()


mon = c(4,5 )
mon_ind = which(mon_df$Mon == mon)
mon_max_levels = as.numeric(mon_df$Mon_Max[mon_ind])
mon_avg_levels = as.numeric(mon_df$Mon_Avg[mon_ind])


print (acf(mon_avg_levels, na.action = na.pass, lag.max = 90,
	 main = "ACF for May Mean values"))
grid()

mon_vals_sorted_D = sort(mon_avg_levels, decreasing = T)
exc_time = order(sort(mon_avg_levels))*100/length(mon_avg_levels)
write.csv(data.frame(exc_time, mon_vals_sorted_D), file = "monthly_duration_curve.csv")

# For a complete flow duration curve where the total probability adds to 100%, it should start from 0 and end at 100
# However, in the above "exc_time" value ranges between 0.94 to 100
# Since this will cause problems further down, we assumed that the mean lake elevation will never exceed this highest lake
# elevation from the dataset. So, we replaced the first "exc_time" with value 0 (i.e. 0(%)). 
# This assumuption increases the probability of reaching the elevation of highest recorded elevation (from 1% to about 2%). 
# And we consider this assumption to be better than extrapolating the data.

exc_time[1] = 0
xx = mon_vals_sorted_D

# Plot 1
plot(exc_time, xx, type = 'b', lwd = 2, cex = 0.6,
	main = "Duration curve for Monthly mean flows",
	 xlab = "Percent of time exceeded", ylab = "Monthly average (4&5)")
lines(exc_time, xx, col = 'lightgray')
points(exc_time, xx, lwd = 2, cex = 0.4)
grid()


dc_avg = c(
4493.825,
4493.07,
4492.094286,
4490.393344,
4489.78103,
4489.403711,
4488.792546,
4487.703522,
4484.581375
)

abline(v = c(1,3,6,10,15,25,40,60,100), lty = 2, lwd = 2, col = 'blue')
lines(c(0,1), rep(dc_avg[1],2))
lines(c(1,3), rep(dc_avg[2],2))
lines(c(3,6), rep(dc_avg[3],2))
lines(c(6,10), rep(dc_avg[4],2))
lines(c(10,15), rep(dc_avg[5],2))
lines(c(15,25), rep(dc_avg[6],2))
lines(c(25,40), rep(dc_avg[7],2))
lines(c(40,60), rep(dc_avg[8],2))
lines(c(60,100), rep(dc_avg[9],2))



































#install.packages("fitdistrplus")
#library(fitdistrplus)
#fit.weibull = fitdist(x, "weibull")
#plot(fit.norm)
#windows()
#fit.norm = fitdist(x, "norm")
#plot(fit.norm,col='green')

#fit.lnorm = fitdist(x, "lnorm", method = "mme")
#plot(fit.lnorm,col='pink')



setwd("C:/Users/sulochan.dhungel/Sulochan/SL_Freq_Analysis")
tt = read.csv("Saltair_common.csv")
dts_str = paste(tt$Year,tt$Month, tt$Day,sep = "-")
dts = strptime(dts_str, format = "%Y-%m-%d")

plot(dts, tt$SaltairElev, 'b', cex = 0.3, xlab = "Years", ylab = "Lake Elev (ft) [NAVD 88]", main = "GREAT SALT LAKE AT SALTAIR BOAT HARBOR, UT \n USGS 10010000")
lines(dts, tt$SaltairElev, col = 'gray')
points(dts, tt$SaltairElev, cex = 0.3)
grid()

#install.packages("zoo")
# used Aggregate functionality of "zoo" package to find yearly / Monthly maximum and averages
library(zoo)
df = zoo(tt$SaltairElev,dts)

yr <- function(x)format(x, '%Y')
yrly_max = aggregate(df, by = yr, FUN = max,na.rm=T)# na.action = "na.omit")
yrly_max[is.infinite(yrly_max)] = NA

# To find which month had the maximum lake elevation for each year
hist(tt$Month[match(yrly_max, tt$SaltairElev)], breaks = 0:12, xlab = "Months", ylab = "Number of max values", main = "Which month had maximum lake elevation \n over years?")
box()
grid()
hist(tt$Month[match(yrly_max, tt$SaltairElev)], breaks = 0:12,add= T, col = 'lightgray')
# Monthly Max
month <- function(x)format(x, '%Y.%m')
mon_max = aggregate(df, by = month, FUN = max,na.rm=T)# na.action = "na.omit")
mon_max[is.infinite(mon_max)] = NA

# Monthly Avg
mon_avg = aggregate(df, by = month, FUN = mean,na.rm=T)# na.action = "na.omit")
mon_avg[is.infinite(mon_avg)] = NA

# plot Monthly avg and Max
plot(1:length(mon_max), mon_max, cex = 0.3, 'p',xlab = "", ylab = "Monthly values (ft)", axes = F, lwd = 2)
lines(1:length(mon_max), mon_max, 'l')
axis(1, at = seq(1,length(mon_max),12*5), index(mon_max)[seq(1,length(mon_max),12*5)], las = 2)
axis(2)
box()
for (i in seq(1,length(mon_max),12)){
	abline(v = i, lty = 2, col = 'lightgray')
}
for (i in seq(4195,4215,1)){
	abline(h = i, lty = 2, col = 'lightgray')
}
lines(1:length(mon_avg), mon_avg, cex = 0.3,'l', col = 'blue')
legend('topright', legend = c("Max", "Avg"), lty = c(1,1), col = c("black", "blue"), bg = 'white')


cor_3 = cor(mon_avg[1:(length(mon_avg)-3)], mon_avg[4:length(mon_avg)])
cor_5 = cor(mon_avg[1:(length(mon_avg)-5)], mon_avg[6:length(mon_avg)])
cor_10 = cor(mon_avg[1:(length(mon_avg)-10)], mon_avg[11:length(mon_avg)])
cor_20 = cor(mon_avg[1:(length(mon_avg)-20)], mon_avg[21:length(mon_avg)])

windows(10,7)
acf_ = acf(as.numeric(mon_avg), na.action = na.pass, lag.max = 100, main = "Autocorrelation among mean monthly values", xlab = "Lag (in months)")
print (acf_$acf[3])
grid()
lines(x=c(3,3), y = c(0,acf_$acf[3]), col = 'red', lwd = 3)

#windows(10,7)
print (acf(as.numeric(mon_avg), na.action = na.pass, lag.max = 100, main = "Autocorrelation among mean monthly values", xlab = "Lag (in months)"))
grid()
lines(x=c(5,5), y = c(0,acf_$acf[5]), col = 'red', lwd = 3)

#windows(10,7)
print (acf(as.numeric(mon_avg), na.action = na.pass, lag.max = 100, main = "Autocorrelation among mean monthly values", xlab = "Lag (in months)"))
grid()
lines(x=c(10,10), y = c(0,acf_$acf[10]), col = 'red', lwd = 3)

#windows(10,7)
print (acf(as.numeric(mon_avg), na.action = na.pass, lag.max = 100, main = "Autocorrelation among mean monthly values", xlab = "Lag (in months)"))
grid()
lines(x=c(20,20), y = c(0,acf_$acf[20]), col = 'red', lwd = 3)

windows(7,7)
plot(as.numeric(mon_avg[1:(length(mon_avg)-3)]), as.numeric(mon_avg[4:length(mon_avg)]), xlab = "Monthly Avg", ylab = "Monthly Avg (lagged by 3 months)", cex = 0.5, lwd =2)
grid()
abline(0,1, lty = 2, lwd = 2, col = "blue")
text(4197, 4215, round(acf_$acf[3],2))


plot(as.numeric(mon_avg[1:(length(mon_avg)-5)]), as.numeric(mon_avg[6:length(mon_avg)]), xlab = "Monthly Avg", ylab = "Monthly Avg (lagged by 5 months)", cex = 0.5, lwd =2)
grid()
abline(0,1, lty = 2, lwd = 2, col = "blue")
text(4197, 4215, round(acf_$acf[5],2))


plot(as.numeric(mon_avg[1:(length(mon_avg)-10)]), as.numeric(mon_avg[11:length(mon_avg)]), xlab = "Monthly Avg", ylab = "Monthly Avg (lagged by 10 months)", cex = 0.5, lwd =2)
grid()
abline(0,1, lty = 2, lwd = 2, col = "blue")
text(4197, 4215, round(acf_$acf[10],2))


plot(as.numeric(mon_avg[1:(length(mon_avg)-20)]), as.numeric(mon_avg[21:length(mon_avg)]), xlab = "Monthly Avg", ylab = "Monthly Avg (lagged by 20 months)", cex = 0.5, lwd =2)
grid()
abline(0,1, lty = 2, lwd = 2, col = "blue")
text(4197, 4215, round(acf_$acf[20],2))










# plot Monthly max - Monthly Avg
diff_mon = mon_max - mon_avg # This is called "Wind Setup" (USACE report 1990 Pg 3)
plot(1:length(diff_mon), diff_mon, cex = 0.3, 'p',xlab = "", ylab = "Max - Avg Monthly values (ft)", axes = F, lwd = 2, main = "Wind Setup")
lines(1:length(diff_mon), diff_mon, col = 'gray')
points(1:length(diff_mon), diff_mon, lwd = 2, cex =0.3)
axis(1, at = seq(1,length(mon_max),12*5), index(mon_max)[seq(1,length(mon_max),12*5)], las = 2)
axis(2)
box()
for (i in seq(1,length(mon_max),12)){
	abline(v = i, lty = 2, col = 'lightgray')
}
for (i in seq(0,0.7,0.1)){
	abline(h = i, lty = 2, col = 'lightgray')
}


print (acf(as.numeric(diff_mon), na.action = na.pass, lag.max = 100))


dt_mon = strptime(paste(index(mon_max),".15",sep=""),"%Y.%m.%d")

mon_df = data.frame(dt_mon, format(dt_mon, "%m"),mon_max, diff_mon, mon_avg)
colnames(mon_df) = c("Date","Mon","Mon_Max", "Diff_mon", "Mon_Avg")
mon_df$Mon = as.numeric(levels(mon_df$Mon)[mon_df$Mon])

for (mon in 1:12){
	mon_ind = which(mon_df$Mon == mon)
	if (mon == 1){
		plot(1:length(mon_ind), mon_df$Mon_Max[mon_ind],
			ylim = c(4480, 4491),
			 'l',cex = 0.4, lwd = 2, col = mon)
	} else {
		lines(1:length(mon_ind), mon_df$Mon_Max[mon_ind], 'l',cex = 0.4, lwd = 2, col = mon)
	}
}
#legend('topright', rep("",12), col = 1:12, lwd =2, lty =1 )
grid()

mon_ind = which(mon_df$Mon == 5 | mon_df$Mon == 6)
print (acf(as.numeric(mon_df$Mon_Max[mon_ind]), na.action = na.pass, lag.max = 100, main = "ACF for May & June Max values"))
print (acf(as.numeric(mon_df$Diff_mon[mon_ind]), na.action = na.pass, lag.max = 100, main = "ACF for May & June Fluctuation values"))

mon = c(5,6)
mon_ind = which(mon_df$Mon == mon)
mon_max_levels = as.numeric(mon_df$Mon_Max[mon_ind])
mon_avg_levels = as.numeric(mon_df$Mon_Avg[mon_ind])

print (acf(as.numeric(mon_df$Mon_Avg[mon_ind]), na.action = na.pass, lag.max = 100, main = "ACF for May & June Mean values"))


## Frequency analysis on the "diff" data
diff_mon = mon_df$Diff_mon[mon_ind]
plot(1:length(diff_mon), diff_mon)
hist(diff_mon, n = 20)
hist(diff_mon, probability = T, xlim = c(0,1), breaks = seq(0,1,0.02))

plot(density(diff_mon), xlim = c(-0.5,1),
	#ylim = c(0,4),
	lwd =2, xlab = "", main = "Density comparison with Normal")
lines(density(rnorm(1000, mean = mean(diff_mon), sd = sd(diff_mon))), col = 'red')
lines(density(diff_mon), lwd =4)
legend('topright', legend = c("Normal", "Fluc"), lty = c(1,1), lwd = c(1,2), col = c("red", "black"), bg = 'white')
grid()

qqnorm(diff_mon, cex = 0.4,lwd = 2, main = "Normal Q-Q plot of Fluctuations")
qqline(diff_mon, col = 'red', lwd = 2, lty = 2)
grid()

diff_mon = sort(as.numeric(diff_mon))
rank_ = rank(sort(as.numeric(diff_mon), decreasing = T), ties.method = "last")
exc_prob_qi = rank_/(length(diff_mon)+1)
non_exc_prob_pi = 1 - exc_prob_qi
Tp = 1/(1-non_exc_prob_pi)

plot(Tp, diff_mon,'p', lwd =3,
	 xlab = "Return Period (months)",
	 ylab = "Fluctuations (Delta H)",
	 main = "Frequency Analysis of monthly fluctuation \n(Assuming Gumbel Distribution)")
grid()

# If needed, we can check this result (i.e. Tp ~ diff_mon) distribution with the theoritical Gumbel distribution


mon_vals_sorted_D = sort(mon_avg_levels, decreasing = T)
exc_time = order(sort(mon_avg_levels))*100/length(mon_avg_levels)

# For a complete flow duration curve where the total probability adds to 100%, it should start from 0 and end at 100
# However, in the above "exc_time" value ranges between 0.94 to 100
# Since this will cause problems further down, we assumed that the mean lake elevation will never exceed this highest lake
# elevation from the dataset. So, we replaced the first "exc_time" with value 0 (i.e. 0(%)). 
# This assumuption increases the probability of reaching the elevation of highest recorded elevation (from 1% to about 2%). 
# And we consider this assumption to be better than extrapolating the data.

exc_time[1] = 0
xx = mon_vals_sorted_D
# Plot 1
plot(exc_time, xx, type = 'b', lwd = 2, cex = 0.6,
	main = "Duration curve for Monthly mean flows",
	 xlab = "Percent of time exceeded", ylab = "Monthly average (5&6)")
lines(exc_time, xx, col = 'lightgray')
points(exc_time, xx, lwd = 2, cex = 0.4)
grid()

abline(v = c(1,3,6,10,15,25,40,60,100), lty = 2, lwd = 2, col = 'blue')
lines(c(0,1), rep(4214.701,2))
lines(c(1,3), rep(4214.561,2))
lines(c(3,6), rep(4213.259,2))
lines(c(6,10), rep(4211.265,2))
lines(c(10,15), rep(4208.534,2))
lines(c(15,25), rep(4205.975,2))
lines(c(25,40), rep(4204.448,2))
lines(c(40,60), rep(4203.179,2))
lines(c(60,100), rep(4199.133,2))










per_time_exc_df = data.frame(exc_time, xx)
colnames(per_time_exc_df) = c("Per_time_Exc", "MonMean")

probs_needed = sort(c(0.1, 1, 2, 5, 10, 15, 25, 50, 80, 90, 95, 99),decreasing = T)
monelev_interpolated = approx(per_time_exc_df$Per_time_Exc,per_time_exc_df$MonMean, probs_needed, rule =2)$y


# Plot 1 shows the elevation duration curve of monthly means (May and June)

xx_from = xx[2:length(xx)]
xx_to = xx[1:length(xx)-1]
xx_index = 1:(length(xx)-1)
xx_val = (xx_from + xx_to)/2
xx_prob = (exc_time[2:length(xx)] - exc_time[1:length(xx)-1])/100


probs = 1/Tp
plot(probs*100, diff_mon,'p', lwd =3, xlab = "Exceedance Prob", ylab = "Fluctuations (Delta H)")

exc_prob_df = data.frame(probs*100, diff_mon)
colnames(exc_prob_df) = c("Exc_prob", "Fluct")

probs_needed = c(0.2, 0.5, 1, 2, 5, 10, 20, 50, 80, 90, 95, 99)
elevs_interpolated = approx(exc_prob_df$Exc_prob, exc_prob_df$Fluct, probs_needed, rule =2)$y


cond_prob_mat_curve = matrix(NA, ncol = length(xx_val), nrow = length(probs))
rownames(cond_prob_mat_curve) = probs
colnames(cond_prob_mat_curve) = xx_val
for (row_ctr in 1:length(probs)){
	cond_prob_mat_curve[row_ctr,] = xx_val + diff_mon[row_ctr]
}

plot(probs, cond_prob_mat_curve[,1], ylim = c(min(cond_prob_mat_curve), max(cond_prob_mat_curve)), 'l')
grid()
for (i in 2:ncol(cond_prob_mat_curve)){
	lines(probs, cond_prob_mat_curve[,i])
}

elevs_reqd = seq(min(cond_prob_mat_curve), max(cond_prob_mat_curve), 0.1)

prob_mat = matrix(NA, ncol = length(xx_val), nrow = length(elevs_reqd))
colnames(prob_mat) = xx_val
rownames(prob_mat) = elevs_reqd








plot(Tp, tail(xx,1)[1] + diff_mon, ylim = c(4195,4215), xlab = "Return Period", ylab = "Monthly averages")
grid()
for (i in 2:length(xx)){
	lines(Tp, xx[i] + diff_mon, col = "lightgray", 'l')
}















prob_ = c()
for (i in 1:length(mon_vals_sorted_D)){
	print (mon_vals_sorted_D[i])
	if (i == 1){
		prob = ((exc_time[i+1] - exc_time[i])/2)/100
	} else {
		if (i == length(mon_vals_sorted_D)){
			prob = ((exc_time[i] - exc_time[i-1])/2)/100
		} else {
			print (i)
			print (exc_time[i-1])
			print (exc_time[i])
			print (exc_time[i+1])
			print ("-")
			prob = (((exc_time[i+1] - exc_time[i])/2) + ((exc_time[i] - exc_time[i-1])/2))/100
			print (prob)
			print ("--==--")
		}
	}
	print (exc_time[i])
	prob_ = c(prob_, prob)
}
 





























plot(1/Tp, diff_mon)
grid()


plot(log(exc_prob), diff_mon)


rec_int = order(as.numeric(diff_mon))/ (length(diff_mon)+1)
plot(log(1/sort(rec_int)), (sort(as.numeric(diff_mon))))

mon = c(6)
mon_ind = which(mon_df$Mon == mon)
x = as.numeric(mon_df$Mon_Max[mon_ind])
pp = x*100/(length(x)+1)
plot(1:length(pp),pp)
sort(x)

abline(0,1)

y= log(diff_mon)
y[is.infinite(y)] = NA
qqnorm(y)
qqline(y)



month <- function(x)format(x, '%Y.%m')
df_mon = aggregate(df, by = month, FUN = max,na.rm=T)# na.action = "na.omit")
df_mon[is.infinite(df_mon)] = NA

plot(dt_mon_dt, df_mon, "b", cex = 0.3, col = 'red')



mon_df = data.frame(dt_mon_dt, df_mon)
acf(mon_df$df_mon, na.action = na.pass, lag.max = 100)

ts = ts(df_mon, start = c(1847,10), end = c(2019,2), frequency = 12)
plot(ts, lag(ts,30))

indmon =5
head(ts)
str(ts)

cor(ts, lag(ts,20), use = 'pairwise.complete.obs')