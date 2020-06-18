sink('analysis_sales.log', split=T)

# #######################################################################
message("tseries package must be installed via install.packages(tseries)")
message("forecast package must be installed via install.packages(forecast)")
require('tseries')
require('forecast')
# #######################################################################
ts = options(width=112, digits=2, error = function() traceback(2))


# ###############################################################################
HIST = function( x, ...) {
    hist(x, freq=FALSE, ...)
    f.den <- function(t) dnorm(t, mean=mean(x,na.rm=TRUE), sd=sd(x,na.rm=TRUE))
    curve(f.den, add=TRUE, col="darkblue", lwd=2)
}
# ###############################################################################


# ###############################################################################
# read the data, basically as is
# ###############################################################################
sales = read.csv('DATA/Sales.csv', colClasses=c('factor', 'factor', 'numeric'))
sales[,"Ice_Cream_Sales"] = ts(sales[,"Ice_Cream_Sales"], frequency=12)
sales[,"t"] = 1:nrow(sales)
summary(sales)


# ###############################################################################
# basic exploration plots, as needed
# ###############################################################################
graphics.off()
m = ndiffs(sales$Ice_Cream_Sales)
par(mfrow=c(ifelse(m,4,1),1))
plot.ts(sales$Ice_Cream_Sales)
if ( m ) {
    plot.ts(log(sales$Ice_Cream_Sales))
    plot.ts(diff(sales$Ice_Cream_Sales))
    plot.ts(diff(log(sales$Ice_Cream_Sales)))
}

# ###############################################################################
# exploratory modeling, 
# ###############################################################################
p0 = lm( Ice_Cream_Sales ~ Year + as.integer(Month) + Month + t, data=sales)
p1 = ets(sales$Ice_Cream_Sales, model='AZZ')
p2 = auto.arima(sales$Ice_Cream_Sales, seasonal=TRUE, trace=TRUE)#,approximation=FALSE, stepwise=FALSE)
p3 = apply(data.frame(fitted(p0), fitted(p1), fitted(p2)), 1, mean, na.rm=TRUE)

print(summary(p0))
print(summary(p1))
print(summary(p2))


# ###############################################################################
# basic residual analysis
# ###############################################################################
graphics.off()
png('plot_residuals_sales.png', 1600, 900)
par(mfrow=c(3,2))
minlim = -max(sales$Ice_Cream_Sales)/10
maxlim =  max(sales$Ice_Cream_Sales)/10
HIST(residuals(p0),             xlim=c(minlim, maxlim), breaks=100, main="lm(Y + M + lag(Sales)")
acf(residuals(p0))
HIST(residuals(p1),             xlim=c(minlim, maxlim), breaks=100, main="ewma(Sales)")
acf(residuals(p1))
HIST(residuals(p2),             xlim=c(minlim, maxlim), breaks=100, main="ARIMA(1,0,0)(1,1,0)[12] with drift")
acf(residuals(p2))
dev.off()


# ###############################################################################
# basic goodness of fit
# ###############################################################################
Box.test(residuals(p2), type="Ljung")
accuracy(p2)


# ###############################################################################
# visualization summary essay
# ###############################################################################
graphics.off()
pdf('plot_diagnostics_sales.pdf', 11, 8)
tsdiag(p2)
nf <- layout(matrix(c(1,1,1,3, 2,2,2,3, 4,4,5,5, 6,6,6,6), 4,4, byrow=TRUE), TRUE)
layout.show(nf)
plot.ts(sales$Ice_Cream_Sales, main='S1: input signal: Ice_Cream_Sales Pints Per Month')
plot.ts(scale(residuals(p2)),  main='S2: std.residuals(arima_model)')
HIST(residuals(p2), breaks=32, main="S3: hist. arima_model residuals")
acf(residuals(p2), lag.max=36, main="S4: acf of arima residuals")
pacf(residuals(p2), lag.max=36,main="S5: pacf of arima residuals")
plot(forecast(p2, h=4),        main="S6: input signal along with predicted values")
dev.off()


# ###############################################################################
# predicted values
# ###############################################################################
end_year_data = as.data.frame(forecast(p2, h=4))
number_pints = as.integer(end_year_data[,1])
end_year_dates = seq(as.Date("2015/9/1"), as.Date("2015/12/31"), "months")
predicted_vals = cbind(as.data.frame(end_year_data), as.data.frame(end_year_dates), number_pints)
print ( predicted_vals )

sink()
