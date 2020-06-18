# ###########################################################################################
sink('thumbtack.html', split=T)
opts = options(width=130, digits=7, error = function() traceback(2))
LICENSETEXT = "Copyright (C) Nelson R. Manohar, comes with ABSOLUTELY NO WARRANTY."  
message( LICENSETEXT )
message("")
# ###########################################################################################


# ############################################################################################
source('multiplot.R')
print("-------------------------------------------------------")
message("RSQLite, reshape, htmltools, ggplot2, rpart required")
print("-------------------------------------------------------")
require(RSQLite)
require(reshape)
require(htmltools)
require(ggplot2)
require(rpart)
print( "<html>")
# ############################################################################################


# ############################################################################################


# ############################################################################################
LOCAL_TIMESTAMP = proc.time()
BN = 1
SN = 1
BANNER = function ( cname, nlines=5, subsection=F ) {
    cat( "</pre>\n")
    cat( "<pre>\n")
    out = proc.time() - LOCAL_TIMESTAMP
    LOCAL_TIMESTAMP <<- proc.time()
        cat("\n"); cat("\n"); cat("\n"); cat("\n"); cat("\n")
    cat( "</pre>\n")
    if (subsection) {
        out = sprintf("<h2> %s: %s </h2>", LETTERS[BN], cname )
        SN <<- 1
        BN <<- BN + 1
    } else {
        out = sprintf("<h2> %s%s: %s </h2>", LETTERS[BN], SN, cname )
        SN <<- SN + 1
    }
    cat(out)
    cat( "<pre>\n")
}
# ############################################################################################


# ############################################################################################
COMMENT = function( x ) {
    cat( "</pre>\n")
    cat( "<strong>\n")
    out = sprintf("<h3> REMARKS: </h3>", x )
    cat( out )
    x = sprintf( "<pre><p>  ....  %s</p></pre>", x)
    cat ( x )
    cat( "</strong>\n")
    cat( "<pre>\n")
}
# ############################################################################################


# ###########################################################################
CI_METRICS = function( x ) {
    n  = length(x)
    mu = mean(x, na.rm=TRUE)
    std = sd(x, na.rm=TRUE)
    lcl = min(mu - 2.0 * std, 0)
    ucl = mu + 2.0 * std
    retvals = c(mu, std, lcl, ucl, n)
    return (retvals) 
}
# ###########################################################################


# ###########################################################################
HEADER  = "------------------------------------------------------------------------\n"
NEWLINE = function(n=1) {
    for( i in 1:n ) cat("\n")
}
# ###########################################################################


# ###########################################################################
PRINT = function(cmd_name, what) {
    NEWLINE(1)
    cat(HEADER)
    out = sprintf("<h3> %s </h3>\n", cmd_name )
    cat(out)
    cat(HEADER)
    print(what)
    cat(HEADER)
    NEWLINE(1)
}
# ###########################################################################


# ###########################################################################
COMPUTE_LOG_LOSS <- function(actual, prediction) {
    actual = as.numeric(as.factor(actual))-1
    if ( class(prediction)[1] == "factor") {
        prediction = as.numeric(as.factor(prediction))-1
    }
    epsilon <- .000000000000001
    yhat <- pmin(pmax(prediction, epsilon), 1.0-epsilon)
    logloss <- -mean(actual*log(yhat) + (1.0-actual)*log(1.0 - yhat))
    P = 3
    PRINT ( "LOG_LOSS_VALUE:", round(logloss, 4) )
    return(logloss)
}
# ###########################################################################


# ###########################################################################
AGG_BY = function(which, what, wrt) {
   f = as.formula(sprintf("%s ~ %s", what, wrt))
   t = as.data.frame(aggregate( f, data=which, CI_METRICS))
   t = cbind(t[,1], as.data.frame(t[,2]))
   colnames(t) = c(wrt, "mean", "std", "lcl", "ucl", "n")
   return (t)
}
# ###########################################################################

# ###########################################################################
cat( sprintf("<h1> %s </h1>\n", "COMPARISON OF AUG v. JUL MONTHS"))
cat( sprintf("<h3> %s </h3>\n", "Nelson R. Manohar"))
cat( sprintf("<h3> %s </h3>\n", "nelsonmanohar@yahoo.com"))

COMMENT("This analysis studies the variable timedelay(invite-to-quote). 
        I measure this metric in terms of hours and as follows: for each 
        invite having a matching quote, substract the invite's sent_time 
        from the quote's sent_time.  Then, for each day of the months in 
        question, we focus on only those invites that were initiated that 
        day and had a matching quote anywhere into the future. This analysis 
        has a weakness with respect to long-term outlooks in quote reply 
        time (e.g., weeks later, spanning into the next month, etc), however, 
        the data reflects tht average time delay typically within a single 
        day.  I will refer from here on to this metric as i2q_hrs. The 
        goal is to determine whether the i2q_hrs for July and August represent 
        different distributions and hopefully identify any relevant business
        insight related to emerging differences. My approach will be to 
        perform t_tests and explore component effects via aggregation with 
        respect to various factors of interest such as day of the month of 
        the invite, category of the invite, location, day of the week, hour 
        of the day for both months and determine if significant differences 
        with respect to this small dataset appear to emerge.")
# ###########################################################################


# ###########################################################################
cat("<pre>")
BANNER("THE INPUT DATA")
# ###########################################################################
sqlite = dbDriver("SQLite")
dbconn = dbConnect(sqlite, "invite_dataset_ff829852.sqlite.db")
tables = dbListTables(dbconn)
thumbstack_tables = list()
for (table in tables) {
    fields = dbListFields(dbconn, table)
    query = sprintf("select * from %s", table)
    data = dbGetQuery(dbconn, query)
    data = as.data.frame(data)
    thumbstack_tables[[table]] = data
    if ( FALSE ) {
        PRINT ("TABLE SUMMARY:", summary(data))
        cat(HEADER)
        str(data)
        cat(HEADER)
    }
    PRINT (sprintf("TABLE %s FIRST ROWS:", table), data[1:5,] )
}

COMMENT("The input data comprises five tables. for this analysis, I will focus on 
        just three of the tables: request, quotes, and invites and augment then 
        with some derived data to analyze the variable of concern: 
        timedelay(invite-to-quote). Next, I construct the design matrix derived 
        via a series of left joins")


# ############################################################################################
# The tables being used so far
# ############################################################################################
invites = thumbstack_tables[['invites']]
quotes = thumbstack_tables[['quotes']]
requests = thumbstack_tables[['requests']]
# ############################################################################################


# ###########################################################################
BANNER("DESIGN MATRIX: MERGE INVITES, REQUEST, QUOTES", subsection=F)
# ###########################################################################
invites_w_quotes = merge(invites, quotes, by.x="invite_id", by.y="invite_id", all.x=TRUE)
iwq = invites_w_quotes
iwq[,"ts.x"] = as.numeric(strptime(iwq[,"sent_time.x"], "%Y-%m-%d %H:%M:%OS"))
iwq[,"ts.y"] = as.numeric(strptime(iwq[,"sent_time.y"], "%Y-%m-%d %H:%M:%OS"))
iwq[,"i2q_hrs"] = as.numeric(iwq[,"ts.y"] - iwq[,"ts.x"])/(60.0*60.0)
iwq[, "replied"] = as.factor(ifelse(is.na(iwq$"i2q_hrs"), 0, 1))
iwq[,"time_hrs"] = (as.numeric(strptime(iwq[,"sent_time.x"], "%Y-%m-%d %H:%M:%OS")) -
                    as.numeric(strptime("2013-07-01",    "%Y-%m-%d")))/(60.0*60.0)
iwq[,"date"] = as.character(strptime(iwq[,"sent_time.x"], "%Y-%m-%d"))
iwq[,"month"] = substr(iwq[, "date"], 6, 7)
iwq[,"dow"] = factor(weekdays(as.Date(iwq[,"date"],'%Y-%m-%d')), 
              c("Monday", "Tuesday", "Wednesday", "Thursday",
                "Friday", "Saturday", "Sunday"))
iwq[,"daynum"] = substr(iwq[, "date"], 9, 10)
iwq[,"hour"] = substr(as.character(iwq[, "sent_time.x"]), 12, 13)
iwq[,"i2q_hrs_log"] = log(iwq[,"i2q_hrs"])
# str(iwq)

request_invites_quotes = merge(iwq, requests, 
                               by.x="request_id", 
                               by.y="request_id", 
                               all.x=TRUE)
riq = request_invites_quotes
COMMENT("Above, I added several derived fields: day, month, day-of-week 
        (all these being defined with respect to invite's sent_time). I 
         also added a replied field which simply codifies whether the 
         invite was answered with a quote. Finally, the i2q_hrs metric 
         was added as defined above")


# ###########################################################################
# DROP SEPTEMBER SAMPLES  
# ###########################################################################
riq = riq[riq[,"month"]!="09", ]
original_riq = riq
riq = riq[!is.na(riq$i2q_hrs), ]
PRINT("DESIGN MATRIX SUMMARY:", summary(riq))


# ###########################################################################
BANNER("VISUALIZATION: DATA INSPECTION AND CONDITIONING", subsection=T)
# ###########################################################################
graphics.off()
png('basic_plot1.png', 1600, 900)
p2 = ggplot( riq, aes(x=i2q_hrs, fill=month)) + geom_histogram(binwidth=0.1, alpha=0.5)
p3 = ggplot( riq, aes(x=i2q_hrs_log, fill=month)) + geom_histogram(binwidth=0.1, alpha=0.5)
multiplot(p2, p3)
dev.off()
cat( "</pre>\n")
cat( sprintf( "<img src=\"basic_plot1.png\" height=\"800\" >")) # width=\"900\">"))
cat( "<pre>\n")

COMMENT("The first plot shows that the i2q_hrs timeseries appears to be 
        lognormally distributed; therefore, requiring a log() transform to 
        bring the i2q_hrs time series into a normally distributed timeseries. 
        As shown by the histogram on the second plot, after the log transform 
        was applied, the log(i2q_hrs) significantly resembled a potentially 
        normal distribution. Of course, this normality assumption will be 
        shortly evaluated.") 


# ###########################################################################
BANNER("VISUALIZATION: PLOT OF AVG(INVITE TO QUOTE DELAY I2Q FOR INVITES SENT IN GIVEN DAY",
       subsection=T)
# ###########################################################################
png('basic_plot2.png', 1600, 900)
par(mfrow=c(2,1))
riq_daily = AGG_BY(riq, "i2q_hrs", "date")
plot(as.Date(riq_daily$date, format = "%Y-%m-%d"), 
             riq_daily$mean, 
             type='b', xlab="Date", 
             lwd=2, col=3,
             ylab="Avg(I2Q)",
             main="Avg(Invite to Quote Delay) \nwith respect to all invites initiated on specified day")
lines(lowess(as.Date(riq_daily$date, format = "%Y-%m-%d"),
             , riq_daily$mean, f=1/7)$y, col=2, t='l', lwd=2)

riq_daily = AGG_BY(riq, "i2q_hrs_log", "date")
plot(as.Date(riq_daily$date, format = "%Y-%m-%d"), 
             riq_daily$mean, 
             type='b', xlab="Date", 
             ylab="Avg(log(I2Q))",
             main="Avg(log(Invite to Quote Delay)) \nwith respect to all invites initiated on specified day")
dev.off()
cat( "</pre>\n")
cat( sprintf( "<img src=\"basic_plot2.png\" height=\"800\" >")) # width=\"900\">"))
cat( "<pre>\n")
COMMENT("The plot above shows the original i2q_hrs timeseries (i.e., 
        delaytime(invite-to-quote) measured in hours. The second plot shows the 
        log-transformed time timeseries, i.e., log(i2q_hrs).  Note that the 
        timeseries appears to have a potential monthly pattern (approximately 
        stationary for first two weeks, followed by dip and then, an upwards 
        rally for two weeks until the end of the month. However, with such 
        little data is impossible to examine this and it is left as a pending 
        issue for subsequent examination." )



# ###########################################################################
BANNER("CONFIDENCE INTERVALS FOR POPULATION MEANS: log(i2q) wrt JUL and AUG")
# ###########################################################################
jul_aug_samples = riq[,"i2q_hrs_log"]
jul_samples = riq[riq[,"month"]=="07","i2q_hrs_log"]
aug_samples = riq[riq[,"month"]=="08","i2q_hrs_log"]
jul_ci = t.test(exp(jul_samples), alternative="two.sided", na.rm=TRUE)
aug_ci = t.test(exp(aug_samples), alternative="two.sided", na.rm=TRUE)
PRINT("CONFIDENCE INTERVAL FOR JUL MEAN:", jul_ci)
PRINT("CONFIDENCE INTERVAL FOR AUG MEAN:", aug_ci)
COMMENT("Both means are non-zero and the confidence intervals of the means 
        allow the inclusion of the mean for Jul significantly overlaps with 
        the confidence interval for the mean for Aug")



# ###########################################################################
BANNER("TEST ASSUMPTION: log(i2q) data normality tests", subsection=T)
# ###########################################################################
subset = sample(1:nrow(riq), 4096)
shapiro.test(jul_aug_samples[subset])
cat(HEADER)

subset = sample(1:length(jul_samples), 4096)
shapiro.test(jul_samples[subset])
cat(HEADER)

subset = sample(1:length(aug_samples), 4096)
shapiro.test(aug_samples[subset])
COMMENT("The null-hypothesis of this test is that the population is normally 
        distributed. Resultant p-values for Jul, Aug, and Jul+Aug indicate that 
        cannot be discarded that the samples were taken from normal distributions")




# ###########################################################################
BANNER("TEST ASSUMPTION: homo/heteroskedasticity", subsection=T)
# ###########################################################################
riq.var = aggregate(i2q_hrs_log ~ month,   data=riq, var)
PRINT( "VARIANCE OF POPULATION SAMPLES BY MONTH", riq.var )
COMMENT("At first inspection, variance of the log(invite_to_quote delay time) 
        appears similar enough. A variance test is applied next.")

vtest = var.test(jul_samples, aug_samples, 
                 ratio = 1,
                 alternative = "two.sided",
                 conf.level = 0.999)
PRINT( "TEST WRT RATIO OF VARIANCES FROM SAMPLED POPULATIONS", vtest)

COMMENT("The alternative hypothesis is rejected, that is, at reasonable 
        confidence levels of 0.999 (or 1 out of 2000), a very high p-value (>0.3) 
        indicates that there is NO evidence that a statistically significant 
        difference between the ratio of the variances exists. Similarly, the 
        confidence interval for the ratio of the variances spans the the ratio 1.")


# ###########################################################################
BANNER("APPLYING STANDARD TWO-SAMPLE T-TEST")
# ###########################################################################
tvals = t.test( i2q_hrs_log ~ month, data=riq, 
                alternative = "two.sided", 
                paired = FALSE, 
                var.equal = TRUE, 
                conf.level = 0.999)
PRINT("TWO SAMPLE, TWO-SIDED, T-TEST FOR DIFFERENCE OF MEANS:", tvals)
COMMENT("A two-sided t-test for difference in the sample means from populations 
        having equal variances was applies. The test indicated thati at a confidence 
        interval of 0.999 (i.e., 1/2000), strongly failed to accept the alternative 
        hypothesis that the difference between the means of the two sampled populations 
        ougth to be zero and thus the same. A very strong p-value indicated this not 
        to be the case. As expected, the confidence interval for the difference 
        between these means spans zero, indicative that there is NO discernible 
        difference between these sampled means for July and August.")


# ###########################################################################
BANNER("ANALYSIS OF VARIANCE: One Way Analysis of Variance")
# ###########################################################################
riq_balanced = riq
c_jul = which(riq_balanced$month=="07")
c_aug = which(riq_balanced$month=="08")
n = min(length(c_jul), length(c_aug))
c_jul = sample(c_jul, n)
c_aug = sample(c_aug, n)
riq_balanced = riq_balanced[c(c_jul, c_aug),]
anova_findings = aov(i2q_hrs_log ~ as.factor(month), riq_balanced)
PRINT("ONE WAY ANOVA RESULTS:", summary(anova_findings) )
COMMENT("A one-way anova test for testing whether samples in these two months are 
        drawn from populations with the same mean values (H0) or (H1) they is 
        statistically significance difference with respect to their means (that is, 
        component effect). The analysis of variance fitted with respect to months, 
        after removing NAs and balancing the data indicates that there is a significant 
        component effect with respec to month towards invite-to-quote delay time 
        (log scale). This is misleading with respect to other findings. Therefore, 
        we review the assumptions necessary for the anova test to be meaningful.</p>
            <pre>
            1) the dependent variable Invite2QuoteDelayTime is continuous: OK, 
            2) the independent variable Month has two levels (Jul, Aug) OK, 
            3) the observed measurements are independent samples (TROUBLESOME). 
               This means that there is no intuition or knowledge about a possible 
               relationship between the observations within or between groups and 
               this is contrary to known human nature in bidding and recommender systems).
            4) there appear to be two significant outliers at 3 sigma levels (TROUBLESOME),
            5) the dependent variable ought to be normally distributed (OK, wrt log(I2Q_hrs).
            6) the variance of the groups is homogeneous (OK).
            </pre>
        Assumption (3) is a known issue on this domain and the presence of apparent 
        seasonality on the dependent variable time series indicates that even an ARIMA 
        model may be better suited to explain recurring end-of-week behavior as hinted 
        in findings below. Finally, this claim is consistent with the facts that the 
        anova coefficients (component contributions) are esssentially the same.")

# PRINT("ANOVA COEFFICIENTS:", coefficients(anova_findings))
# PRINT("ACF OF ", p)


# ###########################################################################
BANNER("ANALYSIS OF VARIANCE: LM")
# ###########################################################################
model = lm(i2q_hrs_log ~ month, data=riq)
PRINT("ALTERNATIVE ANOVA JUL+AUG VIA LINEAR MODEL", summary(model))
COMMENT("Application of ANOVA equivalent techniques via the lm model to access
        individual component effects provides NO indication of a statistically 
        significant effect due to month. Albeit the month8 is selected, the
        model is dominated by the intercept (i.e., a constant output of approx.
        1). Finally, ,R2, F, and p values are indicative of a poor fit for which
        the month provides NO statistical significance component effect.")


# ###########################################################################
BANNER("EXAMINING PRESENCE OF PER FACTOR-LEVEL CHANGES", subsection=F)
# ###########################################################################
jul_aug = riq
for (field in c("request_id", "invite_id", "user_id.x", "quote_id", "replied",
               "date", "month", "daynum", "hour", "user_id.y", "category_id", "location_id"))
    jul_aug[, field] = as.factor(jul_aug[, field])
jul = jul_aug[jul_aug[,"month"]=="07",]
aug = jul_aug[jul_aug[,"month"]=="08",]


# ###########################################################################
BANNER("FACTOR-LEVEL: CHANGES WRT HOUR OF DAY")
# ###########################################################################
riq_hourly_both = AGG_BY(jul_aug, "i2q_hrs", "hour")
riq_hourly_jul  = AGG_BY(jul,     "i2q_hrs", "hour")
riq_hourly_aug  = AGG_BY(aug,     "i2q_hrs", "hour")
to_plot <- data.frame(hour_of_day=riq_hourly_both$hour,
                      jul_i2q_by_hour_of_day=riq_hourly_jul$mean,
                      aug_i2q_by_hour_of_day=riq_hourly_aug$mean)
melted<-melt(to_plot, id="hour_of_day")


png('histograms1.png', 1600, 900)
par(mfrow=c(1,2))
h1 = hist(riq_hourly_jul$mean, col=5, breaks=10)
h2 = hist(riq_hourly_aug$mean, col=3, breaks=10)
dev.off()
cat( "</pre>")
cat( sprintf( "<img src=\"histograms1.png\" height=\"200\" >")) # width=\"900\">"))
cat( "<pre>")


if ( FALSE ) {
   PRINT("SUMMARY OF AGGREGATION BY HOUR OF DAY FOR JUL", summary(riq_hourly_jul))
   PRINT("SUMMARY OF AGGREGATION BY HOUR OF DAY FOR AUG", summary(riq_hourly_aug))
}

jul_anova = aov(i2q_hrs_log ~ as.factor(hour), data=riq[riq$month=="07",])
aug_anova = aov(i2q_hrs_log ~ as.factor(hour), data=riq[riq$month=="08",])
PRINT("HOUR OF DAY: ANOVA JUL VIA AOV", summary(jul_anova))
PRINT("HOUR OF DAY: ANOVA AUG VIA AOV", summary(aug_anova))

jul_anova = lm(i2q_hrs_log ~ as.factor(hour), data=riq[riq$month=="07",])
aug_anova = lm(i2q_hrs_log ~ as.factor(hour), data=riq[riq$month=="08",])
PRINT("HOUR OF DAY: ANOVA JUL VIA LM", summary(jul_anova))
PRINT("HOUR OF DAY: ANOVA AUG VIA LM", summary(aug_anova))

png('basic_plot3.png', 1600, 900)
p1 = ggplot(melted, aes(x=hour_of_day, y=value, fill=variable)) + geom_bar(position="dodge")
COMMENT("From examination of the hour-of-day based aggregation visualization,
        there are appear to be a handful of hours: (specifically, 1AM, 9AM, 1PM, 8PM)
        at which observable difference takes place, nevertheless, for the majority 
        of hour of day, there is NO statistically significant component effect for
        hour-of-day for either Jul or Aug subsets. A MILD exception exist for the month of
        Aug at 9AM hour-of-day but this again occurs at weak R2, F, and p values.  NO 
        day of hour was found to have a strong stat. significant.  component effect.")


# ###########################################################################
BANNER("FACTOR-LEVEL: CHANGES WRT DAY OF MONTH")
# ###########################################################################
riq_daily_both = AGG_BY(jul_aug, "i2q_hrs", "daynum")
riq_daily_jul  = AGG_BY(jul,     "i2q_hrs", "daynum")
riq_daily_aug  = AGG_BY(aug,     "i2q_hrs", "daynum")
to_plot <- data.frame(day_of_month=riq_daily_both$daynum,
                      jul_i2q_by_day_of_moth=riq_daily_jul$mean,
                      aug_i2q_by_day_of_month=riq_daily_aug$mean)
melted<-melt(to_plot, id="day_of_month")


png('histograms2.png', 1600, 900)
par(mfrow=c(1,2))
h1 = hist(riq_daily_jul$mean, col=5, breaks=10)
h2 = hist(riq_daily_aug$mean, col=3, breaks=10)
dev.off()
cat( "</pre>")
cat( sprintf( "<img src=\"histograms2.png\" height=\"200\" >")) # width=\"900\">"))
cat( "<pre>")

if ( FALSE ) {
    PRINT("SUMMARY OF AGGREGATION BY DAY OF MONTH FOR JUL", summary(riq_daily_jul))
    PRINT("SUMMARY OF AGGREGATION BY DAY OF MONTH FOR AUG", summary(riq_daily_aug))
}

jul_anova = aov(i2q_hrs_log ~ as.factor(daynum), data=riq[riq$month=="07",])
aug_anova = aov(i2q_hrs_log ~ as.factor(daynum), data=riq[riq$month=="08",])
PRINT("DAY OF MONTH: ANOVA JUL VIA AOV", summary(jul_anova))
PRINT("DAY OF MONTH: ANOVA AUG VIA AOV", summary(aug_anova))

jul_anova = lm(i2q_hrs_log ~ as.factor(daynum), data=riq[riq$month=="07",])
aug_anova = lm(i2q_hrs_log ~ as.factor(daynum), data=riq[riq$month=="08",])
PRINT("DAY OF MONTH: ANOVA JUL VIA LM", summary(jul_anova))
PRINT("DAY OF MONTH: ANOVA AUG VIA LM", summary(aug_anova))

p2 = ggplot(melted, aes(x=day_of_month, y=value, fill=variable)) + geom_bar(position="dodge")
COMMENT("From examination of the day-of-month based aggregation visualization,
        there appear to be a handful of days, e.g., 6th, 11th, 14th, 20th, 21th, 
        22th, at which observable difference takes place.  The great majority 
        of day-of-month levels exhibit NO discernable effect. Furthermore, when
        the ANOVA test is applied to Jul and Aug subsets, MILD statistically
        significant effects are observed for the Aug.s day-of-month: 2, 5, 8, 11,
        15, 16, 19). Nevertheless, this mild effect occurs at a relatively weak 
        p-value and weak F-value. The particular sequence of days suggests the 
        potential presence of an ARIMA weekly and monthly seasonal process, taking
        place the 1st, 2nd, and then 3rd week of the month.")



# ###########################################################################
BANNER("FACTOR-LEVEL: CHANGES WRT CATEGORY OF REQUEST/INVITE/QUOTE")
# ###########################################################################
riq_catid_both = AGG_BY(jul_aug, "i2q_hrs", "category_id")
riq_catid_jul  = AGG_BY(jul,     "i2q_hrs", "category_id")
riq_catid_aug  = AGG_BY(aug,     "i2q_hrs", "category_id")
riq_catid_both[,"variable"] = "agg_by_jul_aug"
riq_catid_jul[,"variable"]  = "agg_byjul"
riq_catid_aug[,"variable"]  = "agg_by_aug"
melted = rbind(riq_catid_jul, riq_catid_aug)


png('histograms3.png', 1600, 900)
par(mfrow=c(1,2))
h1 = hist(riq_catid_jul$mean, col=5, breaks=10)
h2 = hist(riq_catid_aug$mean, col=3, breaks=10)
dev.off()
cat( "</pre>")
cat( sprintf( "<img src=\"histograms3.png\" height=\"200\" >")) # width=\"900\">"))
cat( "<pre>")

if( FALSE ) {
    PRINT( "SUMMARY OF AGGREGATION BY CATEGORY ID FOR JUL", summary(riq_catid_jul))
    PRINT( "SUMMARY OF AGGREGATION BY CATEGORY ID FOR AUG", summary(riq_catid_aug))
}

jul_anova = aov(i2q_hrs_log ~ as.factor(category_id), data=riq[riq$month=="07",])
aug_anova = aov(i2q_hrs_log ~ as.factor(category_id), data=riq[riq$month=="08",])
PRINT("CATEGORY_ID: ANOVA JUL VIA AOV", summary(jul_anova))
PRINT("CATEGORY_ID: ANOVA AUG VIA AOV", summary(aug_anova))
COMMENT("Anova analysis of the Jul and Aug subsets indicate that there is NO 
        statistically significant component effect for the category_id levels on
        either of the months.")

jul_lm =  lm(i2q_hrs_log ~ as.factor(category_id), data=riq[riq$month=="07",])
aug_lm =  lm(i2q_hrs_log ~ as.factor(category_id), data=riq[riq$month=="08",])
PRINT("CATEGORY_ID: INDIVIDUAL COMPONENT EFFECTS VIA LM FOR JUL", summary(jul_lm))
PRINT("CATEGORY_ID: INDIVIDUAL COMPONENT EFFECTS VIA LM FOR AUG", summary(aug_lm))
COMMENT("Anova analysis of the Jul and Aug subsets indicate that there NO
        statistical significant difference at specific category levels between 
        the two months, specifically, though several categories (10, 16, 21
        44, 67, 68, 79, 82, 88, 95, 110, 111) have MILDLY significant effects
        but at poor R2, F and p values.")


p3 = ggplot(melted, aes(x=category_id, y=mean, fill=variable)) + geom_bar(position="dodge")
COMMENT("From examination of the category_id based aggregation visualization,
        it appears that there are a handful of categories, (e.g., 8, 10, etc.) 
        at which significant difference appears to take place. HOWEVER, when 
        the ANOVA test is applied, only a MILD statistically significant effect 
        is observed in categories 67 (A/V), 68 (Tutoring), and 69 (Land Surveying).")


# ###########################################################################
BANNER("FACTOR-LEVEL: CHANGES WRT LOCATION OF REQUEST/INVITE/QUOTE")
# ###########################################################################
riq_locid_both = AGG_BY(jul_aug, "i2q_hrs", "location_id")
riq_locid_jul  = AGG_BY(jul,     "i2q_hrs", "location_id")
riq_locid_aug  = AGG_BY(aug,     "i2q_hrs", "location_id")
riq_locid_both[,"variable"] = "agg_by_jul_aug"
riq_locid_jul[,"variable"]  = "agg_by_jul"
riq_locid_aug[,"variable"]  = "agg_by_aug"
melted = rbind(riq_locid_jul, riq_locid_aug)

png('histograms4.png', 1600, 900)
par(mfrow=c(1,2))
h1 = hist(riq_locid_jul$mean, col=5, breaks=10)
h2 = hist(riq_locid_aug$mean, col=3, breaks=10)
dev.off()
cat( "</pre>")
cat( sprintf( "<img src=\"histograms4.png\" height=\"200\" >")) # width=\"900\">"))
cat( "<pre>")

if ( FALSE ) {
    PRINT( "SUMMARY OF AGGREGATION BY LOCATION_ID FOR JUL", summary(riq_locid_jul))
    PRINT( "SUMMARY OF AGGREGATION BY LOCATION_ID FOR AUG", summary(riq_locid_aug))
}

jul_anova = aov(i2q_hrs_log ~ as.factor(location_id), data=riq[riq$month=="07",])
aug_anova = aov(i2q_hrs_log ~ as.factor(location_id), data=riq[riq$month=="08",])
PRINT("LOCATION_ID: ANOVA JUL VIA AOV", summary(jul_anova))
PRINT("LOCATION_ID: ANOVA AUG VIA AOV", summary(aug_anova))

jul_anova = lm(i2q_hrs_log ~ as.factor(location_id), data=riq[riq$month=="07",])
aug_anova = lm(i2q_hrs_log ~ as.factor(location_id), data=riq[riq$month=="08",])
PRINT("LOCATION_ID: INDIVIDUAL COMPONENT EFFECTS VIA LM FOR JUL", summary(jul_anova))
PRINT("LOCATION_ID: INDIVIDUAL COMPONENT EFFECTS VIA LM FOR AUG", summary(aug_anova))
COMMENT("Anova analysis of the Jul and Aug subsets indicate there is NO 
        statistically significant component effect for the location_id. When examining
        individul levels there is MILD component effects for location_id 2, 24, 66, and 
        72 but these occur at very weak R2, F and p-values. As a result, there is
        no evidence of a STRONG difference between Jul and Aug months in terms of
        components effects for location_id.")

p5 = ggplot(melted, aes(x=location_id, y=mean, fill=variable)) + geom_bar(position="dodge")
COMMENT("From examination of the location_id based aggregation visualization,
        it appears that there are a handful of distinguishing categories for 
        either Jul and Aug.  HOWEVER, when the ANOVA test is applied, there is
        NO strong evidence of a significant effect being observed due to
        location_id or location_id levels.")


# ###########################################################################
BANNER("FACTOR-LEVEL: CHANGES WRT DAY OF WEEK")
# ###########################################################################
riq_daily_both = AGG_BY(jul_aug, "i2q_hrs", "dow")
riq_daily_jul  = AGG_BY(jul,     "i2q_hrs", "dow")
riq_daily_aug  = AGG_BY(aug,     "i2q_hrs", "dow")
riq_daily_both[,"variable"] = "agg_by_jul_aug"
riq_daily_jul[,"variable"]  = "agg_byjul"
riq_daily_aug[,"variable"]  = "agg_by_aug"
melted = rbind(riq_daily_jul, riq_daily_aug)


png('histograms5.png', 1600, 900)
par(mfrow=c(1,2))
h1 = hist(riq_daily_jul$mean, col=5, breaks=10)
h2 = hist(riq_daily_aug$mean, col=3, breaks=10)
dev.off()
cat( "</pre>")
cat( sprintf( "<img src=\"histograms5.png\" height=\"200\" >")) # width=\"900\">"))
cat( "<pre>")

if ( FALSE ) {
    PRINT("SUMMARY OF AGGREGATION BY DAY OF WEEK FOR JUL", summary(riq_daily_jul))
    PRINT("SUMMARY OF AGGREGATION BY DAY OF WEEK FOR AUG", summary(riq_daily_aug))
}

jul_anova = aov(i2q_hrs_log ~ dow, data=riq[riq$month=="07",])
aug_anova = aov(i2q_hrs_log ~ dow, data=riq[riq$month=="08",])
PRINT("DAY OF WEEK: ANOVA JUL VIA AOV", summary(jul_anova))
PRINT("DAY OF WEEK: ANOVA AUG VIA AOV", summary(aug_anova))


jul_anova = lm(i2q_hrs_log ~ dow, data=riq[riq$month=="07",])
aug_anova = lm(i2q_hrs_log ~ dow, data=riq[riq$month=="08",])
PRINT("DAY OF WEEK: ANOVA JUL VIA LM", summary(jul_anova))
PRINT("DAY OF WEEK: ANOVA AUG VIA LM", summary(aug_anova))
COMMENT("One way anova analysis of the Jul and Aug subsets indicate that there 
        is a not so MILD statistical significant difference at specific day-of-week 
        levels but ONLY for the month of Aug. and not July,  specifically, 
        Wednesdays on August, which holds high statistical significance but 
        at a relatively weak R2, F, and p-values. As a result, again, there is 
        NO strong evidence of significant departure between Jul and Aug months 
        when accounting for day-of-week component effects.")


p4 = ggplot(melted, aes(x=dow, y=mean, fill=variable)) + geom_bar(position="dodge")
COMMENT("From examination of the day-of-week based aggregation visualization,
        there appear to be differences for day-of-week with respect to the 
        months of July and August. HOWEVER, when the ANOVA test is applied, 
        it indicates that NO statistically significant component effects -- 
        for any day of week -- is discernible, at all.")


# ###########################################################################
# PLOT
# ###########################################################################
multiplot(p1, p2, p3, p5, p4)
dev.off()
cat( "</pre>")
cat( sprintf( "<img src=\"basic_plot3.png\" height=\"800\" >")) # width=\"900\">"))
cat( "<pre>")


# ###########################################################################
BANNER("EXAMINING PREDICTIVE MODEL FOR REPLY BIT FOR JUL/AUG CONCEPT DRIFT")
# ###########################################################################
tree.jul_aug = rpart(replied ~ hour + dow + as.factor(category_id), 
                     data=original_riq)
tree.jul     = rpart(replied ~ hour + dow + as.factor(category_id), 
                     data=original_riq[original_riq$month == "07", ])
tree.aug     = rpart(replied ~ hour + dow + as.factor(category_id),
                     data=original_riq[original_riq$month == "08", ])

cmat_jul_aug = table(original_riq$replied, round(predict(tree.jul_aug)[,2],0))
cmat_jul = table(original_riq$replied[original_riq$month=="07"], round(predict(tree.jul)[,2],0))
cmat_aug = table(original_riq$replied[original_riq$month=="08"], round(predict(tree.aug)[,2],0))

COMMENT("Often enough, it is desired to predict whether a quote will be produced. In
        this dataset this is represented by the REPLIED bit. A natural question here
        is whether the predictive model for the reply bit is noticeably impacted when
        switching from the July data to the August data (i.e., concept drift). To
        build a very basic model, I opted for a decision tree and used some basic
        factors which appear promising (e.g., category_id). HOWEVER, when leveraging 
        these factors into simplistic predictive modeling, NO discernible predictive 
        impact (logloss, confusion tables, tree structure, etc) is observed when training
        with Jul or Aug data.")


BANNER("PREDICTIVE MODELING/DECISION TREE: WRT JULY")
PRINT("BASIC DECISION TREE FOR JUL WITH RESPECT TO HOUR AND CATEGORY", tree.jul)
PRINT("JUL DECISION TREE CPTABLE:", tree.jul$cptable)
PRINT("PREDICTED REPLIES PROBABILITIES CONFUSION MATRIX FOR JULY", cmat_jul)
COMPUTE_LOG_LOSS(original_riq$replied[original_riq$month=="07"], predict(tree.jul)[,2])

BANNER("PREDICTIVE MODELING/DECISION TREE: WRT AUGUST")
PRINT("BASIC DECISION TREE FOR AUG WITH RESPECT TO HOUR AND CATEGORY", tree.aug)
PRINT("AUG DECISION TREE CPTABLE:", tree.aug$cptable)
PRINT("PREDICTED REPLIES PROBABILITIES CONFUSION MATRIX FOR AUG",  cmat_aug)
COMPUTE_LOG_LOSS(original_riq$replied[original_riq$month=="08"], predict(tree.aug)[,2])

png('basic_plot5.png', 400, 400)
par(mfrow=c(2,1))
plot(t(cmat_jul), col=c(5,3))
plot(t(cmat_aug), col=c(5,3))
dev.off()
cat( "</pre>")
cat( sprintf( "<img src=\"basic_plot5.png\" height=\"400\" >")) # width=\"900\">"))
cat( "<pre>")
COMMENT("Confusion matrices for the two decision tree predictive models, one trained with
        the JUL dataset and the other trained with the AUG dataset. As also indicated by the
        overall LOGLOSS performance metric, and the structure of the decision trees, and
        now, the similarity between confusion matrices, the predictive modeling is training
        this most basid model with either the Jul or Aug datasets does NOT visibly impact 
        the model's performance nor its construction.")


# ###########################################################################
BANNER("CONCLUSION")
# ###########################################################################
COMMENT("In conclusion, even individual factor levels were accounted for, NO 
        STRONG statistically significant effect was discernable when contrasting
        the invite-to-quote time-delay for the months of July vs. the month of 
        Aug.. A variety of analysis techniques were used, most reaching a similarly
        strong conclusion about the lack of statistically significant effects.
        However, in a few cases, arguably mild statistically significant effects 
        were observed for specific factor levels of day-of-month (e.g., the 16th
        of the month), hour-of-day (e.g., 9AM), and location-id (e.g., 68: PA-NJ).
        On all of these, the effects manifested on the month of Aug and NOT on 
        Jul. However, the category_id provided a countering example, on which the
        strongest yet relatively mildly statistically significant effect was due 
        to category_id 10 on Jul (Pest Control) and then, a similarly mild effect 
        was observed on Aug but this timeon category_id 13 (Home Remodeling).

        Because of time limitations, only one-way anova was applied. As a result,
        higher order effects were not examined. However, the indications so far
        point to the lack of evidence for the presence of a statistical strong 
        effect distinguishing Jul from Aug.

        One weakness of the analysis consists in the handling of the missing values 
        for the i2q_hrs as two alternatives existed for its handling:
           a) drop all such rows for which i2q_hrs does not exists 
              (i.e., invites w/o quotes), or alternatively,
           b) assign an Inf or large value to its i2q_hrs value to represent and
              penalize the fact that such row attributes lead to negative outcomes.

        Such analysis is left as future work with respect to the stated time limits 
        (approx. 6 hrs).  The amount of time to produce this analysis so far was 
        approx. 12 hrs.")

cat( "</html>")
sink()

