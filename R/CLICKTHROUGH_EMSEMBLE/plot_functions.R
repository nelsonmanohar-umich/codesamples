# ########################################################################################
# AUTHOR: NELSON R. MANOHAR
# DATE:   SEP/2014
# LANGUAGE: R
# INTENDED-USE: CODE-SAMPLE, LIBRARY BUILDING
# CODEBASE: MACHINE LEARNING
# FILENAME: 
# ########################################################################################


source('utilities.R')


# #####################################################################################
# http://stat.ethz.ch/R-manual/R-devel/library/graphics/html/hist.html
# #####################################################################################
PLOT_FEATURE = function( xx, xbins=16, ptitle="" ) {
    op <- par(mfrow = c(1, 4))
        # plot1
        utils::str(hist(xx,        col = "gray",     labels = TRUE, main=paste("histogram", ptitle)))

        # plot2
        r = hist(sqrt(abs(xx)),  col = "lightblue",labels = TRUE, main=paste("histogram", "sqrt", ptitle), border = "pink")
        text(r$mids, r$density, r$counts, adj = c(.5, -.5), col = "blue3")
        sapply(r[2:3], sum)
        sum(r$density * diff(r$breaks)) # == 1
        lines(r, lty = 3, border = "purple") # -> lines.histogram(*)

        # plot3: ## Comparing data with a model distribution should be done with qqplot()!
        set.seed(14)
        # x <- rchisq(100, df = 4)
        x <- rnorm(100)
        # CHECK CHISQ PLOT AND DF
        # qqplot(x, qchisq(ppoints(xx), df = 4)); abline(0, 1, col = 2, lty = 2)
        qqplot(x, qnorm(ppoints(xx))); abline(0, 1, col = 2, lty = 2)

        # plot4: ## if you really insist on using hist() ... :
        hist(xx, freq = FALSE, ylim = c(0, 0.42), main=paste( "histogram", ptitle ))
        # curve(dchisq(x, df = 4), col = 2, lty = 2, lwd = 2, add = TRUE)
        curve(dnorm(x, mean=mean(xx), sd=sd(xx)), add=TRUE, col="darkblue", lwd=2) 
    par(op)
}
# #####################################################################################


# #####################################################################################
# http://stats.stackexchange.com/questions/52293/r-qqplot-how-to-see-whether-data-are-normally-distributed
# ###############################################################################
DO_QQPLOT = function( x, nbins=32, use_par=TRUE, ptitle="Residuals" ) {
    require(e1071)
    if ( use_par ) op <- par(mfrow = c(1, 3))
        # plot1: # qq-plot: you should observe a good fit of the straight line
        qqnorm(x, xlab=ptitle)
        qqline(x)
     
        # plot2: # p-plot: you should observe a good fit of the straight line
        probplot(x, qdist=qnorm, xlab=ptitle)
    
        # plot3: # fitted normal density
        DO_HIST( x, nbins=nbins, ptitle="Histogram of Residuals" )
    if ( use_par) par(op)
}
# ###############################################################################


# ###############################################################################
DO_HIST = function( x, nbins=32, ptitle="Histogram of Residuals", ... ) {
    hist(x, freq=FALSE, breaks=nbins, main=ptitle, ...)
    f.den <- function(t) dnorm(t, mean=mean(x,na.rm=TRUE), sd=sd(x,na.rm=TRUE))
    curve(f.den, add=TRUE, col="darkblue", lwd=2)
}
# ###############################################################################


# ###############################################################################
DO_BARPLOT = function( x, ... ) {
    barplot( x, ... )
}
# ###############################################################################


# #####################################################################################
# #####################################################################################
DO_BASIC_FEATURE_ANALYSIS_PLOT = function( xdf, colnum, numbins=32 ) {
    xxx   = xdf[,colnum]
    title = GET_DF_COLNAME( xdf, colnum)
    PLOT_FEATURE( xxx, xbins=numbins, ptitle=title )
}
# #####################################################################################


# #####################################################################################
# http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
# #####################################################################################
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
# #####################################################################################
MULTIPLOT <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
# #####################################################################################


# #####################################################################################
# #####################################################################################
VIOLIN_PLOT = function( X, Y )  {
    p1 = ggplot( as.data.frame(XYdf), aes( y=Y, x=X )  + geom_violin() + theme_wsj(base_size=8))
}
# #####################################################################################


# #####################################################################################
# #####################################################################################
VIOLIN_DENSITY_PLOT = function( XYdf, i )  {
    Y = XYdf[,ncol(XYdf)]
    p1 = ggplot( as.data.frame(XYdf), aes( y=Y, x=RECODE(XYdf[,i],xnbins=2)))  + geom_violin() + theme_wsj(base_size=8)
    p2 = ggplot( as.data.frame(XYdf), aes( y=Y, x=RECODE(XYdf[,i],xnbins=3)))  + geom_violin() + theme_wsj(base_size=8)
    p3 = ggplot( as.data.frame(XYdf), aes( y=Y, x=RECODE(XYdf[,i],xnbins=4)))  + geom_violin() + theme_wsj(base_size=8)
    p4 = ggplot( as.data.frame(XYdf), aes( y=Y, x=RECODE(XYdf[,i],xnbins=8)))  + geom_violin() + theme_wsj(base_size=8)
    MULTIPLOT(p1, p2, p3, p4, cols=2)
    return ( list(p1, p2, p3, p4 ) )
}
# #####################################################################################


# #####################################################################################
# association plot automatically selecting the smallest size factors for quick inspection 
# assoc( Y ~ V8+V5, data=XYdf, shade=TRUE)
# #####################################################################################
ASSOC_PLOT = function( XY, Y, nb=4, nmax=100, plot_output="" ) {
    require('vcd')
    if ( plot_output != "" ) pdf(plot_output)
    # SAMPLED_ROWS2 = RANDOM_ROWS( rownames(XYr), m=min(100,length(rownames(XYr))))
    # XYdf = EXTEND_DF( XY, RECODE(Y,nb) )[SAMPLED_ROWS2,]
    XYdf = DO_STRATIFIED_SUBSAMPLING( XY, Y, nb, nmax )
    txt = summary( XYdf )
    print( txt )
    XYnlevels = mapply( nlevels, XY )
    xn = sort(XYnlevels)
    print( xn )
    for (i in seq(1,length(xn)-3,1)) {
        for (j in seq(i+1,length(xn)-4)) {
        idx  = c(i, j)
        idx_names = names(xn[ idx ])
        if (sum(is.na(idx_names)) > 0) break
        idx_names = c( idx_names, "Y" )
        XYdf1 = XYdf[ idx_names ]
        print( names(XYdf1) )
        assoc( XYdf1, data=XYdf1, shade=TRUE)
        }
    }
    if ( plot_output != "" ) {
        dev.off()
        graphics.off()
    }
}
# #####################################################################################


# #####################################################################################
DO_PERFORMANCE_CURVE_PLOT = function( x, y, xlab, ylab, ... ) {
    plot( x=x, y=y, type='b', pch=22, bg='brown', xlab=xlab, ylab=ylab, ... )
    title( "LEARNING CURVE (for_given_clf)" )

    grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted")

    y_delta = (max(y)-min(y))/length(y)
    for( i in 1:length(y) ) {
        xx=x[i]
        yy=y[i] + y_delta 
        text( xx, yy, as.character( round(yy,2) ), col="blue", cex=0.5 ) 
    }

}
# #####################################################################################


# #####################################################################################
# http://www.rdatamining.com/examples/outlier-detection
# #####################################################################################
PLOT_DENSITY_OUTLIERS = function( NUMERICAL_DATAFRAME=data.frame(), debug=FALSE) {
    require(DMwR)

    OUTLIER.SCORES <- lofactor(NUMERICAL_DATAFRAME, k=5)
    plot(density(OUTLIER.SCORES))

    OUTLIERS <- order(OUTLIER.SCORES, decreasing=T)[1:5]    # pick top 5 as outliers
    print(OUTLIERS)                                         # who are outliers

    n <- nrow(NUMERICAL_DATAFRAME)
    LABELS <- 1:n
    LABELS[-OUTLIERS] <- "."
    biplot(prcomp(NUMERICAL_DATAFRAME), cex=.8, xlabs=LABELS)

    pch <- rep(".", n)
    pch[OUTLIERS] <- "o"
    col <- rep("black", n)
    col[OUTLIERS] <- "red"
    DO_PAIRS_PLOT(NUMERICAL_DATAFRAME, pch=pch, col=col)

    return( OUTLIERS )
}
# #####################################################################################


# #####################################################################################
DO_PAIRS_PLOT = function( DATAFRAME, BASIC=FALSE, ... ) {
    if ( BASIC ) 
        pairs(DATAFRAME, ... )
    else {
        DO_ELIPSE_PAIRS_PLOT( XY, Y, plot="pairs", cex=0.2, ... )
    }
}
# #####################################################################################


# #####################################################################################
# http://topepo.github.io/caret/visualizations.html
# #####################################################################################
DO_ELIPSE_PAIRS_PLOT = function( XY, Y, plot="ellipse", ... ) {
    require(caret)
    N = ncol(XY)
    if ( plot == "ellipse" ) require(ellipse)
    featurePlot(x = XY, y = Y, plot = plot, auto.key = list(columns = 3)) # add a key at the top
}
# #####################################################################################


# #####################################################################################
GET_LAYOUT_PARAMS = function( N ) {
    if ( N == 2 ) return (c(2,1))
    if ( N == 3 ) return (c(3,1))
    if ( N == 4 ) return (c(4,1))
    p=as.integer(sqrt(N))
    print( p )
    if ( p^2<N )
        if ( p*(p+1) < N )
            return (c(p+1,p+1))
        else 
            return (c(p+1, p))
    else 
        return ( c(p,p))
}
# #####################################################################################


# #####################################################################################
# http://topepo.github.io/caret/visualizations.html
# #####################################################################################
DO_DENSITY_PLOT = function( XY, Y, ... ) {
    require(caret)
    N = ncol(XY)
    featurePlot(x = XY, y = Y, plot = "density", adjust = 1.5, pch = "|",
                  scales = list(x = list(relation="free"), y = list(relation="free")), ...,
                  auto.key = list(columns = 3))
}
# #####################################################################################


# #####################################################################################
# http://topepo.github.io/caret/visualizations.html
# #####################################################################################
DO_BOXPLOT_PLOT = function( XY, Y, ... ) {
    N = ncol(XY)
    featurePlot(x = XY, y = Y, plot = "box", scales = list(y = list(relation="free"), x = list(rot = 90)),
                  layout = GET_LAYOUT_PARAMS(N), auto.key = list(columns = 2))
}
# #####################################################################################


# #####################################################################################
# some of these graphics/gglot seem buggy in nature as they require toplevel env eval 
# and thus, cannot be embedded into functions
# #####################################################################################
DO_FEATURE_EVAL_PLOTS = function(XY, Y, DO_PDF=FALSE, filename = "plot_feature_evaluations.pdf" ) {
    N = ncol(XY)
    if( DO_PDF ) pdf( filename, 11, 8 )
    trellis.par.set(caretTheme(), warn = FALSE)					   
    print(featurePlot(x = XY, y = Y, plot = "ellipse", auto.key = list(columns = 3)))
    print(featurePlot(x = XY, y = Y, plot = "density", adjust = 1.5, pch = "|",
                    scales = list(x = list(relation="free"), y = list(relation="free")), auto.key = list(columns = 3)))
    print(featurePlot(x = XY, y = Y, plot = "box", scales = list(y = list(relation="free"), x = list(rot = 90)),
                    layout = GET_LAYOUT_PARAMS(N), auto.key = list(columns = 2)))
    if( DO_PDF ) dev.off()
}
# #####################################################################################


# #####################################################################################
# R layout manpage
# #####################################################################################
DO_SCATTERPLOT_WITH_HISTOGRAMS = function( x, y ) {
     def.par <- par(no.readonly = TRUE) # save default, for resetting...
     
     ## divide the device into two rows and two columns
     ## allocate figure 1 all of row 1
     ## allocate figure 2 the intersection of column 2 and row 2
     layout(matrix(c(1,1,0,2), 2, 2, byrow = TRUE))

     ## show the regions that have been allocated to each plot
     layout.show(2)
     
     ## divide device into two rows and two columns
     ## allocate figure 1 and figure 2 as above
     ## respect relations between widths and heights
     nf <- layout(matrix(c(1,1,0,2), 2, 2, byrow = TRUE), respect = TRUE)
     layout.show(nf)
     
     ## create single figure which is 5cm square
     nf <- layout(matrix(1), widths = lcm(5), heights = lcm(5))
     layout.show(nf)
     
     ##-- Create a scatterplot with marginal histograms -----
     xhist <- hist(x, breaks = seq(-3,3,0.5), plot = FALSE)
     yhist <- hist(y, breaks = seq(-3,3,0.5), plot = FALSE)
     top <- max(c(xhist$counts, yhist$counts))
     xrange <- c(-3, 3)
     yrange <- c(-3, 3)
     nf <- layout(matrix(c(2,0,1,3),2,2,byrow = TRUE), c(3,1), c(1,3), TRUE)
     layout.show(nf)
     
     par(mar = c(3,3,1,1))
     plot(x, y, xlim = xrange, ylim = yrange, xlab = "", ylab = "")
     par(mar = c(0,3,1,1))
     barplot(xhist$counts, axes = FALSE, ylim = c(0, top), space = 0)
     par(mar = c(3,0,1,1))
     barplot(yhist$counts, axes = FALSE, xlim = c(0, top), space = 0, horiz = TRUE)
     
     par(def.par)  #- reset to default
}

# #####################################################################################


