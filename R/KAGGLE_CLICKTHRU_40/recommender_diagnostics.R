# ######################################################################################################
# MACHINE LEARNING TOOLBOX FILES IN R
#           Copyright (C) Nelson R. Manohar
# 
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
# ######################################################################################################
# @AUTHOR:  Nelson R. Manohar Alers
# @EMAIL:   manohar.nelson@gmail.com
# @DATE:    September, 2014
# @URL:     http://www.bitbucket.org/nelsonmanohar/machinelearning
# ######################################################################################################
LICENSETEXT = "These R code samples (version Sep/2014), Copyright (C) Nelson R. Manohar,
comes with ABSOLUTELY NO WARRANTY.  This is free software, and you are welcome to 
redistribute it under conditions of the GNU General Public License." 
message( LICENSETEXT )
message("")
# ######################################################################################################



# ###################################################################################################
MOVIE_COMPARATOR = function(u1, u2, tier=1) {
    u1 = M2ID(u1)
    u2 = M2ID(u2)
    t1 = substr(MOVIES[u1,'movie_title'],1,20)
    t2 = substr(MOVIES[u2,'movie_title'],1,20)
    d12= D2[cbind(u1,u2)]
    if ( tier == 1 ) m0 = MMMAP1[cbind(u1,u2)]
    if ( tier == 2 ) m0 = MMMAP2[cbind(u1,u2)]
    m1 = sprintf( "%20s...[%13s], ", t1, WHICH_GENRES_FOR_MOVIE( u1 ) )
    m2 = sprintf( "%20s...[%13s], ", t2, WHICH_GENRES_FOR_MOVIE( u2 ) )
    m = sprintf( "[%4s] [%8.3f] %36s ||| %36s", m0, d12, m1, m2 )
    return( m ) 
}
# ###################################################################################################


# ###################################################################################################
USER_COMPARATOR = function(u1, u2, tier=1) {
    u1 = U2ID(u1)
    u2 = U2ID(u2)
    t1 = apply( USERS[u1,], 1, CONCAT )
    t2 = apply( USERS[u2,], 1, CONCAT )
    d12= D1[cbind(u1,u2)]
    if ( tier == 1 ) m0 = UUMAP1[cbind(u1,u2)] # CHECK: it is stuck at 1
    if ( tier == 2 ) m0 = UUMAP2[cbind(u1,u2)] # CHECK: it is stuck at 1
    m1 = sprintf( "%24s...", t1 )
    m2 = sprintf( "%24s...", t2 )
    m = sprintf( "[%8.3f] %36s ||| %36s", d12, m1, m2 )
    return( m ) 
}
# ###################################################################################################


# ###################################################################################################
USER2MOVIE_PRINTER = function(u1, u2, tier=1) {
    t1 = apply( USERS[u1,],  1, CONCAT )
    t2 = substr(MOVIES[u2,'movie_title'],1,20)
    if ( tier == 1 ) m0 = UMMAP1[cbind(u1,u2)] # CHECK: it is stuck at 1
    if ( tier == 2 ) m0 = UMMAP2[cbind(u1,u2)] # CHECK: it is stuck at 1
    m1 = sprintf( "%24s...", t1 )
    m2 = sprintf( "%20s...[%13s], ", t2, WHICH_GENRES_FOR_MOVIE( u2 ) )
    m = sprintf( "[%4s] %36s ||| %36s", m0, m1, m2 )
    return( m ) 
}
# ###################################################################################################


# ###################################################################################################
EXTRACT_MAPPINGS = function( MAPS, TITLE="", MAPPER="", TIER=1 ) {
    cat( HEADER )
    print( TITLE )
    MDF_STATS( MAPS )
    cat( HEADER )

    MAPS = as.data.frame(MAPS, stringsAsFactors=FALSE)
    SORTED_MAPS = MAPS[order(MAPS$row,MAPS$col),]
    U1 = SORTED_MAPS[1]
    U2 = SORTED_MAPS[2]
    u1 = U1[,1]
    u2 = U2[,1]

    SORTED_MAPS = MAPS[order(MAPS$col,MAPS$row),]
    T1 = SORTED_MAPS[1]
    T2 = SORTED_MAPS[2]
    t1 = T1[,1]
    t2 = T2[,1]

    if ( class(MAPPER) == "function" )
        cat( sprintf( "%5s --> %5s   %s\n", t1, t2, MAPPER( t1, t2, tier=TIER )))
    else {
        cat( sprintf( "%5s --> %5s ||| %5s --> %5s\n", u1, u2, t1, t2, tier=TIER ))
    }

    cat( HEADER )
    NEWLINE(3)
}
# ###################################################################################################


# ###################################################################################################
UPDATE_MAPPING = function( mtype="", idx1, idx2 ) {
    if ( mtype == "UU1" )
        UUMAP1[ idx1, idx2] <<- UUMAP1[ idx1, idx2] + 1
    else if ( mtype == "UU2" )
        UUMAP2[ idx1, idx2] <<- UUMAP2[ idx1, idx2] + 1
    else if ( mtype == "MM1" )
        MMMAP1[ idx1, idx2] <<- MMMAP1[ idx1, idx2] + 1
    else if ( mtype == "MM2" )
        MMMAP2[ idx1, idx2] <<- MMMAP2[ idx1, idx2] + 1
    else if ( mtype == "MU1" )
        MUMAP1[ idx1, idx2] <<- MUMAP1[ idx1, idx2] + 1
    else if ( mtype == "MU2" )
        MUMAP2[ idx1, idx2] <<- MUMAP2[ idx1, idx2] + 1
    else if ( mtype == "UM1" )
        UMMAP1[ idx1, idx2] <<- UMMAP1[ idx1, idx2] + 1
    else if ( mtype == "UM2" )
        UMMAP2[ idx1, idx2] <<- UMMAP2[ idx1, idx2] + 1
}
# ###################################################################################################


# ###################################################################################################
BUILD_EDGELIST = function ( MAPPINGS, PIVOT=1 ) {
    START = TRUE
    EG = data.frame()
    for ( i in 1:nrow(MAPPINGS) ) 
        for ( j in 1:ncol(MAPPINGS) ) {
            val = MAPPINGS[i,j]
            if ( val >= PIVOT ) {
                if ( START ) {
                    START = FALSE
                    rowlabel = rownames(MAPPINGS)[i]
                    collabel = colnames(MAPPINGS)[j]
                    print( paste( rowlabel, collabel ) )
                    EG = data.frame( 'row'=rowlabel, 'col'=collabel )
                } else {
                    rowlabel = rownames(MAPPINGS)[i]
                    collabel = colnames(MAPPINGS)[j]
                    EG = rbind( EG, data.frame( 'row'=rowlabel, 'col'=collabel ) )
                }
            }
        }
    return ( EG )
}
# ###################################################################################################


# ###################################################################################################
DO_BASIC_PLOTTING = function () {
    # plot basic stats about the selected data
    pdf("plot_basic_plots_collabfilt.pdf", 12, 8 )
    print( "PDF FILE contains before (original ratings) and after (recommended ratings) snapshot comparison of the fit: plot_basic_plots_collabfilt.pdf" )

    UR4R = round(RECOMMENDATIONS_FOR_U2M_RATINGS)       # recommendations generated wrt by filling NA values with collaborative filtered values 
    MR4R = t(RECOMMENDATIONS_FOR_U2M_RATINGS)           # above, but wrt movies and users
    UIDX = rownames(UR4R)           # users to movies 
    MIDX = rownames(MR4R)           # movis to users
    MG = M[M2ID(MIDX),6:ncol(M)]    # genere bits per movie
    UP = USER_PREFERENCES[UIDX,]    # genre preferences by user
    MF = MOVIE_FEATURES[MIDX,]      # genre feature values for each movie
    OR = WHICH_RATINGS              # original ratings of each user for all movies
    RO = t(OR)                      # original ratings of each movie across all users

    o_genre_means = c() 
    r_genre_means = c()  
    o5 = o4 = o3 = o2 = o1 = c()
    r5 = r4 = r3 = r2 = r1 = c()

    #val = sum( apply(ifelse(OBJ[ which_rows,]==level,1,0), 2, sum, na.rm=TRUE) )
    NUM_RATINGS_W = function( OBJ, level, which_rows ) {
        val = ifelse(OBJ[ which_rows,]==level, 1, 0)
        if ( all(is.na(val)) ) {
            val = 0.0
            return ( val )
        }
        if ( class(val) != "matrix" ) {
            val = mean( val, na.rm=TRUE)
            return ( val )
        }
        val = apply(val, 2, SUM )
        val = SUM(val)
    }

    # val = mean(apply( RO[which_rows,],   1, mean, na.rm=TRUE ), na.rm=TRUE))
    GENRE_MEANS_W = function( OBJ, which_rows ) {
        val = OBJ[which_rows,]
        if ( all(is.na(val)) )  {
            val = 0.0
            return ( val )
        } 
        if ( class(val) != "matrix" ) {
            val = mean( val, na.rm=TRUE)
            return ( val )
        }
        val = apply( val, 1, MEAN )
        val = mean( val, na.rm=TRUE)
    }

    for ( col in colnames(MG) ) {
        which_rows = which( MG[,col]== 1 )

        o_genre_means = append ( o_genre_means, GENRE_MEANS_W( RO,   which_rows ))
        r_genre_means = append ( r_genre_means, GENRE_MEANS_W( MR4R, which_rows ))

        o5 = append( o5, NUM_RATINGS_W( RO, 5, which_rows ))
        o4 = append( o4, NUM_RATINGS_W( RO, 4, which_rows ))
        o3 = append( o3, NUM_RATINGS_W( RO, 3, which_rows ))
        o2 = append( o2, NUM_RATINGS_W( RO, 2, which_rows ))
        o1 = append( o1, NUM_RATINGS_W( RO, 1, which_rows ))

        r5 = append( r5, NUM_RATINGS_W( MR4R, 5, which_rows ))
        r4 = append( r4, NUM_RATINGS_W( MR4R, 4, which_rows ))
        r3 = append( r3, NUM_RATINGS_W( MR4R, 3, which_rows ))
        r2 = append( r2, NUM_RATINGS_W( MR4R, 2, which_rows ))
        r1 = append( r1, NUM_RATINGS_W( MR4R, 1, which_rows ))
    } 

    reviews_written_per_user  = rowSums(ifelse(OR>0,1,0), na.rm=TRUE)
    reviews_written_per_movie = colSums(ifelse(OR>0,1,0), na.rm=TRUE)
    number_of_movies_per_genre = apply( MG, 2, sum )

    o = rbind( o1, o2, o3, o4, o5 ) ; r = rbind( r1, r2, r3, r4, r5 )
    colnames(r) = colnames(o) = colnames(MG) ; rownames(o) = rownames(r) = paste(5:1,"*",sep="")

    difference_in_genre_review_means = ( o_genre_means - r_genre_means ) ^2
    names(difference_in_genre_review_means) = colnames(MG)

    ot = apply( o, 1, sum )
    rt = apply( r, 1, sum )
    total_ratings_per_level = rbind( ot, rt )

    uo = rowMeans( OR,      na.rm=TRUE )
    ur = rowMeans( UR4R,    na.rm=TRUE )
    mo = rowMeans( t(OR),   na.rm=TRUE )
    mr = rowMeans( t(UR4R), na.rm=TRUE )

    op = par( mfrow=c(3,2) )
        DO_BARPLOT( number_of_movies_per_genre, cex=0.6, cex.axis=0.6, main="NUMBER MOVIES PER GENRE", las=2 )
        hist(reviews_written_per_user, breaks=32, main="FREQUENCY OF USERS WITH K REVIEWS", xlab="K = NUMBER REVIEWS WRITTEN" ) 

        DO_BARPLOT( total_ratings_per_level, 
                   main="ORIG/AFTER: OVERALL DISTRIBUTION OF RATINGS" )
        DO_BARPLOT( difference_in_genre_review_means, 
                   main="ORIG/AFTR: DIFFERENCE IN GENRE MEANS", 
                   cex=0.6, cex.axis=0.6, las=2 )

        # http://stackoverflow.com/questions/20349929/stacked-bar-plot-in-r, http://www.statmethods.net/graphs/bar.html
        DO_BARPLOT( o, 
                   main="ORIG: NUM. RATINGS PER GENRE",  
                   cex=0.6,   cex.axis=0.6, las=2, legend = rownames(o))
        DO_BARPLOT( r, 
                   main="AFTR: NUM. RATINGS PER GENRE",  
                   cex=0.6,   cex.axis=0.6, las=2, legend = rownames(r))


    par( mfrow=c(4,1) )
        DO_BARPLOT( rowSums( ifelse(OR>0,1,0),na.rm=TRUE ), 
                   main="TOTAL NUMBER REVIEWS PER USER (ORIG)",  
                   col="gray", las=1, cex.axis=0.7, cex=0.7 )
        DO_BARPLOT( colSums( ifelse(OR>0,1,0),na.rm=TRUE ), 
                   main="TOTAL NUMBER REVIEWS PER MOVIE (ORIG)", 
                   col="gray", las=1, cex.axis=0.7, cex=0.7 )
        DO_BARPLOT( uo-ur, 
                   main="IMPACT OVER AVERAGE RATING (PER USER)  DUE TO RECOMMENDATIONS MADE",
                   xlab="DIFFERENCE IN (ORIG-AFTER) USER  RATING MEANS PER INDIVIDUAL USER ",  
                   col="gray", las=1, cex.axis=0.7, cex=0.7 )
        DO_BARPLOT( mo-mr, 
                   main="IMPACT OVER AVERAGE RATING (PER MOVIE) DUE TO RECOMMENDATIONS MADE",
                   xlab="DIFFERENCE IN (ORIG-AFTER) MOVIE RATING MEANS PER INDIVIDUAL MOVIE",  
                   col="gray", las=1, cex.axis=0.7, cex=0.7 )

    op = par( mfrow=c(3,2) )
        DO_HIST(apply(OR, 1, mean, na.rm=TRUE ) - apply(UR4R, 1, mean), nbins=32, 
                ptitle="WITH RESPECT TO USERS: HOW DOES THE RATING ERROR BEHAVES? (DISTRIBUTION)", 
                xlab="ORIG-AFTR DIFFERENCE IN OVERALL AVG RATING PER USER")
        plot(apply(OR, 1, mean, na.rm=TRUE ), apply(UR4R, 1, mean), cex=0.6, 
                main="WITH RESPECT TO USERS: HOW DOES THE RATING ERROR BEHAVES? (INDIVIDUALLY)", 
                xlab="ORIG-AFTR DIFFERENCE IN OVERALL AVG RATING PER USER")

        DO_HIST(apply(OR, 2, mean, na.rm=TRUE ) - apply(UR4R, 2, mean), nbins=32, 
                ptitle="WITH RESPECT TO MOVIES: HOW DOES THE RATING ERROR BEHAVES? (DISTRIBUTION)", 
                xlab="ORIG-AFTR DIFFERENCE IN OVERALL AVG RATING PER MOVIE")
        plot(apply(OR, 2, mean, na.rm=TRUE ), apply(UR4R, 2, mean), cex=0.6,
                main="WITH RESPECT TO MOVIES: HOW DOES THE RATING ERROR BEHAVES? (INDIVIDUALLY)", 
                xlab="ORIG-AFTR DIFFERENCE IN OVERALL AVG RATING PER MOVIE")

        boxplot( UP, cex=0.6, cex.axis=0.6, main="RANGE OF COEFFICIENTS FOR USER PREFERENCES",  las=2 )
        boxplot( MF, cex=0.6, cex.axis=0.6, main="RANGE OF COEFFICIENTS FOR MOVIE FEATURES",    las=2 )

    par( op )
    dev.off()
}
# ###################################################################################################


