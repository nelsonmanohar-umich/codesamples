
############################ 
#  		From The HPS Transform and Its Applications
#                          By Dr. Nelson R. Manohar
############################
# INITIALIZATION
reset
set grid
set palette color
############################
cd "E:/HPS_DATA/"

############################
# RESULTANT HPS DNA MATCH BEHAVIOR
############################
reset
set grid
set terminal png giant butt size 1200,800 enhanced
set output "HPS_OUTPUTS/DNA_MATCH_BEHAVIOR.PNG"
set multiplot
	set title 'HPS DNA SEQUENCE ALIGNER - 8 MATCHES (8 UNIQUE) UNEARTHED (AT LIM=0.37 AND SEGDUR-THRESHOLD=6)'
	   # HPS_MATCH_TITLE
        ############################
        set autoscale y
        ############################
	set arrow from 1010.00, 2.25 	to 33976.00, 2.25 heads front lc rgb 'brown'
 set label "DNA test sequence section [1000:33977](indexes offset by [1000])"	at 1100.00, 2.75 front
 set label "[1DNA-BASES-SEQ1786520TESTCASE]"	at 1100.00, 1.75 front
		# HPS_TESTSEQ_LABEL
	set arrow from 0.00, 3.75 	to 1000.00, 3.75 heads front lc rgb 'blue'
 set label "DNA signature section [0:1000]"	at 100.00, 4.30 
 set label "[1DNA-BASES-E-COLI-K12-1786520-FROM5000-TO5999]"	at 100.00, 3.30
		# HPS_SIGNATURE_LABEL
	
	set label "M0 [5999:6985]" at 6097.60, -.10 front
	set arrow from 5999.00, -.50 	to 6985.00, -.50 heads lw 2 front
	set label "M1 [14467:15443]" at 14564.60, 0.70 front
	set arrow from 14467.00, 0.30 	to 15443.00, 0.30 heads lw 2 front
	set label "M2 [19971:20961]" at 20070.00, 1.50 front
	set arrow from 19971.00, 1.10 	to 20961.00, 1.10 heads lw 2 front
	set label "M3 [17988:18910]" at 18080.20, 2.30 front
	set arrow from 17988.00, 1.90 	to 18910.00, 1.90 heads lw 2 front
	set label "M4 [33017:33952]" at 33110.50, 3.10 front
	set arrow from 33017.00, 2.70 	to 33952.00, 2.70 heads lw 2 front
	set label "M5 [24981:25975]" at 25080.40, -.10 front
	set arrow from 24981.00, -.50 	to 25975.00, -.50 heads lw 2 front
	set label "M6 [8013:8978]" at 8109.50, 0.70 front
	set arrow from 8013.00, 0.30 	to 8978.00, 0.30 heads lw 2 front
	set label "M7 [31007:31980]" at 31104.30, 1.50 front
	set arrow from 31007.00, 1.10 	to 31980.00, 1.10 heads lw 2 front
		# HYP_BASE_LABELS
        ############################
	set grid
	set xtics
	set xlabel ''
	set ylabel ''
	set ytics autofreq  ("  0" 0, "  A" 0, "  C" 1, "  G" 2, "  T" 3)
        ############################
	set tmargin 0
	set bmargin 0
	set size 1.0, 0.18
	set origin 0, 0.75
	set yrange [-1:5]
	     # 0 : 1/3 TESTSEQLEN
	plot  [0:33977/4] "HPS_INPUTS/HPS_1DNA_INPUT_SERIES_ENCODED.DNA" using 1:2   title 'DNA test sequence (i)' with lines lw 1 lc rgb 'salmon', \
	              "HPS_INPUTS/HPS_1DNA_SIGNATURE_ENCODED.DNA"    using 1:2   title 'DNA signature (i)'     with lines lw 1 lc rgb 'light-blue'
        ############################
	set title ''
	set tmargin 0
	set bmargin 0
	set size 1.0, 0.18
	set origin 0, 0.52
	set yrange [-1:5]
	     # 0 : 1/3 TESTSEQLEN
	plot  [33977/4:33977/2] "HPS_INPUTS/HPS_1DNA_INPUT_SERIES_ENCODED.DNA" using 1:2   title 'DNA test sequence (i)' with lines lw 1 lc rgb 'salmon', \
	              "HPS_INPUTS/HPS_1DNA_SIGNATURE_ENCODED.DNA"    using 1:2   title 'DNA signature (i)'     with lines lw 1 lc rgb 'light-blue'
        ############################
	set size 1.0, 0.18
	set origin 0, 0.28
	set xlabel ''
	     # 1/3 : 2/3 TESTSEQLEN
	plot  [33977/2:101931/4] "HPS_INPUTS/HPS_1DNA_INPUT_SERIES_ENCODED.DNA"    using 1:2   title 'DNA test sequence (i)' with lines lw 1 lc rgb 'salmon'
        ############################
	set size 1.0, 0.18
	set origin 0, 0.04
	set xlabel ''
	     # 2/3 : 3/3 TESTSEQLEN
	plot  [101931/4:33977] "HPS_INPUTS/HPS_1DNA_INPUT_SERIES_ENCODED.DNA"    using 1:2   title 'DNA test sequence (i)' with lines lw 1 lc rgb 'salmon'
        ############################
unset multiplot
############################

############################
reset
set grid
set title 'HPS DNA SEQUENCE ALIGNER - 8 MATCHES (8 UNIQUE) UNEARTHED (AT LIM=0.37 AND SEGDUR-THRESHOLD=6)'
	   # HPS_MATCH_TITLE
set terminal png giant butt size 1200,800 enhanced
set output "HPS_OUTPUTS/DNA_MATCH_BEHAVIOR_HISTOGRAM.PNG"
	set multiplot
        	############################
		set size 1.0, 0.60
		set origin 0, 0.35
		set tmargin 0
		set bmargin 0
		f(x) = 30.028 * 1/(2 *   3.14159265 *   0.07795569) * exp( -1 * (x -   0.16853500 )**2 / (2 *   0.07795569 ** 2))
 		 g(x) = 26.078 * 1/(2 *   3.14159265 *   0.06787962) * exp( -1 * (x -   0.16382614 )**2 / (2 *   0.06787962 ** 2))
        	############################
		
		set arrow from 0.17, 27.03 to 0.25, 27.03 heads lc rgb 'light-blue' front
		set arrow from 0.17, 24.03 to 0.40, 24.03 heads lc rgb 'light-blue' front
		set arrow from 0.17, 21.03 to 0.37, 21.03 heads lc rgb 'blue' front
		set arrow from 0.37, 18.03 to 1.01, 18.03 heads lc rgb 'gold' front
		set label "SPREAD   OF 1.00 SIGMA LEVELS OF DETECTION W. NOISE FROM MEAN" at 0.19, 28.03 front
		set label "SPREAD   OF 3.00 SIGMA LEVELS OF DETECTION W. NOISE FROM MEAN" at 0.19, 25.03 front
		set label "SELECTED THRESHOLD IS AT 2.65 SIGMA LEVELS FROM W. NOISE MEAN" at 0.19, 22.03 front
		set label "N(Z>2.65)% SEARCH REGION"          		     	     at 0.40, 19.03 front
		set label "GIVEN LIM=0.37 "            		     	     	     at 0.40, 17.03 front
			# GAUSSIAN LABELS
		set xlabel 'HPS DNA coarsegrain match strength'
		set ylabel 'frequency'
		set xrange [0:1.25]
		set xtics   ("0.0" 0, "0.2" 0.20, "0.4" 0.40, "0.6" 0.6, "LIM" .37, "0.8" 0.80, "1.0" 1)
									  	# *HPS_DNA_matching_goal*
		
	set label ".M0" at 0.73, 2.00 front
	set label ".M1" at 0.62, 1.00 front
	set label ".M2" at 0.56, 1.00 front
	set label ".M3" at 0.45, 1.00 front
	set label ".M4" at 0.44, 1.00 front
	set label ".M5" at 0.41, 2.00 front
	set label ".M6" at 0.39, 1.00 front
	set label ".M7" at 0.37, 2.00 front
			# HIST_LABELs
	
		plot  "HPS_OUTPUTS/HPS_DNA_MATCHBEHAVIOR_HISTOGRAM.DNA"  using 1:2   title 'HPS DNA match rating' with boxes fs solid lw 2 lc rgb 'red', \
			f(x) title 'normal-fit (across all ratings, outliers included)', g(x) lw 2 title 'normal-fit (bestfit w/o outliers (i.e., pattern matches)'
        	############################
		reset
		set grid
		set xtics
		set autoscale x
		set size 1.0, 0.24
		set origin 0, 0.04
		set tmargin 0
		set bmargin 0
		set xlabel ''
		set ylabel 'HPS DNA confidence(i)'
		set yrange [0 : 1 ]
		set ytics  autofreq ( "0.0" 0.0, "0.2" 0.20, "0.4" 0.4, "0.6" 0.6, "LIM" .37, "0.8" 0.8, "1.0" 1 )
									   	            	# *HPS_DNA_MATCHING_GOAL*
		
	set label "M0 [5999:6985]" at 5999.00, 0.40 front
	set label "M1 [14467:15443]" at 14467.00, 0.50 front
	set label "M2 [19971:20961]" at 19971.00, 0.60 front
	set label "M3 [17988:18910]" at 17988.00, 0.70 front
	set label "M4 [33017:33952]" at 33017.00, 0.80 front
	set label "M5 [24981:25975]" at 24981.00, 0.40 front
	set label "M6 [8013:8978]" at 8013.00, 0.50 front
	set label "M7 [31007:31980]" at 31007.00, 0.60 front
			# HYP_MATCH_LABELS
		plot  "HPS_OUTPUTS/HPS_DNA_BEHAVIOR_PATTERN_DISCOVERY.DNA"  using 1:3   title 'HPS DNA coarsegrain match strength (i)' with impulses
        	############################
	unset multiplot
	unset label
############################

############################
# FINAL ALIGNMENT VECTOR PLOT PER PATTERN
############################
reset
set terminal png giant butt size 1200,220 enhanced
set output "HPS_OUTPUTS/HPS_DNA_ALIGNMENT_ADJUSTMENT_LINEPLOT_M0.PNG"
############################
set multiplot
	set size   1.0, 0.65
	set origin 0.0, 0.35
	set grid
	set xtics
	set ytics autofreq  ( "  A" 0, "  C" 1, "  G" 2, "  T" 3, "del" 4, "ins" 5)
	set xlabel ''
	set ylabel ''
	set yrange [-1:10]
		set xrange [5938:7046]
	# set xrange 
	
	set label "PRELIM-ALIGN (5999)"	at 6009, 9.0 front
		set arrow from 5999, 8.5 to 5999, -0.5 head lw 3 lc rgb 'black'
	set label "ALIGN-OFFSET (5999, +0)"	at 6009, 7.8 front
		set arrow from 5999, 7.3 to 5999, 7.3 head lw 3 lc rgb 'black'
	# alignment labels
	set bmargin 0
	plot "HPS_OUTPUTS/HPS_1DNA_ADJUSTED_FINEGRAIN_XY_ALIGNMENT_M0.DNA"    using 7:2   title '' with impulses lc rgb 'light-blue', \
		"HPS_OUTPUTS/HPS_1DNA_ADJUSTED_FINEGRAIN_XY_ALIGNMENT_M0.DNA" using 7:3   title '' with impulses lc rgb 'salmon'
	############################
	set tmargin 0
	unset label
	set yrange [-1:4]
	unset xtics
	unset arrow
	set ytics autofreq  ( "  A" 0, "  C" 1, "  G" 2, "  T" 3, "bst" 4)
	set size   1.0, 0.24
	set origin 0.0, 0.0
	plot    "HPS_OUTPUTS/HPS_1DNA_ADJUSTED_FINEGRAIN_XY_ALIGNMENT_M0.DNA" using 7:2   title '' with impulses lc rgb 'gray', \
		"HPS_INPUTS/HPS_1DNA_INPUT_SERIES_ENCODED.DNA"                 using 1:2   title '' with impulses lc rgb 'salmon'
unset multiplot
############################

############################
# FINAL ALIGNMENT VECTOR PLOT PER PATTERN
############################
reset
set terminal png giant butt size 1200,220 enhanced
set output "HPS_OUTPUTS/HPS_DNA_ALIGNMENT_ADJUSTMENT_LINEPLOT_M1.PNG"
############################
set multiplot
	set size   1.0, 0.65
	set origin 0.0, 0.35
	set grid
	set xtics
	set ytics autofreq  ( "  A" 0, "  C" 1, "  G" 2, "  T" 3, "del" 4, "ins" 5)
	set xlabel ''
	set ylabel ''
	set yrange [-1:10]
		set xrange [14406:15504]
	# set xrange 
	
	set label "PRELIM-ALIGN (14467)"	at 14477, 9.0 front
		set arrow from 14467, 8.5 to 14467, -0.5 head lw 3 lc rgb 'black'
	set label "ALIGN-OFFSET (14467, +13)"	at 14477, 7.8 front
		set arrow from 14467, 7.3 to 14480, 7.3 head lw 3 lc rgb 'black'
	set label "RESYNC (15282, 3)"	at 15292, 6.6 front
		set arrow from 15282, 6.1 to 15282, -0.5 head lw 3 lc rgb 'black'
	set label "DELETE (15282, 30)"	at 15292, 5.4 front
		set arrow from 15282, 4.9 to 15312, 4.9 head lw 3 lc rgb 'black'
	# alignment labels
	set bmargin 0
	plot "HPS_OUTPUTS/HPS_1DNA_ADJUSTED_FINEGRAIN_XY_ALIGNMENT_M1.DNA"    using 7:2   title '' with impulses lc rgb 'light-blue', \
		"HPS_OUTPUTS/HPS_1DNA_ADJUSTED_FINEGRAIN_XY_ALIGNMENT_M1.DNA" using 7:3   title '' with impulses lc rgb 'salmon'
	############################
	set tmargin 0
	unset label
	set yrange [-1:4]
	unset xtics
	unset arrow
	set ytics autofreq  ( "  A" 0, "  C" 1, "  G" 2, "  T" 3, "bst" 4)
	set size   1.0, 0.24
	set origin 0.0, 0.0
	plot    "HPS_OUTPUTS/HPS_1DNA_ADJUSTED_FINEGRAIN_XY_ALIGNMENT_M1.DNA" using 7:2   title '' with impulses lc rgb 'gray', \
		"HPS_INPUTS/HPS_1DNA_INPUT_SERIES_ENCODED.DNA"                 using 1:2   title '' with impulses lc rgb 'salmon'
unset multiplot
############################

############################
# FINAL ALIGNMENT VECTOR PLOT PER PATTERN
############################
reset
set terminal png giant butt size 1200,220 enhanced
set output "HPS_OUTPUTS/HPS_DNA_ALIGNMENT_ADJUSTMENT_LINEPLOT_M2.PNG"
############################
set multiplot
	set size   1.0, 0.65
	set origin 0.0, 0.35
	set grid
	set xtics
	set ytics autofreq  ( "  A" 0, "  C" 1, "  G" 2, "  T" 3, "del" 4, "ins" 5)
	set xlabel ''
	set ylabel ''
	set yrange [-1:10]
		set xrange [19910:21022]
	# set xrange 
	
	set label "PRELIM-ALIGN (19971)"	at 19981, 9.0 front
		set arrow from 19971, 8.5 to 19971, -0.5 head lw 3 lc rgb 'black'
	set label "ALIGN-OFFSET (19971, +28)"	at 19981, 7.8 front
		set arrow from 19971, 7.3 to 19999, 7.3 head lw 3 lc rgb 'black'
	set label "RESYNC (20301, 3)"	at 20311, 6.6 front
		set arrow from 20301, 6.1 to 20301, -0.5 head lw 3 lc rgb 'black'
	set label "DELETE (20301, 30)"	at 20311, 5.4 front
		set arrow from 20301, 4.9 to 20331, 4.9 head lw 3 lc rgb 'black'
	# alignment labels
	set bmargin 0
	plot "HPS_OUTPUTS/HPS_1DNA_ADJUSTED_FINEGRAIN_XY_ALIGNMENT_M2.DNA"    using 7:2   title '' with impulses lc rgb 'light-blue', \
		"HPS_OUTPUTS/HPS_1DNA_ADJUSTED_FINEGRAIN_XY_ALIGNMENT_M2.DNA" using 7:3   title '' with impulses lc rgb 'salmon'
	############################
	set tmargin 0
	unset label
	set yrange [-1:4]
	unset xtics
	unset arrow
	set ytics autofreq  ( "  A" 0, "  C" 1, "  G" 2, "  T" 3, "bst" 4)
	set size   1.0, 0.24
	set origin 0.0, 0.0
	plot    "HPS_OUTPUTS/HPS_1DNA_ADJUSTED_FINEGRAIN_XY_ALIGNMENT_M2.DNA" using 7:2   title '' with impulses lc rgb 'gray', \
		"HPS_INPUTS/HPS_1DNA_INPUT_SERIES_ENCODED.DNA"                 using 1:2   title '' with impulses lc rgb 'salmon'
unset multiplot
############################

############################
# FINAL ALIGNMENT VECTOR PLOT PER PATTERN
############################
reset
set terminal png giant butt size 1200,220 enhanced
set output "HPS_OUTPUTS/HPS_DNA_ALIGNMENT_ADJUSTMENT_LINEPLOT_M3.PNG"
############################
set multiplot
	set size   1.0, 0.65
	set origin 0.0, 0.35
	set grid
	set xtics
	set ytics autofreq  ( "  A" 0, "  C" 1, "  G" 2, "  T" 3, "del" 4, "ins" 5)
	set xlabel ''
	set ylabel ''
	set yrange [-1:10]
		set xrange [17927:18971]
	# set xrange 
	
	set label "PRELIM-ALIGN (17988)"	at 17998, 9.0 front
		set arrow from 17988, 8.5 to 17988, -0.5 head lw 3 lc rgb 'black'
	set label "ALIGN-OFFSET (17988, +11)"	at 17998, 7.8 front
		set arrow from 17988, 7.3 to 17999, 7.3 head lw 3 lc rgb 'black'
	set label "RESYNC (18301, 3)"	at 18311, 6.6 front
		set arrow from 18301, 6.1 to 18301, -0.5 head lw 3 lc rgb 'black'
	set label "INSERT (18301, 30)"	at 18311, 5.4 front
		set arrow from 18301, 4.9 to 18331, 4.9 head lw 3 lc rgb 'black'
	set label "RESYNC (18801, 3)"	at 18811, 9.0 front
		set arrow from 18801, 8.5 to 18801, -0.5 head lw 3 lc rgb 'black'
	set label "DELETE (18801, 30)"	at 18811, 7.8 front
		set arrow from 18801, 7.3 to 18831, 7.3 head lw 3 lc rgb 'black'
	# alignment labels
	set bmargin 0
	plot "HPS_OUTPUTS/HPS_1DNA_ADJUSTED_FINEGRAIN_XY_ALIGNMENT_M3.DNA"    using 7:2   title '' with impulses lc rgb 'light-blue', \
		"HPS_OUTPUTS/HPS_1DNA_ADJUSTED_FINEGRAIN_XY_ALIGNMENT_M3.DNA" using 7:3   title '' with impulses lc rgb 'salmon'
	############################
	set tmargin 0
	unset label
	set yrange [-1:4]
	unset xtics
	unset arrow
	set ytics autofreq  ( "  A" 0, "  C" 1, "  G" 2, "  T" 3, "bst" 4)
	set size   1.0, 0.24
	set origin 0.0, 0.0
	plot    "HPS_OUTPUTS/HPS_1DNA_ADJUSTED_FINEGRAIN_XY_ALIGNMENT_M3.DNA" using 7:2   title '' with impulses lc rgb 'gray', \
		"HPS_INPUTS/HPS_1DNA_INPUT_SERIES_ENCODED.DNA"                 using 1:2   title '' with impulses lc rgb 'salmon'
unset multiplot
############################

############################
# FINAL ALIGNMENT VECTOR PLOT PER PATTERN
############################
reset
set terminal png giant butt size 1200,220 enhanced
set output "HPS_OUTPUTS/HPS_DNA_ALIGNMENT_ADJUSTMENT_LINEPLOT_M4.PNG"
############################
set multiplot
	set size   1.0, 0.65
	set origin 0.0, 0.35
	set grid
	set xtics
	set ytics autofreq  ( "  A" 0, "  C" 1, "  G" 2, "  T" 3, "del" 4, "ins" 5)
	set xlabel ''
	set ylabel ''
	set yrange [-1:10]
		set xrange [32956:33977]
	# set xrange 
	
	set label "PRELIM-ALIGN (33017)"	at 33027, 9.0 front
		set arrow from 33017, 8.5 to 33017, -0.5 head lw 3 lc rgb 'black'
	set label "ALIGN-OFFSET (33017, -16)"	at 33027, 7.8 front
		set arrow from 33017, 7.3 to 33001, 7.3 head lw 3 lc rgb 'black'
	set label "RESYNC (33203, 3)"	at 33213, 6.6 front
		set arrow from 33203, 6.1 to 33203, -0.5 head lw 3 lc rgb 'black'
	set label "DELETE (33203, 10)"	at 33213, 5.4 front
		set arrow from 33203, 4.9 to 33213, 4.9 head lw 3 lc rgb 'black'
	set label "RESYNC (33403, 3)"	at 33413, 9.0 front
		set arrow from 33403, 8.5 to 33403, -0.5 head lw 3 lc rgb 'black'
	set label "DELETE (33403, 10)"	at 33413, 7.8 front
		set arrow from 33403, 7.3 to 33413, 7.3 head lw 3 lc rgb 'black'
	set label "RESYNC (33503, 3)"	at 33513, 6.6 front
		set arrow from 33503, 6.1 to 33503, -0.5 head lw 3 lc rgb 'black'
	set label "DELETE (33503, 8)"	at 33513, 5.4 front
		set arrow from 33503, 4.9 to 33511, 4.9 head lw 3 lc rgb 'black'
	set label "RESYNC (33612, 3)"	at 33622, 9.0 front
		set arrow from 33612, 8.5 to 33612, -0.5 head lw 3 lc rgb 'black'
	set label "DELETE (33612, 10)"	at 33622, 7.8 front
		set arrow from 33612, 7.3 to 33622, 7.3 head lw 3 lc rgb 'black'
	set label "RESYNC (33812, 3)"	at 33822, 6.6 front
		set arrow from 33812, 6.1 to 33812, -0.5 head lw 3 lc rgb 'black'
	set label "DELETE (33812, 10)"	at 33822, 5.4 front
		set arrow from 33812, 4.9 to 33822, 4.9 head lw 3 lc rgb 'black'
	# alignment labels
	set bmargin 0
	plot "HPS_OUTPUTS/HPS_1DNA_ADJUSTED_FINEGRAIN_XY_ALIGNMENT_M4.DNA"    using 7:2   title '' with impulses lc rgb 'light-blue', \
		"HPS_OUTPUTS/HPS_1DNA_ADJUSTED_FINEGRAIN_XY_ALIGNMENT_M4.DNA" using 7:3   title '' with impulses lc rgb 'salmon'
	############################
	set tmargin 0
	unset label
	set yrange [-1:4]
	unset xtics
	unset arrow
	set ytics autofreq  ( "  A" 0, "  C" 1, "  G" 2, "  T" 3, "bst" 4)
	set size   1.0, 0.24
	set origin 0.0, 0.0
	plot    "HPS_OUTPUTS/HPS_1DNA_ADJUSTED_FINEGRAIN_XY_ALIGNMENT_M4.DNA" using 7:2   title '' with impulses lc rgb 'gray', \
		"HPS_INPUTS/HPS_1DNA_INPUT_SERIES_ENCODED.DNA"                 using 1:2   title '' with impulses lc rgb 'salmon'
unset multiplot
############################

############################
# FINAL ALIGNMENT VECTOR PLOT PER PATTERN
############################
reset
set terminal png giant butt size 1200,220 enhanced
set output "HPS_OUTPUTS/HPS_DNA_ALIGNMENT_ADJUSTMENT_LINEPLOT_M5.PNG"
############################
set multiplot
	set size   1.0, 0.65
	set origin 0.0, 0.35
	set grid
	set xtics
	set ytics autofreq  ( "  A" 0, "  C" 1, "  G" 2, "  T" 3, "del" 4, "ins" 5)
	set xlabel ''
	set ylabel ''
	set yrange [-1:10]
		set xrange [24920:26036]
	# set xrange 
	
	set label "PRELIM-ALIGN (24981)"	at 24991, 9.0 front
		set arrow from 24981, 8.5 to 24981, -0.5 head lw 3 lc rgb 'black'
	set label "ALIGN-OFFSET (24981, +18)"	at 24991, 7.8 front
		set arrow from 24981, 7.3 to 24999, 7.3 head lw 3 lc rgb 'black'
	set label "RESYNC (25201, 3)"	at 25211, 6.6 front
		set arrow from 25201, 6.1 to 25201, -0.5 head lw 3 lc rgb 'black'
	set label "DELETE (25201, 10)"	at 25211, 5.4 front
		set arrow from 25201, 4.9 to 25211, 4.9 head lw 3 lc rgb 'black'
	set label "RESYNC (25401, 3)"	at 25411, 9.0 front
		set arrow from 25401, 8.5 to 25401, -0.5 head lw 3 lc rgb 'black'
	set label "DELETE (25401, 10)"	at 25411, 7.8 front
		set arrow from 25401, 7.3 to 25411, 7.3 head lw 3 lc rgb 'black'
	set label "RESYNC (25501, 3)"	at 25511, 6.6 front
		set arrow from 25501, 6.1 to 25501, -0.5 head lw 3 lc rgb 'black'
	set label "DELETE (25501, 8)"	at 25511, 5.4 front
		set arrow from 25501, 4.9 to 25509, 4.9 head lw 3 lc rgb 'black'
	set label "RESYNC (25610, 3)"	at 25620, 9.0 front
		set arrow from 25610, 8.5 to 25610, -0.5 head lw 3 lc rgb 'black'
	set label "DELETE (25610, 10)"	at 25620, 7.8 front
		set arrow from 25610, 7.3 to 25620, 7.3 head lw 3 lc rgb 'black'
	set label "RESYNC (25810, 3)"	at 25820, 6.6 front
		set arrow from 25810, 6.1 to 25810, -0.5 head lw 3 lc rgb 'black'
	set label "DELETE (25810, 10)"	at 25820, 5.4 front
		set arrow from 25810, 4.9 to 25820, 4.9 head lw 3 lc rgb 'black'
	# alignment labels
	set bmargin 0
	plot "HPS_OUTPUTS/HPS_1DNA_ADJUSTED_FINEGRAIN_XY_ALIGNMENT_M5.DNA"    using 7:2   title '' with impulses lc rgb 'light-blue', \
		"HPS_OUTPUTS/HPS_1DNA_ADJUSTED_FINEGRAIN_XY_ALIGNMENT_M5.DNA" using 7:3   title '' with impulses lc rgb 'salmon'
	############################
	set tmargin 0
	unset label
	set yrange [-1:4]
	unset xtics
	unset arrow
	set ytics autofreq  ( "  A" 0, "  C" 1, "  G" 2, "  T" 3, "bst" 4)
	set size   1.0, 0.24
	set origin 0.0, 0.0
	plot    "HPS_OUTPUTS/HPS_1DNA_ADJUSTED_FINEGRAIN_XY_ALIGNMENT_M5.DNA" using 7:2   title '' with impulses lc rgb 'gray', \
		"HPS_INPUTS/HPS_1DNA_INPUT_SERIES_ENCODED.DNA"                 using 1:2   title '' with impulses lc rgb 'salmon'
unset multiplot
############################

############################
# FINAL ALIGNMENT VECTOR PLOT PER PATTERN
############################
reset
set terminal png giant butt size 1200,220 enhanced
set output "HPS_OUTPUTS/HPS_DNA_ALIGNMENT_ADJUSTMENT_LINEPLOT_M6.PNG"
############################
set multiplot
	set size   1.0, 0.65
	set origin 0.0, 0.35
	set grid
	set xtics
	set ytics autofreq  ( "  A" 0, "  C" 1, "  G" 2, "  T" 3, "del" 4, "ins" 5)
	set xlabel ''
	set ylabel ''
	set yrange [-1:10]
		set xrange [7952:9039]
	# set xrange 
	
	set label "PRELIM-ALIGN (8013)"	at 8023, 9.0 front
		set arrow from 8013, 8.5 to 8013, -0.5 head lw 3 lc rgb 'black'
	set label "ALIGN-OFFSET (8013, -14)"	at 8023, 7.8 front
		set arrow from 8013, 7.3 to 7999, 7.3 head lw 3 lc rgb 'black'
	# alignment labels
	set bmargin 0
	plot "HPS_OUTPUTS/HPS_1DNA_ADJUSTED_FINEGRAIN_XY_ALIGNMENT_M6.DNA"    using 7:2   title '' with impulses lc rgb 'light-blue', \
		"HPS_OUTPUTS/HPS_1DNA_ADJUSTED_FINEGRAIN_XY_ALIGNMENT_M6.DNA" using 7:3   title '' with impulses lc rgb 'salmon'
	############################
	set tmargin 0
	unset label
	set yrange [-1:4]
	unset xtics
	unset arrow
	set ytics autofreq  ( "  A" 0, "  C" 1, "  G" 2, "  T" 3, "bst" 4)
	set size   1.0, 0.24
	set origin 0.0, 0.0
	plot    "HPS_OUTPUTS/HPS_1DNA_ADJUSTED_FINEGRAIN_XY_ALIGNMENT_M6.DNA" using 7:2   title '' with impulses lc rgb 'gray', \
		"HPS_INPUTS/HPS_1DNA_INPUT_SERIES_ENCODED.DNA"                 using 1:2   title '' with impulses lc rgb 'salmon'
unset multiplot
############################

############################
# FINAL ALIGNMENT VECTOR PLOT PER PATTERN
############################
reset
set terminal png giant butt size 1200,220 enhanced
set output "HPS_OUTPUTS/HPS_DNA_ALIGNMENT_ADJUSTMENT_LINEPLOT_M7.PNG"
############################
set multiplot
	set size   1.0, 0.65
	set origin 0.0, 0.35
	set grid
	set xtics
	set ytics autofreq  ( "  A" 0, "  C" 1, "  G" 2, "  T" 3, "del" 4, "ins" 5)
	set xlabel ''
	set ylabel ''
	set yrange [-1:10]
		set xrange [30946:32041]
	# set xrange 
	
	set label "PRELIM-ALIGN (31007)"	at 31017, 9.0 front
		set arrow from 31007, 8.5 to 31007, -0.5 head lw 3 lc rgb 'black'
	set label "ALIGN-OFFSET (31007, +0)"	at 31017, 7.8 front
		set arrow from 31007, 7.3 to 31007, 7.3 head lw 3 lc rgb 'black'
	set label "RESYNC (31008, 3)"	at 31018, 6.6 front
		set arrow from 31008, 6.1 to 31008, -0.5 head lw 3 lc rgb 'black'
	set label "DELETE (31008, 10)"	at 31018, 5.4 front
		set arrow from 31008, 4.9 to 31018, 4.9 head lw 3 lc rgb 'black'
	set label "RESYNC (31108, 3)"	at 31118, 9.0 front
		set arrow from 31108, 8.5 to 31108, -0.5 head lw 3 lc rgb 'black'
	set label "DELETE (31108, 0)"	at 31118, 7.8 front
		set arrow from 31108, 7.3 to 31108, 7.3 head lw 3 lc rgb 'black'
	set label "RESYNC (31208, 3)"	at 31218, 6.6 front
		set arrow from 31208, 6.1 to 31208, -0.5 head lw 3 lc rgb 'black'
	set label "DELETE (31208, 10)"	at 31218, 5.4 front
		set arrow from 31208, 4.9 to 31218, 4.9 head lw 3 lc rgb 'black'
	set label "RESYNC (31316, 3)"	at 31326, 9.0 front
		set arrow from 31316, 8.5 to 31316, -0.5 head lw 3 lc rgb 'black'
	set label "DELETE (31316, 0)"	at 31326, 7.8 front
		set arrow from 31316, 7.3 to 31316, 7.3 head lw 3 lc rgb 'black'
	set label "RESYNC (31408, 3)"	at 31418, 6.6 front
		set arrow from 31408, 6.1 to 31408, -0.5 head lw 3 lc rgb 'black'
	set label "DELETE (31408, 20)"	at 31418, 5.4 front
		set arrow from 31408, 4.9 to 31428, 4.9 head lw 3 lc rgb 'black'
	set label "RESYNC (31509, 3)"	at 31519, 9.0 front
		set arrow from 31509, 8.5 to 31509, -0.5 head lw 3 lc rgb 'black'
	set label "DELETE (31509, 8)"	at 31519, 7.8 front
		set arrow from 31509, 7.3 to 31517, 7.3 head lw 3 lc rgb 'black'
	set label "RESYNC (31618, 3)"	at 31628, 6.6 front
		set arrow from 31618, 6.1 to 31618, -0.5 head lw 3 lc rgb 'black'
	set label "DELETE (31618, 10)"	at 31628, 5.4 front
		set arrow from 31618, 4.9 to 31628, 4.9 head lw 3 lc rgb 'black'
	set label "RESYNC (31818, 3)"	at 31828, 9.0 front
		set arrow from 31818, 8.5 to 31818, -0.5 head lw 3 lc rgb 'black'
	set label "DELETE (31818, 10)"	at 31828, 7.8 front
		set arrow from 31818, 7.3 to 31828, 7.3 head lw 3 lc rgb 'black'
	# alignment labels
	set bmargin 0
	plot "HPS_OUTPUTS/HPS_1DNA_ADJUSTED_FINEGRAIN_XY_ALIGNMENT_M7.DNA"    using 7:2   title '' with impulses lc rgb 'light-blue', \
		"HPS_OUTPUTS/HPS_1DNA_ADJUSTED_FINEGRAIN_XY_ALIGNMENT_M7.DNA" using 7:3   title '' with impulses lc rgb 'salmon'
	############################
	set tmargin 0
	unset label
	set yrange [-1:4]
	unset xtics
	unset arrow
	set ytics autofreq  ( "  A" 0, "  C" 1, "  G" 2, "  T" 3, "bst" 4)
	set size   1.0, 0.24
	set origin 0.0, 0.0
	plot    "HPS_OUTPUTS/HPS_1DNA_ADJUSTED_FINEGRAIN_XY_ALIGNMENT_M7.DNA" using 7:2   title '' with impulses lc rgb 'gray', \
		"HPS_INPUTS/HPS_1DNA_INPUT_SERIES_ENCODED.DNA"                 using 1:2   title '' with impulses lc rgb 'salmon'
unset multiplot
############################

############################
reset
set grid
set format y "%3.0f"
set title ''
set terminal png giant butt size 1200,800 enhanced
set output "HPS_OUTPUTS/HPS_DNA_FINEGRAIN_SIG2MATCH_XY_PLOTS_FOR_M0_TO_M8.PNG"
set multiplot
set pointsize 0.25
############################

        set title ''
	set size   0.3, 0.25
	set origin 0.0, 0.08
	set tmargin 0
	set bmargin 0
	set xlabel ''
	set ylabel ''
	plot    "HPS_OUTPUTS/HPS_1DNA_CODED_FINEGRAIN_XY_ALIGNMENT_M0.DNA"     using 4:5 title 'ORIGINAL SIG-TO-M0', \
		"HPS_OUTPUTS/HPS_1DNA_ADJUSTED_FINEGRAIN_XY_ALIGNMENT_M0.DNA"  using 4:5 title 'ADJUSTED SIG-TO-M0'
	############################

        set title ''
	set size   0.3, 0.25
	set origin 0.33, 0.08
	set tmargin 0
	set bmargin 0
	set xlabel ''
	set ylabel ''
	plot    "HPS_OUTPUTS/HPS_1DNA_CODED_FINEGRAIN_XY_ALIGNMENT_M1.DNA"     using 4:5 title 'ORIGINAL SIG-TO-M1', \
		"HPS_OUTPUTS/HPS_1DNA_ADJUSTED_FINEGRAIN_XY_ALIGNMENT_M1.DNA"  using 4:5 title 'ADJUSTED SIG-TO-M1'
	############################

        set title ''
	set size   0.3, 0.25
	set origin 0.67, 0.08
	set tmargin 0
	set bmargin 0
	set xlabel ''
	set ylabel ''
	plot    "HPS_OUTPUTS/HPS_1DNA_CODED_FINEGRAIN_XY_ALIGNMENT_M2.DNA"     using 4:5 title 'ORIGINAL SIG-TO-M2', \
		"HPS_OUTPUTS/HPS_1DNA_ADJUSTED_FINEGRAIN_XY_ALIGNMENT_M2.DNA"  using 4:5 title 'ADJUSTED SIG-TO-M2'
	############################

        set title ''
	set size   0.3, 0.25
	set origin 0.0, 0.4
	set tmargin 0
	set bmargin 0
	set xlabel ''
	set ylabel ''
	plot    "HPS_OUTPUTS/HPS_1DNA_CODED_FINEGRAIN_XY_ALIGNMENT_M3.DNA"     using 4:5 title 'ORIGINAL SIG-TO-M3', \
		"HPS_OUTPUTS/HPS_1DNA_ADJUSTED_FINEGRAIN_XY_ALIGNMENT_M3.DNA"  using 4:5 title 'ADJUSTED SIG-TO-M3'
	############################

        set title ''
	set size   0.3, 0.25
	set origin 0.33, 0.4
	set tmargin 0
	set bmargin 0
	set xlabel ''
	set ylabel ''
	plot    "HPS_OUTPUTS/HPS_1DNA_CODED_FINEGRAIN_XY_ALIGNMENT_M4.DNA"     using 4:5 title 'ORIGINAL SIG-TO-M4', \
		"HPS_OUTPUTS/HPS_1DNA_ADJUSTED_FINEGRAIN_XY_ALIGNMENT_M4.DNA"  using 4:5 title 'ADJUSTED SIG-TO-M4'
	############################

        set title ''
	set size   0.3, 0.25
	set origin 0.67, 0.4
	set tmargin 0
	set bmargin 0
	set xlabel ''
	set ylabel ''
	plot    "HPS_OUTPUTS/HPS_1DNA_CODED_FINEGRAIN_XY_ALIGNMENT_M5.DNA"     using 4:5 title 'ORIGINAL SIG-TO-M5', \
		"HPS_OUTPUTS/HPS_1DNA_ADJUSTED_FINEGRAIN_XY_ALIGNMENT_M5.DNA"  using 4:5 title 'ADJUSTED SIG-TO-M5'
	############################

        set title ''
	set size   0.3, 0.25
	set origin 0.0, 0.7
	set tmargin 0
	set bmargin 0
	set xlabel ''
	set ylabel ''
	plot    "HPS_OUTPUTS/HPS_1DNA_CODED_FINEGRAIN_XY_ALIGNMENT_M6.DNA"     using 4:5 title 'ORIGINAL SIG-TO-M6', \
		"HPS_OUTPUTS/HPS_1DNA_ADJUSTED_FINEGRAIN_XY_ALIGNMENT_M6.DNA"  using 4:5 title 'ADJUSTED SIG-TO-M6'
	############################

        set title ''
	set size   0.3, 0.25
	set origin 0.33, 0.7
	set tmargin 0
	set bmargin 0
	set xlabel ''
	set ylabel ''
	plot    "HPS_OUTPUTS/HPS_1DNA_CODED_FINEGRAIN_XY_ALIGNMENT_M7.DNA"     using 4:5 title 'ORIGINAL SIG-TO-M7', \
		"HPS_OUTPUTS/HPS_1DNA_ADJUSTED_FINEGRAIN_XY_ALIGNMENT_M7.DNA"  using 4:5 title 'ADJUSTED SIG-TO-M7'
	############################

unset multiplot
############################

############################
# PHYLOGENIC TREE PLOT ATTEMPT
############################
reset
set terminal png giant butt size 1200,800 enhanced
set output "HPS_OUTPUTS/HPS_DNA_PHYLOGENIC_TREEPLOT.PNG"
############################
	set size   1.0, 1.0
	set origin 0.0, 0.05
	set grid
	set bmargin 0
	set tmargin 0
	set xlabel ''
	set ylabel ''
	set zlabel ''
	set xrange [0:150]
	set yrange [0:25]
	set zrange [0:100]
	set xtics autofreq ( "CLOSER" 25,  "" 50, "FARTHER" 75, "" 100 )
	set ytics autofreq ( "CLOSER" 5,   "" 10, "FARTHER" 15, "" 20  )
	set ztics autofreq ( "CLOSER" 20,  "" 40, "FARTHER" 60, "" 80 )
	
set label "M0 [5999:6985]" at 0, 0.0, 26.82927 front
set label "M1 [14467:15443]" at 34, 2.5, 38.461536 front
set label "M2 [19971:20961]" at 34, 3.0, 43.90244 front
set label "M6 [8013:8978]" at 42, 0.0, 60.975613 front
set label "M3 [17988:18910]" at 64, 4.5, 55.0 front
set label "M4 [33017:33952]" at 68, 11.0, 55.555553 front
set label "M5 [24981:25975]" at 68, 10.0, 58.536587 front
set label "M7 [31007:31980]" at 106, 16.5, 62.5 front
set label "SIGNATURE [0:1000]" at -1.5, -1.5, 1.5 front
	# tree labels
	set pm3d map
	splot "HPS_OUTPUTS/HPS_DNA_PHYLOTREE.DAT" title 'SIG-TO-M (MATCHING INSTANCE) PHYLOGENIC-EVOLUTION DISTANCES' with lines lc rgb 'blue' 
############################

############################
# SEQUENCE ANALYSIS PLOT
############################
reset
set terminal png giant butt size 1200,800 enhanced
set output "HPS_OUTPUTS/HPS_DNA_SEQUENCE_ANALYZER_PLOT.PNG"
############################
set multiplot
	set grid
	set clip
	# set bmargin 0
	# set tmargin 0
	set xtics
	set ytics ( "  A" 0, "  C" 1, "  G" 2, "  T" 3 )
	set xlabel ''
	set ylabel ''
	set yrange [-1:15]
	set tmargin 0
	set bmargin 0
	set size 1.0, 0.20
	set origin 0.0, 0.73
	
	set arrow from 1, 8 to 1, 7.2 head lw 2 lc rgb 'black'
	set arrow from 18, 8 to 18, 7.2 head lw 2 lc rgb 'black'
	set arrow from 28, 8 to 28, 7.2 head lw 2 lc rgb 'black'
	set arrow from 45, 8 to 45, 7.2 head lw 2 lc rgb 'black'
	set arrow from 61, 8 to 61, 7.2 head lw 2 lc rgb 'black'
	set arrow from 72, 8 to 72, 7.2 head lw 2 lc rgb 'black'
	set arrow from 77, 8 to 77, 7.2 head lw 2 lc rgb 'black'
	set arrow from 82, 8 to 82, 7.2 head lw 2 lc rgb 'black'
	set arrow from 88, 8 to 88, 7.2 head lw 2 lc rgb 'black'
	set arrow from 91, 8 to 91, 7.2 head lw 2 lc rgb 'black'
	set arrow from 102, 8 to 102, 7.2 head lw 2 lc rgb 'black'
	set arrow from 119, 8 to 119, 7.2 head lw 2 lc rgb 'black'
	set arrow from 121, 8 to 121, 7.2 head lw 2 lc rgb 'black'
	set arrow from 129, 8 to 129, 7.2 head lw 2 lc rgb 'black'
	set arrow from 136, 8 to 136, 7.2 head lw 2 lc rgb 'black'
	set arrow from 140, 8 to 140, 7.2 head lw 2 lc rgb 'black'
	set arrow from 145, 8 to 145, 7.2 head lw 2 lc rgb 'black'
	set arrow from 162, 8 to 162, 7.2 head lw 2 lc rgb 'black'
	set arrow from 165, 8 to 165, 7.2 head lw 2 lc rgb 'black'
	set arrow from 168, 8 to 168, 7.2 head lw 2 lc rgb 'black'
	set arrow from 171, 8 to 171, 7.2 head lw 2 lc rgb 'black'
	set arrow from 177, 8 to 177, 7.2 head lw 2 lc rgb 'black'
	set arrow from 191, 8 to 191, 7.2 head lw 2 lc rgb 'black'
	set arrow from 196, 8 to 196, 7.2 head lw 2 lc rgb 'black'
	set arrow from 205, 8 to 205, 7.2 head lw 2 lc rgb 'black'
	set arrow from 207, 8 to 207, 7.2 head lw 2 lc rgb 'black'
	set arrow from 211, 8 to 211, 7.2 head lw 2 lc rgb 'black'
	set arrow from 219, 8 to 219, 7.2 head lw 2 lc rgb 'black'
	set arrow from 244, 8 to 244, 7.2 head lw 2 lc rgb 'black'
	set arrow from 253, 8 to 253, 7.2 head lw 2 lc rgb 'black'
	set arrow from 255, 8 to 255, 7.2 head lw 2 lc rgb 'black'
	set arrow from 257, 8 to 257, 7.2 head lw 2 lc rgb 'black'
	set arrow from 268, 8 to 268, 7.2 head lw 2 lc rgb 'black'
	set arrow from 282, 8 to 282, 7.2 head lw 2 lc rgb 'black'
	set arrow from 285, 8 to 285, 7.2 head lw 2 lc rgb 'black'
	set arrow from 294, 8 to 294, 7.2 head lw 2 lc rgb 'black'
	set arrow from 328, 8 to 328, 7.2 head lw 2 lc rgb 'black'
	set arrow from 331, 8 to 331, 7.2 head lw 2 lc rgb 'black'
	set arrow from 335, 8 to 335, 7.2 head lw 2 lc rgb 'black'
	set arrow from 345, 8 to 345, 7.2 head lw 2 lc rgb 'black'
	set arrow from 352, 8 to 352, 7.2 head lw 2 lc rgb 'black'
	set arrow from 358, 8 to 358, 7.2 head lw 2 lc rgb 'black'
	set arrow from 367, 8 to 367, 7.2 head lw 2 lc rgb 'black'
	set arrow from 370, 8 to 370, 7.2 head lw 2 lc rgb 'black'
	set arrow from 394, 8 to 394, 7.2 head lw 2 lc rgb 'black'
	set arrow from 397, 8 to 397, 7.2 head lw 2 lc rgb 'black'
	set arrow from 408, 8 to 408, 7.2 head lw 2 lc rgb 'black'
	set arrow from 426, 8 to 426, 7.2 head lw 2 lc rgb 'black'
	set arrow from 438, 8 to 438, 7.2 head lw 2 lc rgb 'black'
	set arrow from 451, 8 to 451, 7.2 head lw 2 lc rgb 'black'
	set arrow from 454, 8 to 454, 7.2 head lw 2 lc rgb 'black'
	set arrow from 460, 8 to 460, 7.2 head lw 2 lc rgb 'black'
	set arrow from 469, 8 to 469, 7.2 head lw 2 lc rgb 'black'
	set arrow from 488, 8 to 488, 7.2 head lw 2 lc rgb 'black'
	set arrow from 498, 8 to 498, 7.2 head lw 2 lc rgb 'black'
	set arrow from 505, 8 to 505, 7.2 head lw 2 lc rgb 'black'
	set arrow from 523, 8 to 523, 7.2 head lw 2 lc rgb 'black'
	set arrow from 525, 8 to 525, 7.2 head lw 2 lc rgb 'black'
	set arrow from 535, 8 to 535, 7.2 head lw 2 lc rgb 'black'
	set arrow from 537, 8 to 537, 7.2 head lw 2 lc rgb 'black'
	set arrow from 544, 8 to 544, 7.2 head lw 2 lc rgb 'black'
	set arrow from 551, 8 to 551, 7.2 head lw 2 lc rgb 'black'
	set arrow from 553, 8 to 553, 7.2 head lw 2 lc rgb 'black'
	set arrow from 565, 8 to 565, 7.2 head lw 2 lc rgb 'black'
	set arrow from 568, 8 to 568, 7.2 head lw 2 lc rgb 'black'
	set arrow from 573, 8 to 573, 7.2 head lw 2 lc rgb 'black'
	set arrow from 585, 8 to 585, 7.2 head lw 2 lc rgb 'black'
	set arrow from 602, 8 to 602, 7.2 head lw 2 lc rgb 'black'
	set arrow from 607, 8 to 607, 7.2 head lw 2 lc rgb 'black'
	set arrow from 612, 8 to 612, 7.2 head lw 2 lc rgb 'black'
	set arrow from 615, 8 to 615, 7.2 head lw 2 lc rgb 'black'
	set arrow from 619, 8 to 619, 7.2 head lw 2 lc rgb 'black'
	set arrow from 625, 8 to 625, 7.2 head lw 2 lc rgb 'black'
	set arrow from 638, 8 to 638, 7.2 head lw 2 lc rgb 'black'
	set arrow from 644, 8 to 644, 7.2 head lw 2 lc rgb 'black'
	set arrow from 646, 8 to 646, 7.2 head lw 2 lc rgb 'black'
	set arrow from 684, 8 to 684, 7.2 head lw 2 lc rgb 'black'
	set arrow from 697, 8 to 697, 7.2 head lw 2 lc rgb 'black'
	set arrow from 705, 8 to 705, 7.2 head lw 2 lc rgb 'black'
	set arrow from 707, 8 to 707, 7.2 head lw 2 lc rgb 'black'
	set arrow from 725, 8 to 725, 7.2 head lw 2 lc rgb 'black'
	set arrow from 732, 8 to 732, 7.2 head lw 2 lc rgb 'black'
	set arrow from 744, 8 to 744, 7.2 head lw 2 lc rgb 'black'
	set arrow from 754, 8 to 754, 7.2 head lw 2 lc rgb 'black'
	set arrow from 759, 8 to 759, 7.2 head lw 2 lc rgb 'black'
	set arrow from 778, 8 to 778, 7.2 head lw 2 lc rgb 'black'
	set arrow from 781, 8 to 781, 7.2 head lw 2 lc rgb 'black'
	set arrow from 785, 8 to 785, 7.2 head lw 2 lc rgb 'black'
	set arrow from 795, 8 to 795, 7.2 head lw 2 lc rgb 'black'
	set arrow from 832, 8 to 832, 7.2 head lw 2 lc rgb 'black'
	set arrow from 868, 8 to 868, 7.2 head lw 2 lc rgb 'black'
	set arrow from 877, 8 to 877, 7.2 head lw 2 lc rgb 'black'
	set arrow from 904, 8 to 904, 7.2 head lw 2 lc rgb 'black'
	set arrow from 908, 8 to 908, 7.2 head lw 2 lc rgb 'black'
	set arrow from 910, 8 to 910, 7.2 head lw 2 lc rgb 'black'
	set arrow from 916, 8 to 916, 7.2 head lw 2 lc rgb 'black'
	set arrow from 924, 8 to 924, 7.2 head lw 2 lc rgb 'black'
	set arrow from 927, 8 to 927, 7.2 head lw 2 lc rgb 'black'
	set arrow from 937, 8 to 937, 7.2 head lw 2 lc rgb 'black'
	set arrow from 944, 8 to 944, 7.2 head lw 2 lc rgb 'black'
	set arrow from 965, 8 to 965, 7.2 head lw 2 lc rgb 'black'
	set arrow from 973, 8 to 973, 7.2 head lw 2 lc rgb 'black'
	set arrow from 990, 8 to 990, 7.2 head lw 2 lc rgb 'black'
	set arrow from 998, 8 to 998, 7.2 head lw 2 lc rgb 'black'
	set label "103 <  |(C G)|  < 104 (black)" at 2, 13 front
	set label " 98 <  |(G A)|  <  98 (green)" at 2, 12 front
	set label " 56 <  |(A A)|  <  77 (brown)" at 2, 11 front
	set label " 70 <  |(T C)|  <  70 (yellow)" at 2, 10 front
	set label " 68 <  |(A C)|  <  68 (red)" at 2, 9 front
	set arrow from 46, 7 to 46, 6.2 head lw 2 lc rgb 'green'
	set arrow from 89, 7 to 89, 6.2 head lw 2 lc rgb 'green'
	set arrow from 92, 7 to 92, 6.2 head lw 2 lc rgb 'green'
	set arrow from 120, 7 to 120, 6.2 head lw 2 lc rgb 'green'
	set arrow from 172, 7 to 172, 6.2 head lw 2 lc rgb 'green'
	set arrow from 192, 7 to 192, 6.2 head lw 2 lc rgb 'green'
	set arrow from 206, 7 to 206, 6.2 head lw 2 lc rgb 'green'
	set arrow from 208, 7 to 208, 6.2 head lw 2 lc rgb 'green'
	set arrow from 220, 7 to 220, 6.2 head lw 2 lc rgb 'green'
	set arrow from 254, 7 to 254, 6.2 head lw 2 lc rgb 'green'
	set arrow from 256, 7 to 256, 6.2 head lw 2 lc rgb 'green'
	set arrow from 286, 7 to 286, 6.2 head lw 2 lc rgb 'green'
	set arrow from 336, 7 to 336, 6.2 head lw 2 lc rgb 'green'
	set arrow from 409, 7 to 409, 6.2 head lw 2 lc rgb 'green'
	set arrow from 439, 7 to 439, 6.2 head lw 2 lc rgb 'green'
	set arrow from 452, 7 to 452, 6.2 head lw 2 lc rgb 'green'
	set arrow from 470, 7 to 470, 6.2 head lw 2 lc rgb 'green'
	set arrow from 524, 7 to 524, 6.2 head lw 2 lc rgb 'green'
	set arrow from 536, 7 to 536, 6.2 head lw 2 lc rgb 'green'
	set arrow from 545, 7 to 545, 6.2 head lw 2 lc rgb 'green'
	set arrow from 552, 7 to 552, 6.2 head lw 2 lc rgb 'green'
	set arrow from 586, 7 to 586, 6.2 head lw 2 lc rgb 'green'
	set arrow from 603, 7 to 603, 6.2 head lw 2 lc rgb 'green'
	set arrow from 613, 7 to 613, 6.2 head lw 2 lc rgb 'green'
	set arrow from 639, 7 to 639, 6.2 head lw 2 lc rgb 'green'
	set arrow from 645, 7 to 645, 6.2 head lw 2 lc rgb 'green'
	set arrow from 698, 7 to 698, 6.2 head lw 2 lc rgb 'green'
	set arrow from 706, 7 to 706, 6.2 head lw 2 lc rgb 'green'
	set arrow from 708, 7 to 708, 6.2 head lw 2 lc rgb 'green'
	set arrow from 745, 7 to 745, 6.2 head lw 2 lc rgb 'green'
	set arrow from 760, 7 to 760, 6.2 head lw 2 lc rgb 'green'
	set arrow from 779, 7 to 779, 6.2 head lw 2 lc rgb 'green'
	set arrow from 786, 7 to 786, 6.2 head lw 2 lc rgb 'green'
	set arrow from 905, 7 to 905, 6.2 head lw 2 lc rgb 'green'
	set arrow from 909, 7 to 909, 6.2 head lw 2 lc rgb 'green'
	set arrow from 928, 7 to 928, 6.2 head lw 2 lc rgb 'green'
	set arrow from 974, 7 to 974, 6.2 head lw 2 lc rgb 'green'
	set arrow from 999, 7 to 999, 6.2 head lw 2 lc rgb 'green'
	set label " 35 <  |(C G C)|  <  38 (green)" at 70, 13 front
	set label " 31 <  |(G C C)|  <  36 (brown)" at 70, 12 front
	set label " 29 <  |(C C G)|  <  30 (gray)" at 70, 11 front
	set label " 29 <  |(C T G)|  <  29 (blue)" at 70, 10 front
	set label " 19 <  |(A A A)|  <  29 (red)" at 70, 9 front
	set arrow from 18, 6 to 18, 5.2 head lw 2 lc rgb 'orange'
	set arrow from 45, 6 to 45, 5.2 head lw 2 lc rgb 'orange'
	set arrow from 165, 6 to 165, 5.2 head lw 2 lc rgb 'orange'
	set arrow from 168, 6 to 168, 5.2 head lw 2 lc rgb 'orange'
	set arrow from 253, 6 to 253, 5.2 head lw 2 lc rgb 'orange'
	set arrow from 282, 6 to 282, 5.2 head lw 2 lc rgb 'orange'
	set arrow from 331, 6 to 331, 5.2 head lw 2 lc rgb 'orange'
	set arrow from 370, 6 to 370, 5.2 head lw 2 lc rgb 'orange'
	set arrow from 397, 6 to 397, 5.2 head lw 2 lc rgb 'orange'
	set arrow from 426, 6 to 426, 5.2 head lw 2 lc rgb 'orange'
	set arrow from 535, 6 to 535, 5.2 head lw 2 lc rgb 'orange'
	set arrow from 544, 6 to 544, 5.2 head lw 2 lc rgb 'orange'
	set arrow from 568, 6 to 568, 5.2 head lw 2 lc rgb 'orange'
	set arrow from 573, 6 to 573, 5.2 head lw 2 lc rgb 'orange'
	set arrow from 732, 6 to 732, 5.2 head lw 2 lc rgb 'orange'
	set arrow from 754, 6 to 754, 5.2 head lw 2 lc rgb 'orange'
	set arrow from 868, 6 to 868, 5.2 head lw 2 lc rgb 'orange'
	set arrow from 924, 6 to 924, 5.2 head lw 2 lc rgb 'orange'
	set arrow from 927, 6 to 927, 5.2 head lw 2 lc rgb 'orange'
	set label " 17 <  |(G G C C)|  <  19 (orange)" at 142, 13 front
	set label " 14 <  |(G C T C)|  <  14 (salmon)" at 142, 12 front
	set label " 12 <  |(C G G C)|  <  13 (red)" at 142, 11 front
	set label " 13 <  |(C G A A)|  <  13 (black)" at 142, 10 front
	set label " 12 <  |(T C G A)|  <  12 (brown)" at 142, 9 front
	set label "|A|=235" at 225.0, 13 front
	set label "|C|=285" at 225.0, 12 front
	set label "|G|=276" at 225.0, 11 front
	set label "|T|=204" at 225.0, 10 front
	# analysis labels
	set title 'SEQUENCE MOTIF FINDER (TOP [5] DUETS [5] TRIPLETS AND [5] QUARTETS) WITHIN [0:1000]'
	plot [0:250] "HPS_INPUTS/HPS_1DNA_INPUT_SERIES_ENCODED.DNA" using 1:2 title '' with boxes fs solid lc rgb 'salmon'
############################
	unset label
	set title ''
	set tmargin 0
	set bmargin 0
	set size 1.0, 0.18
	set origin 0, 0.51
	plot  [250.0:500] "HPS_INPUTS/HPS_1DNA_INPUT_SERIES_ENCODED.DNA" using 1:2    title '' with boxes fs solid lc rgb 'salmon'
        ############################
	set size 1.0, 0.18
	set origin 0, 0.27
	set xlabel ''
	plot  [500.0:750] "HPS_INPUTS/HPS_1DNA_INPUT_SERIES_ENCODED.DNA"    using 1:2 title '' with boxes fs solid lc rgb 'salmon'
        ############################
	set size 1.0, 0.18
	set origin 0, 0.03
	set xlabel ''
	plot  [750.0:1000] "HPS_INPUTS/HPS_1DNA_INPUT_SERIES_ENCODED.DNA"    using 1:2 title '' with boxes fs solid lc rgb 'salmon'
        ############################
unset multiplot

############################
# SEQUENCE ANALYSIS PLOT
############################
reset
set terminal png giant butt size 1200,800 enhanced
set output "HPS_OUTPUTS/HPS_DNA_REGULAR_EXPRESSION_PLOT.PNG"
############################
set size 1.0, 0.95
set multiplot
	set yrange [-1:11]
	set grid
	set title 'REGULAR-EXPRESSION: (TTG)[1](T)[2](AT)[1](AA)[2](AT)[1](GCT)[1](ACT)[1](GC)[1](AC)[1](GG)[4](CAC)[1](GGA)[1](TAT)[2](GTA)[1]'
        		# regexpr
	
set label "T" at 6, 4
set label "T" at 7, 4
set label "C" at 8, 4
set label "T" at 9, 4
set label "T" at 10, 4
set label "G" at 11, 4
set label "A" at 12, 4
set label "T" at 13, 4
set label "C" at 14, 4
set label "T" at 15, 4
set label "G" at 16, 4
set label "G" at 17, 4
set label "C" at 18, 4
set label "G" at 19, 4
set label "A" at 20, 4
set label "A" at 21, 4
set label "A" at 22, 4
set label "C" at 23, 4
set label "A" at 24, 4
set label "T" at 25, 4
set label "T" at 26, 4
set label "G" at 27, 4
set label "C" at 28, 4
set label "G" at 29, 4
set label "A" at 30, 4
set label "A" at 31, 4
set label "T" at 32, 4
set label "T" at 33, 4
set label "T" at 34, 4
set label "G" at 35, 4
set label "A" at 36, 4
set label "A" at 37, 4
set label "G" at 38, 4
set label "A" at 39, 4
set label "A" at 40, 4
set label "G" at 41, 4
set label "T" at 42, 4
set label "G" at 43, 4
set label "G" at 44, 4
set label "C" at 45, 4
set label "G" at 46, 4
set label "C" at 47, 4
set label "A" at 48, 4
set label "T" at 49, 4
set label "C" at 50, 4
set label "T" at 51, 4
set label "G" at 52, 4
set label "C" at 53, 4
set label "T" at 54, 4
set label "G" at 55, 4
set label "A" at 56, 4
set label "T" at 57, 4
set label "C" at 58, 4
set label "C" at 59, 4
set label "A" at 60, 4
set label "C" at 61, 4
set label "G" at 62, 4
set label "G" at 63, 4
set label "C" at 64, 4
set label "A" at 65, 4
set label "A" at 66, 4
set label "A" at 67, 4
set label "C" at 68, 4
set label "T" at 69, 4
set label "G" at 70, 4
set label "C" at 71, 4
set label "C" at 72, 4
set label "G" at 73, 4
set label "A" at 74, 4
set label "C" at 75, 4
set label "C" at 76, 4
set label "C" at 77, 4
set label "G" at 78, 4
set label "T" at 79, 4
set label "G" at 80, 4
set label "A" at 81, 4
set label "C" at 82, 4
set label "G" at 83, 4
set label "A" at 84, 4
set label "A" at 85, 4
set label "C" at 86, 4
set label "T" at 87, 4
set label "C" at 88, 4
set label "G" at 89, 4
set label "C" at 90, 4
set label "C" at 91, 4
set label "G" at 92, 4
set label "C" at 93, 4
set label "T" at 94, 4
set label "T" at 95, 4
set label "A" at 96, 4
set label "C" at 97, 4
set label "A" at 98, 4
set label "A" at 99, 4
set label "A" at 100, 4
set label "A" at 101, 4
set label "C" at 102, 4
set label "G" at 103, 4
set label "A" at 104, 4
set label "A" at 105, 4
set label "A" at 106, 4
set label "C" at 107, 4
set label "T" at 108, 4
set label "G" at 109, 4
set label "A" at 110, 4
set label "A" at 111, 4
set label "A" at 112, 4
set label "G" at 113, 4
set label "C" at 114, 4
set label "C" at 115, 4
set label "C" at 116, 4
set label "T" at 117, 4
set label "G" at 118, 4
set label "C" at 119, 4
set label "G" at 120, 4
set label "C" at 121, 4
set label "G" at 122, 4
set label "G" at 123, 4
set label "T" at 124, 4
set label "T" at 125, 4
set label "T" at 126, 4
set label "A" at 127, 4
set label "C" at 128, 4
set label "C" at 129, 4
set label "G" at 130, 4
set label "G" at 131, 4
set label "C" at 132, 4
set label "T" at 133, 4
set label "A" at 134, 4
set label "A" at 135, 4
set label "C" at 136, 4
set label "G" at 137, 4
set label "T" at 138, 4
set label "G" at 139, 4
set label "C" at 140, 4
set label "G" at 141, 4
set label "T" at 142, 4
set label "A" at 143, 4
set label "C" at 144, 4
set label "C" at 145, 4
set label "G" at 146, 4
set label "T" at 147, 4
set label "G" at 148, 4
set label "C" at 149, 4
set label "T" at 150, 4
set label "G" at 151, 4
set label "G" at 152, 4
set label "A" at 153, 4
set label "A" at 154, 4
set label "G" at 155, 4
set label "C" at 156, 4
set label "C" at 157, 4
set label "T" at 158, 4
set label "T" at 159, 4
set label "A" at 160, 4
set label "C" at 161, 4
set label "C" at 162, 4
set label "G" at 163, 4
set label "G" at 164, 4
set label "C" at 165, 4
set label "G" at 166, 4
set label "G" at 167, 4
set label "C" at 168, 4
set label "G" at 169, 4
set label "T" at 170, 4
set label "C" at 171, 4
set label "G" at 172, 4
set label "C" at 173, 4
set label "A" at 174, 4
set label "C" at 175, 4
set label "C" at 176, 4
set label "C" at 177, 4
set label "G" at 178, 4
set label "A" at 179, 4
set label "T" at 180, 4
set label "G" at 181, 4
set label "G" at 182, 4
set label "A" at 183, 4
set label "T" at 184, 4
set label "G" at 185, 4
set label "T" at 186, 4
set label "T" at 187, 4
set label "A" at 188, 4
set label "T" at 189, 4
set label "G" at 190, 4
set label "C" at 191, 4
set label "G" at 192, 4
set label "C" at 193, 4
set label "A" at 194, 4
set label "C" at 195, 4
set label "C" at 196, 4
set label "G" at 197, 4
set label "G" at 198, 4
set label "T" at 199, 4
set label "G" at 200, 4
set label "T" at 201, 4
set label "T" at 202, 4
set label "T" at 203, 4
set label "C" at 204, 4
set label "C" at 205, 4
set label "G" at 206, 4
set label "C" at 207, 4
set label "G" at 208, 4
set label "C" at 209, 4
set label "T" at 210, 4
set label "C" at 211, 4
set label "G" at 212, 4
set label "G" at 213, 4
set label "C" at 214, 4
set label "T" at 215, 4
set label "G" at 216, 4
set label "C" at 217, 4
set label "A" at 218, 4
set label "C" at 219, 4
set label "G" at 220, 4
set label "C" at 221, 4
set label "T" at 222, 4
set label "G" at 223, 4
set label "C" at 224, 4
set label "C" at 225, 4
set label "A" at 226, 4
set label "G" at 227, 4
set label "A" at 228, 4
set label "A" at 229, 4
set label "A" at 230, 4
set label "A" at 231, 4
set label "A" at 232, 4
set label "G" at 233, 4
set label "A" at 234, 4
set label "G" at 235, 4
set label "G" at 236, 4
set label "G" at 237, 4
set label "G" at 238, 4
set label "C" at 239, 4
set label "A" at 240, 4
set label "T" at 241, 4
set label "A" at 242, 4
set label "C" at 243, 4
set label "C" at 244, 4
set label "G" at 245, 4
set label "T" at 246, 4
set label "C" at 247, 4
set label "T" at 248, 4
set label "C" at 249, 4
set label "T" at 250, 4
set label "G" at 251, 4
set label "G" at 252, 4
set label "C" at 253, 4
set label "G" at 254, 4
set label "C" at 255, 4
set label "G" at 256, 4
set label "C" at 257, 4
set label "G" at 258, 4
set label "G" at 259, 4
set label "G" at 260, 4
set label "A" at 261, 4
set label "T" at 262, 4
set label "A" at 263, 4
set label "T" at 264, 4
set label "T" at 265, 4
set label "G" at 266, 4
set label "C" at 267, 4
set label "C" at 268, 4
set label "G" at 269, 4
set label "A" at 270, 4
set label "C" at 271, 4
set label "A" at 272, 4
set label "A" at 273, 4
set label "A" at 274, 4
set label "C" at 275, 4
set label "T" at 276, 4
set label "G" at 277, 4
set label "C" at 278, 4
set label "T" at 279, 4
set label "G" at 280, 4
set label "G" at 281, 4
set label "C" at 282, 4
set label "G" at 283, 4
set label "T" at 284, 4
set label "C" at 285, 4
set label "G" at 286, 4
set label "C" at 287, 4
set label "T" at 288, 4
set label "T" at 289, 4
set label "A" at 290, 4
set label "G" at 291, 4
set label "C" at 292, 4
set label "T" at 293, 4
set label "C" at 294, 4
set label "G" at 295, 4
set label "A" at 296, 4
set label "T" at 297, 4
set label "T" at 298, 4
set label "C" at 299, 4
set label "T" at 300, 4
set label "C" at 301, 4
set label "C" at 302, 4
set label "T" at 303, 4
set label "T" at 304, 4
set label "T" at 305, 4
set label "A" at 306, 4
set label "T" at 307, 4
set label "T" at 308, 4
set label "G" at 309, 4
set label "G" at 310, 4
set label "T" at 311, 4
set label "A" at 312, 4
set label "T" at 313, 4
set label "C" at 314, 4
set label "A" at 315, 4
set label "C" at 316, 4
set label "AT:9(T T G)[1]" at 9, 8
set label "AT:13(T)[2]" at 13, 7
set label "AT:15(T)[2]" at 15, 6
set label "AT:24(A T)[1]" at 24, 8
set label "AT:30(A A)[2]" at 30, 7
set label "AT:36(A A)[2]" at 36, 6
set label "AT:48(A T)[1]" at 48, 8
set label "AT:52(G C T)[1]" at 52, 7
set label "AT:67(A C T)[1]" at 67, 6
set label "AT:70(G C)[1]" at 70, 8
set label "AT:74(A C)[1]" at 74, 7
set label "AT:122(G G)[4]" at 122, 6
set label "AT:130(G G)[4]" at 130, 8
set label "AT:151(G G)[4]" at 151, 7
set label "AT:163(G G)[4]" at 163, 6
set label "AT:173(C A C)[1]" at 173, 8
set label "AT:181(G G A)[1]" at 181, 7
set label "AT:187(T A T)[2]" at 187, 6
set label "AT:262(T A T)[2]" at 262, 8
set label "AT:310(G T A)[1]" at 310, 7
			# labels
        ############################
	set grid
	set xtics
	set xlabel ''
	set ylabel ''
	set ytics autofreq  ("  0" 0, "  A" 0, "  C" 1, "  G" 2, "  T" 3)
        ############################
	set tmargin 0
	set bmargin 0
	set size 1.0, 0.18
	set origin 0, 0.75
	plot  [5:79] "HPS_INPUTS/HPS_1DNA_INPUT_SERIES_ENCODED.DNA" using 1:2   title 'DNA INPUT SEQUENCE'  with boxes fs solid lc rgb 'salmon'
        ############################
	set title ''
	set tmargin 0
	set bmargin 0
	set size 1.0, 0.18
	set origin 0, 0.52
	plot  [79.0:158] "HPS_INPUTS/HPS_1DNA_INPUT_SERIES_ENCODED.DNA" using 1:2    title 'DNA INPUT SEQUENCE' with boxes fs solid lc rgb 'salmon'
        ############################
	set size 1.0, 0.18
	set origin 0, 0.28
	set xlabel ''
	plot  [158.0:237] "HPS_INPUTS/HPS_1DNA_INPUT_SERIES_ENCODED.DNA"    using 1:2 title 'DNA INPUT SEQUENCE' with boxes fs solid lc rgb 'salmon'
        ############################
	set size 1.0, 0.18
	set origin 0, 0.04
	set xlabel ''
	plot  [237.0:316] "HPS_INPUTS/HPS_1DNA_INPUT_SERIES_ENCODED.DNA"    using 1:2 title 'DNA INPUT SEQUENCE' with boxes fs solid lc rgb 'salmon'
        ############################
unset multiplot
