;; ********************************************************************************************
;; 		        GNUPLOT GENERATOR HEADERS USED FOR PRINTING COMMAND FILE
;; ********************************************************************************************


;; ********************************************************************************************
;; ********************************************************************************************
;; THIS MODULE IS PART OF AN IMPLEMENTATION OF AN ONLINE HPS TRANSFORM.  THE HPS TRANSFORM WAS 
;; DESCRIBED IN THE PAPER ENTITLED: 
;; 			    THE HPS TRANSFORM AND ITS APPLICATIONS
;;                          www3.webng.com/nelsonmanohar/research/
;;
;; 	          THIS CODE WAS WRITTEN AND ITS COPYRIGHTED (2007) BY 
;; 			    DR. NELSON R. MANOHAR ALERS
;; 			      nelsonmanohar@yahoo.com
;; 			           AUGUST 2007
;;
;; THIS CODE IS NOT IN THE PUBLIC DOMAIN. THIS CODE IS NOT INTENDED FOR DISTRIBUTION. PLEASE DO
;; NOT MODIFY THIS CODE. PLEASE INFORM THE AUTHOR OF ANY UNINTENDED OR UNAUTHORIZED DISTRIBUTION 
;; AND/OR ACCESS TO THIS CODE.
;; ********************************************************************************************


;; ***********************************************************************************************************************
;; the html headers and footers of the html report to be generated 
;; ***********************************************************************************************************************
;; ***********************************************************************************************************************
(defmacro HPS_GNUPLOT_INIT () "
############################ 
#  		From The HPS Transform and Its Applications
#                          By Dr. Nelson R. Manohar
############################
# INITIALIZATION
reset
set grid
set palette color
############################
cd \"~A\"
" )
;; ***********************************************************************************************************************


(defmacro HPS_GNUPLOT_MATCH_BEHAVIOR_PLOT_3DNA () "
############################
reset
set grid
set terminal png giant butt size 1200,800 enhanced
set output \"HPS_OUTPUTS/DNA_MATCH_BEHAVIOR_LAMBDA_TVALS.PNG\"
set title '~A'
		  # HPS_MATCH_TITLE
set multiplot
        ############################
	set grid
	set format y \"%3.0f\"
	set size 1.0, 0.40
	set origin 0, 0.5
	set tmargin 0
	set bmargin 0
	set yrange [0:]
	set xlabel ''
	set ylabel 'HPS DNA lambda(i)'
	plot  \"HPS_OUTPUTS/HPS_DNA_SEGMENT_TABLE.DNA\"  using 1:3   title 'HPS DNA segment duration (i)' with impulses
	reset
        ############################
	set grid
        set title ''
	set bmargin 0
	set size 1.0, 0.40
	set origin 0, 0.05
	set xlabel ''
	set ylabel 'HPS DNA approx (i)'
	set yrange [16 : 48]
	set ytics autofreq ( \"AAA\" 0,  \"AAC\" 1,  \"AAG\" 2,  \"AAT\" 3,  \"ACA\" 4,  \"ACC\" 5,  \"ACG\" 6,  \"ACT\" 7,  \"AGA\" 8,  \"AGC\" 9, \"AGG\" 10,  \"AGT\" 11,  \"ATA\" 12,  \"ATC\" 13,  \"ATG\" 14,  \"ATT\" 15,  \"CAA\" 16,  \"CAC\" 17,  \"CAG\" 18, \"CAT\" 19,  \"CCA\" 20,  \"CCC\" 21,  \"CCG\" 22,  \"CCT\" 23,  \"CGA\" 24,  \"CGC\" 25,  \"CGG\" 26,  \"CGT\" 27,  \"CTA\" 28,  \"CTC\" 29,  \"CTG\" 30,  \"CTT\" 31,  \"GAA\" 32,  \"GAC\" 33,  \"GAG\" 34,  \"GAT\" 35,  \"GCA\" 36,  \"GCC\" 37,  \"GCG\" 38,  \"GCT\" 39,  \"GGA\" 40,  \"GGC\" 41,  \"GGG\" 42,  \"GGT\" 43,  \"GTA\" 44,  \"GTC\" 45,  \"GTG\" 46,  \"GTT\" 47,  \"TAA\" 48,  \"TAC\" 49,  \"TAG\" 50,  \"TAT\" 51,  \"TCA\" 52,  \"TCC\" 53,  \"TCG\" 54,  \"TCT\" 55,  \"TGA\" 56,  \"TGC\" 57,  \"TGG\" 58,  \"TGT\" 59,  \"TTA\" 60,  \"TTC\" 61,  \"TTG\" 62,  \"TTT\" 63 )
	plot  \"HPS_OUTPUTS/HPS_3DNA_APPROXIMATION_SERIES.DNA\"  using 1:2   title 'HPS DNA approximation (i)' with lines
	############################
unset multiplot
############################
")

;; ***********************************************************************************************************************
;; ***********************************************************************************************************************
(defmacro HPS_GNUPLOT_MATCH_BEHAVIOR_PLOT_START () "
############################
# RESULTANT HPS DNA MATCH BEHAVIOR
############################
reset
set grid
set terminal png giant butt size 1200,800 enhanced
set output \"HPS_OUTPUTS/DNA_MATCH_BEHAVIOR.PNG\"
set multiplot
	set title '~A'
	   # HPS_MATCH_TITLE
        ############################
        set autoscale y
        ############################
	~A
		# HPS_TESTSEQ_LABEL
	~A
		# HPS_SIGNATURE_LABEL
	~A
		# HYP_BASE_LABELS
        ############################
	set grid
	set xtics
	set xlabel ''
	set ylabel ''
	set ytics autofreq  (\"  0\" 0, \"  A\" 0, \"  C\" 1, \"  G\" 2, \"  T\" 3)
        ############################
	set tmargin 0
	set bmargin 0
	set size 1.0, 0.18
	set origin 0, 0.75
	set yrange [-1:5]
	     # 0 : 1/3 TESTSEQLEN
	plot  [~A:~A] \"HPS_INPUTS/HPS_1DNA_INPUT_SERIES_ENCODED.DNA\" using 1:2   title 'DNA test sequence (i)' with lines lw 1 lc rgb 'salmon', \\
	              \"HPS_INPUTS/HPS_1DNA_SIGNATURE_ENCODED.DNA\"    using 1:2   title 'DNA signature (i)'     with lines lw 1 lc rgb 'light-blue'
        ############################
	set title ''
	set tmargin 0
	set bmargin 0
	set size 1.0, 0.18
	set origin 0, 0.52
	set yrange [-1:5]
	     # 0 : 1/3 TESTSEQLEN
	plot  [~A:~A] \"HPS_INPUTS/HPS_1DNA_INPUT_SERIES_ENCODED.DNA\" using 1:2   title 'DNA test sequence (i)' with lines lw 1 lc rgb 'salmon', \\
	              \"HPS_INPUTS/HPS_1DNA_SIGNATURE_ENCODED.DNA\"    using 1:2   title 'DNA signature (i)'     with lines lw 1 lc rgb 'light-blue'
        ############################
	set size 1.0, 0.18
	set origin 0, 0.28
	set xlabel ''
	     # 1/3 : 2/3 TESTSEQLEN
	plot  [~A:~A] \"HPS_INPUTS/HPS_1DNA_INPUT_SERIES_ENCODED.DNA\"    using 1:2   title 'DNA test sequence (i)' with lines lw 1 lc rgb 'salmon'
        ############################
	set size 1.0, 0.18
	set origin 0, 0.04
	set xlabel ''
	     # 2/3 : 3/3 TESTSEQLEN
	plot  [~A:~A] \"HPS_INPUTS/HPS_1DNA_INPUT_SERIES_ENCODED.DNA\"    using 1:2   title 'DNA test sequence (i)' with lines lw 1 lc rgb 'salmon'
        ############################
unset multiplot
############################
")


(defmacro HPS_GNUPLOT_MATCH_BEHAVIOR_PLOT_END () "
############################
reset
set grid
set title '~A'
	   # HPS_MATCH_TITLE
set terminal png giant butt size 1200,800 enhanced
set output \"HPS_OUTPUTS/DNA_MATCH_BEHAVIOR_HISTOGRAM.PNG\"
	set multiplot
        	############################
		set size 1.0, 0.60
		set origin 0, 0.35
		set tmargin 0
		set bmargin 0
		~A
        	############################
		~A
			# GAUSSIAN LABELS
		set xlabel 'HPS DNA coarsegrain match strength'
		set ylabel 'frequency'
		set xrange [0:1.25]
		set xtics   (\"0.0\" 0, \"0.2\" 0.20, \"0.4\" 0.40, \"0.6\" 0.6, \"LIM\" ~3,2F, \"0.8\" 0.80, \"1.0\" 1)
									  	# *HPS_DNA_matching_goal*
		~A
			# HIST_LABELs
	
		plot  \"HPS_OUTPUTS/HPS_DNA_MATCHBEHAVIOR_HISTOGRAM.DNA\"  using 1:2   title 'HPS DNA match rating' with boxes fs solid lw 2 lc rgb 'red', \\
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
		set ytics  autofreq ( \"0.0\" 0.0, \"0.2\" 0.20, \"0.4\" 0.4, \"0.6\" 0.6, \"LIM\" ~3,2F, \"0.8\" 0.8, \"1.0\" 1 )
									   	            	# *HPS_DNA_MATCHING_GOAL*
		~A
			# HYP_MATCH_LABELS
		plot  \"HPS_OUTPUTS/HPS_DNA_BEHAVIOR_PATTERN_DISCOVERY.DNA\"  using 1:3   title 'HPS DNA coarsegrain match strength (i)' with impulses
        	############################
	unset multiplot
	unset label
############################
")
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; ***********************************************************************************************************************
(defmacro HPS_GNUPLOT_SEGMENT_ALIGNMENT_PLOT_START () "
############################
# CLOSE UPS OF RESULTANT HPS APPROXIMATION IN TERMS OF SEGMENT ALIGNMENT
############################
reset
set grid
set format y \"%3.0f\"
set size 1.0, 1.0
set terminal png giant butt size 1200,800 enhanced
set output \"HPS_OUTPUTS/HPS_DNA_BESTMATCHING_PATTERNS_DETAILED_LABELED_M~D.PNG\"
set title 'DNA SEQUENCE ALIGNMENT FOR HPS PATTERN MATCH ~A'
	set multiplot
	########################
		set ylabel 'HPS DNA sequence (i)'
		set ytics autofreq ( \"AAA\" 0,  \"AAC\" 1,  \"AAG\" 2,  \"AAT\" 3,  \"ACA\" 4,  \"ACC\" 5,  \"ACG\" 6,  \"ACT\" 7,  \"AGA\" 8,  \"AGC\" 9,  \"AGG\" 10,  \"AGT\" 11,  \"ATA\" 12,  \"ATC\" 13,  \"ATG\" 14,  \"ATT\" 15,  \"CAA\" 16,  \"CAC\" 17,  \"CAG\" 18,  \"CAT\" 19,  \"CCA\" 20,  \"CCC\" 21,  \"CCG\" 22,  \"CCT\" 23,  \"CGA\" 24,  \"CGC\" 25,  \"CGG\" 26,  \"CGT\" 27,  \"CTA\" 28,  \"CTC\" 29,  \"CTG\" 30,  \"CTT\" 31,  \"GAA\" 32,  \"GAC\" 33,  \"GAG\" 34,  \"GAT\" 35,  \"GCA\" 36,  \"GCC\" 37,  \"GCG\" 38,  \"GCT\" 39,  \"GGA\" 40,  \"GGC\" 41,  \"GGG\" 42,  \"GGT\" 43,  \"GTA\" 44,  \"GTC\" 45,  \"GTG\" 46,  \"GTT\" 47,  \"TAA\" 48,  \"TAC\" 49,  \"TAG\" 50,  \"TAT\" 51,  \"TCA\" 52,  \"TCC\" 53,  \"TCG\" 54,  \"TCT\" 55,  \"TGA\" 56,  \"TGC\" 57,  \"TGG\" 58,  \"TGT\" 59,  \"TTA\" 60,  \"TTC\" 61,  \"TTG\" 62,  \"TTT\" 63)
	########################
")

(defmacro HPS_GNUPLOT_SEGMENT_ALIGNMENT_TOP_PANEL () "
	########################
		set tmargin 0
		set size 1.00, 0.23
		set origin 0.0, 0.70
		set bmargin 0
		set ylabel 'DNA/SEG (i)'
		set xrange [~6,3F:~6,3F]
		set yrange [20: 42]
		plot \"HPS_OUTPUTS/HPS_3DNA_APPROXIMATION_SERIES.DNA\"  using 1:2 title 'HPS SIGNATURE INPUT SIGVALS'     with impulses lc rgb 'light-blue', \\
		     \"HPS_OUTPUTS/HPS_DNA_SEGMENT_TABLE.DNA\"   	using (($1+$2)/2):4:3 title 'HPS SIGNATURE SEGMENT TVALS'     with boxes lw 3
	########################
		set title ''
		set size 1.00, 0.23
		set origin 0.0, 0.40
		set bmargin 0
		~A
		set xrange [~6,3F:~6,3F]
		set yrange [20: 42]
		plot \"HPS_OUTPUTS/HPS_3DNA_APPROXIMATION_SERIES.DNA\"  using 1:2 title '~A - INPUT SIGVALS'  with impulses lc rgb 'salmon', \\
                     \"HPS_OUTPUTS/HPS_DNA_SEGMENT_TABLE.DNA\"   	using (($1+$2)/2):4:3 title '~A - SEGMENT TVALS'  with boxes lw 3
		unset label
	########################
")

(defmacro HPS_GNUPLOT_SEGMENT_ALIGNMENT_BOTTOM_PANEL () "
  	reset
	set grid
	set autoscale y
	set ytics autofreq
	########################
		set tmargin 0
		set size 0.50, 0.28
		set origin 0.0, 0.05
		set bmargin 0
		set xrange [~6,3F:~6,3F]
		set ylabel 'HPS LAMBDA (i)'
		plot \"HPS_OUTPUTS/HPS_DNA_SEGMENT_TABLE.DNA\"   using 1:3 title 'HPS SIGNATURE SEGMENT SDUR' with boxes fs solid lc rgb 'light-blue'
	########################
		set size 0.50, 0.28
		set origin 0.5, 0.05
		set bmargin 0
		~A
		set xrange [~6,3F:~6,3F]
		plot \"HPS_OUTPUTS/HPS_DNA_SEGMENT_TABLE.DNA\"   using 1:3 title '~A - SEGMENT SDUR' with boxes fs solid lc rgb 'salmon'
		unset label
	########################
	unset multiplot
	reset
############################
")
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; ***********************************************************************************************************************
(defmacro HPS_GNUPLOT_SIG2MATCH_CROSSREF_DETAILS_PLOT_START () "
############################
# SIGNATURE AND PATERN MATCH CROSS REFS
############################
reset
set grid
set format y \"%3.0f\"
set title ''
set terminal png giant butt size 1200,800 enhanced
set output \"HPS_OUTPUTS/HPS_DNA_BESTMATCHING_PATTERNS_DETAILED_PAIRWISE_M~D.PNG\"
############################
set multiplot
        ############################
        set title ''
	set size 0.5, 0.28
	set origin 0, 0.650
	set tmargin 0
	set bmargin 0
	set xlabel 'index (i)'
	set ylabel 'HPS DNA lambda(i)'
 	plot    \"HPS_OUTPUTS/HPS_DNA_PATTERN_MATCH_SEGTABLE_~D.DNA\"       	using 5:3 title '~A' with boxes fs solid lc rgb 'salmon', \\
		\"HPS_OUTPUTS/HPS_DNA_SIGNATURE_SEGTABLE.DNA\" 	       	     	using 5:3 title 'HPS DNA signature' with steps lw 3 lc rgb 'light-blue'
        ############################
        set title ''
	set size 0.5, 0.28
	set origin 0.5, 0.650
	set tmargin 0
	set bmargin 0
	set xrange [1:]
	set xlabel 'HPS DNA lambda(i)'
	set ylabel 'frequency'
 	plot    \"HPS_OUTPUTS/HPS_DNA_PATTERN_MATCH_HISTOGRAM_LAMBDAS_~D.DNA\" 	using 1:2 title '~A' with boxes fs solid lc rgb 'salmon', \\
		\"HPS_OUTPUTS/HPS_DNA_SIGNATURE_HISTOGRAM_LAMBDAS.DNA\" using 1:2 title 'HPS DNA signature(i)' with histeps lw 3 lc rgb 'light-blue'
        ############################
	set xtics autofreq rotate ( \"AAA\" 0,  \"AAC\" 1,  \"AAG\" 2,  \"AAT\" 3,  \"ACA\" 4,  \"ACC\" 5,  \"ACG\" 6,  \"ACT\" 7,  \"AGA\" 8,  \"AGC\" 9,  \"AGG\" 10,  \"AGT\" 11,  \"ATA\" 12,  \"ATC\" 13,  \"ATG\" 14,  \"ATT\" 15,  \"CAA\" 16,  \"CAC\" 17,  \"CAG\" 18,  \"CAT\" 19,  \"CCA\" 20,  \"CCC\" 21,  \"CCG\" 22,  \"CCT\" 23,  \"CGA\" 24,  \"CGC\" 25,  \"CGG\" 26,  \"CGT\" 27,  \"CTA\" 28,  \"CTC\" 29,  \"CTG\" 30,  \"CTT\" 31,  \"GAA\" 32,  \"GAC\" 33,  \"GAG\" 34,  \"GAT\" 35,  \"GCA\" 36,  \"GCC\" 37,  \"GCG\" 38,  \"GCT\" 39,  \"GGA\" 40,  \"GGC\" 41,  \"GGG\" 42,  \"GGT\" 43,  \"GTA\" 44,  \"GTC\" 45,  \"GTG\" 46,  \"GTT\" 47,  \"TAA\" 48,  \"TAC\" 49,  \"TAG\" 50,  \"TAT\" 51,  \"TCA\" 52,  \"TCC\" 53,  \"TCG\" 54,  \"TCT\" 55,  \"TGA\" 56,  \"TGC\" 57,  \"TGG\" 58,  \"TGT\" 59,  \"TTA\" 60,  \"TTC\" 61,  \"TTG\" 62,  \"TTT\" 63)
        ############################
        set title ''
	set size 0.5, 0.28
	set origin 0.5, 0.30
	set tmargin 0
	set bmargin 0
	set xlabel 'HPS DNA code(i)'
	set ylabel 'frequency'
	set xrange [20: 42]
	plot	\"HPS_OUTPUTS/HPS_DNA_PATTERN_MATCH_HISTOGRAM_TVALS_~D.DNA\" 	using 1:2 title '~A' with boxes fs solid lc rgb 'salmon', \\
		\"HPS_OUTPUTS/HPS_DNA_SIGNATURE_HISTOGRAM_TVALS.DNA\"   using 1:2 title 'HPS DNA signature' with histeps lw 3 lc rgb 'light-blue'
        ############################
	reset
	set grid
	set xtics autofreq
	set ytics autofreq ( \"AAA\" 0,  \"AAC\" 1,  \"AAG\" 2,  \"AAT\" 3,  \"ACA\" 4,  \"ACC\" 5,  \"ACG\" 6,  \"ACT\" 7,  \"AGA\" 8,  \"AGC\" 9,  \"AGG\" 10,  \"AGT\" 11,  \"ATA\" 12,  \"ATC\" 13,  \"ATG\" 14,  \"ATT\" 15,  \"CAA\" 16,  \"CAC\" 17,  \"CAG\" 18,  \"CAT\" 19,  \"CCA\" 20,  \"CCC\" 21,  \"CCG\" 22,  \"CCT\" 23,  \"CGA\" 24,  \"CGC\" 25,  \"CGG\" 26,  \"CGT\" 27,  \"CTA\" 28,  \"CTC\" 29,  \"CTG\" 30,  \"CTT\" 31,  \"GAA\" 32,  \"GAC\" 33,  \"GAG\" 34,  \"GAT\" 35,  \"GCA\" 36,  \"GCC\" 37,  \"GCG\" 38,  \"GCT\" 39,  \"GGA\" 40,  \"GGC\" 41,  \"GGG\" 42,  \"GGT\" 43,  \"GTA\" 44,  \"GTC\" 45,  \"GTG\" 46,  \"GTT\" 47,  \"TAA\" 48,  \"TAC\" 49,  \"TAG\" 50,  \"TAT\" 51,  \"TCA\" 52,  \"TCC\" 53,  \"TCG\" 54,  \"TCT\" 55,  \"TGA\" 56,  \"TGC\" 57,  \"TGG\" 58,  \"TGT\" 59,  \"TTA\" 60,  \"TTC\" 61,  \"TTG\" 62,  \"TTT\" 63)
        ############################
        set title ''
	set size 0.5, 0.28
	set origin 0, 0.30
	set tmargin 0
	set bmargin 0
	set xlabel 'index (i)'
	set ylabel 'HPS DNA code(i)'
	set yrange [20: 42]
 	plot    \"HPS_OUTPUTS/HPS_DNA_PATTERN_MATCH_SEGTABLE_~D.DNA\" using 5:4 title '~A' with boxes fs solid lc rgb 'salmon', \\
		\"HPS_OUTPUTS/HPS_DNA_SIGNATURE_SEGTABLE.DNA\" 	      using 5:4 title 'HPS DNA signature' with steps lw 3 lc rgb 'light-blue'
        ############################
        set title ''
	set size 1.0, 0.17
	set origin 0, 0.05
	set tmargin 0
	set bmargin 0
	set xlabel ''
	set ylabel 'HPS DNA sequence (i)'
	~A
 	plot    \"HPS_OUTPUTS/HPS_DNA_SEQUENCE_SIG2PAT_OVERLAPS_PLOT_~D.DNA\" using 3:5 title '~A' with boxes fs solid lc rgb 'salmon', \\
		\"HPS_OUTPUTS/HPS_DNA_SEQUENCE_SIG2PAT_OVERLAPS_PLOT_~D.DNA\" using 3:4 title 'MATCHED HPS DNA signature' ps 1 lc rgb 'light-blue'
        ############################
unset multiplot
############################
")
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
(defmacro HPS_GNUPLOT_SIG2MATCH_FINEGRAIN_CROSSREF_PLOT_INSTANCE () "
        set title ''
	set size   ~A, ~A
	set origin ~A, ~A
	set tmargin 0
	set bmargin 0
	set xlabel ''
	set ylabel ''
	plot    \"HPS_OUTPUTS/HPS_1DNA_CODED_FINEGRAIN_XY_ALIGNMENT_M~D.DNA\"     using 4:5 title 'ORIGINAL SIG-TO-M~D', \\
		\"HPS_OUTPUTS/HPS_1DNA_ADJUSTED_FINEGRAIN_XY_ALIGNMENT_M~D.DNA\"  using 4:5 title 'ADJUSTED SIG-TO-M~D'
	############################
")
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
(defmacro HPS_GNUPLOT_SIG2MATCH_FINEGRAIN_CROSSREF_PLOT () "
############################
reset
set grid
set format y \"%3.0f\"
set title ''
set terminal png giant butt size 1200,800 enhanced
set output \"HPS_OUTPUTS/HPS_DNA_FINEGRAIN_SIG2MATCH_XY_PLOTS_FOR_M~D_TO_M~D.PNG\"
set multiplot
set pointsize 0.25
############################
~A
unset multiplot
############################
")
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; ***********************************************************************************************************************
(defmacro HPS_DNA_PHYLOGENIC_TREEPLOT () "
############################
# PHYLOGENIC TREE PLOT ATTEMPT
############################
reset
set terminal png giant butt size 1200,800 enhanced
set output \"HPS_OUTPUTS/HPS_DNA_PHYLOGENIC_TREEPLOT.PNG\"
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
	set xtics autofreq ( \"CLOSER\" 25,  \"\" 50, \"FARTHER\" 75, \"\" 100 )
	set ytics autofreq ( \"CLOSER\" 5,   \"\" 10, \"FARTHER\" 15, \"\" 20  )
	set ztics autofreq ( \"CLOSER\" 20,  \"\" 40, \"FARTHER\" 60, \"\" 80 )
	~A
	# tree labels
	set pm3d map
	splot \"HPS_OUTPUTS/HPS_DNA_PHYLOTREE.DAT\" title 'SIG-TO-M (MATCHING INSTANCE) PHYLOGENIC-EVOLUTION DISTANCES' with lines lc rgb 'blue' 
############################
")
;; ***********************************************************************************************************************

;; ***********************************************************************************************************************
;; ***********************************************************************************************************************
(defmacro HPS_DNA_GNUPLOT_SEQUENCE_ANALYZER_PLOT () "
############################
# SEQUENCE ANALYSIS PLOT
############################
reset
set terminal png giant butt size 1200,800 enhanced
set output \"HPS_OUTPUTS/HPS_DNA_SEQUENCE_ANALYZER_PLOT.PNG\"
############################
set multiplot
	set grid
	set clip
	# set bmargin 0
	# set tmargin 0
	set xtics
	set ytics ( \"  A\" 0, \"  C\" 1, \"  G\" 2, \"  T\" 3 )
	set xlabel ''
	set ylabel ''
	set yrange [-1:15]
	set tmargin 0
	set bmargin 0
	set size 1.0, 0.20
	set origin 0.0, 0.73
	~A
	# analysis labels
	set title 'SEQUENCE MOTIF FINDER (TOP [~D] DUETS [~D] TRIPLETS AND [~D] QUARTETS) WITHIN [~D:~D]'
	plot [~A:~A] \"HPS_INPUTS/HPS_1DNA_INPUT_SERIES_ENCODED.DNA\" using 1:2 title '' with boxes fs solid lc rgb 'salmon'
############################
	unset label
	set title ''
	set tmargin 0
	set bmargin 0
	set size 1.0, 0.18
	set origin 0, 0.51
	plot  [~A:~A] \"HPS_INPUTS/HPS_1DNA_INPUT_SERIES_ENCODED.DNA\" using 1:2    title '' with boxes fs solid lc rgb 'salmon'
        ############################
	set size 1.0, 0.18
	set origin 0, 0.27
	set xlabel ''
	plot  [~A:~A] \"HPS_INPUTS/HPS_1DNA_INPUT_SERIES_ENCODED.DNA\"    using 1:2 title '' with boxes fs solid lc rgb 'salmon'
        ############################
	set size 1.0, 0.18
	set origin 0, 0.03
	set xlabel ''
	plot  [~A:~A] \"HPS_INPUTS/HPS_1DNA_INPUT_SERIES_ENCODED.DNA\"    using 1:2 title '' with boxes fs solid lc rgb 'salmon'
        ############################
unset multiplot
")
;; ***********************************************************************************************************************

;; ***********************************************************************************************************************
(defmacro HPS_DNA_ALIGNMENT_ADJUSTMENT_LINEPLOT () "
############################
# FINAL ALIGNMENT VECTOR PLOT PER PATTERN
############################
reset
set terminal png giant butt size 1200,220 enhanced
set output \"HPS_OUTPUTS/HPS_DNA_ALIGNMENT_ADJUSTMENT_LINEPLOT_M~D.PNG\"
############################
set multiplot
	set size   1.0, 0.65
	set origin 0.0, 0.35
	set grid
	set xtics
	set ytics autofreq  ( \"  A\" 0, \"  C\" 1, \"  G\" 2, \"  T\" 3, \"del\" 4, \"ins\" 5)
	set xlabel ''
	set ylabel ''
	set yrange [-1:10]
	~A
	# set xrange 
	~A
	# alignment labels
	set bmargin 0
	plot \"HPS_OUTPUTS/HPS_1DNA_ADJUSTED_FINEGRAIN_XY_ALIGNMENT_M~D.DNA\"    using 7:2   title '' with impulses lc rgb 'light-blue', \\
		\"HPS_OUTPUTS/HPS_1DNA_ADJUSTED_FINEGRAIN_XY_ALIGNMENT_M~D.DNA\" using 7:3   title '' with impulses lc rgb 'salmon'
	############################
	set tmargin 0
	unset label
	set yrange [-1:4]
	unset xtics
	unset arrow
	set ytics autofreq  ( \"  A\" 0, \"  C\" 1, \"  G\" 2, \"  T\" 3, \"bst\" 4)
	set size   1.0, 0.24
	set origin 0.0, 0.0
	plot    \"HPS_OUTPUTS/HPS_1DNA_ADJUSTED_FINEGRAIN_XY_ALIGNMENT_M~D.DNA\" using 7:2   title '' with impulses lc rgb 'gray', \\
		\"HPS_INPUTS/HPS_1DNA_INPUT_SERIES_ENCODED.DNA\"                 using 1:2   title '' with impulses lc rgb 'salmon'
unset multiplot
############################
")


;; ***********************************************************************************************************************
;; ***********************************************************************************************************************
(defmacro HPS_DNA_REGEXPR_PLOT () "
############################
# SEQUENCE ANALYSIS PLOT
############################
reset
set terminal png giant butt size 1200,800 enhanced
set output \"HPS_OUTPUTS/HPS_DNA_REGULAR_EXPRESSION_PLOT.PNG\"
############################
set size 1.0, 0.95
set multiplot
	set yrange [-1:11]
	set grid
	set title '~A'
        		# regexpr
	~A
			# labels
        ############################
	set grid
	set xtics
	set xlabel ''
	set ylabel ''
	set ytics autofreq  (\"  0\" 0, \"  A\" 0, \"  C\" 1, \"  G\" 2, \"  T\" 3)
        ############################
	set tmargin 0
	set bmargin 0
	set size 1.0, 0.18
	set origin 0, 0.75
	plot  [~A:~A] \"HPS_INPUTS/HPS_1DNA_INPUT_SERIES_ENCODED.DNA\" using 1:2   title '~A'  with boxes fs solid lc rgb 'salmon'
        ############################
	set title ''
	set tmargin 0
	set bmargin 0
	set size 1.0, 0.18
	set origin 0, 0.52
	plot  [~A:~A] \"HPS_INPUTS/HPS_1DNA_INPUT_SERIES_ENCODED.DNA\" using 1:2    title '~A' with boxes fs solid lc rgb 'salmon'
        ############################
	set size 1.0, 0.18
	set origin 0, 0.28
	set xlabel ''
	plot  [~A:~A] \"HPS_INPUTS/HPS_1DNA_INPUT_SERIES_ENCODED.DNA\"    using 1:2 title '~A' with boxes fs solid lc rgb 'salmon'
        ############################
	set size 1.0, 0.18
	set origin 0, 0.04
	set xlabel ''
	plot  [~A:~A] \"HPS_INPUTS/HPS_1DNA_INPUT_SERIES_ENCODED.DNA\"    using 1:2 title '~A' with boxes fs solid lc rgb 'salmon'
        ############################
unset multiplot
")

;; ***********************************************************************************************************************

(defmacro HPS_DNA_DUMMY_FINAL_PLOT () "
############################
# FINAL DUMMY PLOT
############################
reset
set terminal png giant butt size 1200,800 enhanced
set output \"HPS_OUTPUTS/HPS_DNA_DUMMY_FINAL_PLOT.PNG\"
############################
        set title 'DUMMY PLOT'
	f(x) = x
 	plot  f(x)
############################
reset
")



