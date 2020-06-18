;; ********************************************************************************************
;; 			HEADERS FOR THE REPORT GENERATOR FOR DNA PATTERN MINER
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
(setf *HPS_DNA_HTML_MAIN_HEADER* "
<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Frameset//EN\" \"http://www.w3.org/TR/html4/frameset.dtd\">
<HTML> 
<HEAD> <TITLE>HPS DNA MINING SYSTEMS </TITLE> <STYLE>
<!--
@font-face {font-family:Verdana; panose-1:2 11 6 4 3 5 4 4 2 4;}
@font-face {font-family:Garamond; panose-1:2 2 4 4 3 3 1 1 8 3;}
@font-face {font-family:\"Lucida Console\"; panose-1:2 11 6 9 4 5 4 2 2 4;}
@font-face {font-family:\"Lucida Sans Unicode\"; panose-1:2 11 6 2 3 5 4 2 2 4;}
p.MsoNormal, li.MsoNormal, div.MsoNormal {margin:0in; margin-bottom:.0001pt; font-size:8.0pt; font-family:Verdana;}
@page Section0 {size:8.5in 11.0in; margin:0.5in 0.5in 0.5in 0.5in; font-size:8.0pt; font-family:Verdana;}
div.Section0 {page:Section0;}
-->
</STYLE> </HEAD> <BODY LANG=EN-US> 
<DIV CLASS=MSONORMAL ALIGN=center> 	<H3> BEST MATCHES BETWEEN DNA SIGNATURE AND TEST SEQUENCE </H3>
					<P> <STRONG> (c) 2007 <A HREF=http://www3.webng.com/nelsonmanohar/research.htm> 
					The HPS Transform <BR> by Nelson R. Manohar, Ph.D.  </A> 
					<BR> nelsonmanohar@yahoo.com </STRONG> </P> 
")
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
(setf *HPS_DNA_HTML_MAIN_HEADER_TOPLEVEL* "
<HTML> 
<HEAD> <TITLE>BEST MATCHING INSTANCES OF DNA SIGNATURE WITHIN TEST SEQUENCE </TITLE> <STYLE>
<!--
@font-face {font-family:Verdana; panose-1:2 11 6 4 3 5 4 4 2 4;}
@font-face {font-family:Garamond; panose-1:2 2 4 4 3 3 1 1 8 3;}
p.MsoNormal, li.MsoNormal, div.MsoNormal {margin:0in; margin-bottom:.0001pt; font-size:8.0pt; font-family:Verdana;}
@page Section0 {size:8.5in 11.0in; margin:0.5in 0.5in 0.5in 0.5in; font-size:8.0pt; font-family:Verdana;}
div.Section0 {page:Section0;}
-->
</STYLE> </HEAD> <BODY LANG=EN-US> 
<DIV CLASS=MSONORMAL ALIGN=center> 	
")
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; <DIV CLASS=MSONORMAL ALIGN=center>
(setf *HPS_DNA_HTML_MAIN_FOOTER*  " 
<P ALIGN=CENTER> <B> <EM> HPS DNA mining systems (c) 2005, 2006, 2007 - Dr. Nelson R. Manohar. All rights reserved. </EM> </B> </P>
</DIV>
</BODY> 
</HTML> " )
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; this macro is used to insert into the generated reports a forward reference to subsequently generated images
;; ***********************************************************************************************************************
(defmacro HPS_DNA_DETAILED_PLOT () "
<BR>
<TABLE WIDTH=1200 ALIGN=JUSTIFY COLOR=#E6F2FF> 
<TH> MATCHING INSTANCE M~D - ~A </TH>
<TR> <TD> 
<IFRAME SRC=~A HEIGHT=800 WIDTH=1200 MARGINWIDTH=0 MARGINHEIGHT=0 FRAMEBORDER=yes SCROLLING=no> 
</IFRAME> 
</TABLE>
")
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
(defmacro HPS_HTML_VERTICAL_WHITESPACE () "
<TABLE HEIGHT=~D WIDTH=1200 ALIGN=CENTER><TR><TD> 
<IMG SRC=HPS_INPUTS/HPS_WHITESPACEBAR.PNG ALIGN=CENTER WEIGHT=~D WIDTH=1190 MARGINWIDTH=0 MARGINHEIGHT=0> 
</TD></TR></TABLE>
")
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
(setf HPS_DNA_HTML_MAIN_ITEM_P0 "  <H3> <A HREF=FILE://" )
(setf HPS_DNA_HTML_MAIN_ITEM_P1 "> ATTEMPTED AUTO DECODING NUM. " )
(setf HPS_DNA_HTML_MAIN_ITEM_P2 " </A> </H3> " )
(setf HPS_DNA_HTML_MAIN_ITEM_P3 " <PRE CLASS=MSONORMAL ALIGN=CENTER> " )
(setf HPS_DNA_HTML_MAIN_ITEM_P4 " </PRE> " )
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; available html composition elements
;; ***********************************************************************************************************************
(setf _TABLE0 	"<TABLE ALIGN=JUSTIFY BGCOLOR=#FFFFFF>")
(setf _TABLE1 	"</TABLE>")
(setf _TITLE0 	"<TH>")
(setf _TITLE1 	"</TH>")
(setf _TROW0  	"<TR> ")
(setf _TROW1  	"</TR>")
(setf _TCOL0a  	"<TD WIDTH=60 VALIGN=TOP STYLE='WIDTH:44PT;BORDER-TOP:SOLID BLACK 1.0PT;BORDER-LEFT:NONE;BORDER-BOTTOM:NONE;BORDER-RIGHT:SOLID BLACK 1.0PT;PADDING:0IN 5.4PT 0IN 5.4PT'> <P  ALIGN=CENTER> <SPAN STYLE='FONT-SIZE:6.0PT;FONT-FAMILY:VERDANA'>")
(setf _TCOL0b  	"<TD WIDTH=90  BGCOLOR=#FFFFFF VALIGN=TOP STYLE='WIDTH:72PT;BORDER-TOP:SOLID BLACK 1.0PT;BORDER-LEFT:NONE;BORDER-BOTTOM:NONE;BORDER-RIGHT:SOLID BLACK 1.0PT;PADDING:0IN 5.4PT 0IN 5.4PT'> <P  ALIGN=CENTER> <SPAN STYLE='FONT-SIZE:6.0PT;FONT-FAMILY:VERDANA'>")
(setf _TCOL0c  	"<TD WIDTH=140 BGCOLOR=#CCCCCC VALIGN=TOP STYLE='WIDTH:110PT;BORDER-TOP:SOLID BLACK 1.0PT;BORDER-LEFT:NONE;BORDER-BOTTOM:NONE;BORDER-RIGHT:SOLID BLACK 1.0PT;PADDING:0IN 5.4PT 0IN 5.4PT'> <P  ALIGN=CENTER> <SPAN STYLE='FONT-SIZE:6.0PT;FONT-FAMILY:VERDANA'>")
(setf _TCOL0d  	"<TD WIDTH=140 BGCOLOR=#666666 VALIGN=TOP STYLE='WIDTH:110PT;BORDER-TOP:SOLID BLACK 1.0PT;BORDER-LEFT:NONE;BORDER-BOTTOM:NONE;BORDER-RIGHT:SOLID BLACK 1.0PT;PADDING:0IN 5.4PT 0IN 5.4PT'> <P  ALIGN=CENTER> <SPAN STYLE='FONT-SIZE:6.0PT;FONT-FAMILY:VERDANA'>")
(setf _TCOL0e  	"<TD WIDTH=200 BGCOLOR=#CCCCCC VALIGN=TOP STYLE='WIDTH:235PT;BORDER-TOP:SOLID BLACK 1.0PT;BORDER-LEFT:NONE;BORDER-BOTTOM:NONE;BORDER-RIGHT:SOLID BLACK 1.0PT;PADDING:0IN 5.4PT 0IN 5.4PT'> <P  ALIGN=LEFT>  <SPAN STYLE='FONT-SIZE:6.0PT;FONT-FAMILY:VERDANA'>")
(setf _TCOL0x  	"<TD VALIGN=TOP STYLE='WIDTH:235PT;BORDER-TOP:SOLID BLACK 1.0PT;BORDER-LEFT:NONE;BORDER-BOTTOM:NONE;BORDER-RIGHT:SOLID BLACK 1.0PT;PADDING:0IN 5.4PT 0IN 5.4PT'> <P  ALIGN=LEFT>  <SPAN STYLE='FONT-SIZE:6.0PT;FONT-FAMILY:VERDANA'>")
(setf _TCOL0z  	"<TD BGCOLOR=#CCCCCC VALIGN=TOP STYLE='WIDTH:235PT;BORDER-TOP:SOLID BLACK 1.0PT;BORDER-LEFT:NONE;BORDER-BOTTOM:NONE;BORDER-RIGHT:SOLID BLACK 1.0PT;PADDING:0IN 5.4PT 0IN 5.4PT'> <P  ALIGN=LEFT>  <SPAN STYLE='FONT-SIZE:6.0PT;FONT-FAMILY:VERDANA'>")
(setf _TCOL1  	"</SPAN> </P> </TD>")
(setf _DIV0   	"<DIV ALIGN=JUSTIFY>")
(setf _DIV1   	"</DIV>")
(setf _PRE0   	"<PRE CLASS=MSONORMAL ALIGN=CENTER> ")
(setf _PRE1   	"</PRE>")
(setf _HREF0 	"<A HREF=")
(setf _HREF1 	"</A>")
(setf _BREAK 	"<BR>")
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; ***********************************************************************************************************************
(defmacro HPS_HTML_DNA_MAIN_REPORT () "
<HTML> <HEAD> <TITLE>HPS DNA MINING SYSTEMS</TITLE> <STYLE>
<!--
@font-face {font-family:Helvetica; panose-1:2 11 6 4 2 2 2 2 2 4;}
@font-face {font-family:Courier; panose-1:2 7 4 9 2 2 5 2 4 4;}
@font-face {font-family:Times; panose-1:2 2 6 3 5 4 5 2 3 4;}
@font-face {font-family:Verdana; panose-1:2 11 6 4 3 5 4 4 2 4;}
@font-face {font-family:Garamond; panose-1:2 2 4 4 3 3 1 1 8 3;}
p.MsoNormal, li.MsoNormal, div.MsoNormal {margin:0in; margin-bottom:.0001pt; font-size:10.0pt; font-family:Verdana;}
@page Section0 {size:8.5in 11.0in; margin:0.5in 0.5in 0.5in 0.5in; font-size:10.0pt; font-family:Verdana;}
div.Section0 {page:Section0;}
-->
</STYLE> </HEAD> <BODY LANG=EN-US> 

<DIV CLASS=MSONORMAL ALIGN=center>
<H2> HPS DNA DATA MINING SYSTEM </H2> <P> <STRONG> (c) 2007 
<A HREF=http://www3.webng.com/nelsonmanohar/research.htm> HPS Transform Research 
<BR> by Nelson R. Manohar, Ph.D. </A> <BR> nelsonmanohar@yahoo.com </STRONG> </P>

<HTML> <HEAD> <TITLE>HPS DNA MINER</TITLE>
<STYLE>
<!--
@font-face {font-family:Helvetica; panose-1:2 11 6 4 2 2 2 2 2 4;}
@font-face {font-family:Courier; panose-1:2 7 4 9 2 2 5 2 4 4;}
@font-face {font-family:Times; panose-1:2 2 6 3 5 4 5 2 3 4;}
@font-face {font-family:Verdana; panose-1:2 11 6 4 3 5 4 4 2 4;}
@font-face {font-family:Garamond; panose-1:2 2 4 4 3 3 1 1 8 3;}
p.MsoNormal, li.MsoNormal, div.MsoNormal {margin:0in; margin-bottom:.0001pt; font-size:10.0pt; font-family:Verdana;}
@page Section0 {size:8.5in 11.0in; margin:0.5in 0.5in 0.5in 0.5in; font-size:10.0pt; font-family:Verdana;}
div.Section0 {page:Section0;}
-->
</STYLE> </HEAD> <BODY LANG=EN-US> 
<DIV CLASS=MSONORMAL ALIGN=JUSTIFY>
<H3>FOREWORD</H3>
<P> 	<span> <font color=red> 
		<B> THIS DOCUMENT PRESENTS PRELIMINARY PATENT-PENDING RESEARCH RESULTS;
		THEREFORE DISCLOSURE LEVEL IS PURPOSELY CURTAILED AND LIMITED.
		</B> </font> </span>
	The findings presented here relate to a theoretical signal processing and data mining 
	breakthrough, demonstrated herewith through its application to fundamental similarity/homology-based 
	problems on the bioinformatics domain area.  These systems (herein referred to as \"HPS DNA mining systems\") 
	on their own right constitute a significant and evolving body of bioinformatics research 
	contributions and deliverables.  The long-term orientation for this exploratory interdisciplinary 
	research is directed towards enabling large-scale comparative genomics research.
</P>
<H3>AUTO-GENERATED REPORT FOR SIMILARITY SEARCH </H3>
<P> 	This <B> auto-generated </B> report presents results of a <B> sequence aligning and phylogenetic 
	mining search. </B> Given a DNA signature and a DNA test sequence, \"HPS DNA mining systems\" are capable of 
	correctly identifying, extracting, and aligning ALL (that is, zero or more) TRUE matching 
	instances of said DNA signature found to be within said DNA test sequence.
	We define a <B> true matching instance </B> to be a subsequence of the DNA test sequence for 
	which either of the following (necessary and sufficient) criteria is met:
	(1) <B> the subsequence is an EXACT replica (100% compatible) of the DNA signature </B> or
	(2) <B> the subsequence exhibits TOLERABLE accumulation of DNA match errors WHILE retaining
	        certain identifying qualities.</B>  
	Specifically, accumulation of error may take place in any of the following ways:
	(2a) a matching instance exhibits a tolerable accumulation of <B> SPORADIC SINGLE-POINT 
		BASE-PAIR MUTATIONS </B> (such as inversions, transcription errors, duplications, etc.),
	(2b) a matching instance exhibits tolerable error bursts of <B> DELETED base-pairs </B>
		in either DNA signature or DNA test sequence,
	(2c) a matching instance exhibits tolerable error bursts of <B> INSERTED base-pairs </B> in 
		either DNA signature or the DNA test sequence, or
	(2d) a matching instance exhibits tolerable accumulation of error due to 
		<B> ANY COMBINATION OF THE ABOVE. </B>
	The details of how data mining parameters are selected by \"HPS DNA mining systems\" are purposely curtailed. 
	Suffice to say that the user needs only provide the DNA signature and the DNA test sequence and nothing more.  
	\"HPS DNA mining systems\" will then examine potential matching instances of the DNA signature found within 
	the DNA test sequence and reports true, optimal, and feasible matching instances found within.  
</P> 

<H4>NEW RUN-TIME COMPLEXITY LOWER BOUNDS </H4>
<P> 	More importantly, \"HPS DNA mining systems\" complete said tasks within NEW AND LOWER RUN-TIME COMPLEXITY BOUNDS 
	previously considered not feasible in practical cases and general use. 
	For example, \"HPS DNA mining systems\" can find matching 
	instances of a DNA signature of size <B> M </B> within a sequence of size <B> N </B> in sub-linear 
	time with respect to the input size <B> N. </B> That is, <B> \"HPS DNA mining systems\" can perform 
	<em> (certain types of) </em> COMBINATORIAL DATA MINING over sequences of size N in SUB-LINEAR TIME! </B>
	As a matter of fact, in practice (and regardless of input size), linear run-time cost operations 
	(such as time-series reading/writing) do take more time than the run-time cost for the \"HPS DNA mining systems\" 
	data mining core which (as stated) performs a COMBINATORIAL ANALYSIS of the entire DNA sequence. 
	Those versed in the arts, will recognize this to be an extraordinary result of vast implications.
</P> 
<H4>PHYLOGENETIC AND MUTATION SEARCH </H4>
<P> 	Moreover, <B> given R potentially phylogenetic DNA sequences,</B> by simply selecting (subsequence content) 
	from any one such as the DNA signature,  \"HPS DNA mining systems\" make possible to <B> find true matching instances 
	from the remaining R-1 </B> (potentially phylogenetic or damaged) DNA sequences in optimal time.
	Matching instances unearthed by \"HPS DNA mining systems\" are guaranteed to be true instances of the given 
	DNA signature, this being true EVEN when said matching instances may exhibit substantial DNA damage 
	(for example, due to mutations, deletions, inversions, insertions, duplications, transcription errors).
	Those versed in the arts will recognize that such error cases typically complicate correct identification 
	by inducing probabilities of misidentifications as well as resulting in significantly larger (such as 
	sub-quadratic as opposed to sub-linear) run-time computational costs in existing alternative approaches 
	in use today.  Moreover, \"HPS DNA mining systems\" not only unearth and identify true matching instances 
	but also produce rich and detailed output which identifies 
		(1) all <B> DIFFERENCES </B> between a DNA signature and any such matching instance 
		(2) as well as all <B> REPAIRS </B> (e.g., the logical placement of necessary DELETES 
		    and/or INSERTS operations) needed to transform (or repair) any 
	            (<B>potentially phylogenetic or damaged</B>) matching instance into the DNA 
		    signature (and vice versa). 
	For example, \"HPS DNA mining systems\" produce alignment-edit graphs, similarity graphs,
	base-to-base comparison reports, graphical identification of matching instances, and other 
	not yet disclosed analysis reports and graphs, etc.
</P> 
<H4>DATABASE AND WEB MINING </H4>
<P> 	Additionally, \"HPS DNA mining systems\" allow the search for matching instances of the given DNA signature 
	to be performed against a DNA sequence database (whether such is remotely located (as in the case of ENTREZ) 
	or stored on your hard-disk. Moreover, database sequences can be accessed in either the ENTREZ format or 
	in a time series format ((genomic-address, base) tuples).  Moreover, \"HPS DNA mining systems\" are 
	capable of automatically extracting time-series data from DNA sequence data found in ENTREZ webpages. 
</P>
<H4>DEMONSTRATION DATABASE </H4>
<P>	For demonstration purposes, \"HPS DNA mining systems\" have been tested on the complete Escherichia coli 
	K12 bacterium genome (that is, a genome of approximately 4.7 million base pairs (bp) and close to 
	400 DNA (10,000+ bp) DNA fragments).  The Escherichia coli K12 bacteria genome (U00096) - by 
	    <EM> Blattner,F.R., Plunkett,G. III, Bloch,C.A., Perna,N.T., Burland,V.,
            	 Riley,M., Collado-Vides,J., Glasner,J.D., Rode,C.K., Mayhew,G.F., 
	    	 Gregor,J., Davis,N.W., Kirkpatrick,H.A., Goeden,M.A., Rose,D.J., Mau,B. and Shao,Y.,
	    	 from \"The complete genome sequence of Escherichia coli K-12\", Science 277 (5331), 1453-1474 (1997) 
	    </EM> - was obtained from ENTREZ.
</P> 
<H3>DNA INPUTS: SIGNATURE AND TEST SEQUENCE</H3>
<P> 	The DNA sequences you provided for us to analyze were contained within TWO input data files (these 
	corresponding to DNA SIGNATURE and DNA TEST SEQUENCE).  These two DNA sequences were 
	automatically merged into a global sequence datafile, which was used for internal representation 
	of the inputs. No changes to your original data were done UNLESS you explicitly specified the 
	application of a test mutation to one of the two sequences AND requested that the resultant mutation 
	be stored into the corresponding input file. However, a result of this scheme is that genomic 
	addresses shown on this SUMMARY REPORT page are SHIFTED by the (ACGT-based) size of the DNA signature.
	To find the true and exacting genomic addresses (with respect to your given DNA test sequence),
	please access the link to the DETAILED REPORT page associated with any of the resultant matching 
	instance. The DNA sequences analyzed were the following:
</P>
</DIV> </BODY> </HTML> 
~A 
<HTML> <HEAD> <TITLE>HPS DNA MINER</TITLE>
<STYLE>
<!--
@font-face {font-family:Helvetica; panose-1:2 11 6 4 2 2 2 2 2 4;}
@font-face {font-family:Courier; panose-1:2 7 4 9 2 2 5 2 4 4;}
@font-face {font-family:Times; panose-1:2 2 6 3 5 4 5 2 3 4;}
@font-face {font-family:Verdana; panose-1:2 11 6 4 3 5 4 4 2 4;}
@font-face {font-family:Garamond; panose-1:2 2 4 4 3 3 1 1 8 3;}
p.MsoNormal, li.MsoNormal, div.MsoNormal {margin:0in; margin-bottom:.0001pt; font-size:10.0pt; font-family:Verdana;}
@page Section0 {size:8.5in 11.0in; margin:0.5in 0.5in 0.5in 0.5in; font-size:10.0pt; font-family:Verdana;}
div.Section0 {page:Section0;}
-->
</STYLE> </HEAD> <BODY LANG=EN-US> 
<DIV CLASS=MSONORMAL ALIGN=JUSTIFY>
<H3>DESCRIPTION OF THE TEST DATA</H3>
<P> 	<B> THIS </B> SECTION WAS NOT AUTO-GENERATED. It is provided for convenience of those 
	seeking to review, rate, or compare the results presented herein.  To test the HPS DNA 
	pattern miner, a DNA input sequence of roughly 20000 base pairs (bp) was constructed 
	using the 13000 bp E-COLI DNA sequence 1786520 (obtained from ENTREZ) as a starting point. 
	Said sequence was then MODIFIED as follows.  First, the DNA signature 
	was selected to be simply the bases contained within the genomic addresses 5000 TO 5999. 
	Then, to test several key conditions, various <B> damaged instances of the DNA signature </B> 
	were manufactured (as described below) and INSERTED INTO SPECIFIC LOCATIONS.
	This augmentation process is described below. AS SHOWN, ALL TEST CASES WERE CORRECTLY UNEARTHED, RATED, AND REPORTED.
</P>
<H4>LAYOUT OF TEST-CASE MATCHING INSTANCES </H4>
<TABLE CLASS=MSONORMAL ALIGN=JUSTIFY>
		   <TR> <TD> <SPAN STYLE='font-size:8.0pt;font-family:Verdana'>
		   	Between [5000:6000]
		   </SPAN> </TD> <TD> <SPAN STYLE='font-size:8.0pt;font-family:Verdana'>
		   	the <B> identical matching instance </B> of the DNA signature is found.
		   </SPAN> </TD> <TD> <SPAN STYLE='font-size:8.0pt;font-family:Verdana'>
	 	   	The original DNA test sequence 1786520 had 13480 bp.
		   </SPAN> </TD> </TR> <TR> <TD> <SPAN STYLE='font-size:8.0pt;font-family:Verdana'>
	           	Between [7000:8000] 
		   </SPAN> </TD><TD> <SPAN STYLE='font-size:8.0pt;font-family:Verdana'>
		   	a <B> heavily single-point mutated version </B> of the original DNA signature was inserted, 
			consisting of approximately pseudo-random 25 single-point mutations from A->T coupled with
			approximately 25 single-point mutations from T->A. The mutations were chosen to take place at
			particular indexes (e.g., every 10th base which happened to be A) to easily verify via 
			eye inspection of proper detection within the matching instance. Effectively, the resulting 
			matching instance had about 5% (50/1000) randomly single-point mutations which HPS algorithms
			would have to deal with in the identification of the intrinsic structure of the underlying 
			DNA signature found within. 
		   </SPAN> </TD><TD> <SPAN STYLE='font-size:8.0pt;font-family:Verdana'>
		   	This transformation increased the resultant DNA test sequence in size from 13480 to 14480.
		   </SPAN> </TD> </TR> <TR> <TD> <SPAN STYLE='font-size:8.0pt;font-family:Verdana'>
	           	Between [13481:14450] 
		   </SPAN> </TD><TD> <SPAN STYLE='font-size:8.0pt;font-family:Verdana'>
			a <B> heavily damaged version of the DNA signature </B> was inserted, which exhibited a 
			SINGLE DELETION BURST of exactly 30 base pairs at (intra) genomic address (800). To accomplish
			this, a true DNA signature match was inserted therein, but then basepairs at (DNA signature) 
			genomic addresses [800:830] were deleted. No single point mutations were inflicted onto this 
			true matching instance. 
		   </SPAN> </TD><TD> <SPAN STYLE='font-size:8.0pt;font-family:Verdana'>
			This increased the resultant DNA test sequence in size fro 14480 to 15550.
		   </SPAN> </TD> </TR> <TR> <TD> <SPAN STYLE='font-size:8.0pt;font-family:Verdana'>
	           	Between [14550:17000] 
		   </SPAN> </TD><TD> <SPAN STYLE='font-size:8.0pt;font-family:Verdana'>
			a <B> DNA filler </B> sequence extracted from a subsequence of the original DNA test sequence
			(but OUTSIDE of the DNA signature region) was inserted into the DNA test sequence.  
		   </SPAN> </TD><TD> <SPAN STYLE='font-size:8.0pt;font-family:Verdana'>
			This increased the size from 15550 to 17000.
		   </SPAN> </TD> </TR> <TR> <TD> <SPAN STYLE='font-size:8.0pt;font-family:Verdana'>
	           	Between [17000:18000] 
		   </SPAN> </TD><TD> <SPAN STYLE='font-size:8.0pt;font-family:Verdana'>
			a <B> heavily damaged version of the DNA signature </B> was inserted, which exhibited (1) a DELETION 
			BURST of exactly 30 base pairs at (intra) genomic address (300) coupled with (2) a RANDOM INSERTION 
			BURST of 30 base pairs at (intra) genomic address (800). Effectively, a true DNA signature match was 
			inserted therein, but with deleted basepairs at genomic addresses [300:330] and inserted RANDOM 
			bases at genomic addresses [800:830]. No single point mutations were inflicted onto this true 
			matching instance. 
		   </SPAN> </TD><TD> <SPAN STYLE='font-size:8.0pt;font-family:Verdana'>
			This increased the size from 17000 to 18000.
		   </SPAN> </TD> </TR> <TR> <TD> <SPAN STYLE='font-size:8.0pt;font-family:Verdana'>
	           	Between [18000:19000] 
		   </SPAN> </TD><TD> <SPAN STYLE='font-size:8.0pt;font-family:Verdana'>
			another <B> DNA filler </B> sequence extracted from a subsequence of the original DNA test sequence
			(but OUTSIDE of the DNA signature region) was inserted into the DNA test sequence.  
		   </SPAN> </TD><TD> <SPAN STYLE='font-size:8.0pt;font-family:Verdana'>
			This increased the size from 18000 to 19000.
		   </SPAN> </TD> </TR> <TR> <TD> <SPAN STYLE='font-size:8.0pt;font-family:Verdana'>
	           	Between [19000:19970] 
		   </SPAN> </TD><TD> <SPAN STYLE='font-size:8.0pt;font-family:Verdana'>
			a <B> heavily damaged version of the DNA signature </B> was inserted, which exhibited a SINGLE DELETION 
			BURST of exactly 30 base pairs at (intra) genomic address (300). Effectively, a true DNA signature 
			match was inserted therein, but with deleted basepairs at genomic addresses [300:330]. No single 
			point mutations were inflicted onto this true matching instance. 
		   </SPAN> </TD><TD> <SPAN STYLE='font-size:8.0pt;font-family:Verdana'>
			This increased the size from 19000 to 20000.
		   </SPAN> </TD> </TR> <TR> <TD> <SPAN STYLE='font-size:8.0pt;font-family:Verdana'>
	           	Between [19970:30050] 
		   </SPAN> </TD><TD> <SPAN STYLE='font-size:8.0pt;font-family:Verdana'>
			a <B> DNA filler </B> sequence extracted from SEQUENCE_1786454.DNA (10080 bp) was inserted.
		   </SPAN> </TD><TD> <SPAN STYLE='font-size:8.0pt;font-family:Verdana'>
			This increased the size from 20000 to 30000.
		   </SPAN> </TD> </TR> <TR> <TD> <SPAN STYLE='font-size:8.0pt;font-family:Verdana'>
	           	Between [24000:24975] 
		   </SPAN> </TD><TD> <SPAN STYLE='font-size:8.0pt;font-family:Verdana'>
			a <B> heavily damaged version of the DNA signature </B> was inserted, which exhibited (1) multiple DELETION 
			BURSTs of exactly 8 base pairs at every 200 (intra) genomic address (i.e., 200, 400, 600, 800) and then 
			coupled with (2) a single INSERTION BURST of 8 base pairs at (intra) genomic address (500).  No single point 
			mutations were inflicted onto this true matching instance. 
		   </SPAN> </TD><TD> <SPAN STYLE='font-size:8.0pt;font-family:Verdana'>
			This increased the size from 30000 to 31000.
		   </SPAN> </TD> </TR> <TR> <TD> <SPAN STYLE='font-size:8.0pt;font-family:Verdana'>
	           	Between [30000:30975] 
		   </SPAN> </TD><TD> <SPAN STYLE='font-size:8.0pt;font-family:Verdana'>
			a <B> heavily damaged version of the DNA signature </B> was inserted, which exhibited the above DNA damage
			coupled with several random single point mutations at periodic intervals (1 base every 50 bases, for the 
			first 500 genomic addresses). 
		   </SPAN> </TD><TD> <SPAN STYLE='font-size:8.0pt;font-family:Verdana'>
			This increased the size from 31000 to 32000.
		   </SPAN> </TD> </TR> <TR> <TD> <SPAN STYLE='font-size:8.0pt;font-family:Verdana'>
	           	Between [32002:32978] 
		   </SPAN> </TD><TD> <SPAN STYLE='font-size:8.0pt;font-family:Verdana'>
			a <B> heavily damaged version of the DNA signature </B> was inserted, which exhibited multiple DELETION 
			BURSTs of exactly 8 base pairs at every 200 (intra) genomic address (i.e., 200, 400, 600, 800).
		   </SPAN> </TD><TD> <SPAN STYLE='font-size:8.0pt;font-family:Verdana'>
			This increased the size from 32000 to 33000. Total input size (approx. 1000+33000bp).
		   </SPAN> </TD> </TR>
</TABLE>
</DIV> </BODY> </HTML> 
~A
<HTML> <HEAD> <TITLE>HPS DNA MINER</TITLE> 
<STYLE>
<!--
@font-face {font-family:Helvetica; panose-1:2 11 6 4 2 2 2 2 2 4;}
@font-face {font-family:Courier; panose-1:2 7 4 9 2 2 5 2 4 4;}
@font-face {font-family:Times; panose-1:2 2 6 3 5 4 5 2 3 4;}
@font-face {font-family:Verdana; panose-1:2 11 6 4 3 5 4 4 2 4;}
@font-face {font-family:Garamond; panose-1:2 2 4 4 3 3 1 1 8 3;}
p.MsoNormal, li.MsoNormal, div.MsoNormal {margin:0in; margin-bottom:.0001pt; font-size:10.0pt; font-family:Verdana;}
@page Section0 {size:8.5in 11.0in; margin:0.5in 0.5in 0.5in 0.5in; font-size:10.0pt; font-family:Verdana;}
div.Section0 {page:Section0;}
-->
</STYLE> </HEAD> <BODY LANG=EN-US> 
<DIV CLASS=MSONORMAL ALIGN=JUSTIFY>
<H3> IDENTIFIED MATCHING INSTANCES OF DNA SIGNATURE </H3>
<P>  The following figure identifies the approximate relative placement of matching instances of the DNA signature 
	found to exist within the given DNA test sequence.  To enhance viewing and close up examination of findings, 
	the resulting time plot has been divided into four close-up subintervals of approximately the same size.
	Correspondingly, from top to bottom, a time-plot panel shows each such subinterval, with the top panel 
	containing the first quarter of the span of the DNA input sequence and the bottom panel containing the 
	last quarter.  As shown, the (combined) DNA input sequence consists of the DNA signature being prefixed to 
	the DNA test sequence.  Therefore, genomic addresses shown are shifted by the size of the DNA signature.
</P> 
<H4> IDENTIFICATION OF MATCHING INSTANCES </H3>
<P> 
	Matching instances of the DNA signature are identified via a <B> SOLID BLACK ARROW. </B> Each such arrow
	is also annotated with a TEXT LABEL having the format: 
		<STRONG> \"<I>M#</I> [<I>START</I>:<I>END</I>]\"; </STRONG> where
	<B> M# </B> uniquely identifies a matching instance across all auto-generated \"HPS DNA mining systems\" reports. 
	For example, for any matching instance on the SUMMARY REPORT page, there exists a DETAILED REPORT page, 
	identified and linked by said number.
	<B> START </B> and <B> END </B> represent the starting and ending genomic addresses of the matching instance 
	as found within the (combined) DNA input sequence.  These genomic addresses are APPROXIMATE. To obtain the 
	TRUE AND EXACTING genomic address for any matching instance with respect to the given DNA test sequence,
	you MUST refer to said matching instance's DETAILED REPORT page.
	Nevertheless, the TRUE genomic address can also be estimated by simply subtracting the size of the DNA 
	signature from such approximate alignment indexes.  However, this number represents only an approximation 
	that lies within +/- a small constant.  
</P>
<TABLE WIDTH=1200 BGCOLOR=#E6F2FF ALIGN=JUSTIFY>
<TH> IDENTIFIED MATCHING INSTANCES WITHIN THE DNA INPUT SEQUENCE </TH>
<TR> <TD> <IMG SRC=DNA_MATCH_BEHAVIOR.PNG WIDTH=1200 HEIGHT=800 HSPACE=0 VSPACE=0 BORDER=0 ALIGN=JUSTIFY> </TD> </TR> <TR>
<TD AlIGN=TOP> <SPAN STYLE='font-size:9.0pt;font-family:Verdana'> <DL> 
<STRONG>TOP:</STRONG> the first  subinterval, base pairs [0         .. 1/4 (M+N)), 
<STRONG>2ND:</STRONG> the second subinterval, base pairs [1/4 (M+N) .. 2/4 (M+N)), 
<STRONG>3RD:</STRONG> the third  subinterval, base pairs [2/4 (M+N) .. 3/4 (M+N)), and
<STRONG>BOT:</STRONG> the fourth subinterval, base pairs [3/4 (M+N) .. 4/4 (M+N)).
</SPAN> </TD> </TR> </TABLE>
</DIV> </BODY> </HTML>
<HTML> <HEAD> <TITLE>HPS DNA MINER</TITLE> 
<STYLE>
<!--
@font-face {font-family:Helvetica; panose-1:2 11 6 4 2 2 2 2 2 4;}
@font-face {font-family:Courier; panose-1:2 7 4 9 2 2 5 2 4 4;}
@font-face {font-family:Times; panose-1:2 2 6 3 5 4 5 2 3 4;}
@font-face {font-family:Verdana; panose-1:2 11 6 4 3 5 4 4 2 4;}
@font-face {font-family:Garamond; panose-1:2 2 4 4 3 3 1 1 8 3;}
p.MsoNormal, li.MsoNormal, div.MsoNormal {margin:0in; margin-bottom:.0001pt; font-size:10.0pt; font-family:Verdana;}
@page Section0 {size:8.5in 11.0in; margin:0.5in 0.5in 0.5in 0.5in; font-size:10.0pt; font-family:Verdana;}
div.Section0 {page:Section0;}
-->
</STYLE> </HEAD> <BODY LANG=EN-US> 
<DIV CLASS=MSONORMAL ALIGN=JUSTIFY>
<H3> RECONSTRUCTION OF BURST DAMAGED DNA (OR PHYLOGENETIC) MATCHING INSTANCES </H3>
<P>  	Matching instances unearthed by \"HPS DNA mining systems\" may actually be (mutated as well as potentially phylogenetic) 
	versions of the given DNA signature, which nevertheless retain some special intrinsic qualities associated with 
	the DNA signature.
	Typically, in such cases, special operations are used to show how, for any matching instance said to represent 
	a damaged or mutated version of the DNA signature, it is possible to (optimally) reconstruct (i.e. with few
	operations and few changes) the targeted DNA signature sequence.
</P>
<H4> RECONSTRUCTION OPERATIONS </H4>
<P> 	The minimal abstract base machine needed to perform such reconstruction consists of just two operations: 
	<B> DELETE(X) </B> and <B> INSERT(X).</B>  The <B> DELETE(X) </B> operation edits a DNA sequence (at 
	the current genomic address) by deleting  the next <B> X bases </B> from it.  The <B> INSERT(X) </B> 
	operation edits a DNA sequence (at the current genomic address) by inserting <B> X base placeholders </B> 
	into it.  A <B> DELETE/INSERT </B> operation is triggered when a damage burst of more than <B> Z bases </B> 
	is observed within a DNA sequence.  Suppose that such event is detected at genomic address <B> Y.</B> 
	Then, such triggering event will be identified by the <B> RESYNC (Y, Z)</B>  operation, which is 
	therefore always followed by either a <B> DELETE(X) </B> or <B> INSERT(X) </B> operation.
	Finally, we choose to use another couple of operations to identify the exacting genomic address 
	of any matching instance.  First, a <B> PRELIM-ALIGN(Y) </B> operation marks the preliminary 
	genomic address believed to be the start of a matching instance.
	Then, an <B> OFFSET-ALIGN(W) </B> operation identifies how far away (in number of bases, 
	in either direction) is the exacting genomic address that corresponds to the start of the 
	matching instance. Note that (as stated), both these genomic addresses refer to indexing within 
	the DNA input sequence, which consists of the DNA signature PREPENDED to the DNA test sequence).
	As stated, to obtain the true genomic address (MSTART) for the start  of a matching instance, 
	one need only to refer to the DETAILED REPORT page for said matching instance or alternatively, 
	subtract the size <B> M </B> of the DNA signature from the offset-corrected preliminary addresses. 
	That is, <B> MSTART=Y+W-M. </B>
</P>
<H4> ADJUSTED AND ORIGINAL PAIRINGS </H4>
<P> 	Finally, to differentiate between original and reconstructed cases, we refer to the pairing of 
	the DNA signature to the ORIGINAL (i.e., as found) matching instance M# from the DNA test 
	sequence to as the <B> ORIGINAL SIG-TO-M# pairing. </B> Similarly, we refer to the pairing 
	of the DNA signature to the ADJUSTED (i.e., after reconstruction operations) matching instance M# 
	to as the <B> ADJUSTED SIG-TO-M# pairing. </B>  The following figures show, for each matching 
	instance extracted from the DNA test sequence, the resultant reconstruction operations needed 
	to recover the DNA signature from said matching instance.  For this reason, each figure has 
	two parts. Whereas the top part of each figure shows the  <B> ADJUSTED SIG-TO-M# pairing </B> 
	together with the various reconstruction operations needed, the bottom part of each figure 
	shows the <B> ORIGINAL SIG-TO-M# pairing </B> of DNA signature to matching instance (as given). 
	Within any pairing plot, the DNA signature is always colored in \"light-blue\" color whereas 
	the matching instance is always colored in \"light-pink\".  
</P>
<H4> BEHAVIOR OF THE RECONSTRUCTION </H3>
<P> 	Note that when a matching instance exhibits no burst DNA damage, no reconstruction needs 
	to be apply (i.e., all DNA damage is due to single-point errors) and thus, ORIGINAL and ADJUSTED 
	pairings are the same.  Moreover, note that when a matching instance exhibits burst DNA damage, 
	while the ORIGINAL pairing shows clear cross-correspondence discrepancies between DNA signature 
	and matching instance due to the misalignment induced by such burst DNA damage, after such 
	burst DNA damage is corrected via reconstruction operations, the resultant ADJUSTED pairing 
	exhibits consistently accurate cross-correspondence.  Note however, that while burst DNA damage 
	is corrected during reconstruction, single point random errors are always left (on either pairing) 
	uncorrected (such showing in various test-cases). To this end, note that for each matching instance 
	a summary statistics tuple is also given, which shows the number of mismatched base pairs in the
	cross-correspondence check between the DNA signature and the ADJUSTED (reconstructed) matching instance.
	Such reported mismatches are due to single-point random errors plus the repair cost (in bases) of 
	reconstruction operations.
</P> 
</DIV> </BODY> </HTML>
<DIV CLASS=MSONORMAL ALIGN=JUSTIFY> 
<TABLE WIDTH=1200 BGCOLOR=#E6F2FF> 
<TR> <TD ALIGN=TOP> <SPAN STYLE='font-size:9.0pt;font-family:Verdana'> <DL> <TR><TD> ~A </TD> </TR> </TABLE> 
</DIV>
<HTML> <HEAD> <TITLE>HPS DNA MINER</TITLE> 
<STYLE>
<!--
@font-face {font-family:Helvetica; panose-1:2 11 6 4 2 2 2 2 2 4;}
@font-face {font-family:Courier; panose-1:2 7 4 9 2 2 5 2 4 4;}
@font-face {font-family:Times; panose-1:2 2 6 3 5 4 5 2 3 4;}
@font-face {font-family:Verdana; panose-1:2 11 6 4 3 5 4 4 2 4;}
@font-face {font-family:Garamond; panose-1:2 2 4 4 3 3 1 1 8 3;}
p.MsoNormal, li.MsoNormal, div.MsoNormal {margin:0in; margin-bottom:.0001pt; font-size:10.0pt; font-family:Verdana;}
@page Section0 {size:8.5in 11.0in; margin:0.5in 0.5in 0.5in 0.5in; font-size:10.0pt; font-family:Verdana;}
div.Section0 {page:Section0;}
-->
</STYLE> </HEAD> <BODY LANG=EN-US> 
<DIV CLASS=MSONORMAL ALIGN=JUSTIFY>
<H3> SIMILARITY PLOTS BETWEEN DNA SIGNATURE AND MATCHING INSTANCES </H3>
<P> 	Similarity plots (i.e., dot-matrix alignment plots) provide quick visual identification of the fitness of 
	the resultant alignment between the DNA signature and a given matching instance.  Note that TWO 
	similarity plots are shown on each panel, a RED line and a GREEN line.  The RED one corresponds to a similarity plot 
	between the DNA signature and the ORIGINAL DNA subsequence data that corresponds to the matching instance 
	found genomic addresses. The GREEN line represents a similarity plot between the DNA signature and the ADJUSTED DNA 
	subsequence obtained through the application of RECONSTRUCTION operations over the ORIGINAL DNA subsequence. 
	Recall that reconstruction operations are used to correct BURSTS of DNA damage found to be present within 
	the ORIGINAL DNA subsequence. Such reconstructions generate a new ADJUSTED outlook over the ORIGINAL the 
	DNA subsequence data of the matching instance, on which DNA bases or placeholders are deleted or inserted to
	maintain synchrony. Therefore, note how GREEN and RED plots DO differ in cases where a matching instance 
	exhibits BURST DNA damage. In such cases, note that the GREEN line exhibits FAITHFUL ALIGNMENT 
	(just after reconstruction operations are applied). Such case manifests, during each burst of DNA damage, 
	as a reset of the green line back to the X=Y diagonal just after a COUPLE OF INITIAL BURST-DETECTION MISSES.
	These initial mismatches are due to an initial detection cost and relate to algorithmic cost decisions.  
	That is, the cost of reconstruction operations manifest as initial short-term GREEN discrepancies that 
	always coincide with the start of large RED discrepancies and that after such initial repair cost, 
	the GREEN line depicts through the resynching of the ADJUSTED matching instance to the DNA signature, whereas 
	the RED similarity plot depicts a furtherance of the discrepancy between the ORIGINAL matching instance with
	respect to the DNA signature.  The detailed analysis of such resynching is found in the DETAILED REPORT 
	page that corresponds to said matching instance.  As stated, only <B> TRUE, OPTIMAL, and FEASIBLE matching instances </B>
	are reported. Finally, plots are generated using an IN-HOUSE distance metric designed for visual acuity, 
	which somewhat distorts the genomic address correspondence of those points lying OUTSIDE 
	the X=Y line (i.e., the discrepancies).  
</P>
<TABLE WIDTH=1200 BGCOLOR=#E6F2FF ALIGN=JUSTIFY>
<TH> \"SIMILARITY PLOTS\" DNA SIGNATURE AGAINST MATCHING INSTANCE (UNDER UNADJUSTED ALIGNMENT) </TH>
<TR> <TD> <IMG SRC=HPS_DNA_FINEGRAIN_SIG2MATCH_XY_PLOTS_FOR_M0_TO_M8.PNG 
WIDTH=1200 HEIGHT=800 HSPACE=0 VSPACE=0 BORDER=0 ALIGN=JUSTIFY> </TD> </TR>
<TABLE WIDTH=1200 BGCOLOR=#E6F2FF> <TR> <TD AlIGN=TOP> <SPAN STYLE='font-size:9.0pt;font-family:Verdana'> <DL> 
<DT> <STRONG>FROM BOTTOM LEFT TO TOP RIGHT: </STRONG>
	Similarity plots (TWO per panel).  The RED one is a similarity plot between the DNA signature and the 
	ORIGINAL DNA subsequence corresponding to the matching instance found.  The GREEN one is a similarity 
	plot between the DNA signature and the ADJUSTED DNA subsequence obtained through the application of 
	reconstruction operations.
</DT>
</DL> </SPAN> </TD> </TR> </TABLE>
</TABLE>
</DIV> </BODY> </HTML>
<HTML> <HEAD> <TITLE>HPS DNA MINER</TITLE> 
<STYLE>
<!--
@font-face {font-family:Helvetica; panose-1:2 11 6 4 2 2 2 2 2 4;}
@font-face {font-family:Courier; panose-1:2 7 4 9 2 2 5 2 4 4;}
@font-face {font-family:Times; panose-1:2 2 6 3 5 4 5 2 3 4;}
@font-face {font-family:Verdana; panose-1:2 11 6 4 3 5 4 4 2 4;}
@font-face {font-family:Garamond; panose-1:2 2 4 4 3 3 1 1 8 3;}
p.MsoNormal, li.MsoNormal, div.MsoNormal {margin:0in; margin-bottom:.0001pt; font-size:10.0pt; font-family:Verdana;}
@page Section0 {size:8.5in 11.0in; margin:0.5in 0.5in 0.5in 0.5in; font-size:10.0pt; font-family:Verdana;}
div.Section0 {page:Section0;}
-->
</STYLE> </HEAD> <BODY LANG=EN-US> 
<DIV CLASS=MSONORMAL ALIGN=JUSTIFY>
<H3> DNA SIGNATURE-ROOTED PHYLOGRAM </H3>
<P> 	As stated, all matching instances are TRUE matching instances of the DNA signature, i.e., the limiting probability 
	of (such matching instance being a TRUE match to the DNA signature) is ONE in the formal sense (i.e., virtual 
	certainty for all practical purposes).  Therefore, the distance metrics used in this plot rather than representing 
	measurements of the probabilistic fitness of any such match represent instead abstraction measures of the DNA 
	damage that is accounted for within a matching instance. As shown above, in the reconstruction edit plots, it has 
	been determined that after such specified DNA reconstruction, all such burst DNA damage could be corrected and an 
	exacting reproduction of the original DNA signature is therefore achievable from the matching instance. 
</P>
<H4> RELATIVE EVOLUTIONARY COST METRICS </H4>
<P> 	In the following plot, points represent the exacting (x,y,z) cost-metric positioning of the given matching instance,
	The line shown traverses the set of matching instances along a sorted partial order based on the total number of 
	uncorrectable errors (as well as some other cost-metrics).  This way, the DNA signature is found at the (0,0,0) 
	coordinates and each point along the line represents a matching instance of greater number of accumulated 
	uncorrectable errors.  In actuality, the evolutionary cost-metric used to generate the plot is a three-dimensional 
	cost metric and thus this ordering is a partial order in the formal sense. As a result, the positioning (x2, y2, z2) 
	of the subsequent (partially ordered) matching instance is found to be in (and displayed as) a proportional 
	delta with respect to the previous (x1, y1, z1) coordinates.  This way, edges represent an estimate of the 
	total evolutionary 3D-cost-metric change between consecutive matching instances.  Note that the distance from 
	the (0,0,0)=DNA-SIGNATURE vertex to the actual (x,y,z) coordinate of any matching instance represents a form of 
	absolute evolutionary distance.  Similarly, the lenght of an edge between consecutive nodes simply allows computing 
	the magnitude of their internodal evolutionary distance by a straightforward application of the Phytagoras' theorem. 
</P>
<H4> PHYLOGENETIC SORTING </H4>
<P> 	Note that, in the pure sense, the following plot does NOT represent a PHYLOGENETIC TREE as edges between nodes (i.e., 
	matching instances) relate differential cost-metrics among matching instances (as well as with respect to the DNA 
	signature) as opposed to evolutionary relationships between matching instances.  Note however, that the plot can be 
	used as a STARTING POINT to derive a phylogenetic tree between the matching instances and the DNA signature. 
</P>
<TABLE WIDTH=1200 BGCOLOR=#E6F2FF ALIGN=JUSTIFY>
<TH> PHYLOGENETIC SORTING </TH>
<TR> <TD> <IMG SRC=HPS_DNA_PHYLOGENIC_TREEPLOT.PNG WIDTH=1200 HEIGHT=800 HSPACE=0 VSPACE=0 BORDER=0 ALIGN=JUSTIFY> </TD> </TR> <TR>
<TD AlIGN=TOP> <SPAN STYLE='font-size:9.0pt;font-family:Verdana'> <DL> 
<DT><STRONG>BOT</STRONG> Distance 3D metric relationship between the DNA signature and unearthed matching instances. This plot 
can be used to derive a phylogenetic tree showing the evolutionary relationship between the DNA signature and matching instances. 
</DT> </DL> </SPAN> </TD> </TR> </TABLE>
</DIV> </BODY> </HTML>
<HTML> <HEAD> <TITLE>HPS DNA MINER</TITLE> 
<STYLE>
<!--
@font-face {font-family:Helvetica; panose-1:2 11 6 4 2 2 2 2 2 4;}
@font-face {font-family:Courier; panose-1:2 7 4 9 2 2 5 2 4 4;}
@font-face {font-family:Times; panose-1:2 2 6 3 5 4 5 2 3 4;}
@font-face {font-family:Verdana; panose-1:2 11 6 4 3 5 4 4 2 4;}
@font-face {font-family:Garamond; panose-1:2 2 4 4 3 3 1 1 8 3;}
p.MsoNormal, li.MsoNormal, div.MsoNormal {margin:0in; margin-bottom:.0001pt; font-size:10.0pt; font-family:Verdana;}
@page Section0 {size:8.5in 11.0in; margin:0.5in 0.5in 0.5in 0.5in; font-size:10.0pt; font-family:Verdana;}
div.Section0 {page:Section0;}
-->
</STYLE> </HEAD> <BODY LANG=EN-US> 
<DIV CLASS=MSONORMAL ALIGN=JUSTIFY>
<H3> SEQUENCE MOTIF FINDER </H3>
<P>	\"HPS DNA mining systems\" provide additional functions such as the <B> sequence motif 
	finder. </B> The sequence motif finder is a sequence-motif miner and visualizer 
	that allows identifying the relative frequency and location for any, some, or even all
	DNA sequence-motifs DUETS, TRIPLETS, and/or QUARTETS (i.e., 2, 3, and 4-base).  
	The sequence motif finder has linear run-time costs. For your convenience, 
   	\"HPS DNA mining systems\" applies the sequence-motif finder to the DNA signature 
	by default - unless otherwise you specified a different subsequence interval. 
</P>
<H4> SUMMARY INFORMATION FOR MOTIF DUETS, TRIPLETS, AND QUARTETS </H4>
<P> 	The figure below shows the top (most frequent) sequence-motif DUETS, TRIPLETS, 
	and QUARTETS found to lie within the specified subsequence of the DNA input sequence. 
	As stated, the full extent of the DNA signature is examined by default, although it 
	is possible to examine ANY subsequence of the <B> DNA input sequence. </B> Therefore, 
	the subsequence interval analyzed is identified in the plot by a range notation of 
	the form [#:#].  To allow closer examination, the selected interval of the DNA input 
	sequence has been automatically divided into four subintervals of similar size being 
	ordered (by increasing genomic address) from top to bottom.
	Summary data along the top part of the figure provides information 
	about the relative frequency and identity of sequence-motifs found based on top-most 
	frequency rank. Summary data is divided into four columns that corresponds (from left 
	to right) to: (1) summary data for the top 5 motif DUETS,  (2) summary data for the 
	top 5 motif TRIPLETS, (3) summary data for the top 5 motif QUARTETS, and (4) summary 
	data for the 4 DNA bases.  By default, the top 5 sequence-motifs duets, triplets, and 
	quartets are analyzed; however, this number can be specified.  Moreover, for each 
	sequence-motif, a <B> tight-bound interval </B> describes the number of <B> motif-repeats 
	</B> found for said sequence-motif.  In practicality, the lower-estimate (of the interval 
	given) represents the true number of motif-repeats that corresponds to a sequence-motif. 
	Furthermore, for each sequence-motif found subsequence, a color-code is assigned (note 
	however that colors are re-used across columns).  The motif's assigned color is used to 
	color-code the display of <B> all repeats of its corresponding sequence-motif. </B> This 
	way, each sequence-motif is shown by a stream of color-coded vertical arrows/lines placed 
	in exactly <B> one row per sequence-motif. </B> Normally, all arrow-rows for sequence-motif 
	DUETS are printed first, then all arrow-rows for TRIPLETS are printed, and finally, all 
	arrow-rows for QUARTETS are printed.  However, by default, the display of said motif-repeats 
	arrow-rows is shown <B> only for the topmost sequence-motif </B> each for DUETS (top row), 
	TRIPLETS (middle row), and QUARTETS (bottom row). The number of arrow-rows to print for 
	the resultant DUETS, TRIPLETS, AND QUARTETS motif-set can be specified and its limited only 
	by physical constraints of the plot.
</P>
<TABLE WIDTH=1200 BGCOLOR=#E6F2FF ALIGN=JUSTIFY>
<TH> SEQUENCE MOTIF FINDER </TH>
<TR> <TD> <IMG SRC=HPS_DNA_SEQUENCE_ANALYZER_PLOT.PNG WIDTH=1200 HEIGHT=800 HSPACE=0 VSPACE=0 BORDER=0 ALIGN=JUSTIFY> 
</TD> </TR> <TR> <TD AlIGN=TOP> <SPAN STYLE='font-size:9.0pt;font-family:Verdana'> <DL> 
<DT> Repeats of the highest frequency sequence-motif(s) found to lie within the specified subsequence. Default
setup displays summary (frequency and identify) data for only the TOP FIVE FREQUENCY sequence-motif DUETs, TRIPLETs, 
and QUARTETs (i.e., 2, 3, and 4 DNA-base sequences).  The relative location for each of the motif-repeats of the 
TOPMOST DUET, TRIPLET, and QUARTET is also shown by means of color-coded arrow-rows. Both the number of (2, 3, 4-base) 
motif-repeat arrow-rows to display and the number of sequence-motifs to rank can be specified. </DT>
</DL> </SPAN> </TD> </TR> </TABLE>
</DIV> </BODY> </HTML>
<HTML> <HEAD> <TITLE>HPS DNA MINER</TITLE> 
<STYLE>
<!--
@font-face {font-family:Helvetica; panose-1:2 11 6 4 2 2 2 2 2 4;}
@font-face {font-family:Courier; panose-1:2 7 4 9 2 2 5 2 4 4;}
@font-face {font-family:Times; panose-1:2 2 6 3 5 4 5 2 3 4;}
@font-face {font-family:Verdana; panose-1:2 11 6 4 3 5 4 4 2 4;}
@font-face {font-family:Garamond; panose-1:2 2 4 4 3 3 1 1 8 3;}
p.MsoNormal, li.MsoNormal, div.MsoNormal {margin:0in; margin-bottom:.0001pt; font-size:10.0pt; font-family:Verdana;}
@page Section0 {size:8.5in 11.0in; margin:0.5in 0.5in 0.5in 0.5in; font-size:10.0pt; font-family:Verdana;}
div.Section0 {page:Section0;}
-->
</STYLE> </HEAD> <BODY LANG=EN-US> 
<DIV CLASS=MSONORMAL ALIGN=JUSTIFY>
<H3> REGULAR-EXPRESSION MOTIF MINER </H3>
<P>	\"HPS DNA mining systems\" provide also the ability to search a given DNA sequence for instances 
	of a matching subsequence with respect to a specified <B> motif-based regular-expression </B>. 
	This regular expression applied against a sequence can be arbitrarily complex and when combined with the
	above motif finder, it is quite simple to construct.  Complex sequence motifs can be specified with
	ease by non-specialists using a simple and easy to write <B> UNIX-like syntax/grammar </B> that 
	allows the <B> specification of complex DNA patterns of repeated sequence-motifs of varying lengths. </B>
	The upcoming release of \"HPS DNA mining systems\" provides even more extensive regular-expression 
	composition, discovery, and search tools.  
</P>
<H4> SPECIFICATION OF MOTIF REGULAR EXPRESSIONS </H4>
<P> 	A motif-based regular expression is specified as a <B> sequence of (one or more) tuple-pairs constraints </B> of the 
	form <B> \"(DNA_MOTIF)[NUMBER]\" </B> where <B> (DNA_MOTIF) </B> represents a <B> (1, 2, 3, 4 DNA-base) 
	sequence-motif specifier subsequence </B>.  Valid examples of such are any of the following: 
	(ACG), (CG), (AAAA), (ATA), (T), and (C). Note that the the motif must be delimited by opening and 
	closing parenthesis. Similarly, <B> [NUMBER] </B>  represents a positive number <B> specifier of 
	the number of repeats </B> associated with the preceding sequence-motif. Valid examples of such 
	are any of the following: [1], [3], [10], [100], and [*].  Note that the unknown number of repeats 
	is specified by the special qualifier [*] and that numerical repeat qualifier must be delimited by 
	opening and closing square brackets.  This way, the following regular expression 
			<B> \"(AAT)[2](GG)[1](A)[3](C)[1](CGTA)[2)\" </B> 
	translates to a variable-length sequence-motif search against the specified sequence for the 
	first subsequence of DNA-bases that FULFILLS ALL specified tuple-pair constraints found within the
	regular expression in the exact order given and with the exact number of motif-repeats specified.
	For example, assume the symbol \"*\" represents a random DNA base, then the above specified motif-based
	regular expression would be matched against a subsequence such as the following:
		      <B> \"AAT***AAT**********GG***A*****A*A***C****CGTA****CGTA\" </B> 
	if one such like exists from within the specified interval of the DNA input sequence. The motif-based 
	regular expression finder has linear-time run-time complexity.
</P>
<H4> SUMMARY INFORMATION FOR MOTIF REGULAR EXPRESSION MINING </H4>
<P> The following figure depicts the results of a regular expression search within the DNA input sequence.
	By default, the motif-based regular expression is applied to (the full extent of) the DNA signature 
	found within the DNA input sequence. However,  any given interval-based subsequence of the DNA 
	input sequence can be examined by the motif-based regular expression miner. Moreover, one or more 
	matching instance can be extracted (if any) but by default, only the very first match from within 
	the specified interval is shown.  The plot automatically zooms in into the MAXIMUM DETAIL POSSIBLE 
	for the interval of the matching subsequence found to match all the tuple-pair constraint of the 
	motif-based regular expression. Moreover, said resultant matching interval is further divided into 
	FOUR EQUAL LENGTH CONTIGUOUS SUBINTERVALS, which are then plotted one per panel, with the first 
	of said subintervals shown at the top and correspondingly, the last subinterval at the bottom.
	The motif-based regular expression that was searched-for is printed atop of the figure while
	the precise location of each (thus) successfully met tuple-pair constraint of the motif-based 
	regular expression is shown within the exact location of the matching subinterval plot. Each DNA base
	of the matching subinterval is printed (along a single row across all subplots) and above the 
	relevant location of a precise match of a tuple-pair constraint in a subplot, the actual tuple-pair 
	constraint being met is printed (across any of three alloted tuple-pair constraint rows). 
	Note that for tuple-pair constraints specified a repeat, the tuple-pair constraint is shown
	the corresponding that many times on the relevant subinterval and subplot.  Note the precise 
	and exacting correspondance - as with any of the features of our system.
</P>
<TABLE WIDTH=1200 BGCOLOR=#E6F2FF ALIGN=JUSTIFY>
<TH> DNA MINING BASED ON MOTIF REGULAR EXPRESSIONS </TH>
<TR> <TD> <IMG SRC=HPS_DNA_REGULAR_EXPRESSION_PLOT.PNG WIDTH=1200 HEIGHT=800 HSPACE=0 VSPACE=0 BORDER=0 ALIGN=JUSTIFY> 
</TD> </TR> <TR> <TD AlIGN=TOP> <SPAN STYLE='font-size:9.0pt;font-family:Verdana'> <DL> 
<DT> Resultant matching subsequence interval associated with the successful mining of the specified motif-based 
regular expression against the specified interval of the DNA input sequence.  The motif-based regular expression 
miner handles motif-repeats of variable-length across variable gap lengths.  Any regular expression consisting of 
(1, 2, 3, 4-base) sequence-motifs can be specified.  Regular expressions follow an easy Unix-like model described 
above.  
</DT>
</DL> </SPAN> </TD> </TR> </TABLE>
</DIV> </BODY> </HTML>
<HTML> <HEAD> <TITLE>HPS DNA MINER</TITLE> 
<STYLE>
<!--
@font-face {font-family:Helvetica; panose-1:2 11 6 4 2 2 2 2 2 4;}
@font-face {font-family:Courier; panose-1:2 7 4 9 2 2 5 2 4 4;}
@font-face {font-family:Times; panose-1:2 2 6 3 5 4 5 2 3 4;}
@font-face {font-family:Verdana; panose-1:2 11 6 4 3 5 4 4 2 4;}
@font-face {font-family:Garamond; panose-1:2 2 4 4 3 3 1 1 8 3;}
p.MsoNormal, li.MsoNormal, div.MsoNormal {margin:0in; margin-bottom:.0001pt; font-size:10.0pt; font-family:Verdana;}
@page Section0 {size:8.5in 11.0in; margin:0.5in 0.5in 0.5in 0.5in; font-size:10.0pt; font-family:Verdana;}
div.Section0 {page:Section0;}
-->
</STYLE> </HEAD> <BODY LANG=EN-US> 
<DIV CLASS=MSONORMAL ALIGN=JUSTIFY>
<H3>ABOUT \"HPS DNA MINING SYSTEMS\" </H3>
<P>	\"HPS DNA mining systems\" are implemented in COMMON LISP with 100% proprietary in-house code that 
	has been designed for maintenance and extensibility.  Moreover, \"HPS DNA mining systems\" is not 
	just an application but rather an easily extensible programming environment for bioinformatic applications.  
	The HPS code base has been designed with portability in mind, with neither operating system nor compiler 
	dependencies. Use of external (non-incorporated into) and unmodified applications GNUplot <EM> (Copyright 
	1986 - 1993, 1998, 2004   Thomas Williams, Colin Kelley) </EM> and GNUwget <EM> (Copyright © 1996–2005 
	Free Software Foundation, Inc.) </EM> is made under respective licenses        
	(<A HREF=./../HPS_INPUTS/GNUPLOT.HTM> GNUPLOT license </A>) and 
	(<A HREF=./../HPS_INPUTS/GNUWGET.HTM> GNUWGET license. </A>)
</P>
<H4> HPCC SUITABILITY </H4>
<P> 	Current implementation has been tested on the Windows XP operating system under typical Pentium-class 
	personal computer power.  Even though \"HPS DNA mining systems\" are specifically designed to allow data mining 
	of large DNA databases on Pentium-class personal computers, \"HPS DNA mining systems\" are inherently suitable 
	for future deployment on HPCC's grid-computing systems through batched pre-computation and adaptive parceling 
	of key \"HPS DNA mining systems\" tasks.
</P>
</DIV> </BODY> </HTML>
<HTML> <HEAD> <TITLE>HPS DNA MINER</TITLE>
<STYLE>
<!--
@font-face {font-family:Helvetica; panose-1:2 11 6 4 2 2 2 2 2 4;}
@font-face {font-family:Courier; panose-1:2 7 4 9 2 2 5 2 4 4;}
@font-face {font-family:Times; panose-1:2 2 6 3 5 4 5 2 3 4;}
@font-face {font-family:Verdana; panose-1:2 11 6 4 3 5 4 4 2 4;}
@font-face {font-family:Garamond; panose-1:2 2 4 4 3 3 1 1 8 3;}
p.MsoNormal, li.MsoNormal, div.MsoNormal {margin:0in; margin-bottom:.0001pt; font-size:10.0pt; font-family:Verdana;}
@page Section0 {size:8.5in 11.0in; margin:0.5in 0.5in 0.5in 0.5in; font-size:10.0pt; font-family:Verdana;}
div.Section0 {page:Section0;}
-->
</STYLE> </HEAD> <BODY LANG=EN-US> 
<DIV CLASS=MSONORMAL ALIGN=JUSTIFY>
<H3> DISCLAIMER </H3>
<P> 	This report was automatically generated by \"HPS DNA mining systems\".  \"HPS DNA mining systems\" 
	are based on proprietary patent-pending technologies developed by Dr. Nelson R. Manohar-Alers. No permit 
	to use \"HPS DNA mining systems\" algorithms, software, technologies, disclosures, or derivatives 
	of such is granted without authorization from Dr. Nelson R. Manohar.  The research direction underlying 
	\"HPS DNA mining systems\" are a result of the unique blend, refinement, and reuse of skills (accumulated 
	by Dr.  Manohar-Alers across 20 years of professional experience) to continuously achieve an innovative 
	leaping furtherance of our initial (c. 1992) long-term-held research thread on adaptive systems.
	<B> Qualified principals 
	and/or reviewers from qualified institutions </B> may present inquiries by e-mailing the author 
	<B> (Dr. Nelson R. Manohar, Principal Research Scientist, and Principal of \"HPS DNA mining systems\")  
	</B> at the e-mail address given atop this report. Dr. Nelson R. Manohar is open to consider \"qualified 
	and appropriate\" collaboration, seeding, funding, or contract inquiries from <B> qualified principals 
	</B> from <B> reputable and qualified (US/EU) institutions.</B> However, because of the nature, volume, 
	intensity, and disclosure-level of our (environments and) research work, we reserve the right to reply 
	to any inquiry.  For example, inquiries that somehow could be related to disclosure of patent-pending 
	\"HPS DNA mining systems\" may be politely ignored.  Moreover, inquiries from corporate laboratories 
	may not be answered.
</P><P> HPS research work has entirely been self-funded and achieved under sustained and adverse malfeasance 
	conditions.  \"HPS DNA mining systems\" research work did NOT received support from either the 
	<B> National Science Foundation (NSF) </B>, the <B> National Institutes of Health (NIH)</B>, 
	or any other similar granting agencies.  It is relevantly disclosed that <B> (U.S.) FEDERAL AND 
	(N.Y., P.R.) STATE MALFEASANCES </B> have previously been documented, disclosed, and filed and such 
	relate to matters of STRONG NATIONAL INTEREST.  Please inform the Inspector General of the U.S. 
	Department of Justice (Mr. Glenn A. Fine) (at askdoj@usdoj.gov) of any \"suspected-to-be malfeasance 
	order\" interfering with inquiries to us on areas related to (\"but not limited to\") research, 
	funding, employment, collaborations, etc.  Finally, we will politely ignore inquiries from parties 
	suspected to have compromised, malfeasance, or political interests -- proper outlets for such have 
	been given.  Dr. Nelson R. Manohar-Alers is a U.S. Citizen.
</P><P> This webpage is best seen WITHOUT font/color substitution at <B> 1200x800 </B> pixel resolution (e.g., 
	the WIDESCREEN/LANDSCAPE format typically used in laptop displays).  This webpage is also designed to 
	fit and print on PORTRAIT format as long as font override is NOT specified.  Therefore, for best 
	viewing and printing, you should have the <B> \"Verdana\" </B> font installed in your system.  Plots 
	found within use the PNG (Portable Graphics) format and are generated at the relatively high (1200x800) 
	pixel resolution. This resolution allows for reasonable magnification detail in most image editors 
	or modern web browsers. Some links on our reports are (by default) set to open as a new browser window 
	or browser tab; therefore, check that your computer does not mistake such opening window or tab as an apparent 
	popup window --- it is not.  This webpage or site NEITHER contains scripts NOR active code.  Display of 
	this webpage has been tested for the Opera browser on a Windows XP platform.  We value your copyrights, 
	please respect ours.
</P> 	<P align=center> <B> <EM> (c) Nelson R. Manohar, Ph.D. - 2005-2007. All rights are reserved. </EM> </B> </span> </P> 
</DIV> </BODY> </HTML> 
")
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
(defmacro HPS_DNA_MATCH_BEHAVIOR_COMPONENT () "
<HTML> <HEAD> <TITLE>HPS DNA MINER</TITLE> 
<STYLE>
<!--
@font-face {font-family:Helvetica; panose-1:2 11 6 4 2 2 2 2 2 4;}
@font-face {font-family:Courier; panose-1:2 7 4 9 2 2 5 2 4 4;}
@font-face {font-family:Times; panose-1:2 2 6 3 5 4 5 2 3 4;}
@font-face {font-family:Verdana; panose-1:2 11 6 4 3 5 4 4 2 4;}
@font-face {font-family:Garamond; panose-1:2 2 4 4 3 3 1 1 8 3;}
p.MsoNormal, li.MsoNormal, div.MsoNormal {margin:0in; margin-bottom:.0001pt; font-size:10.0pt; font-family:Verdana;}
@page Section0 {size:8.5in 11.0in; margin:0.5in 0.5in 0.5in 0.5in; font-size:10.0pt; font-family:Verdana;}
div.Section0 {page:Section0;}
-->
</STYLE> </HEAD> <BODY LANG=EN-US> 

<H3> BEHAVIOR OF THE MINING PROCESS ACROSS TIME </H3>
<P> 	The graphs below show the behavior of the data mining process over time as it operates in the
        multidimensional space of the transformed data. The bottom graph on the following figure shows 
	a time plot of the behavior of the mining process in term of the observed match (feasibility) 
	strength signal as it gets evaluated across increasing genomic addresses of the DNA input 
	sequence. A combinatorial data mining is performed yet in sub-linear time with respect to the
	size of the DNA input sequence. This data mining always operates at a fraction of the linear 
	run-time cost associated with sequential operations such as either I/O-bound (i.e., reading 
	and writing) of the DNA input sequence or CPU-bound operations over the entire DNA input sequence.
	Moreover, this high-performance data mining is combinatorial in nature, that is, it exhaustively
	examines ALL possible match opportunities. More importantly, the behavior of the combinatorial
	data-mining process is well-behaved; that is, even when extracted by said high-performance 
	combinatorial data mining engine, matching instances exhibit an extraordinary signal-to-noise 
	ratio over non-matching instances.   
	The upper graph shows a histogram of the resultant match strenght signal which in turn reveals 
	a GAUSSIAN fit coupled with a HEAVY-TAIL outlier presence (i.e., LOGNORMAL fit in the formal 
	sense).  That is, under this well-behaved mining process, matching instances represent 
	HEAVY-TAIL outliers with respect to the general population of values which behaves as Gaussian noise. 
</P> 
<BR>
<BR>
<BR>
<BR>



<TABLE WIDTH=1200 BGCOLOR=#E6F2FF ALIGN=JUSTIFY>
<TH> HISTOGRAM OF MATCH RATING SIGNAL VALUES ACROSS ALL PIVOT POINTS IN THE DNA TEST SEQUENCE</TH>
<TR> <TD> <IMG SRC=DNA_MATCH_BEHAVIOR_HISTOGRAM.PNG WIDTH=1200 HEIGHT=800 HSPACE=0 VSPACE=0 BORDER=0 ALIGN=JUSTIFY> </TD> </TR>
</TABLE>

<TABLE WIDTH=1200 BGCOLOR=#E6F2FF> <TR> <TD AlIGN=TOP> <SPAN STYLE='font-size:9.0pt;font-family:Verdana'> <DL> 
<DT> <STRONG>TOP:</STRONG> Histogram of the match strength signal with search region, white noise, and matching instances identified.</DT>
<DT> <STRONG>TOP: GREEN-FIT</STRONG> Resultant gaussian curve for mean and sigma parameters derived from the data (when heavy tail outliers are included).  </DT> 
<DT> <STRONG>TOP: BLUE-FIT</STRONG> Best normal curve fit for resultant mean and sigma parameters (when heavy tail ooutliers are removed).  </DT> 
<DT> <STRONG>BOTTOM</STRONG> Time plot of the match strength signal in the nth-dimensional optimization space, with matching instances identified. </DT> 
</DL> </SPAN> </TD> </TR> </TABLE>
</BODY> </HTML>
")
;; ***********************************************************************************************************************


;; ***********************************************************************************************************************
;; ***********************************************************************************************************************
(defmacro HPS_DNA_TABBED_REPORT_NEWTAB () "
<!DOCTYPE html PUBLIC -//W3C//DTD XHTML 1.0 Strict//EN http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd>
<html xmlns=http://www.w3.org/1999/xhtml xml:lang=en lang=en>
<head>
<meta http-equiv=content-type content=text/html;charset=iso-8859-2 />
<meta name=author content=free-css-templates.com />
<link rel=stylesheet href=./../HPS_INPUTS/REPORT/stylesheet.css type=text/css />
<title>HPS Research - Dr. Nelson R. Manohar</title>
<!--
@font-face {font-family:Helvetica; panose-1:2 11 6 4 2 2 2 2 2 4;}
@font-face {font-family:Courier; panose-1:2 7 4 9 2 2 5 2 4 4;}
@font-face {font-family:Times; panose-1:2 2 6 3 5 4 5 2 3 4;}
@font-face {font-family:Verdana; panose-1:2 11 6 4 3 5 4 4 2 4;}
@font-face {font-family:Garamond; panose-1:2 2 4 4 3 3 1 1 8 3;}
p.MsoNormal, li.MsoNormal, div.MsoNormal {margin:0in; margin-bottom:.0001pt; font-size:10.0pt; font-family:Verdana;}
@page Section0 {size:8.5in 11.0in; margin:0.5in 0.5in 0.5in 0.5in; font-size:10.0pt; font-family:Verdana;}
div.Section0 {page:Section0;}
-->
</STYLE> </HEAD> <BODY LANG=EN-US> 

<div id=content>
<div id=title> <h1> <img src=./../HPS_INPUTS/REPORT/IMAGES/cat.jpg height=80 alt=hpslogo /> <span> HPS DNA MINING SYSTEMS RESEARCH - <a href=HPS_FOREWORD.HTM> <span class=green> DR. NELSON R.
MANOHAR </span> </a> </span> </h1> <span id=slogan> HPS DNA MINING SYSTEMS (c) 2007 BY NELSON R. MANOHAR-ALERS, PH.D.  </span> </div>

<div id=menu>
<div class=submit>
<ul>
<li><a href=HPS_FOREWORD.HTM ~A>                 <span><b>FOREWORD</b></span></a></li>
<li><a href=HPS_INTRODUCTION.HTM ~A>             <span><b>INTRO</b></span></a></li>
<li><a href=HPS_INPUTS.HTM ~A>                   <span><b>INPUTS</b></span></a></li>
<li><a href=HPS_DATA_DESCRIPTION.HTM ~A>         <span><b>OPTIONAL</b></span></a></li>
<li><a href=HPS_BEHAVIOR.HTM ~A>                 <span><b>PERF</b></span></a></li>
<li><a href=HPS_MATCHING_INSTANCES.HTM ~A>       <span><b>MATCHES</b></span></a></li>
<li><a href=HPS_RECONSTRUCTION_OPS.HTM ~A>       <span><b>REPAIR</b></span></a></li>
<li><a href=HPS_SIMILARITY_PLOTS.HTM ~A>         <span><b>SIMILARITY</b></span></a></li>
<li><a href=HPS_PHILOGRAM_PLOT.HTM ~A>           <span><b>PHILOGRAM</b></span></a></li>
<li><a href=HPS_MOTIF_SEARCH.HTM ~A>             <span><b>MOTIFS</b></span></a></li>
<li><a href=HPS_MOTIF_REGEXPR.HTM ~A>            <span><b>MOTIF-MINER</b></span></a></li>
<li><a href=HPS_ABOUT_US.HTM ~A>                 <span><b>ABOUT</b></span></a></li>
<li><a href=HPS_DISCLAIMER.HTM ~A>               <span><b>DISCLAIMER</b></span></a></li>
</ul>
</div>
</div>


<div id=subheader>
<div class=rside>
Auto-generated report for multiple sequence alignment (MSA) analysis of the DNA signature against DNA test sequence.
</div>

<div class=lside>
<strong> Welcome! </strong> 
</div>
</div>

<div id=maincontent>
<A HREF=http://www3.webng.com/nelsonmanohar/research.htm> <P align=center> <B> <EM> (c) Nelson R. Manohar, Ph.D. - 2005-2007. All rights are reserved. </EM> </B> </span> </A> 

<div id=right_side>
<div class=lcontent align=justify> <h3> SITE MAP FOR AUTO-GENERATED REPORT</h3> 
<BR>   <a href=HPS_FOREWORD.HTM>                 <span><b>FOREWORD</b></span></a> 	 - important remarks.</li>
<BR>   <a href=HPS_INTRODUCTION.HTM>             <span><b>INTRODUCTION</b></span></a>    - to HPS DNA Mining Systems.</li>
<BR>   <a href=HPS_INPUTS.HTM>                   <span><b>INPUTS</b></span></a> 	 - description of the processed inputs.</li>
<BR>   <a href=HPS_DATA_DESCRIPTION.HTM>         <span><b>OPTIONAL</b></span></a> 	 - user-provide description of inputs.</li>
<BR>   <a href=HPS_BEHAVIOR.HTM>                 <span><b>PERFORMANCE</b></span></a>     - performance of dna mining.</li>
<BR>   <a href=HPS_MATCHING_INSTANCES.HTM>       <span><b>MATCHES</b></span></a>         - overview of matching instances.</li>
<BR>   <a href=HPS_RECONSTRUCTION_OPS.HTM>       <span><b>RECONSTRUCTION</b></span></a>  - summary of matching instances.</li>
<BR>   <a href=HPS_SIMILARITY_PLOTS.HTM>         <span><b>SIMILARITY</b></span></a>      - before/after dot matrix plots.</li>
<BR>   <a href=HPS_PHILOGRAM_PLOT.HTM>           <span><b>PHILOGRAM</b></span></a>       - signature-rooted philogram.</li>
<BR>   <a href=HPS_MOTIF_SEARCH.HTM>             <span><b>MOTIF-ID</b></span></a>        - motif characterization tool.</li>
<BR>   <a href=HPS_MOTIF_REGEXPR.HTM>            <span><b>MOTIF-MINER</b></span></a>     - motif miner tool.</li>
<BR>   <a href=HPS_ABOUT_US.HTM>                 <span><b>ABOUT USR</b></span></a>       - about us.</li>
<BR>   <a href=HPS_DISCLAIMER.HTM>               <span><b>DISCLAIMER</b></span></a>      - important remarks.</li>
</div>
</div>

<div id=left_side align=justify>


~A

<BR>

<h2 class=underline> About us (www_nelsonmanohar_us) </h2> <img src=./../HPS_INPUTS/REPORT/IMAGES/bookcase.jpg alt=aboutus/>
<p> <strong>Dr. Manohar-Alers</strong> earned a B.S. in computer engineering in 1987. 
He also earned a M.S. computer engineering from the University of Wisconsin at Madison in 1988 and a M.S.E. in 
industrial engineering from the University of Michigan at Ann Arbor in 1992.  Dr. Manohar-Alers earned his Ph.D. 
in computer science and engineering specializing in software systems from the University of Michigan at Ann Arbor 
in 1997.  Previously, he worked at the IBM T. J. Watson Research Center as a Research Staff Member and at AT&T 
Bell Laboratories (now Lucent Bell Labs) as a Member of the Technical Staff.  His interests are in experimental 
software systems dealing with scalable systems theory, measurements, and resource management.  He has been lead 
inventor in several patents on resource management.  Previous research related to groupware, multimedia authoring, 
and advanced intelligent networks.  Dr. Manohar-Alers' credentials are found on his <a href=vitae/2008-NRM_CV.pdf 
target=_blank> <b> curriculum vitae. </b></a> He currently works as an independent researcher in Puerto Rico - 
USA, where Dr. Manohar-Alers has threaded his long-term research onto applied research on <strong>state estimation 
for random processes.</strong> This research introduces the <strong>Harmonic Process State (HPS) transform</strong>.
The <strong>HPS Transform</strong> exhibits desirable qualities on implementation ease, algorithmic complexity, 
computational stability, signal compressibility, decision-making robustness, information loss, and error behavior. 
Although the <strong>HPS Transform</strong> is particularly suited for process control, the results have vast 
implications to other fields.  You may contact Dr. Manohar-Alers at <a href=NRM_Contact.htm target=_blank> 
<strong>nelsonmanohar@yahoo.com</strong>. </a> </p>

<h2>OUR ASSETS</h2>
<p> Dr. Manohar-Alers is currently unaffiliated to a research institution or grant. His body of research work has 
been done independently, in a self-supported manner, and thus readily available for use at a future research affiliation.
His research work has the potential to produce high-impact (theoretical and applied) assets related to research 
publications, software, and patents of particular targeting toward valuable application domains.  </p>

<h2>CONTACT US</h2>
<p> This website is chiefly made available toward principals from research and academic institutes expressly evaluating 
the credentials of Dr. Manohar-Alers for employment within their institutes.  Partly because of time-zone differences, 
e-mail is preferred in contacting Dr. Manohar-Alers.  </p>

<h2>DISCLAIMER</H2>
<p> No data is collected (by this website) during your visit to this website.  This website may provide access to its 
author's copyrighted material, intellectual property, and/or privileged information for expressly confidential review.
We ask that you respect our copyrights and confidentiality requests.  This website may contain documents filed pursuant 
USC-compliant claims entered with the USDOJ.  This website does not intentionally support nor contain illegal nor 
unlawful content.  Please read important information about this website's license terms <a href=NRM_license.htm> 
(i.e., <b>TERMS-OF-USE</b>). </a> </p>

</div>
</div>

<div id=footer>
<p class=right> NELSON R. MANOHAR &copy; 2007  &middot; Updated on 5/07 &middot; Design: 
<a href=http://www.free-css-templates.com title=Designed by free-css-templates.com>free-css-templates.com</a> &middot;
<a href=HPS_DISCLAIMER.HTM target=_blank>License Terms</a> 
</p>
</div> 
</div> </body> </html>
")
;; ***********************************************************************************************************************

;; ***********************************************************************************************************************
;; ***********************************************************************************************************************
(defmacro HPS_HTML_TAB_FOREWORD () "
<HTML> <HEAD> <TITLE>HPS DNA MINING SYSTEMS</TITLE> <STYLE>
<!--
@font-face {font-family:Helvetica; panose-1:2 11 6 4 2 2 2 2 2 4;}
@font-face {font-family:Courier; panose-1:2 7 4 9 2 2 5 2 4 4;}
@font-face {font-family:Times; panose-1:2 2 6 3 5 4 5 2 3 4;}
@font-face {font-family:Verdana; panose-1:2 11 6 4 3 5 4 4 2 4;}
@font-face {font-family:Garamond; panose-1:2 2 4 4 3 3 1 1 8 3;}
p.MsoNormal, li.MsoNormal, div.MsoNormal {margin:0in; margin-bottom:.0001pt; font-size:10.0pt; font-family:Verdana;}
@page Section0 {size:8.5in 11.0in; margin:0.5in 0.5in 0.5in 0.5in; font-size:10.0pt; font-family:Verdana;}
div.Section0 {page:Section0;}
-->
</STYLE> </HEAD> <BODY LANG=EN-US> 

<H3>FOREWORD</H3>
<P> 	<span> <font color=red> 
		<B> THIS DOCUMENT PRESENTS PRELIMINARY PATENT-PENDING RESEARCH RESULTS;
		THEREFORE DISCLOSURE LEVEL IS PURPOSELY CURTAILED AND LIMITED.
		</B> </font> </span>
	The findings presented here relate to a theoretical signal processing and data mining 
	breakthrough capable (for some particular problem domains) of <B> SUB-LINEAR TIME 
	COMBINATORIAL DATA MINING ANALYSIS (though coupled with LINEAR-TIME TRANSFORM 
	PREPROCESSING)</B>  and demonstrated herewith through its application to fundamental similarity/homology-based 
	problems on the bioinformatics domain area.  These systems (herein referred to as \"HPS DNA mining systems\") 
	on their own right constitute a significant and evolving body of bioinformatics research 
	contributions and deliverables.  The long-term orientation for this exploratory interdisciplinary 
	research is directed towards enabling large-scale comparative genomics research.
</P>
</BODY> </HTML> 
")
;; **************************************************************************************************************************


(defmacro HPS_HTML_TAB_INTRODUCTION () "
<H3>AUTO-GENERATED REPORT FOR SIMILARITY SEARCH </H3>
<P> 	This <B> auto-generated </B> report presents results of a <B> sequence aligning and phylogenetic 
	mining search. </B> Given a DNA signature and a DNA test sequence, \"HPS DNA mining systems\" are capable of 
	correctly identifying, extracting, and aligning ALL (that is, zero or more) TRUE matching 
	instances of said DNA signature found to be within said DNA test sequence.
	We define a <B> true matching instance </B> to be a subsequence of the DNA test sequence for 
	which either of the following (necessary and sufficient) criteria is met:
	(1) <B> the subsequence is an EXACT replica (100% compatible) of the DNA signature </B> or
	(2) <B> the subsequence exhibits TOLERABLE accumulation of DNA match errors WHILE retaining
	        certain identifying qualities.</B>  
	Specifically, accumulation of error may take place in any of the following ways:
	(2a) a matching instance exhibits a tolerable accumulation of <B> SPORADIC SINGLE-POINT 
		BASE-PAIR MUTATIONS </B> (such as inversions, transcription errors, duplications, etc.),
	(2b) a matching instance exhibits tolerable error bursts of <B> DELETED base-pairs </B>
		in either DNA signature or DNA test sequence,
	(2c) a matching instance exhibits tolerable error bursts of <B> INSERTED base-pairs </B> in 
		either DNA signature or the DNA test sequence, or
	(2d) a matching instance exhibits tolerable accumulation of error due to 
		<B> ANY COMBINATION OF THE ABOVE. </B>
	The details of how data mining parameters are selected by \"HPS DNA mining systems\" are purposely curtailed. 
	Suffice to say that the user needs only provide the DNA signature and the DNA test sequence and nothing more.  
	\"HPS DNA mining systems\" will then examine potential matching instances of the DNA signature found within 
	the DNA test sequence and reports true, optimal, and feasible matching instances found within.  
</P> 

<H4>NEW RUN-TIME COMPLEXITY LOWER BOUNDS </H4>
<P> 	More importantly, \"HPS DNA mining systems\" complete said tasks within NEW AND LOWER RUN-TIME COMPLEXITY BOUNDS 
	previously considered not feasible in practical cases and general use. 
	For example, \"HPS DNA mining systems\" can find matching 
	instances of a DNA signature of size <B> M </B> within a sequence of size <B> N </B> in sub-linear 
	time with respect to the input size <B> N. </B> That is, <B> \"HPS DNA mining systems\" can perform 
	<em> (certain types of) </em> COMBINATORIAL DATA MINING over sequences of size N in SUB-LINEAR TIME! </B>
	As a matter of fact, in practice (and regardless of input size), linear run-time cost operations 
	(such as time-series reading/writing) do take more time than the run-time cost for the \"HPS DNA mining systems\" 
	data mining core which (as stated) performs a COMBINATORIAL ANALYSIS of the entire DNA sequence. 
	Those versed in the arts, will recognize this to be an extraordinary result of vast implications.
</P> 
<H4>PHYLOGENETIC AND MUTATION SEARCH </H4>
<P> 	Moreover, <B> given R potentially phylogenetic DNA sequences,</B> by simply selecting (subsequence content) 
	from any one such as the DNA signature,  \"HPS DNA mining systems\" make possible to <B> find true matching instances 
	from the remaining R-1 </B> (potentially phylogenetic or damaged) DNA sequences in optimal time.
	Matching instances unearthed by \"HPS DNA mining systems\" are guaranteed to be true instances of the given 
	DNA signature, this being true EVEN when said matching instances may exhibit substantial DNA damage 
	(for example, due to mutations, deletions, inversions, insertions, duplications, transcription errors).
	Those versed in the arts will recognize that such error cases typically complicate correct identification 
	by inducing probabilities of misidentifications as well as resulting in significantly larger (such as 
	sub-quadratic as opposed to sub-linear) run-time computational costs in existing alternative approaches 
	in use today.  Moreover, \"HPS DNA mining systems\" not only unearth and identify true matching instances 
	but also produce rich and detailed output which identifies 
		(1) all <B> DIFFERENCES </B> between a DNA signature and any such matching instance 
		(2) as well as all <B> REPAIRS </B> (e.g., the logical placement of necessary DELETES 
		    and/or INSERTS operations) needed to transform (or repair) any 
	            (<B>potentially phylogenetic or damaged</B>) matching instance into the DNA 
		    signature (and vice versa). 
	For example, \"HPS DNA mining systems\" produce alignment-edit graphs, similarity graphs,
	base-to-base comparison reports, graphical identification of matching instances, and other 
	not yet disclosed analysis reports and graphs, etc.
</P> 
<H4>DATABASE AND WEB MINING </H4>
<P> 	Additionally, \"HPS DNA mining systems\" allow the search for matching instances of the given DNA signature 
	to be performed against a DNA sequence database (whether such is remotely located (as in the case of ENTREZ) 
	or stored on your hard-disk. Moreover, database sequences can be accessed in either the ENTREZ format or 
	in a time series format ((genomic-address, base) tuples).  Moreover, \"HPS DNA mining systems\" are 
	capable of automatically extracting time-series data from DNA sequence data found in ENTREZ webpages. 
</P>
<H4>DEMONSTRATION DATABASE </H4>
<P>	For demonstration purposes, \"HPS DNA mining systems\" have been tested on the complete Escherichia coli 
	K12 bacterium genome (that is, a genome of approximately 4.7 million base pairs (bp) and close to 
	400 DNA (10,000+ bp) DNA fragments).  The Escherichia coli K12 bacteria genome (U00096) - by 
	    <EM> Blattner,F.R., Plunkett,G. III, Bloch,C.A., Perna,N.T., Burland,V.,
            	 Riley,M., Collado-Vides,J., Glasner,J.D., Rode,C.K., Mayhew,G.F., 
	    	 Gregor,J., Davis,N.W., Kirkpatrick,H.A., Goeden,M.A., Rose,D.J., Mau,B. and Shao,Y.,
	    	 from \"The complete genome sequence of Escherichia coli K-12\", Science 277 (5331), 1453-1474 (1997) 
	    </EM> - was obtained from ENTREZ.
</P> 
</P> 	<P align=center> <B> <EM> (c) Nelson R. Manohar, Ph.D. - 2005-2007. All rights are reserved. </EM> </B> </span> </P> 
</BODY> </HTML> 
")
;; **************************************************************************************************************************


(defmacro HPS_HTML_TAB_INPUTS () "
<HTML> <HEAD> <TITLE>HPS DNA MINING SYSTEMS</TITLE> <STYLE>
<!--
@font-face {font-family:Helvetica; panose-1:2 11 6 4 2 2 2 2 2 4;}
@font-face {font-family:Courier; panose-1:2 7 4 9 2 2 5 2 4 4;}
@font-face {font-family:Times; panose-1:2 2 6 3 5 4 5 2 3 4;}
@font-face {font-family:Verdana; panose-1:2 11 6 4 3 5 4 4 2 4;}
@font-face {font-family:Garamond; panose-1:2 2 4 4 3 3 1 1 8 3;}
p.MsoNormal, li.MsoNormal, div.MsoNormal {margin:0in; margin-bottom:.0001pt; font-size:10.0pt; font-family:Verdana;}
@page Section0 {size:8.5in 11.0in; margin:0.5in 0.5in 0.5in 0.5in; font-size:10.0pt; font-family:Verdana;}
div.Section0 {page:Section0;}
-->
</STYLE> </HEAD> <BODY LANG=EN-US> 

<H3>DNA INPUTS: SIGNATURE AND TEST SEQUENCE</H3>
<P> 	The DNA sequences you provided for us to analyze were contained within TWO input data files (these 
	corresponding to DNA SIGNATURE and DNA TEST SEQUENCE).  These two DNA sequences were 
	automatically merged into a global sequence datafile, which was used for internal representation 
	of the inputs. No changes to your original data were done UNLESS you explicitly specified the 
	application of a test mutation to one of the two sequences AND requested that the resultant mutation 
	be stored into the corresponding input file. However, a result of this scheme is that genomic 
	addresses shown on this SUMMARY REPORT page are SHIFTED by the (ACGT-based) size of the DNA signature.
	To find the true and exacting genomic addresses (with respect to your given DNA test sequence),
	please access the link to the DETAILED REPORT page associated with any of the resultant matching 
	instance. The DNA sequences analyzed were the following:
</P>
~A 
</BODY> </HTML> 
")
;; **************************************************************************************************************************


(defmacro HPS_HTML_TAB_OPTIONAL () "
<HTML> <HEAD> <TITLE>HPS DNA MINING SYSTEMS</TITLE> <STYLE>
<!--
@font-face {font-family:Helvetica; panose-1:2 11 6 4 2 2 2 2 2 4;}
@font-face {font-family:Courier; panose-1:2 7 4 9 2 2 5 2 4 4;}
@font-face {font-family:Times; panose-1:2 2 6 3 5 4 5 2 3 4;}
@font-face {font-family:Verdana; panose-1:2 11 6 4 3 5 4 4 2 4;}
@font-face {font-family:Garamond; panose-1:2 2 4 4 3 3 1 1 8 3;}
p.MsoNormal, li.MsoNormal, div.MsoNormal {margin:0in; margin-bottom:.0001pt; font-size:10.0pt; font-family:Verdana;}
@page Section0 {size:8.5in 11.0in; margin:0.5in 0.5in 0.5in 0.5in; font-size:10.0pt; font-family:Verdana;}
div.Section0 {page:Section0;}
-->
</STYLE> </HEAD> <BODY LANG=EN-US> 

<H3>DESCRIPTION OF THE TEST DATA</H3>
<P> 	<B> THIS </B> SECTION WAS NOT AUTO-GENERATED. It is provided for convenience of those 
	seeking to review, rate, or compare the results presented herein.  To test the HPS DNA 
	pattern miner, a DNA input sequence of roughly 20000 base pairs (bp) was constructed 
	using the 13000 bp E-COLI DNA sequence 1786520 (obtained from ENTREZ) as a starting point. 
	Said sequence was then MODIFIED as follows.  First, the DNA signature 
	was selected to be simply the bases contained within the genomic addresses 5000 TO 5999. 
	Then, to test several key conditions, various <B> damaged instances of the DNA signature </B> 
	were manufactured (as described below) and INSERTED INTO SPECIFIC LOCATIONS.
	This augmentation process is described below. AS SHOWN, ALL TEST CASES WERE CORRECTLY UNEARTHED, RATED, AND REPORTED.
</P>
<H4>LAYOUT OF TEST-CASE MATCHING INSTANCES </H4>
<TABLE CLASS=MSONORMAL ALIGN=JUSTIFY>
		   <TR> <TD> <SPAN STYLE='font-size:8.0pt;font-family:Verdana'>
		   	Between [5000:6000]
		   </SPAN> </TD> <TD> <SPAN STYLE='font-size:8.0pt;font-family:Verdana'>
		   	the <B> identical matching instance </B> of the DNA signature is found.
		   </SPAN> </TD> <TD> <SPAN STYLE='font-size:8.0pt;font-family:Verdana'>
	 	   	The original DNA test sequence 1786520 had 13480 bp.
		   </SPAN> </TD> </TR> <TR> <TD> <SPAN STYLE='font-size:8.0pt;font-family:Verdana'>
	           	Between [7000:8000] 
		   </SPAN> </TD><TD> <SPAN STYLE='font-size:8.0pt;font-family:Verdana'>
		   	a <B> heavily single-point mutated version </B> of the original DNA signature was inserted, 
			consisting of approximately pseudo-random 25 single-point mutations from A->T coupled with
			approximately 25 single-point mutations from T->A. The mutations were chosen to take place at
			particular indexes (e.g., every 10th base which happened to be A) to easily verify via 
			eye inspection of proper detection within the matching instance. Effectively, the resulting 
			matching instance had about 5% (50/1000) randomly single-point mutations which HPS algorithms
			would have to deal with in the identification of the intrinsic structure of the underlying 
			DNA signature found within. 
		   </SPAN> </TD><TD> <SPAN STYLE='font-size:8.0pt;font-family:Verdana'>
		   	This transformation increased the resultant DNA test sequence in size from 13480 to 14480.
		   </SPAN> </TD> </TR> <TR> <TD> <SPAN STYLE='font-size:8.0pt;font-family:Verdana'>
	           	Between [13481:14450] 
		   </SPAN> </TD><TD> <SPAN STYLE='font-size:8.0pt;font-family:Verdana'>
			a <B> heavily damaged version of the DNA signature </B> was inserted, which exhibited a 
			SINGLE DELETION BURST of exactly 30 base pairs at (intra) genomic address (800). To accomplish
			this, a true DNA signature match was inserted therein, but then basepairs at (DNA signature) 
			genomic addresses [800:830] were deleted. No single point mutations were inflicted onto this 
			true matching instance. 
		   </SPAN> </TD><TD> <SPAN STYLE='font-size:8.0pt;font-family:Verdana'>
			This increased the resultant DNA test sequence in size fro 14480 to 15550.
		   </SPAN> </TD> </TR> <TR> <TD> <SPAN STYLE='font-size:8.0pt;font-family:Verdana'>
	           	Between [14550:17000] 
		   </SPAN> </TD><TD> <SPAN STYLE='font-size:8.0pt;font-family:Verdana'>
			a <B> DNA filler </B> sequence extracted from a subsequence of the original DNA test sequence
			(but OUTSIDE of the DNA signature region) was inserted into the DNA test sequence.  
		   </SPAN> </TD><TD> <SPAN STYLE='font-size:8.0pt;font-family:Verdana'>
			This increased the size from 15550 to 17000.
		   </SPAN> </TD> </TR> <TR> <TD> <SPAN STYLE='font-size:8.0pt;font-family:Verdana'>
	           	Between [17000:18000] 
		   </SPAN> </TD><TD> <SPAN STYLE='font-size:8.0pt;font-family:Verdana'>
			a <B> heavily damaged version of the DNA signature </B> was inserted, which exhibited (1) a DELETION 
			BURST of exactly 30 base pairs at (intra) genomic address (300) coupled with (2) a RANDOM INSERTION 
			BURST of 30 base pairs at (intra) genomic address (800). Effectively, a true DNA signature match was 
			inserted therein, but with deleted basepairs at genomic addresses [300:330] and inserted RANDOM 
			bases at genomic addresses [800:830]. No single point mutations were inflicted onto this true 
			matching instance. 
		   </SPAN> </TD><TD> <SPAN STYLE='font-size:8.0pt;font-family:Verdana'>
			This increased the size from 17000 to 18000.
		   </SPAN> </TD> </TR> <TR> <TD> <SPAN STYLE='font-size:8.0pt;font-family:Verdana'>
	           	Between [18000:19000] 
		   </SPAN> </TD><TD> <SPAN STYLE='font-size:8.0pt;font-family:Verdana'>
			another <B> DNA filler </B> sequence extracted from a subsequence of the original DNA test sequence
			(but OUTSIDE of the DNA signature region) was inserted into the DNA test sequence.  
		   </SPAN> </TD><TD> <SPAN STYLE='font-size:8.0pt;font-family:Verdana'>
			This increased the size from 18000 to 19000.
		   </SPAN> </TD> </TR> <TR> <TD> <SPAN STYLE='font-size:8.0pt;font-family:Verdana'>
	           	Between [19000:19970] 
		   </SPAN> </TD><TD> <SPAN STYLE='font-size:8.0pt;font-family:Verdana'>
			a <B> heavily damaged version of the DNA signature </B> was inserted, which exhibited a SINGLE DELETION 
			BURST of exactly 30 base pairs at (intra) genomic address (300). Effectively, a true DNA signature 
			match was inserted therein, but with deleted basepairs at genomic addresses [300:330]. No single 
			point mutations were inflicted onto this true matching instance. 
		   </SPAN> </TD><TD> <SPAN STYLE='font-size:8.0pt;font-family:Verdana'>
			This increased the size from 19000 to 20000.
		   </SPAN> </TD> </TR> <TR> <TD> <SPAN STYLE='font-size:8.0pt;font-family:Verdana'>
	           	Between [19970:30050] 
		   </SPAN> </TD><TD> <SPAN STYLE='font-size:8.0pt;font-family:Verdana'>
			a <B> DNA filler </B> sequence extracted from SEQUENCE_1786454.DNA (10080 bp) was inserted.
		   </SPAN> </TD><TD> <SPAN STYLE='font-size:8.0pt;font-family:Verdana'>
			This increased the size from 20000 to 30000.
		   </SPAN> </TD> </TR> <TR> <TD> <SPAN STYLE='font-size:8.0pt;font-family:Verdana'>
	           	Between [24000:24975] 
		   </SPAN> </TD><TD> <SPAN STYLE='font-size:8.0pt;font-family:Verdana'>
			a <B> heavily damaged version of the DNA signature </B> was inserted, which exhibited (1) multiple DELETION 
			BURSTs of exactly 8 base pairs at every 200 (intra) genomic address (i.e., 200, 400, 600, 800) and then 
			coupled with (2) a single INSERTION BURST of 8 base pairs at (intra) genomic address (500).  No single point 
			mutations were inflicted onto this true matching instance. 
		   </SPAN> </TD><TD> <SPAN STYLE='font-size:8.0pt;font-family:Verdana'>
			This increased the size from 30000 to 31000.
		   </SPAN> </TD> </TR> <TR> <TD> <SPAN STYLE='font-size:8.0pt;font-family:Verdana'>
	           	Between [30000:30975] 
		   </SPAN> </TD><TD> <SPAN STYLE='font-size:8.0pt;font-family:Verdana'>
			a <B> heavily damaged version of the DNA signature </B> was inserted, which exhibited the above DNA damage
			coupled with several random single point mutations at periodic intervals (1 base every 50 bases, for the 
			first 500 genomic addresses). 
		   </SPAN> </TD><TD> <SPAN STYLE='font-size:8.0pt;font-family:Verdana'>
			This increased the size from 31000 to 32000.
		   </SPAN> </TD> </TR> <TR> <TD> <SPAN STYLE='font-size:8.0pt;font-family:Verdana'>
	           	Between [32002:32978] 
		   </SPAN> </TD><TD> <SPAN STYLE='font-size:8.0pt;font-family:Verdana'>
			a <B> heavily damaged version of the DNA signature </B> was inserted, which exhibited multiple DELETION 
			BURSTs of exactly 8 base pairs at every 200 (intra) genomic address (i.e., 200, 400, 600, 800).
		   </SPAN> </TD><TD> <SPAN STYLE='font-size:8.0pt;font-family:Verdana'>
			This increased the size from 32000 to 33000. Total input size (approx. 1000+33000bp).
		   </SPAN> </TD> </TR>
</TABLE>
</BODY> </HTML> 
")
;; **************************************************************************************************************************


(defmacro HPS_HTML_TAB_BEHAVIOR () "
<HTML> <HEAD> <TITLE>HPS DNA MINING SYSTEMS</TITLE> <STYLE>
<!--
@font-face {font-family:Helvetica; panose-1:2 11 6 4 2 2 2 2 2 4;}
@font-face {font-family:Courier; panose-1:2 7 4 9 2 2 5 2 4 4;}
@font-face {font-family:Times; panose-1:2 2 6 3 5 4 5 2 3 4;}
@font-face {font-family:Verdana; panose-1:2 11 6 4 3 5 4 4 2 4;}
@font-face {font-family:Garamond; panose-1:2 2 4 4 3 3 1 1 8 3;}
p.MsoNormal, li.MsoNormal, div.MsoNormal {margin:0in; margin-bottom:.0001pt; font-size:10.0pt; font-family:Verdana;}
@page Section0 {size:8.5in 11.0in; margin:0.5in 0.5in 0.5in 0.5in; font-size:10.0pt; font-family:Verdana;}
div.Section0 {page:Section0;}
-->
</STYLE> </HEAD> <BODY LANG=EN-US> 

~A

</BODY> </HTML> 
")
;; **************************************************************************************************************************



(defmacro HPS_HTML_TAB_MATCHES () "
<HTML> <HEAD> <TITLE>HPS DNA MINING SYSTEMS</TITLE> <STYLE>
<!--
@font-face {font-family:Helvetica; panose-1:2 11 6 4 2 2 2 2 2 4;}
@font-face {font-family:Courier; panose-1:2 7 4 9 2 2 5 2 4 4;}
@font-face {font-family:Times; panose-1:2 2 6 3 5 4 5 2 3 4;}
@font-face {font-family:Verdana; panose-1:2 11 6 4 3 5 4 4 2 4;}
@font-face {font-family:Garamond; panose-1:2 2 4 4 3 3 1 1 8 3;}
p.MsoNormal, li.MsoNormal, div.MsoNormal {margin:0in; margin-bottom:.0001pt; font-size:10.0pt; font-family:Verdana;}
@page Section0 {size:8.5in 11.0in; margin:0.5in 0.5in 0.5in 0.5in; font-size:10.0pt; font-family:Verdana;}
div.Section0 {page:Section0;}
-->
</STYLE> </HEAD> <BODY LANG=EN-US> 

<H3> IDENTIFIED MATCHING INSTANCES OF DNA SIGNATURE </H3>
<P>  The following figure identifies the approximate relative placement of matching instances of the DNA signature 
	found to exist within the given DNA test sequence.  To enhance viewing and close up examination of findings, 
	the resulting time plot has been divided into four close-up subintervals of approximately the same size.
	Correspondingly, from top to bottom, a time-plot panel shows each such subinterval, with the top panel 
	containing the first quarter of the span of the DNA input sequence and the bottom panel containing the 
	last quarter.  As shown, the (combined) DNA input sequence consists of the DNA signature being prefixed to 
	the DNA test sequence.  Therefore, genomic addresses shown are shifted by the size of the DNA signature.
</P> 
<H4> IDENTIFICATION OF MATCHING INSTANCES </H3>
<P> 
	Matching instances of the DNA signature are identified via a <B> SOLID BLACK ARROW. </B> Each such arrow
	is also annotated with a TEXT LABEL having the format: 
		<STRONG> \"<I>M#</I> [<I>START</I>:<I>END</I>]\"; </STRONG> where
	<B> M# </B> uniquely identifies a matching instance across all auto-generated \"HPS DNA mining systems\" reports. 
	For example, for any matching instance on the SUMMARY REPORT page, there exists a DETAILED REPORT page, 
	identified and linked by said number.
	<B> START </B> and <B> END </B> represent the starting and ending genomic addresses of the matching instance 
	as found within the (combined) DNA input sequence.  These genomic addresses are APPROXIMATE. To obtain the 
	TRUE AND EXACTING genomic address for any matching instance with respect to the given DNA test sequence,
	you MUST refer to said matching instance's DETAILED REPORT page.
	Nevertheless, the TRUE genomic address can also be estimated by simply subtracting the size of the DNA 
	signature from such approximate alignment indexes.  However, this number represents only an approximation 
	that lies within +/- a small constant.  
</P>
<TABLE WIDTH=1200 BGCOLOR=#E6F2FF ALIGN=JUSTIFY>
<TH> IDENTIFIED MATCHING INSTANCES WITHIN THE DNA INPUT SEQUENCE </TH>
<TR> <TD> <IMG SRC=DNA_MATCH_BEHAVIOR.PNG WIDTH=1200 HEIGHT=800 HSPACE=0 VSPACE=0 BORDER=0 ALIGN=JUSTIFY> </TD> </TR> <TR>
<TD AlIGN=TOP> <SPAN STYLE='font-size:9.0pt;font-family:Verdana'> <DL> 
<STRONG>TOP:</STRONG> the first  subinterval, base pairs [0         .. 1/4 (M+N)), 
<STRONG>2ND:</STRONG> the second subinterval, base pairs [1/4 (M+N) .. 2/4 (M+N)), 
<STRONG>3RD:</STRONG> the third  subinterval, base pairs [2/4 (M+N) .. 3/4 (M+N)), and
<STRONG>BOT:</STRONG> the fourth subinterval, base pairs [3/4 (M+N) .. 4/4 (M+N)).
</SPAN> </TD> </TR> </TABLE>
</BODY> </HTML>

")
;; **************************************************************************************************************************


(defmacro HPS_HTML_TAB_RECONSTRUCTION () "
<HTML> <HEAD> <TITLE>HPS DNA MINING SYSTEMS</TITLE> <STYLE>
<!--
@font-face {font-family:Helvetica; panose-1:2 11 6 4 2 2 2 2 2 4;}
@font-face {font-family:Courier; panose-1:2 7 4 9 2 2 5 2 4 4;}
@font-face {font-family:Times; panose-1:2 2 6 3 5 4 5 2 3 4;}
@font-face {font-family:Verdana; panose-1:2 11 6 4 3 5 4 4 2 4;}
@font-face {font-family:Garamond; panose-1:2 2 4 4 3 3 1 1 8 3;}
p.MsoNormal, li.MsoNormal, div.MsoNormal {margin:0in; margin-bottom:.0001pt; font-size:10.0pt; font-family:Verdana;}
@page Section0 {size:8.5in 11.0in; margin:0.5in 0.5in 0.5in 0.5in; font-size:10.0pt; font-family:Verdana;}
div.Section0 {page:Section0;}
-->
</STYLE> </HEAD> <BODY LANG=EN-US> 

<H3> RECONSTRUCTION OF BURST DAMAGED DNA (OR PHYLOGENETIC) MATCHING INSTANCES </H3>
<P>  	Matching instances unearthed by \"HPS DNA mining systems\" may actually be (mutated as well as potentially phylogenetic) 
	versions of the given DNA signature, which nevertheless retain some special intrinsic qualities associated with 
	the DNA signature.
	Typically, in such cases, special operations are used to show how, for any matching instance said to represent 
	a damaged or mutated version of the DNA signature, it is possible to (optimally) reconstruct (i.e. with few
	operations and few changes) the targeted DNA signature sequence.
</P>
<H4> RECONSTRUCTION OPERATIONS </H4>
<P> 	The minimal abstract base machine needed to perform such reconstruction consists of just two operations: 
	<B> DELETE(X) </B> and <B> INSERT(X).</B>  The <B> DELETE(X) </B> operation edits a DNA sequence (at 
	the current genomic address) by deleting  the next <B> X bases </B> from it.  The <B> INSERT(X) </B> 
	operation edits a DNA sequence (at the current genomic address) by inserting <B> X base placeholders </B> 
	into it.  A <B> DELETE/INSERT </B> operation is triggered when a damage burst of more than <B> Z bases </B> 
	is observed within a DNA sequence.  Suppose that such event is detected at genomic address <B> Y.</B> 
	Then, such triggering event will be identified by the <B> RESYNC (Y, Z)</B>  operation, which is 
	therefore always followed by either a <B> DELETE(X) </B> or <B> INSERT(X) </B> operation.
	Finally, we choose to use another couple of operations to identify the exacting genomic address 
	of any matching instance.  First, a <B> PRELIM-ALIGN(Y) </B> operation marks the preliminary 
	genomic address believed to be the start of a matching instance.
	Then, an <B> OFFSET-ALIGN(W) </B> operation identifies how far away (in number of bases, 
	in either direction) is the exacting genomic address that corresponds to the start of the 
	matching instance. Note that (as stated), both these genomic addresses refer to indexing within 
	the DNA input sequence, which consists of the DNA signature PREPENDED to the DNA test sequence).
	As stated, to obtain the true genomic address (MSTART) for the start  of a matching instance, 
	one need only to refer to the DETAILED REPORT page for said matching instance or alternatively, 
	subtract the size <B> M </B> of the DNA signature from the offset-corrected preliminary addresses. 
	That is, <B> MSTART=Y+W-M. </B>
</P>
<H4> ADJUSTED AND ORIGINAL PAIRINGS </H4>
<P> 	Finally, to differentiate between original and reconstructed cases, we refer to the pairing of 
	the DNA signature to the ORIGINAL (i.e., as found) matching instance M# from the DNA test 
	sequence to as the <B> ORIGINAL SIG-TO-M# pairing. </B> Similarly, we refer to the pairing 
	of the DNA signature to the ADJUSTED (i.e., after reconstruction operations) matching instance M# 
	to as the <B> ADJUSTED SIG-TO-M# pairing. </B>  The following figures show, for each matching 
	instance extracted from the DNA test sequence, the resultant reconstruction operations needed 
	to recover the DNA signature from said matching instance.  For this reason, each figure has 
	two parts. Whereas the top part of each figure shows the  <B> ADJUSTED SIG-TO-M# pairing </B> 
	together with the various reconstruction operations needed, the bottom part of each figure 
	shows the <B> ORIGINAL SIG-TO-M# pairing </B> of DNA signature to matching instance (as given). 
	Within any pairing plot, the DNA signature is always colored in \"light-blue\" color whereas 
	the matching instance is always colored in \"light-pink\".  
</P>
<H4> BEHAVIOR OF THE RECONSTRUCTION </H3>
<P> 	Note that when a matching instance exhibits no burst DNA damage, no reconstruction needs 
	to be apply (i.e., all DNA damage is due to single-point errors) and thus, ORIGINAL and ADJUSTED 
	pairings are the same.  Moreover, note that when a matching instance exhibits burst DNA damage, 
	while the ORIGINAL pairing shows clear cross-correspondence discrepancies between DNA signature 
	and matching instance due to the misalignment induced by such burst DNA damage, after such 
	burst DNA damage is corrected via reconstruction operations, the resultant ADJUSTED pairing 
	exhibits consistently accurate cross-correspondence.  Note however, that while burst DNA damage 
	is corrected during reconstruction, single point random errors are always left (on either pairing) 
	uncorrected (such showing in various test-cases). To this end, note that for each matching instance 
	a summary statistics tuple is also given, which shows the number of mismatched base pairs in the
	cross-correspondence check between the DNA signature and the ADJUSTED (reconstructed) matching instance.
	Such reported mismatches are due to single-point random errors plus the repair cost (in bases) of 
	reconstruction operations.
</P> 
</BODY> </HTML>
<TABLE WIDTH=1200 BGCOLOR=#E6F2FF> 
<TR> <TD ALIGN=TOP> <SPAN STYLE='font-size:9.0pt;font-family:Verdana'> <DL> <TR><TD> ~A </TD> </TR> </TABLE> 
</BODY> </HTML> 
")
;; **************************************************************************************************************************


(defmacro HPS_HTML_TAB_SIMILARITY () "
<HTML> <HEAD> <TITLE>HPS DNA MINING SYSTEMS</TITLE> <STYLE>
<!--
@font-face {font-family:Helvetica; panose-1:2 11 6 4 2 2 2 2 2 4;}
@font-face {font-family:Courier; panose-1:2 7 4 9 2 2 5 2 4 4;}
@font-face {font-family:Times; panose-1:2 2 6 3 5 4 5 2 3 4;}
@font-face {font-family:Verdana; panose-1:2 11 6 4 3 5 4 4 2 4;}
@font-face {font-family:Garamond; panose-1:2 2 4 4 3 3 1 1 8 3;}
p.MsoNormal, li.MsoNormal, div.MsoNormal {margin:0in; margin-bottom:.0001pt; font-size:10.0pt; font-family:Verdana;}
@page Section0 {size:8.5in 11.0in; margin:0.5in 0.5in 0.5in 0.5in; font-size:10.0pt; font-family:Verdana;}
div.Section0 {page:Section0;}
-->
</STYLE> </HEAD> <BODY LANG=EN-US> 

<H3> SIMILARITY PLOTS BETWEEN DNA SIGNATURE AND MATCHING INSTANCES </H3>
<P> 	Similarity plots (i.e., dot-matrix alignment plots) provide quick visual identification of the fitness of 
	the resultant alignment between the DNA signature and a given matching instance.  Note that TWO 
	similarity plots are shown on each panel, a RED line and a GREEN line.  The RED one corresponds to a similarity plot 
	between the DNA signature and the ORIGINAL DNA subsequence data that corresponds to the matching instance 
	found genomic addresses. The GREEN line represents a similarity plot between the DNA signature and the ADJUSTED DNA 
	subsequence obtained through the application of RECONSTRUCTION operations over the ORIGINAL DNA subsequence. 
	Recall that reconstruction operations are used to correct BURSTS of DNA damage found to be present within 
	the ORIGINAL DNA subsequence. Such reconstructions generate a new ADJUSTED outlook over the ORIGINAL the 
	DNA subsequence data of the matching instance, on which DNA bases or placeholders are deleted or inserted to
	maintain synchrony. Therefore, note how GREEN and RED plots DO differ in cases where a matching instance 
	exhibits BURST DNA damage. In such cases, note that the GREEN line exhibits FAITHFUL ALIGNMENT 
	(just after reconstruction operations are applied). Such case manifests, during each burst of DNA damage, 
	as a reset of the green line back to the X=Y diagonal just after a COUPLE OF INITIAL BURST-DETECTION MISSES.
	These initial mismatches are due to an initial detection cost and relate to algorithmic cost decisions.  
	That is, the cost of reconstruction operations manifest as initial short-term GREEN discrepancies that 
	always coincide with the start of large RED discrepancies and that after such initial repair cost, 
	the GREEN line depicts through the resynching of the ADJUSTED matching instance to the DNA signature, whereas 
	the RED similarity plot depicts a furtherance of the discrepancy between the ORIGINAL matching instance with
	respect to the DNA signature.  The detailed analysis of such resynching is found in the DETAILED REPORT 
	page that corresponds to said matching instance.  As stated, only <B> TRUE, OPTIMAL, and FEASIBLE matching instances </B>
	are reported. Finally, plots are generated using an IN-HOUSE distance metric designed for visual acuity, 
	which somewhat distorts the genomic address correspondence of those points lying OUTSIDE 
	the X=Y line (i.e., the discrepancies).  
</P>
<TABLE WIDTH=1200 BGCOLOR=#E6F2FF ALIGN=JUSTIFY>
<TH> \"SIMILARITY PLOTS\" DNA SIGNATURE AGAINST MATCHING INSTANCE (UNDER UNADJUSTED ALIGNMENT) </TH>
<TR> <TD> <IMG SRC=HPS_DNA_FINEGRAIN_SIG2MATCH_XY_PLOTS_FOR_M0_TO_M8.PNG 
WIDTH=1200 HEIGHT=800 HSPACE=0 VSPACE=0 BORDER=0 ALIGN=JUSTIFY> </TD> </TR>
<TABLE WIDTH=1200 BGCOLOR=#E6F2FF> <TR> <TD AlIGN=TOP> <SPAN STYLE='font-size:9.0pt;font-family:Verdana'> <DL> 
<DT> <STRONG>FROM BOTTOM LEFT TO TOP RIGHT: </STRONG>
	Similarity plots (TWO per panel).  The RED one is a similarity plot between the DNA signature and the 
	ORIGINAL DNA subsequence corresponding to the matching instance found.  The GREEN one is a similarity 
	plot between the DNA signature and the ADJUSTED DNA subsequence obtained through the application of 
	reconstruction operations.
</DT>
</DL> </SPAN> </TD> </TR> </TABLE>
</TABLE>
</BODY> </HTML>
")
;; **************************************************************************************************************************


(defmacro HPS_HTML_TAB_PHILOGRAM () "
<HTML> <HEAD> <TITLE>HPS DNA MINING SYSTEMS</TITLE> <STYLE>
<!--
@font-face {font-family:Helvetica; panose-1:2 11 6 4 2 2 2 2 2 4;}
@font-face {font-family:Courier; panose-1:2 7 4 9 2 2 5 2 4 4;}
@font-face {font-family:Times; panose-1:2 2 6 3 5 4 5 2 3 4;}
@font-face {font-family:Verdana; panose-1:2 11 6 4 3 5 4 4 2 4;}
@font-face {font-family:Garamond; panose-1:2 2 4 4 3 3 1 1 8 3;}
p.MsoNormal, li.MsoNormal, div.MsoNormal {margin:0in; margin-bottom:.0001pt; font-size:10.0pt; font-family:Verdana;}
@page Section0 {size:8.5in 11.0in; margin:0.5in 0.5in 0.5in 0.5in; font-size:10.0pt; font-family:Verdana;}
div.Section0 {page:Section0;}
-->
</STYLE> </HEAD> <BODY LANG=EN-US> 

<H3> DNA SIGNATURE-ROOTED PHYLOGRAM </H3>
<P> 	As stated, all matching instances are TRUE matching instances of the DNA signature, i.e., the limiting probability 
	of (such matching instance being a TRUE match to the DNA signature) is ONE in the formal sense (i.e., virtual 
	certainty for all practical purposes).  Therefore, the distance metrics used in this plot rather than representing 
	measurements of the probabilistic fitness of any such match represent instead abstraction measures of the DNA 
	damage that is accounted for within a matching instance. As shown above, in the reconstruction edit plots, it has 
	been determined that after such specified DNA reconstruction, all such burst DNA damage could be corrected and an 
	exacting reproduction of the original DNA signature is therefore achievable from the matching instance. 
</P>
<H4> RELATIVE EVOLUTIONARY COST METRICS </H4>
<P> 	In the following plot, points represent the exacting (x,y,z) cost-metric positioning of the given matching instance,
	The line shown traverses the set of matching instances along a sorted partial order based on the total number of 
	uncorrectable errors (as well as some other cost-metrics).  This way, the DNA signature is found at the (0,0,0) 
	coordinates and each point along the line represents a matching instance of greater number of accumulated 
	uncorrectable errors.  In actuality, the evolutionary cost-metric used to generate the plot is a three-dimensional 
	cost metric and thus this ordering is a partial order in the formal sense. As a result, the positioning (x2, y2, z2) 
	of the subsequent (partially ordered) matching instance is found to be in (and displayed as) a proportional 
	delta with respect to the previous (x1, y1, z1) coordinates.  This way, edges represent an estimate of the 
	total evolutionary 3D-cost-metric change between consecutive matching instances.  Note that the distance from 
	the (0,0,0)=DNA-SIGNATURE vertex to the actual (x,y,z) coordinate of any matching instance represents a form of 
	absolute evolutionary distance.  Similarly, the lenght of an edge between consecutive nodes simply allows computing 
	the magnitude of their internodal evolutionary distance by a straightforward application of the Phytagoras' theorem. 
</P>
<H4> PHYLOGENETIC SORTING </H4>
<P> 	Note that, in the pure sense, the following plot does NOT represent a PHYLOGENETIC TREE as edges between nodes (i.e., 
	matching instances) relate differential cost-metrics among matching instances (as well as with respect to the DNA 
	signature) as opposed to evolutionary relationships between matching instances.  Note however, that the plot can be 
	used as a STARTING POINT to derive a phylogenetic tree between the matching instances and the DNA signature. 
</P>
<TABLE WIDTH=1200 BGCOLOR=#E6F2FF ALIGN=JUSTIFY>
<TH> PHYLOGENETIC SORTING </TH>
<TR> <TD> <IMG SRC=HPS_DNA_PHYLOGENIC_TREEPLOT.PNG WIDTH=1200 HEIGHT=800 HSPACE=0 VSPACE=0 BORDER=0 ALIGN=JUSTIFY> </TD> </TR> <TR>
<TD AlIGN=TOP> <SPAN STYLE='font-size:9.0pt;font-family:Verdana'> <DL> 
<DT><STRONG>BOT</STRONG> Distance 3D metric relationship between the DNA signature and unearthed matching instances. This plot 
can be used to derive a phylogenetic tree showing the evolutionary relationship between the DNA signature and matching instances. 
</DT> </DL> </SPAN> </TD> </TR> </TABLE>
</BODY> </HTML>
")
;; **************************************************************************************************************************


(defmacro HPS_HTML_TAB_MOTIF_ID () "
<HTML> <HEAD> <TITLE>HPS DNA MINING SYSTEMS</TITLE> <STYLE>
<!--
@font-face {font-family:Helvetica; panose-1:2 11 6 4 2 2 2 2 2 4;}
@font-face {font-family:Courier; panose-1:2 7 4 9 2 2 5 2 4 4;}
@font-face {font-family:Times; panose-1:2 2 6 3 5 4 5 2 3 4;}
@font-face {font-family:Verdana; panose-1:2 11 6 4 3 5 4 4 2 4;}
@font-face {font-family:Garamond; panose-1:2 2 4 4 3 3 1 1 8 3;}
p.MsoNormal, li.MsoNormal, div.MsoNormal {margin:0in; margin-bottom:.0001pt; font-size:10.0pt; font-family:Verdana;}
@page Section0 {size:8.5in 11.0in; margin:0.5in 0.5in 0.5in 0.5in; font-size:10.0pt; font-family:Verdana;}
div.Section0 {page:Section0;}
-->
</STYLE> </HEAD> <BODY LANG=EN-US> 

<H3> SEQUENCE MOTIF FINDER </H3>
<P>	\"HPS DNA mining systems\" provide additional functions such as the <B> sequence motif 
	finder. </B> The sequence motif finder is a sequence-motif miner and visualizer 
	that allows identifying the relative frequency and location for any, some, or even all
	DNA sequence-motifs DUETS, TRIPLETS, and/or QUARTETS (i.e., 2, 3, and 4-base).  
	The sequence motif finder has linear run-time costs. For your convenience, 
   	\"HPS DNA mining systems\" applies the sequence-motif finder to the DNA signature 
	by default - unless otherwise you specified a different subsequence interval. 
</P>
<H4> SUMMARY INFORMATION FOR MOTIF DUETS, TRIPLETS, AND QUARTETS </H4>
<P> 	The figure below shows the top (most frequent) sequence-motif DUETS, TRIPLETS, 
	and QUARTETS found to lie within the specified subsequence of the DNA input sequence. 
	As stated, the full extent of the DNA signature is examined by default, although it 
	is possible to examine ANY subsequence of the <B> DNA input sequence. </B> Therefore, 
	the subsequence interval analyzed is identified in the plot by a range notation of 
	the form [#:#].  To allow closer examination, the selected interval of the DNA input 
	sequence has been automatically divided into four subintervals of similar size being 
	ordered (by increasing genomic address) from top to bottom.
	Summary data along the top part of the figure provides information 
	about the relative frequency and identity of sequence-motifs found based on top-most 
	frequency rank. Summary data is divided into four columns that corresponds (from left 
	to right) to: (1) summary data for the top 5 motif DUETS,  (2) summary data for the 
	top 5 motif TRIPLETS, (3) summary data for the top 5 motif QUARTETS, and (4) summary 
	data for the 4 DNA bases.  By default, the top 5 sequence-motifs duets, triplets, and 
	quartets are analyzed; however, this number can be specified.  Moreover, for each 
	sequence-motif, a <B> tight-bound interval </B> describes the number of <B> motif-repeats 
	</B> found for said sequence-motif.  In practicality, the lower-estimate (of the interval 
	given) represents the true number of motif-repeats that corresponds to a sequence-motif. 
	Furthermore, for each sequence-motif found subsequence, a color-code is assigned (note 
	however that colors are re-used across columns).  The motif's assigned color is used to 
	color-code the display of <B> all repeats of its corresponding sequence-motif. </B> This 
	way, each sequence-motif is shown by a stream of color-coded vertical arrows/lines placed 
	in exactly <B> one row per sequence-motif. </B> Normally, all arrow-rows for sequence-motif 
	DUETS are printed first, then all arrow-rows for TRIPLETS are printed, and finally, all 
	arrow-rows for QUARTETS are printed.  However, by default, the display of said motif-repeats 
	arrow-rows is shown <B> only for the topmost sequence-motif </B> each for DUETS (top row), 
	TRIPLETS (middle row), and QUARTETS (bottom row). The number of arrow-rows to print for 
	the resultant DUETS, TRIPLETS, AND QUARTETS motif-set can be specified and its limited only 
	by physical constraints of the plot.
</P>
<TABLE WIDTH=1200 BGCOLOR=#E6F2FF ALIGN=JUSTIFY>
<TH> SEQUENCE MOTIF FINDER </TH>
<TR> <TD> <IMG SRC=HPS_DNA_SEQUENCE_ANALYZER_PLOT.PNG WIDTH=1200 HEIGHT=800 HSPACE=0 VSPACE=0 BORDER=0 ALIGN=JUSTIFY> 
</TD> </TR> <TR> <TD AlIGN=TOP> <SPAN STYLE='font-size:9.0pt;font-family:Verdana'> <DL> 
<DT> Repeats of the highest frequency sequence-motif(s) found to lie within the specified subsequence. Default
setup displays summary (frequency and identify) data for only the TOP FIVE FREQUENCY sequence-motif DUETs, TRIPLETs, 
and QUARTETs (i.e., 2, 3, and 4 DNA-base sequences).  The relative location for each of the motif-repeats of the 
TOPMOST DUET, TRIPLET, and QUARTET is also shown by means of color-coded arrow-rows. Both the number of (2, 3, 4-base) 
motif-repeat arrow-rows to display and the number of sequence-motifs to rank can be specified. </DT>
</DL> </SPAN> </TD> </TR> </TABLE>
</BODY> </HTML>
")
;; **************************************************************************************************************************


(defmacro HPS_HTML_TAB_MOTIF_MINER () "
<HTML> <HEAD> <TITLE>HPS DNA MINING SYSTEMS</TITLE> <STYLE>
<!--
@font-face {font-family:Helvetica; panose-1:2 11 6 4 2 2 2 2 2 4;}
@font-face {font-family:Courier; panose-1:2 7 4 9 2 2 5 2 4 4;}
@font-face {font-family:Times; panose-1:2 2 6 3 5 4 5 2 3 4;}
@font-face {font-family:Verdana; panose-1:2 11 6 4 3 5 4 4 2 4;}
@font-face {font-family:Garamond; panose-1:2 2 4 4 3 3 1 1 8 3;}
p.MsoNormal, li.MsoNormal, div.MsoNormal {margin:0in; margin-bottom:.0001pt; font-size:10.0pt; font-family:Verdana;}
@page Section0 {size:8.5in 11.0in; margin:0.5in 0.5in 0.5in 0.5in; font-size:10.0pt; font-family:Verdana;}
div.Section0 {page:Section0;}
-->
</STYLE> </HEAD> <BODY LANG=EN-US> 

<H3> REGULAR-EXPRESSION MOTIF MINER </H3>
<P>	\"HPS DNA mining systems\" provide also the ability to search a given DNA sequence for instances 
	of a matching subsequence with respect to a specified <B> motif-based regular-expression </B>. 
	This regular expression applied against a sequence can be arbitrarily complex and when combined with the
	above motif finder, it is quite simple to construct.  Complex sequence motifs can be specified with
	ease by non-specialists using a simple and easy to write <B> UNIX-like syntax/grammar </B> that 
	allows the <B> specification of complex DNA patterns of repeated sequence-motifs of varying lengths. </B>
	The upcoming release of \"HPS DNA mining systems\" provides even more extensive regular-expression 
	composition, discovery, and search tools.  
</P>
<H4> SPECIFICATION OF MOTIF REGULAR EXPRESSIONS </H4>
<P> 	A motif-based regular expression is specified as a <B> sequence of (one or more) tuple-pairs constraints </B> of the 
	form <B> \"(DNA_MOTIF)[NUMBER]\" </B> where <B> (DNA_MOTIF) </B> represents a <B> (1, 2, 3, 4 DNA-base) 
	sequence-motif specifier subsequence </B>.  Valid examples of such are any of the following: 
	(ACG), (CG), (AAAA), (ATA), (T), and (C). Note that the the motif must be delimited by opening and 
	closing parenthesis. Similarly, <B> [NUMBER] </B>  represents a positive number <B> specifier of 
	the number of repeats </B> associated with the preceding sequence-motif. Valid examples of such 
	are any of the following: [1], [3], [10], [100], and [*].  Note that the unknown number of repeats 
	is specified by the special qualifier [*] and that numerical repeat qualifier must be delimited by 
	opening and closing square brackets.  This way, the following regular expression 
			<B> \"(AAT)[2](GG)[1](A)[3](C)[1](CGTA)[2)\" </B> 
	translates to a variable-length sequence-motif search against the specified sequence for the 
	first subsequence of DNA-bases that FULFILLS ALL specified tuple-pair constraints found within the
	regular expression in the exact order given and with the exact number of motif-repeats specified.
	For example, assume the symbol \"*\" represents a random DNA base, then the above specified motif-based
	regular expression would be matched against a subsequence such as the following:
		      <B> \"AAT***AAT**********GG***A*****A*A***C****CGTA****CGTA\" </B> 
	if one such like exists from within the specified interval of the DNA input sequence. The motif-based 
	regular expression finder has linear-time run-time complexity.
</P>
<H4> SUMMARY INFORMATION FOR MOTIF REGULAR EXPRESSION MINING </H4>
<P> The following figure depicts the results of a regular expression search within the DNA input sequence.
	By default, the motif-based regular expression is applied to (the full extent of) the DNA signature 
	found within the DNA input sequence. However,  any given interval-based subsequence of the DNA 
	input sequence can be examined by the motif-based regular expression miner. Moreover, one or more 
	matching instance can be extracted (if any) but by default, only the very first match from within 
	the specified interval is shown.  The plot automatically zooms in into the MAXIMUM DETAIL POSSIBLE 
	for the interval of the matching subsequence found to match all the tuple-pair constraint of the 
	motif-based regular expression. Moreover, said resultant matching interval is further divided into 
	FOUR EQUAL LENGTH CONTIGUOUS SUBINTERVALS, which are then plotted one per panel, with the first 
	of said subintervals shown at the top and correspondingly, the last subinterval at the bottom.
	The motif-based regular expression that was searched-for is printed atop of the figure while
	the precise location of each (thus) successfully met tuple-pair constraint of the motif-based 
	regular expression is shown within the exact location of the matching subinterval plot. Each DNA base
	of the matching subinterval is printed (along a single row across all subplots) and above the 
	relevant location of a precise match of a tuple-pair constraint in a subplot, the actual tuple-pair 
	constraint being met is printed (across any of three alloted tuple-pair constraint rows). 
	Note that for tuple-pair constraints specified a repeat, the tuple-pair constraint is shown
	the corresponding that many times on the relevant subinterval and subplot.  Note the precise 
	and exacting correspondance - as with any of the features of our system.
</P>
<TABLE WIDTH=1200 BGCOLOR=#E6F2FF ALIGN=JUSTIFY>
<TH> DNA MINING BASED ON MOTIF REGULAR EXPRESSIONS </TH>
<TR> <TD> <IMG SRC=HPS_DNA_REGULAR_EXPRESSION_PLOT.PNG WIDTH=1200 HEIGHT=800 HSPACE=0 VSPACE=0 BORDER=0 ALIGN=JUSTIFY> 
</TD> </TR> <TR> <TD AlIGN=TOP> <SPAN STYLE='font-size:9.0pt;font-family:Verdana'> <DL> 
<DT> Resultant matching subsequence interval associated with the successful mining of the specified motif-based 
regular expression against the specified interval of the DNA input sequence.  The motif-based regular expression 
miner handles motif-repeats of variable-length across variable gap lengths.  Any regular expression consisting of 
(1, 2, 3, 4-base) sequence-motifs can be specified.  Regular expressions follow an easy Unix-like model described 
above.  
</DT>
</DL> </SPAN> </TD> </TR> </TABLE>
</BODY> </HTML>
")
;; **************************************************************************************************************************


(defmacro HPS_HTML_TAB_ABOUT_US () "
<HTML> <HEAD> <TITLE>HPS DNA MINING SYSTEMS</TITLE> <STYLE>
<!--
@font-face {font-family:Helvetica; panose-1:2 11 6 4 2 2 2 2 2 4;}
@font-face {font-family:Courier; panose-1:2 7 4 9 2 2 5 2 4 4;}
@font-face {font-family:Times; panose-1:2 2 6 3 5 4 5 2 3 4;}
@font-face {font-family:Verdana; panose-1:2 11 6 4 3 5 4 4 2 4;}
@font-face {font-family:Garamond; panose-1:2 2 4 4 3 3 1 1 8 3;}
p.MsoNormal, li.MsoNormal, div.MsoNormal {margin:0in; margin-bottom:.0001pt; font-size:10.0pt; font-family:Verdana;}
@page Section0 {size:8.5in 11.0in; margin:0.5in 0.5in 0.5in 0.5in; font-size:10.0pt; font-family:Verdana;}
div.Section0 {page:Section0;}
-->
</STYLE> </HEAD> <BODY LANG=EN-US> 

<H3>ABOUT \"HPS DNA MINING SYSTEMS\" </H3>
<P>	\"HPS DNA mining systems\" are implemented in COMMON LISP with 100% proprietary in-house code that 
	has been designed for maintenance and extensibility.  Moreover, \"HPS DNA mining systems\" is not 
	just an application but rather an easily extensible programming environment for bioinformatic applications.  
	The HPS code base has been designed with portability in mind, with neither operating system nor compiler 
	dependencies. Use of external (non-incorporated into) and unmodified applications GNUplot <EM> (Copyright 
	1986 - 1993, 1998, 2004   Thomas Williams, Colin Kelley) </EM> and GNUwget <EM> (Copyright © 1996–2005 
	Free Software Foundation, Inc.) </EM> is made under respective licenses        
	(<A HREF=./../HPS_INPUTS/GNUPLOT.HTM> GNUPLOT license </A>) and 
	(<A HREF=./../HPS_INPUTS/GNUWGET.HTM> GNUWGET license. </A>)
</P>
<H4> HPCC SUITABILITY </H4>
<P> 	Current implementation has been tested on the Windows XP operating system under typical Pentium-class 
	personal computer power.  Even though \"HPS DNA mining systems\" are specifically designed to allow data mining 
	of large DNA databases on Pentium-class personal computers, \"HPS DNA mining systems\" are inherently suitable 
	for future deployment on HPCC's grid-computing systems through batched pre-computation and adaptive parceling 
	of key \"HPS DNA mining systems\" tasks.
</P>
</BODY> </HTML>
")
;; **************************************************************************************************************************


(defmacro HPS_HTML_TAB_DISCLAIMER () "
<HTML> <HEAD> <TITLE>HPS DNA MINING SYSTEMS</TITLE> <STYLE>
<!--
@font-face {font-family:Helvetica; panose-1:2 11 6 4 2 2 2 2 2 4;}
@font-face {font-family:Courier; panose-1:2 7 4 9 2 2 5 2 4 4;}
@font-face {font-family:Times; panose-1:2 2 6 3 5 4 5 2 3 4;}
@font-face {font-family:Verdana; panose-1:2 11 6 4 3 5 4 4 2 4;}
@font-face {font-family:Garamond; panose-1:2 2 4 4 3 3 1 1 8 3;}
p.MsoNormal, li.MsoNormal, div.MsoNormal {margin:0in; margin-bottom:.0001pt; font-size:10.0pt; font-family:Verdana;}
@page Section0 {size:8.5in 11.0in; margin:0.5in 0.5in 0.5in 0.5in; font-size:10.0pt; font-family:Verdana;}
div.Section0 {page:Section0;}
-->
</STYLE> </HEAD> <BODY LANG=EN-US> 

<H3> DISCLAIMER </H3>
<P> 	This report was automatically generated by \"HPS DNA mining systems\".  \"HPS DNA mining systems\" 
	are based on proprietary patent-pending technologies developed by Dr. Nelson R. Manohar-Alers. No permit 
	to use \"HPS DNA mining systems\" algorithms, software, technologies, disclosures, or derivatives 
	of such is granted without authorization from Dr. Nelson R. Manohar.  The research direction underlying 
	\"HPS DNA mining systems\" are a result of the unique blend, refinement, and reuse of skills (accumulated 
	by Dr.  Manohar-Alers across 20 years of professional experience) to continuously achieve an innovative 
	leaping furtherance of our initial (c. 1992) long-term-held research thread on adaptive systems.
	<B> Qualified principals 
	and/or reviewers from qualified institutions </B> may present inquiries by e-mailing the author 
	<B> (Dr. Nelson R. Manohar, Principal Research Scientist, and Principal of \"HPS DNA mining systems\")  
	</B> at the e-mail address given atop this report. Dr. Nelson R. Manohar is open to consider \"qualified 
	and appropriate\" collaboration, seeding, funding, or contract inquiries from <B> qualified principals 
	</B> from <B> reputable and qualified (US/EU) institutions.</B> However, because of the nature, volume, 
	intensity, and disclosure-level of our (environments and) research work, we reserve the right to reply 
	to any inquiry.  For example, inquiries that somehow could be related to disclosure of patent-pending 
	\"HPS DNA mining systems\" may be politely ignored.  Moreover, inquiries from corporate laboratories 
	may not be answered.
</P><P> HPS research work has entirely been self-funded and achieved under sustained and adverse malfeasance 
	conditions.  \"HPS DNA mining systems\" research work did NOT received support from either the 
	<B> National Science Foundation (NSF) </B>, the <B> National Institutes of Health (NIH)</B>, 
	or any other similar granting agencies.  It is relevantly disclosed that <B> (U.S.) FEDERAL AND 
	(N.Y., P.R.) STATE MALFEASANCES </B> have previously been documented, disclosed, and filed and such 
	relate to matters of STRONG NATIONAL INTEREST.  Please inform the Inspector General of the U.S. 
	Department of Justice (Mr. Glenn A. Fine) (at askdoj@usdoj.gov) of any \"suspected-to-be malfeasance 
	order\" interfering with inquiries to us on areas related to (\"but not limited to\") research, 
	funding, employment, collaborations, etc.  Finally, we will politely ignore inquiries from parties 
	suspected to have compromised, malfeasance, or political interests -- proper outlets for such have 
	been given.  Dr. Nelson R. Manohar-Alers is a U.S. Citizen.
</P><P> This webpage is best seen WITHOUT font/color substitution at <B> 1200x800 </B> pixel resolution (e.g., 
	the WIDESCREEN/LANDSCAPE format typically used in laptop displays).  This webpage is also designed to 
	fit and print on PORTRAIT format as long as font override is NOT specified.  Therefore, for best 
	viewing and printing, you should have the <B> \"Verdana\" </B> font installed in your system.  Plots 
	found within use the PNG (Portable Graphics) format and are generated at the relatively high (1200x800) 
	pixel resolution. This resolution allows for reasonable magnification detail in most image editors 
	or modern web browsers. Some links on our reports are (by default) set to open as a new browser window 
	or browser tab; therefore, check that your computer does not mistake such opening window or tab as an apparent 
	popup window --- it is not.  This webpage or site NEITHER contains scripts NOR active code.  Display of 
	this webpage has been tested for the Opera browser on a Windows XP platform.  We value your copyrights, 
	please respect ours.
</BODY> </HTML> 
")
;; ***********************************************************************************************************************

