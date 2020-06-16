# #########################################################################
import numpy
from pipeline_configuration import NA_RECODE
from pipeline_configuration import TARGET_VAR
# #########################################################################


# #########################################################################
DATASET_NAME = "digits"
# #########################################################################


# #########################################################################
YEARS = ["%02d" % x for x in range(0, 20)]
# ---------------------------------------------------------------------------
MONTHS = ["JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"]
MONTHS = dict(list(zip(MONTHS, MONTHS)))
# ---------------------------------------------------------------------------
DAYS = ["%02d" % x for x in range(0, 31)]
# #########################################################################


# #########################################################################
TITLES = \
    {}
# #########################################################################


# #########################################################################
ROLES = \
    {}
# #########################################################################


# #########################################################################
CITIES = \
    {}
# #########################################################################


# #########################################################################
BRANCHES = \
    {}
# #########################################################################


# #########################################################################
DATE_COLS = {'Dates': 1}
# #########################################################################


# #########################################################################
SPECIAL_COLS = ['Dates', 'Category', 'Descript', 'DayOfWeek', 'PdDistrict', 'Resolution', 'Address', 'X', 'Y', 'Id']
SPECIAL_COLS = dict(list(zip(SPECIAL_COLS, list(range(len(SPECIAL_COLS))))))
# #########################################################################


# #########################################################################
def get_facet_sum(token, dictionary, quantize=True):
    token = str(token)
    token = token.replace(", ", "")
    token = token.replace("(", "")
    token = token.replace(")", "")
    token = token.replace('"', "")
    facets = [dictionary[x]
              if x in dictionary else NA_RECODE for x in token.split()]
    if quantize:
        facets = numpy.log([max(x+2, 1) for x in facets])
    retval = numpy.round(numpy.sum(facets), 2)
    return retval
# #########################################################################


# #########################################################################
def get_clean_token(x, debug=False):
    if debug:
        print(x, end=' ')
    x = str(x)
    x = x.strip()
    x = x.replace('0', '')
    x = x.replace('Block of', '')
    x = x.replace('Block', '')
    x = x.replace(' ', '')
    if debug:
        print(x)
    return x
# #########################################################################


# #########################################################################
def get_marginal_score(x, y, from_dict, sep="|", debug=False):
    keyval = "%s%s%s" % (x, sep, y)
    if debug:
        print(keyval)
    if keyval in from_dict:
        return from_dict[keyval]
    else:
        return NA_RECODE
# #########################################################################


# #########################################################################
def get_history_for(x, y, ktype, xy_historical_dictionary,
                    dval=0, debug=False):
    hkey = "%s|%s" % (x, y)
    if hkey in xy_historical_dictionary:
        if debug:
            print("%32s : %s" % (hkey, xy_historical_dictionary[hkey]))
        if ktype != "P":
            return xy_historical_dictionary[hkey][0]
        else:
            return float(xy_historical_dictionary[hkey][1])
    return dval
# #########################################################################


# #########################################################################
def augment_data(data, m=14, n=28):
    imptpts = [386, 358, 414, 350, 539, 511, 413, 567, 385, 568, 330, 427, 514, 455, 542, 483, 596, 540, 428, 441, 510, 400, 541, 538, 378, 515, 482, 512, 442, 357, 359, 399, 513, 543, 387, 456, 595, 454, 569, 597, 484, 372, 566, 469]
    corners = [172, 176, 200, 229, 204, 257, 191, 106, 217, 330, 372, 399, 510, 536, 678, 675, 637, 583, 552]
    centers = [606, 377, 384, 488, 206, 461, 464, 263, 268, 437, 325]
    keypnts = [378, 437, 409, 377, 155, 460, 210, 378]
    lr = [659, 633, 774, 580, 749, 666]
    ul = [231, 250, 233, 255, 342, 312]

    def points_wrt(x):
        def valid(x):
            if x < 0 or x > 783:
                return False
            return True

        def above(x):
            s = []
            if x not in list(range(0, 28)):
                s = [p for p in (x-28*1-1, x-28*1-0, x-28*1+1) if valid(p)]
            print('above', s)
            return s

        def below(x):
            s = []
            if x not in list(range(756, 784)):
                s = [p for p in (x+28*1-1, x+28*1-0, x+28*1+1) if valid(p)]
            print('below', s)
            return s

        def left(x):
            s = []
            if x not in list(range(0, 784, 28)):
                s = [p for p in (x-28*1+1, x-28*0+1, x+28*1+1) if valid(p)]
            print('left', s)
            return s

        def rigth(x):
            s = []
            if x not in list(range(27, 784, 28)):
                s = [p for p in (x-28*1-1, x-28*0-1, x+28*1-1) if valid(p)]
            print('right', s)
            return s

        pnts = sorted(set([x, ] + above(x) + below(x) + left(x) + rigth(x)))
        print(x, pnts)
        print('-' * 80)
        return pnts[:]

    def get_pointset(points, data):
        final_set = []
        for p in points:
            final_set.extend(points_wrt(p))
        final_set = sorted(set(final_set))
        points = ["pixel%s" % x for x in final_set]
        points = [x for x in points if x in data]
        return points

    def augment_wrt_pointset(data, pointset, stem="", parts=2, std=False):
        w = len(pointset)
        gap = int(w/parts)
        steps = list(range(gap, w, gap))
        for idx in steps:
            if std:
                col = "%s_%s_%s_of_%s" % (stem, "std", int(idx/gap), parts)
                data[col] = data[pointset[idx:idx+gap]].std(axis=1)
            col = "%s_%s_%s_of_%s" % (stem, "sum", int(idx/gap), parts)
            data[col] = data[pointset[idx:idx+gap]].sum(axis=1)
            # colname = "%s_%s_%s_of_%s_sqrd" % (stem, "sum", int(idx/gap), parts)
            # data[colname] = data[col] * data[col]
            data[col] = data[col] * data[col]

        if std:
            col = "%s_%s_%s" % (stem, "std", "tot")
            data[col] = data[pointset[idx:idx+gap]].std(axis=1)
        col = "%s_%s_%s" % (stem, "sum", "tot")
        data[col] = data[pointset[idx:idx+gap]].sum(axis=1)
        # colname = "%s_%s_%s_sqrd" % (stem, "sum", "tot")
        # data[colname] = data[col]*data[col]
        data[col] = data[col]*data[col]
        return data

    for col in data:
        if TARGET_VAR not in col:
            data[col] = (data[col]/4.).astype('int')

    rows, columns = {}, {}
    for i in range(28):
        rows[i] = list(range(i*28, (i+1)*28))
        pnts = ["pixel%s" % x for x in rows[i]]
        data = augment_wrt_pointset(data, pnts, stem="row_%s" % i, parts=5)
        data = augment_wrt_pointset(data, pnts, stem="row_%s" % i, parts=3)

        columns[i] = list(range(i, 784, 28))
        pnts = ["pixel%s" % x for x in columns[i]]
        data = augment_wrt_pointset(data, pnts, stem="col_%s" % i, parts=5)
        data = augment_wrt_pointset(data, pnts, stem="col_%s" % i, parts=3)

    imptpts = get_pointset(imptpts, data)
    corners = get_pointset(corners, data)
    centers = get_pointset(centers, data)
    lr = get_pointset(lr, data)
    ul = get_pointset(ul, data)
    keypnts = get_pointset(keypnts, data)

    data = augment_wrt_pointset(data, imptpts, stem="imptpnts", parts=5)
    data = augment_wrt_pointset(data, imptpts, stem="imptpnts", parts=3)
    data = augment_wrt_pointset(data, corners, stem="corners", parts=3)
    data = augment_wrt_pointset(data, centers, stem="centers", parts=3)
    data = augment_wrt_pointset(data, keypnts, stem="keypnts", parts=5)
    data = augment_wrt_pointset(data, ul, stem="ul", parts=3)
    data = augment_wrt_pointset(data, lr, stem="lr", parts=3)

    print(data.describe(include='all'))
    return data
# #########################################################################


# #########################################################################
if __name__ == "__main__":
    rows, columns = {}, {}
    for i in range(28):
        rows[i] = list(range(i*28, (i+1)*28))
        columns[i] = list(range(i, 784, 28))
        print(rows[i])
        print(columns[i])


# #########################################################################
#   0    1    2    3    4    5    6    7    8    9   10   11   12   13   14   15   16   17   18   19   20   21   22   23   24   25   26   27
#  28   29   30   31   32   33   34   35   36   37   38   39   40   41   42   43   44   45   46   47   48   49   50   51   52   53   54   55
#  56   57   58   59   60   61   62   63   64   65   66   67   68   69   70   71   72   73   74   75   76   77   78   79   80   81   82   83
#  84   85   86   87   88   89   90   91   92   93   94   95   96   97   98   99  100  101  102  103  104  105  106  107  108  109  110  111
# 112  113  114  115  116  117  118  119  120  121  122  123  124  125  126  127  128  129  130  131  132  133  134  135  136  137  138  139
# 140  141  142  143  144  145  146  147  148  149  150  151  152  153  154  155  156  157  158  159  160  161  162  163  164  165  166  167
# 168  169  170  171  172  173  174  175  176  177  178  179  180  181  182  183  184  185  186  187  188  189  190  191  192  193  194  195
# 196  197  198  199  200  201  202  203  204  205  206  207  208  209  210  211  212  213  214  215  216  217  218  219  220  221  222  223
# 224  225  226  227  228  229  230  231  232  233  234  235  236  237  238  239  240  241  242  243  244  245  246  247  248  249  250  251
# 252  253  254  255  256  257  258  259  260  261  262  263  264  265  266  267  268  269  270  271  272  273  274  275  276  277  278  279
# 280  281  282  283  284  285  286  287  288  289  290  291  292  293  294  295  296  297  298  299  300  301  302  303  304  305  306  307
# 308  309  310  311  312  313  314  315  316  317  318  319  320  321  322  323  324  325  326  327  328  329  330  331  332  333  334  335
# 336  337  338  339  340  341  342  343  344  345  346  347  348  349  350  351  352  353  354  355  356  357  358  359  360  361  362  363
# 364  365  366  367  368  369  370  371  372  373  374  375  376  377  378  379  380  381  382  383  384  385  386  387  388  389  390  391
# 392  393  394  395  396  397  398  399  400  401  402  403  404  405  406  407  408  409  410  411  412  413  414  415  416  417  418  419
# 420  421  422  423  424  425  426  427  428  429  430  431  432  433  434  435  436  437  438  439  440  441  442  443  444  445  446  447
# 448  449  450  451  452  453  454  455  456  457  458  459  460  461  462  463  464  465  466  467  468  469  470  471  472  473  474  475
# 476  477  478  479  480  481  482  483  484  485  486  487  488  489  490  491  492  493  494  495  496  497  498  499  500  501  502  503
# 504  505  506  507  508  509  510  511  512  513  514  515  516  517  518  519  520  521  522  523  524  525  526  527  528  529  530  531
# 532  533  534  535  536  537  538  539  540  541  542  543  544  545  546  547  548  549  550  551  552  553  554  555  556  557  558  559
# 560  561  562  563  564  565  566  567  568  569  570  571  572  573  574  575  576  577  578  579  580  581  582  583  584  585  586  587
# 588  589  590  591  592  593  594  595  596  597  598  599  600  601  602  603  604  605  606  607  608  609  610  611  612  613  614  615
# 616  617  618  619  620  621  622  623  624  625  626  627  628  629  630  631  632  633  634  635  636  637  638  639  640  641  642  643
# 644  645  646  647  648  649  650  651  652  653  654  655  656  657  658  659  660  661  662  663  664  665  666  667  668  669  670  671
# 672  673  674  675  676  677  678  679  680  681  682  683  684  685  686  687  688  689  690  691  692  693  694  695  696  697  698  699
# 700  701  702  703  704  705  706  707  708  709  710  711  712  713  714  715  716  717  718  719  720  721  722  723  724  725  726  727
# 728  729  730  731  732  733  734  735  736  737  738  739  740  741  742  743  744  745  746  747  748  749  750  751  752  753  754  755
# 756  757  758  759  760  761  762  763  764  765  766  767  768  769  770  771  772  773  774  775  776  777  778  779  780  781  782  783
# #########################################################################

