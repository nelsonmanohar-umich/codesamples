43c43
< APPLY_THESE = [14, 15, 24] + [29, 31, 33]
---
> APPLY_THESE = [10, 13, 14, 15] + [25, ]
59,60c59,60
< TARGET_VAR, LABEL_VAR, ID_VAR = 'Survived', 'LABEL', 'PassengerId'
< TARGET_COVAR = ''
---
> TARGET_VAR, LABEL_VAR, ID_VAR = 'label', 'xxx', 'Id'
> TARGET_COVAR = 'xxx'
62c62
< FIRST_NROWS = 5000
---
> FIRST_NROWS = 80000
64c64
<     FIRST_NROWS = 900
---
>     FIRST_NROWS = 43000
66c66
< MULTICLASS = False
---
> MULTICLASS = True
217d216
< CENTROID_APPROACH = "medoid"
222c221
< MIN_VALID_CLUSTER_SIZE = 24
---
> MIN_VALID_CLUSTER_SIZE = 12
228c227
< C1_OVERSAMPLING = 2.00
---
> C1_OVERSAMPLING = 1.00
230c229
< C1_WITH_REPLACEMENT = True
---
> C1_WITH_REPLACEMENT = False
