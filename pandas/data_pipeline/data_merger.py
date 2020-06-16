# ###########################################################################
NA = "NA"
NA = ""
MT = "-1"
DO_EVAL = False
DEBUG = False
# ###########################################################################


# ###########################################################################
if 1:
    train = 'DATA/train.csv'
    test = 'DATA/test.csv'
    in_separator = ","
    out_separator = "|"
# ###########################################################################


import pandas
training_file = pandas.read_csv(train, sep=in_separator)
test_file = pandas.read_csv(test, sep=in_separator)
test_file['label'] = MT
test_file = test_file[training_file.columns]
merged_file = pandas.concat([training_file, test_file])
merged_file.to_csv('merged.csv', index=False, header=True, sep=out_separator)
