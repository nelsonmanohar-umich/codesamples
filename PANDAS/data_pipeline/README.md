
An configurable and feature and modeling self-exploratory ML data pipeline.  

It showcases many of my approaches and tricks in modeling and towards the 
development of predictive features. It has extensive data cleaning, autonomous
feture exploration and feature selection.

It was developed by me back in 2014. By now, probably all these tricks are
now in the mainstream of pandas and scikit-learn.


It is designed for configurable emsembled predictors, do correction of 
probabilities, autonomously do feature selection and explore features,
has hooks for incorporating your own feature builders, and much more.

It is designed for multi-label classification.

It was used in kaggle competitions and does ok unless the government 
seeks to discredit you. Ha ha ha. Seriously, now. I have tried on a
half dozen kaggle multi-label classification problems (competitions
and toy ones).

To run it, do
gunzip DATA/merged.csv.gz
gunzip DATA/train.csv.gz
gunzip DATA/test.csv.gz
python3 ensemble_classifier_predictor_pipeline.py > run.out

output is VERY detailed and extensive.
run.out
output

configuration is here:
pipeline_configuration.py

user specification hookups are here:
definitions.py

ensemble classifier componenet fine-tuning:
classifier_configuration.py

data folder:
DATA

Documentation:
PipelineIII.pdf
PipelineIII.odp
Autonomous Predictive Pipeline Architecture.odg

Modules for Support of Autononous Feature and Model refinement
differentiator.py
step_fitter.py
reductionism.py
dataset_reduction.py
factor_selector.py

The stuff that goes underneath:
data_merger.py
feature_selected_vars.py
differences_pipeline_configuration.py
definitions.py
contributors.txt
retrofit.sh
definitions.py
multiclass.py
optimal_factor_cuts.py
original_pipeline_configuration.py

Pipeline core functions:
feature_plot.py
feature_generators.py
pipeline_functions.py
pipeline_configuration.py
ensemble_classifier_predictor_pipeline.py

OUTPUT and support of submission files 
prepare_multiclass_submission.R
prepare_submission.R
submission files
yp_Mon_09pm.csv
