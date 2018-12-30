# The reciprocal effect of value and arousal on conscious awareness in binocular rivalry task

The effect of value and arousal on dominance in conscious visual awareness was tested in a series of three binocular rivalry experiments summarized here:
* **Experiment 1** - Celebrities Faces
* **Experiment 2** - Politicians Faces
* **Experiment 3** - IAPS images (international affective picture system; Lang et al., 1997) 

## Authors

* **Tom Salomon**, *Department of Neurobiology, Tel Aviv University*
* **Yael Solar Priel**, *Department of Neurobiology, Sagol School of Neuroscience, School of Psychological Sciences, Tel Aviv University* 
* **Yarden Shir**, *Sagol School of Neuroscience, Tel Aviv University*
* **Keren Shoval**, *Sagol School of Neuroscience, Tel Aviv University*
* **Rinat Kiperman**, *Sagol School of Neuroscience, Tel Aviv University*
* **Liad Mudrik**, *Sagol School of Neuroscience, School of Psychological Sciences, Tel Aviv University*
* **Tom Schonberg**, *Sagol School of Neuroscience, Department of Neurobiology, Tel Aviv University*

## In This Repository

### BR Experiment directories

In each experiment directory you will find 3 folder:
* BR_Data - Raw data from the binocular rivalry task
* Behavioral_Data - Output from non-BR tasks, such as subjective value evaluation tasks
* tmp_data - BR_Data read into MATLAB variables for fast and easy access.

#### Missing data notes - Experiment 2 (Politicians)
Some data files are missing from the BR_Politicians repository due to incompletion of the full task
* Subject 034 - has no Subjective Arousal data file
* Subjects: 056, 058 - has no Subjective Arousal and no subjective value data file
* Subjects: 062, 068 - stopped before the experiment ended (no data)
* Subjects: 052, 053, 057 - experiment BR_Politicians - had technical difficulties and were not run (no data)

### Analysis directory
Here are the codes used to process the raw data, perform the main analysis (MATLAB) and some additional regression analysis (R)

#### Pre-processing Codes:
* Script1_merge_BR_data.m - Read raw BR data, merge into MATLAB temporary files and merge data for all subjects together into one table (saved in sub-directory 'processed data'). *This script was already run in the current repository*.
* Script2_SubjectsValidity.m - Read merged data table created by script 1, generate lists of valid and invalid subject to be used by following codes (saved in sub-directory 'processed data'). *This script was already run in the current repository*.

#### Analysis Codes:
* Script3_personal_details_and_familiarity_exclusion.m - Descriptive statistics of the age and gender of valid subjects and exclusion criteria used to remove invalid subjects from following analyses.
* Script4_BR_IAPS_permutation_analysis.m - Main statistical analysis. Examine dominance of high-value and/or high-arousal stimuli in the BR task.
* Script4_v2_BR_IAPS_permutation_analysis_with_subjective_ratings.m - Same as script4, only removing trials in experiment 3 where subjective ratings did not match the experimental manipulation.
* Script5_Exp3_power_analysis.m - Power analysis used to determine the minimal n for Experiment 3 (IAPS) using Experiment 2 (Politicians) data.
* Script6_subjective_ratings_descriptive.m - descriptive statistic for the subjective rating tasks in Experiment 3.
* BR_subjective_ratings_regression_analysis.R - Exploratory regression analyses accounting for subjective ratings of value and/or arousal.

#### Dependencies
* sign_flip_permutation_test.m - permutation test code.

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details.
You are welcome to use the data and scripts published here for any purpose. Please give appropriate academic credit to this repository by citing the manuscript associated with it: (in prep, will soon be published).

## Contact information

Feel free to contact me (tomsalomon@mail.tau.ac.il) with any question or suggestion you have regarding the data and codes shared here

