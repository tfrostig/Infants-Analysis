## Documentation: 
The repository contains anything required to replicate the results of the paper “Exploration in the presence of mother in typically and non-typically developing pre-walking human infants”. 
## Infants Data: 
Each infants has a directory named according to him. At each such directory there are 3 sub directories: 
1.	Movement – Contains the coordinate of the infant in the relevant duration. 
2.	Notation – Manual notation of the infant behaviour (touching the mother, eye contact, etc)
3.	Objects – Contains the CSV files with the coordinates of the objects in the room. 
## Input: 
Contains the Parameters Table file, hold the parameters used by the SEE program (MovingMedCutOffValue, HalfWindowWidth, Arrests) to smooth each child, as well as general parameters relevant to each infant (IS.TD, Begin and End). 
## R Scripts: 
1.	Step 1 – Step 1 – RSEE and object lists
Conducts the smoothing using RSEE, creates the following files: 
a.	Center_list – A list contains the center of all the objects for each infant
b.	Object_list – A list contains the coordinates of all objects for each infants 
c.	Smooth_see_list – A list of all smoothed coordinates returned from smooth.see function of package RSEE 
2.	Step 2 – RSEE and object lists 
Analyses the data, creates the following files: 
a.	An HTML file named Step 2 – Analysis with the complete figures and statistical analysis as appeared in the paper 
b.	A directory named Output/Images with all figures as standalones. 
3.	Functions – Functions used throughout the analysis 
4.	Plotting Fucntions – Functions used for plotting and preprocessing 
5.	Package required – Downloads required packages and loading them 
6.	RSEE_smoothing – Wrapper for RSEE for each infants extracting relevant information 


## To reproduce the results: 
Reproducing figures:
If the sole interest is in the figures and statistical analysis run only Step 2 – Analysis. 
Reproducing full paper:
This includes using RSEE for smoothing the raw data. 
1.	Run script Step 1 – RSEE (From project, required R version <3.4.2). 
2.	Run rMarkdown script Step 2 – Analysis. 
For any questions: tfrostig@gmail.com 
