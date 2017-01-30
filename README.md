# BounderR
Analysis of filopodia dynamics

This is a suite of scripts for R containing analysis tools for quantifying dynamic 
behaviour of filopodia, using data extracted from timelapse microscopy videos with the Fiji plugin CAD-Bounder.  

'Masterscript' defines the filepaths of the data directories, and the filepath location of the scripts comprising the analysis suite ('modules'). It then iterates the import and processing scripts (see below) over the defined data folders, storing all results in a single list object ('metalist') in a format that allows fast graphing of results with Script 3.

Scripts 1 and 1-1 import data and initial processing, e.g. normalisation. Variables relating to image file properties such as pixel width  and timepoint interval may need adjusting at the top of the script.

Script 2 performs additional data processing, extracting new metrics from the measurements for each structure over time.

Script 3 draws graphs (boxplots and CDF curves) comparing different properties of filopodia across different input folders. It requires scripts 'GraphingTemplates.R' and 'ColourSchemes.R'. 

More information available from: vu203@cam.ac.uk. 
