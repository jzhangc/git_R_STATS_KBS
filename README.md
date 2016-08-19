# git_R_STATS_KBS
RBioplot (former: frogplots) package for fully automated statistical analysis and data visualization.

It is awesome.

Change Log

0.3.0 (beta - more things to be added)

- It is now possible to change the width of the errorbar with the argument errorbarWidth. Applicable functions: rbioplot(), rbioplot_curve().

		errorbarWidth: Set the width for errorbar. Defualt is 0.2.

- it is now possible to change the size of the symbols for curve plots with the argument symbolSize. Application function: rbioplot_curve().

		symbolSize: Set the size of symbols. Default is 2.


0.2.5


- RBioplot now supports heatmap with the new function rbioplot_heatmap().

- Bug fixes.



0.2.4

- It is now possible to use standard deviation (SD) as error bar via the new argument:	errorbar

		errorbar: Set the type of errorbar. Options are standard error of mean ("SEM"), or standard deviation ("SD"). Default is "SEM".

	Functions with errorbar argument are: rbioplot(), rbioplot_curve(), autorange_bar_y(), autorange_curve()


- It is now possible to change the font type for the graphs via the new argument: fontType

		fontType: The type of font in the figure. Default is "sans". For all options please refer to R font table, which is avaiable on the website: http://kenstoreylab.com/?page_id=69

	Functions with fontType argument are: rbioplot(), rbioplot_curve()



- It is now possible to change the font size for ticks via the new arguments: xTickLblSize, yTickLblSize

		xTickLblSize: Font size of x axis ticks. Default is 10.
		yTickLblSize: Font size of x axis ticks. Default is 10.

	Functions with fontType argument are: rbioplot(), rbioplot_curve()


- It is now possible to set tick font to italic via the new arguments: xTickItalic, yTickItalic

		xTickItalic: Set x axis tick font to italic. Default is FALSE.
		yTickItalic: Set y axis tick font to italic. Default is FALSE.

	Functions with fontType argument are: rbioplot(), rbioplot_curve()

- Bug fixes.


0.2.3

The program has a new name: RBioplot.

Along with the necessary name change for some functions, some minor new features have also been added to the existing functions. 

- Function name change (old -> new)	*Note that the usage of those functions are unchanged*

		frogstats -> rbiostats
		frogplots -> rbioplot
		frogplots_curve -> rbioplot_cruve
	
- New features and changes
	
		rbioplot_curve(): 
			- the function now detects the number of end numbers (i.e., if the data has replicates or not) and determines if error bar is needed for plotting.
			- the function now automatically assigns different line patterns for each experimental group. 

		autorange_curve():
			-consistent with the changes to rbioplot_curve, necessary adjustments have been made to make sure proper space management of the graph

- Bug fixes.

- Other
	
		The contact email address specific to the program is changed to jzhangcad@gmail.com. (Please note that this will not affect my current Gmail address, i.e., I am NOT switching my main email address to this)




0.2.2

- Bug fixes.



0.2.1

- Bug fixes.



0.2.0

- New functions:
 
		frogplots_curve(): A simple to use function for plotting joining-point curves with continuous x and y axises values.
 
		autorange_bar_y(): A function to get custom lower/upper limit, major tick range, as well as minor tick options for y axis, based on a user-defined major tick number.

		autorange_curve(): A function to get custom lower/upper limit, major tick range, as well as minor tick options for both axises of a joint-piont curve with continuous x AND y values, based on a user-defined major tick number.
 
		nat_dvsr(): A simple to use function to find all divisors for an integer number.
 
		all_dvsr(): A function to find all divisors for any types of number.
 
 
- Changes to existing functions:
 
	frogstats(): 
 
		1. Bartlett test has been added to test if the data have equal variance among groups - p > 0.05 means the variance is equal. Joining the S-W normality test, the Bartlett test is a default test that will be conducted regardless of the stats test type. Make sure the data meet the equal variance criterion as both conventional t-test and ANOVA (with all the post-hoc tests) assume the data is (1) normally distributed and (2) have equal variance. The results are included in the output .txt file.
 
		2. t-test has been adjusted to “student’s t-test” (from “Welch's t-test”) to accurately reflect the equal variance aspect.
 
	frogplots():
 
		1. New arguments have added to allow user-defined y axis range, y axis major tick range and minor tick patterns:
 
			- y_custom_tick_range: To initiate setting the custom y_upper_limit, y_lower_limit, y_major_tick_range, y_n_minor_ticks. Default is FALSE.
 
			- y_lower_limit: Can only be set when y_custom_tick_range = TRUE. Set custom lower limt for y axis. Default is 0. Value can be obtained from the new function autorange_bar_y.
 
			- y_upper_limit: Can only be set when y_custom_tick_range = TRUE. Set custom upper limt for y axis. Value can be obtained from the new function autorange_bar_y.
 
			- y_major_tick_range: Can only be set when y_custom_tick_range = TRUE. Set custom major tick range for y axis. Value can be obtained from the new function autorange_bar_y.
 
			- y_n_minor_ticks: Can only be set when y_custom_tick_range = TRUE. Set custom numbers of minor ticks. Default is 4. Value can be obtained from the new function autorange_bar_y.
 	
 
		2. t-test within the function has been adjusted to “student’s t-test” from “Welch t-test” to reflect equal variance requirement.

		3. Plot and axis titles are now bold by default. 
 
		4. Small fixes, etc. 

- Bug fixes.


0.1.0
	Initial release
