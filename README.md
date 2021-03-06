# git_R_STATS_KBS
RBioplot (former: frogplots) package for fully automated statistical analysis and data visualization.

It is awesome.

To cite in publication
  
    Zhang J, Storey KB. (2016) RBioplot: an easy-to-use R pipeline for automated statistical analysis and data visualization in molecular biology and biochemistry. PeerJ 4:e2436.


Installation

  - Install devtools (if not already done)
  
        install.packages("devtools")
    
  - Install stable release
  
        devtools::install_github("jzhangc/git_R_STATS_KBS/package/rbioplot")
        
  - Install development build
  
        devtools::install_github("jzhangc/git_R_STATS_KBS/package/rbioplot", ref = "beta")   


Change Log

    0.5.5 (5.10.2021)
      - Updates to plotting functions
        - All plotting functions now accepts data.frames
        - rbioplot() now will automatically adjust figures for data only have one variable
        

    0.5.4 (5.6.2021) 
       - General fix
        - namespace updated with foreach pacakge
        
      
    0.5.3 (11.11.2019)
      - new functions to stats model
        - rbiostats_normal(): stand-alone Shapiro-Wilk normality test function
      
      - Updates to shiny apps
        - rbioplot_app(): y_upper_limit now functional
        - rbioplot_app(): colourpicker pacakge now imports updateColourInput function (due to: updateColourInput moved from Shiny to colourpicker)
      
      
    0.5.2 (5.1.2018)
      - multi_plot_shared_legend() added in preparation for multi-panel figure functionality: share one set of legend between multiple plots
      - Shared functions added to the sharedfunctions.R file
      

    0.5.1
      - rightside y-axis now a seperate function: rightside_y()
      - Bug fixes
      

    0.5.0
      - Shiny app interface overhaul with navigation bar at top
      - Manual colour change added for rbioplot_app()
      - Reset bar colour button added for rbioplot_app()
      - Control panel items re-arranged for all shiny apps for a clearer presentation
      - Output file suffix for rbioplot() and rbioplot_app() changed to ".bar.pdf"
      - Bar graph outline colour customizable for rbioplot() via argument outlineCol
      - Axes tick labels now can set as bold for the plotting functions via arguments xTickBold and yTickBold
      - Axes tick labels now can set as bold for all plotting Shiny apps
      - Intermediate function set_hue() added
      - Bug fixes
      
    
    0.4.3
      - Font size for axis labels now customizable for all plotting functions via arguments xLabelSize, yLabelSize
      - Font size for axis labels now customizable for all plotting Shiny apps
      - Font size for axis title now customizable for all plotting functions vis argument TitleSize
      - Font size for axis title now customizable for all plotting Shiny apps
      - Font size for legend now customizable for all plotting functions via argument legendSize
      - Font size for legend now customizable for all plotting Shiny apps
      - Font size for legend title now customizable for all plotting functions via argument legendTtlSize
      - Font size for legend title now customizable for all plotting Shiny apps
      - Bar graph now can set as grey scale or automatic colour for rbioplot() via argument greyScale
      - Bar graph now can set as grey scale or automatic colour for rbioplot_app()
      - Bar graph outline colour customizable for rbioplot_app()
      

    0.4.2
      - Remote deployment concept for rbioplot_app() added
      

    0.4.1
      - The default position for plot titles set to "centre" for the web apps
      - Typo fixes
      - Bug fixes
      

    0.4.0
      - Web app version of the main functions added. To launch the app in the default web browser, run the corresponding app function (see below) in the R console:
        - rbiostats_app()
        - rbioplot_app()
        - rbioplot_curve_app()
        - rbioplot_heatmap_app()
        
      - minor_tick() separeted from the main functions
      - rbioplot(): errorbar label size now can be set via argument errorbarLblSize
      - rbioplot_heatmap(): space between x-axis tick marks and the plot now can be set via arugment xSpace
      - Bug fixes
      
      
    0.3.8
      - the case sensitive requirement for the “errorbar" argument of rbioplot(), rbioplot_curve(), autorange_bar_y() and autorange_curve() are now removed
      - the case sensitive requirement for the “Tp" argument of rbiostats(), rbioplot() and rbioplot_heatmap() are now removed.
      - display messages are added to rbiostats(), rbioplot(), rbioplot_curve() and rbioplot_heatmap()
      
      - the suffix of the output file names has been changed:
        - rbioplot(): .histogram.csv, .histogram.pdf
        - rbioplot_curve(): .curve.csv, .curve.pdf
        - rbioplot_heatmap(): .heatmap.csv, .heatmap.pdf
          
      - editorial changes in documentation
      
      - zzz.R file added
      

    0.3.7
      - rbioplot() now gives a proper error message if only one group (e.g. experimental condition) is present
      - editorial changes in documentation
      

    0.3.6
      - Citation info added
      

    0.3.5
      - Bars now won't disappear for rbioplot() when the y-axis lower limit is set to a value other than zero.
      - The distance between the errorbar label and errorbar now can be adjusted for rbioplot() through argument "errorbarLblSpace"
      

    0.3.4
      - Characters considered as special operators now can be included in the target names for the stats, histogram and heatmap modules, and will be displayed properly in the figures
      - Users now can choose to avoid setting control to 1 for rbioplot() function, by setting  "Nrm = FALSE”
      - Users now can choose to display or hide the right side y-axis for histogram and curve modules by setting "rightsideY = TRUE" or "rightsideY = FALSE", respectively
      - Bug fixes
      
    
    0.3.3
      - Backend updates for next feature update
      

    0.3.1 - 0.3.2
      - Bug fixes
      

    0.3.0
      - It is now possible to change the width of the errorbar with the argument errorbarWidth. Applicable functions: rbioplot(), rbioplot_curve()
		    - errorbarWidth: Set the width for errorbar. Defualt is 0.2
		    
      - it is now possible to change the size of the symbols for curve plots with the argument symbolSize. Applicable function: rbioplot_curve()
		    - symbolSize: Set the size of symbols. Default is 2
		    
      - It is now possible to hide the first column of the heatmap with the argument rmCntl. Applicable function: rbioplot_heatmap()
		    - rmCntl Remove the first column (i.e., control). Default is FALSE
      - Bug fixes
      

    0.2.5
      - RBioplot now supports heatmap with the new function rbioplot_heatmap()
      - Bug fixes
      

    0.2.4
      - It is now possible to use standard deviation (SD) as error bar via the new argument: errorbar. Functions with errorbar argument are: rbioplot(), rbioplot_curve(), autorange_bar_y(), autorange_curve()
		      - errorbar: Set the type of errorbar. Options are standard error of mean ("SEM"), or standard deviation ("SD"). Default is "SEM"
		      
      - It is now possible to change the font type for the graphs via the new argument: fontType. Functions with fontType argument are: rbioplot(), rbioplot_curve()
	  	    - fontType: The type of font in the figure. Default is "sans". For all options please refer to R font table, which is avaiable on the website: http://kenstoreylab.com/?page_id=69
	  	      
      - It is now possible to change the font size for ticks via the new arguments: xTickLblSize, yTickLblSize. Functions with fontType argument are: rbioplot(), rbioplot_curve()
		      - xTickLblSize: Font size of x axis ticks. Default is 10
		      - yTickLblSize: Font size of x axis ticks. Default is 10
		      
      - It is now possible to set tick font to italic via the new arguments: xTickItalic, yTickItalic. Functions with fontType argument are: rbioplot(), rbioplot_curve()
		      xTickItalic: Set x axis tick font to italic. Default is FALSE
		      yTickItalic: Set y axis tick font to italic. Default is FALSE
		      
      - Bug fixes
      

    0.2.3
      - The program has a new name: RBioplot. Along with the necessary name change for some functions, some minor new features have also been added to the existing functions
      - Function name change (old -> new)	*Note that the usage of those functions are unchanged*
  		  - frogstats -> rbiostats
  		  - frogplots -> rbioplot
  		  - frogplots_curve -> rbioplot_cruve
  		  
      - New features and changes
  		  - rbioplot_curve(): 
  			    - the function now detects the number of end numbers (i.e., if the data has replicates or not) and determines if error bar is needed for plotting
  			    - the function now automatically assigns different line patterns for each experimental group
  			    
  		  - autorange_curve(): consistent with the changes to rbioplot_curve, necessary adjustments have been made to make sure proper space management of the graph
      - Bug fixes
      
      - Other
          The contact email address specific to the program is changed to jzhangcad@gmail.com. (Please note that this will not affect my current Gmail address, i.e., I am NOT switching my main email address to this)
          

    0.2.2
      - Bug fixes
      

    0.2.1
      - Bug fixes
      

    0.2.0
      - New functions:
  		  - frogplots_curve(): A simple to use function for plotting joining-point curves with continuous x and y axises values
  		  - autorange_bar_y(): A function to get custom lower/upper limit, major tick range, as well as minor tick options for y axis, based on a user-defined major tick number.
  		  - autorange_curve(): A function to get custom lower/upper limit, major tick range, as well as minor tick options for both axises of a joint-piont curve with continuous x AND y values, based on a user-defined major tick number
    		- nat_dvsr(): A simple to use function to find all divisors for an integer number
  		  - all_dvsr(): A function to find all divisors for any types of number
  		  
      - Changes to existing functions:
  	    -  frogstats(): 
  		    - Bartlett test has been added to test if the data have equal variance among groups - p > 0.05 means the variance is equal. Joining the S-W normality test, the Bartlett test is a default test that will be conducted regardless of the stats test type. Make sure the data meet the equal variance criterion as both conventional t-test and ANOVA (with all the post-hoc tests) assume the data is (1) normally distributed and (2) have equal variance. The results are included in the output .txt file
  		    - t-test has been adjusted to “student’s t-test” (from “Welch's t-test”) to accurately reflect the equal variance aspect
  		    
  	    - frogplots():
      		- New arguments have added to allow user-defined y axis range, y axis major tick range and minor tick patterns:
      			- y_custom_tick_range: To initiate setting the custom y_upper_limit, y_lower_limit, y_major_tick_range, y_n_minor_ticks. Default is FALSE
      			- y_lower_limit: Can only be set when y_custom_tick_range = TRUE. Set custom lower limt for y axis. Default is 0. Value can be obtained from the new function autorange_bar_y
      			- y_upper_limit: Can only be set when y_custom_tick_range = TRUE. Set custom upper limt for y axis. Value can be obtained from the new function autorange_bar_y
      			- y_major_tick_range: Can only be set when y_custom_tick_range = TRUE. Set custom major tick range for y axis. Value can be obtained from the new function autorange_bar_y
      			- y_n_minor_ticks: Can only be set when y_custom_tick_range = TRUE. Set custom numbers of minor ticks. Default is 4. Value can be obtained from the new function autorange_bar_y
      		- t-test within the function has been adjusted to “student’s t-test” from “Welch t-test” to reflect equal variance requirement
      		- Plot and axis titles are now bold by default.
      		- Small fixes, etc
      		
      - Bug fixes
      

    0.1.0
      - Initial commit
