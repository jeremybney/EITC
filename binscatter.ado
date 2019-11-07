/*
binscatter, version 5.1, 20 September 2012.

Written by Michael Stepner, based on code originally written by Jessica Laird.
For assistance, e-mail michaelstepner@gmail.com


***********************
*** Getting Started ***
***********************

To use this program, you must either put "binscatter.ado" in your Stata 'ado folder', or:

   . run binscatter.ado

(You may wish to also install fastxtile.ado in the same way. It will increase the binning speed,
    and the difference is especially noticeable for large datasets.)


******************
*** binscatter ***
******************

This program creates a binned scatter plot.

Syntax:
	binscatter y_var [y2_var] x_var [if] [weight] [, options]

Options:

  MAIN
    - by(varname): create scatters separately by groups
    - method(string): may take one of the following values
	- preserve: [default] single-process method, may fail for large datasets (which are too large to save temporarily in the TEMP folder)
	- tempvars: single-process method, cannot be combined with savedata()
	- log: two-process method, runs significantly faster for large datasets. Requires a separate Stata process running graphlog (which loops infinitely waiting for logs to appear).

  SPECIFY BINS
    - x_q(varname): specifies a variable in the dataset which represents the bins. If this option is not specified, the program will define its own bins.

    - discrete: specifies that the x_var is discrete, and each of its unique values is to be used as a bin.
    - unique(integer): if the x_var has fewer than `unique' distinct values, binscatter will automatically set it as discrete.  This number is 100 by default.
		If set to 0, will not check.

    - nbins(#): the number of equal-sized bins to be created.  If no bin number is specified it defaults to 20 bins. Cannot be combined with x_q(varname) or 'discrete'.
    - create_xq: specifies that the program should create the quantile variable with the name q_`x_var'. If there is already a variable with that name in the dataset it will be replaced.

  CONTROLS
    - controls(varlist): a list of variables that are controlled for when plotting.  it will scatter the residuals after regressing the y-var(s) against these vars
    - absorb(varname): a single variable to be absorbed
    - noaddmean: if specified in combination with either controls() or absorb(), the mean of the y-var(s) will not be added back to the residuals

  FIT LINE
    - linetype(string): may take one of the following values
	- lfit: [default] a linear fit using the microdata
	- qfit: a quadratic fit using the microdata
	- lfitscatter: a linear fit using the scatter points
	- qfitscatter: a quadratic fit using the scatter points
	- connect: connect the scatter points
	- noline: no line displayed, only scatter points

    - reportreg: reports the regression used to define the fitline. can only be used in combination with linestyle(): lfit, qfit, lfitscatter, qfitscatter.

  GRAPH STYLE
    - colors(string): specifies an ordered list of colors, separated by spaces
    - mcolors(string): an overriding ordered list of colors for the markers (dots)
    - lcolors(string): an overriding ordered list of colors for the lines

    - symbols(string): specifies an ordered list of symbols, separated by spaces

  SAVE OUTPUT
    - savegraph(string): specifies the filename to which the graph should be saved.  The format must be specified as a file extension (ex: .wmf or .png).
		Be sure to enclose in quotes any filename including spaces.
    - savedata(string): specifies the filename to which the dataset used to generate the graph should be saved.  Note that the command used to generate the graph
		from that dataset can be seen using the command "notes".  Be sure to enclose in quotes any filename including spaces.

  FASTXTILE
    - nofastxtile: specifies that fastxtile should not be used, even if installed. It is not clear why this should ever be done.
    - randvar(varname): specifies the variable defining a random sample, which is used to perform a faster but approximate binning.
		Defaults to the variable "rand" if it exists. Otherwise, if randcut() is in (0,1) a uniform random variable will be used.
    - randcut(real): specifies the upper bound on randvar.

  MISCELLANEOUS
    - logfile(string): specifies the filename to which the logfile should be saved. Defaults to 'tab.txt'
    - DISPlaygraphcmd: displays in the results window the text of the command used to generate the graph

  USER-DEFINED GRAPH STYLE
    - Any other "twoway" parameters for controlling the style of the graph output can be used as well. e.g.: 
	- legend(off)
	- title("Binscatter Plot")
	- ylabel(0(3)15)


****************
*** graphlog ***
****************

graphlog is the fastest way to generate binscatters from large datasets.  It is used in combination with binscatter method(log).

graphlog must be run in a separate Stata window. You run it once, and then leave it alone.
   It will run in an infinite loop, watching and waiting for a log with the correct filename to appear.
   To end graphlog you need to 'Break' the process manually.

Syntax:
	graphlog using *filename*

Typically run as: 
	. cd *working_directory*
	. graphlog using tab.txt

*/


capture program drop binscatter
program define binscatter
	version 11

	* Parse weights, if any
	_parsewt "fweight aweight pweight" `0' 
	local 0  "`s(newcmd)'" /* command minus weight statement */
	local wt "`s(weight)'"  /* contains [weight=exp] or nothing */
	
	syntax varlist(min=2 max=3 numeric) [if], [by(varname) method(string) ///
		x_q(varname numeric) discrete unique(integer 100) nbins(integer 20) create_xq ///
		CONTROLs(varlist numeric) absorb(varname numeric) NOAddmean ///
		LINEtype(string) reportreg ///
		colors(string) mcolors(string) lcolors(string) symbols(string) ///
		savegraph(string) savedata(string) ///
		nofastxtile randvar(varname numeric) randcut(real 1) ///
		logfile(string) ///
		DISPlaygraphcmd ///
		graph(string) qfit connect NOLine saving(string) weight(varname) fastxtile(string) ///
		*]

	/*** Begin legacy parameter compatibility ***/

	if "`graph'"!="" {
		if "`method'"!="" {
			di as error "Cannot specify legacy graph() parameter AND method() parameter."
			exit
		}
		else if ("`graph'"=="1") local method tempvars
		else if ("`graph'"=="0") local method log
		else {
			di as error "graph() must be either 0 or 1."
			exit
		}
		local legacyparams "`legacyparams' graph()"
	}
	
	if "`qfit'"!="" | "`connect'"!="" | "`noline'"!="" {
		if ("`qfit'`connect'`noline'"!="qfit") & ("`qfit'`connect'`noline'"!="connect") & ("`qfit'`connect'`noline'"!="noline") {
			di as error "You can only specify one of the legacy parameters qfit, connect or noline."
			exit
		}
		else if "`linetype'"!="" {
			di as error "Cannot specify legacy `qfit'`connect'`noline' parameter AND linetype() parameter."
			exit
		}
		else local linetype `qfit'`connect'`noline'
		local legacyparams "`legacyparams' `qfit'`connect'`noline'"
	}
	
	if "`saving'"!="" {
		if "`savegraph'"!="" {
			di as error "Cannot specify legacy saving() parameter AND savegraph() parameter."
			exit
		}
		else local savegraph `saving'
		local legacyparams "`legacyparams' savegraph()"
	}
	
	if "`savedata'"=="0" {
		local savedata ""
		local legacyparams "`legacyparams' savedata()"
	}
	if "`savedata'"=="1" {
		if ("`saving'"!="") local savedata=regexr(`"`saving'"',"\.[a-zA-Z][a-zA-Z][a-zA-Z]?$",".dta")
		else local savedata "tab.dta"
		local legacyparams "`legacyparams' savedata()"
	}
	
	if "`weight'"!="" {
		if "`wt'"!="" {
			di as error "Cannot specify legacy weight() parameter AND [w=varname]"
		}
		else { 
			local wt [w=`weight']
			local weight ""
		}
		local legacyparams "`legacyparams' weight()"
	}
	
	if "`fastxtile'"!="" {
		if "`nofastxtile'"!="" {
			di as error "Cannot specify legacy fastxtile() parameter AND nofastxtile parameter."
			exit
		}
		else if ("`fastxtile'"=="0") local nofastxtile nofastxtile
		else if ("`fastxtile'"!="1") {
			di as error "fastxtile() must be either 0 or 1."
			exit
		}
		local legacyparams "`legacyparams' fastxtile()"			
	}


	if ("`legacyparams'"!="") di "NOTE the following legacy parameters were specified:`legacyparams'"

	/*** End legacy parameter compatibility ***/

	* Set default method and check valid
	if ("`method'"=="") local method preserve
	else if "`method'"!="preserve" & "`method'"!="log" & "`method'"!="tempvars" {
		di as error "method() must either be preserve, log, or tempvars"
		exit
	}

	* Set default linetype and check valid
	if ("`linetype'"=="") local linetype lfit
	else if ("`linetype'"!="connect") & ("`linetype'"!="lfit") & ("`linetype'"!="qfit") & ("`linetype'"!="lfitscatter") & ("`linetype'"!="qfitscatter") & ("`linetype'"!="noline") {
		di as error "linetype() must either be connect, lfit, qfit, lfitscatter, qfitscatter, or noline"
		exit
	}

	* fastxtile check
	capture fastxtile
	if (_rc==199) local nofastxtile nofastxtile

	if "`nofastxtile'"!="" & ("`randvar'"!="" | `randcut'!=1) {
		di as error "Cannot use randvar or randcut without fastxtile"
		exit
	}

	* Misc checks
	if ("`create_xq'"!="" & ("`x_q'"!="" | "`discrete'"!="")) | ("`x_q'"!="" & "`discrete'"!="") {
		di as error "Cannot specify more than one of create_xq, x_q(), and discrete simultaneously."
		exit
	}
	if "`logfile'"!="" & "`method'"!="log" & "`method'"!="tempvars" {
		di as error "Cannot specify a logfile when method is not log or tempvars."
		exit
	}
	if `nbins'!=20 & ("`x_q'"!="" | "`discrete'"!="") {
		di as error "Cannot specify nbins in combination with discrete or an x_q variable."
		exit
	}
	if "`reportreg'"!="" & "`linetype'"!="lfit" & "`linetype'"!="qfit" & "`linetype'"!="lfitscatter" & "`linetype'"!="qfitscatter" {
		di as error "Cannot specify 'reportreg' when no fit line is being created."
		exit
	}
	if "`savedata'"!="" & "`method'"=="tempvars" {
		di as error "Cannot 'savedata' with method tempvars."
		exit
	}

	* Mark sample (reflects the if conditions, and includes only nonmissing observations)
	marksample touse
	markout `touse' `by', strok

	* Parse varlist
	local y_var : word 1 of `varlist'
	local ynum=`: word count `varlist''-1
	if (`ynum' == 1) local x_var : word 2 of `varlist'
	else {
		local y2_var : word 2 of `varlist'
		local x_var : word 3 of `varlist'
	}

	* Check number of unique byvals & create local storing byvals
	if "`by'"!="" {
		* Check number of unique byvals
		qui tab `by' if `touse', nofreq
		if r(r)>20 {
			di as error "The specified by variable has `r(r)' unique values. It may have no more than 20."
			exit
		}

		* Create local storing byvals
		quietly levelsof `by' if `touse'
		local byvals `"`r(levels)'"'
	}

	* Check number of unique values if not already discrete and x_q not specified
	if "`discrete'"=="" & `unique'!=0 & "`x_q'"=="" {
		capture tab `x_var' if `touse', nofreq
		if (_rc==0 & r(r)<=`unique') { 
			local discrete discrete
			if "`create_xq'"!="" {
				di as error "Binscatter has determined that `x_var' is discrete, because it has fewer than `unique' unique values. The create_xq parameter therefore cannot be specified."
				exit
			}
		}
	}

	* Parse absorb to define the type of regression to be used
	if `"`absorb'"'!="" {
		local regtype `"areg"'
		local absorb `"absorb(`absorb')"'
	}
	else {
		local regtype `"reg"'
	}


	* Create residuals if needed
	if (`"`controls'"'!="" || `"`absorb'"'!="") quietly {
		local y_r
		local y2_r
		local x_r

		* y1 residuals
		tempvar y_r
		`regtype' `y_var' `controls' `wt' if `touse', `absorb'		
		predict `y_r' if e(sample), residuals
		if "`noaddmean'"=="" {
			summarize `y_var' `wt'  if `touse', meanonly
			replace `y_r'=`y_r'+r(mean)
		}

		* y2 residuals
		if "`y2_var'"!="" {
			tempvar y2_r
			`regtype' `y2_var' `controls' `wt' if `touse', `absorb'
			predict `y2_r' if e(sample), residuals
			if ("`noaddmean'"=="") {
				summarize `y2_var' `wt' if `touse', meanonly
				replace `y2_r'=`y2_r'+r(mean)
			}
		}

		* x residuals
		if "`discrete'"=="" {
			tempvar x_r
			`regtype' `x_var' `controls' `wt' if `touse', `absorb'
			predict `x_r' if e(sample), residuals
			if ("`noaddmean'"=="") {
				summarize `x_var' `wt' if `touse', meanonly
				replace `x_r'=`x_r'+r(mean)
			}
		}
		else local x_r `x_var'
	}
	else { 	/*absorb and controls both empty, no need for regression*/
		local y_r `y_var' 
		local y2_r `y2_var'
		local x_r `x_var'
	}


	* Regressions
	if ("`linetype'"=="lfitscatter") | ("`linetype'"=="qfitscatter") local fitscatterpts true
	if ("`reportreg'"=="") | ("`fitscatterpts'"!="") local reg_verbosity "quietly"

	if ("`fitscatterpts'"=="" | "`reportreg'"!="") & ("`linetype'"=="lfit" | "`linetype'"=="qfit") `reg_verbosity' {
		if ("`fitscatterpts'"=="") tempname fit_coefs

		if "`linetype'"=="qfit" {
			tempvar x_r2
			gen `x_r2'=`x_r'^2
		}

		if "`by'"=="" {
			reg `y_r' `x_r2' `x_r' `wt' if `touse'
			if ("`fitscatterpts'"=="") matrix `fit_coefs'=e(b)
			if ("`y2_var'"!="") {
				reg `y2_r' `x_r2' `x_r' `wt' if `touse'
				if ("`fitscatterpts'"=="") matrix `fit_coefs'=`fit_coefs' \ e(b)
			}
		}
		else {
			local firstloop=1
			foreach byval in `byvals' {
				* make output readable
				if "`reportreg'"!="" {
					di "{txt}{hline}"
					di "-> `by' = `byval'"
				}

				* y_var
				reg `y_r' `x_r2' `x_r' `wt' if `touse' & `by'==`byval'
				if ("`fitscatterpts'"=="") {
					if (`firstloop'==1) matrix `fit_coefs'=e(b)
					else matrix `fit_coefs'=`fit_coefs' \ e(b)
				}
				
				* y2_var
				if ("`y2_var'"!="") {
					reg `y2_r' `x_r2' `x_r' `wt' if `touse' & `by'==`byval'
					if ("`fitscatterpts'"=="") matrix `fit_coefs'=`fit_coefs' \ e(b)
				}

				* next loop won't be the first one
				local firstloop=0
			}
		}
	}


	* Specify and/or create the x_q var, as necessary
	if "`x_q'"=="" {
		if "`discrete'"=="" { /* x_q() and discrete are not specified */
			if ("`create_xq'"!="") {
				capture drop q_`x_var'
				local x_q q_`x_var'
			}
			else tempvar x_q

			if ("`nofastxtile'"=="") fastxtile `x_q' = `x_r' `wt' if `touse', nq(`nbins') randvar(`randvar') randcut(`randcut')
			else xtile `x_q' = `x_r' `wt' if `touse', nq(`nbins')
		}
		else { /* discrete is specified, x_q() & create_xq are not */
			local x_q `x_var'
			* set nbins var
			capture tab `x_q' if `touse', nofreq
			local nbins=r(r)
		}
	}
	else {
		* set nbins var
		capture tab `x_q' if `touse', nofreq
		local nbins=r(r)
	}


	**************************************


	if ("`method'"=="log" | "`method'"=="tempvars") {
		set more off

		* Write the parameters
		capture file close tab
		quietly file open tab using "tab_temp.txt", write text replace
		file write tab `"<BeginParameters>"' _n
		file write tab `"y_var,`y_var'"' _n
		file write tab `"y2_var,`y2_var'"' _n
		file write tab `"x_var,`x_var'"' _n
		file write tab `"by,`by'"' _n
		file write tab `"byvals,`"`byvals'"'"' _n
		file write tab `"nbins,`nbins'"' _n
		file write tab `"discrete,`discrete'"' _n
		file write tab `"savedata,`"`savedata'"'"' _n
		file write tab `"savegraph,`"`savegraph'"'"' _n
		file write tab `"displaygraphcmd,`displaygraphcmd'"' _n
		file write tab `"linetype,`linetype'"' _n
		file write tab `"colors,`colors'"' _n
		file write tab `"mcolors,`mcolors'"' _n
		file write tab `"lcolors,`lcolors'"' _n
		file write tab `"symbols,`symbols'"' _n
		file write tab `"reportreg,`reportreg'"' _n
		file write tab `"options,`"`options'"'"' _n
		file write tab `"<EndParameters>"' _n
		if "`fit_coefs'"!="" {
			file write tab `"<BeginCoefs>"' _n
			local nrows=rowsof(`fit_coefs')

			forvalues i=1/`nrows' {
				file write tab (`fit_coefs'[`i',1]) "," (`fit_coefs'[`i',2]) "," (`fit_coefs'[`i',3]) _n
			}

			file write tab `"<EndCoefs>"' _n
		}
		file close tab


		* Start the log
		capture log close tab
		quietly log using "tab_temp.txt", name(tab) append text


		* Output tabs (note: must be noisy in order to be captured by log)
		if "`discrete'"==""{
			noisily di "tab;x_var;"
			noisily tab `x_q' if `touse' `wt', sum(`x_r') means wrap nolabel noobs
		}


		if "`by'"=="" {
			noisily di "tab;y_var;"
			noisily tab `x_q' if `touse' `wt', sum(`y_r') means wrap nolabel noobs
		  	if "`y2_var'"!="" {
				noisily di "tab;y2_var;"
				noisily tab `x_q'  if `touse' `wt', sum(`y2_r') means wrap nolabel noobs
			}
		}
		else {
			foreach byval in `byvals' {
				noisily di "tab;y_var;`byval'"
				noisily tab `x_q'  if `touse' & `by'==`byval' `wt', sum(`y_r') means wrap nolabel noobs
			  	if "`y2_var'"!="" {
					noisily di "tab;y2_var;`byval'"
					noisily tab `x_q'  if `touse' & `by'==`byval' `wt', sum(`y2_r') means wrap nolabel noobs
				}
			}
		}

		* Close and rename the log
		qui log close tab
		if "`logfile'"=="" {
			 local logfile tab.txt
		}

		qui copy tab_temp.txt `"`logfile'"', replace
		erase tab_temp.txt

		* Checks
		if "`reportreg'"!="" & ("`linetype='"=="lfitscatter" | "`linetype='"=="qfitscatter") & "`method'"=="log" {
			di ""
			di "NOTE: Regression will be reported in the graphlog Stata window."
		}
		if "`savedata'"=="" & "`savegraph'"=="" & "`method'"=="log" {
			di ""
			di as error "WARNING: method(log) was specified without savedata() or savegraph().  The log has been created, but graphlog will not save any output."
		}

		* Graph the log, if method is tempvars
		if ("`method'"=="tempvars") graphlog using `"`logfile'"', tempvars

	}


	if "`method'"=="preserve" {
		preserve

		if ("`x_q'"!="`x_r'") collapse (mean) `y_r' `y2_r' `x_r' if `touse' `wt', by(`x_q' `by')
		else collapse (mean) `y_r' `y2_r' if `touse' `wt', by(`x_q' `by')

		* Reshape data to wide if there is a by_var
		if "`by'"!="" {
			local i=0
			tempvar bystringvar
			qui g `bystringvar'=""
			foreach byval in `byvals' {
				local i=`i'+1
				qui replace `bystringvar'="_by`i'" if `by'==`byval'
			}
			drop `by'
			rename `bystringvar' `by'

			quietly reshape wide `y_r' `y2_r', i(`x_r') j(`by') string
		}

		* Call graph command
		_binsc_graph, y_var(`y_r') y2_var(`y2_r') x_var(`x_r') y_name(`y_r') y2_name(`y2_r') x_name(`x_r') by(`by') byvals(`byvals') linetype(`linetype') fit_coefs(`fit_coefs') ///
				colors(`colors') mcolors(`mcolors') lcolors(`lcolors') symbols(`symbols') displaygraphcmd(`displaygraphcmd') reportreg(`reportreg') options(`"`options'"')

		* Save graph
		if "`savegraph'"!="" {
			* check file extension using a regular expression
			local temp=regexm(`"`savegraph'"',"\.[a-zA-Z0-9]+$")
			
			if (regexs(0)==".gph") quietly graph save `"`savegraph'"', replace
			else quietly graph export `"`savegraph'"', replace
		}
		
		* Save data
		if ("`savedata'"!="") save `"`savedata'"', replace
	}

end


capture program drop graphlog
program define graphlog
	version 11

	syntax using/, [tempvars]

	local loop=1

	while 1==`loop' {

		* Set loop or no loop
		if ("`tempvars'"=="") {
			clear

			* wait for a log to appear
			capture confirm file `"`using'"'
	 		while _rc {      /* file does not exist! */
			    sleep 1000   /* wait 1000 ms = 1 second before trying again */
	  		    capture confirm file `"`using'"'
	    		}
		}
		else local loop=0

		* Open tab
		capture file close tab
		file open tab using `"`using'"', read
	

		* Read in parameters
		file read tab line /* skip first line, which says <BeginParameters> */
		file read tab line
		while `"`line'"'!="<EndParameters>" {
			tokenize `"`line'"', parse(",")
			*di `"`1': `3'"'
			local `1' `3'
			file read tab line
		}

		* Set fixed var names
		local y_name `y_var'
		local y2_name `y2_var'
		local x_name `x_var'

		* Read in microdata fit coefficients, if applicable
		file read tab line
		if "`line'"=="<BeginCoefs>" {
			tempname fit_coefs
			local firstloop=1

			file read tab line
			while `"`line'"'!="<EndCoefs>" {
				if (`firstloop'==1) matrix `fit_coefs' = (`line')
				else matrix `fit_coefs' = `fit_coefs' \ (`line')

				file read tab line
				local firstloop=0
			}
		}

		* Set obs if looping
		if ("`tempvars'"=="") qui set obs `nbins'

		*** Read in tabs ***

		* Create var to store the x values (left side of each tab, which is always identical)
		if ("`tempvars'"!="") tempvar x_var
		quietly gen `x_var'=.

		* Tab reading
		local t=0
		local by_i=0
		while r(eof)==0 {
			local t=`t'+1
			*di "Tab `t'"

			* Find the line: tab;vartype;byval
			tokenize `"`line'"', parse(";")
			while `"`1'"'!="tab" & r(eof)==0 {
				file read tab line
				tokenize `"`line'"', parse(";")
			}
			*di `"`1':`3':`5'"'

			* Create variable to store the right side of the tab
			if ("`3'"=="x_var") {
				if ("`tempvars'"!="") tempvar x_r
				else local x_r x_r

				local genvar `x_r'
			}
			else { /* `3' == y_var or y2_var */
				if ("`5'"=="") {
					if ("`tempvars'"!="") tempvar `3'
					local genvar ``3''
				}
				else {
					if "`5'"!="`byval_old'" {
						local by_i=`by_i'+1
						local byval_old "`5'"
					}

					if ("`tempvars'"!="") {
						tempvar genvar
						if ("`3'"=="y_var") local y_by_varlist `y_by_varlist' `genvar'
						else local y2_by_varlist `y2_by_varlist' `genvar'
					}
					else local genvar ``3''_by`by_i'
				}
			}
			qui gen `genvar'=.

			* Find the top crossline of the table
			file read tab line
			tokenize `"`line'"', parse("|")
			while (`"`1'"'!="------------+------------" | `"`2'"'!="") & r(eof)==0 {
				file read tab line
				tokenize `"`line'"', parse("|")
			}

			* Load the table into the dataset until we reach the bottom crossline of the table
			local i=1

			file read tab line
			tokenize `"`line'"', parse("|")

			while (`"`1'"'!="------------+------------" | `"`2'"'!="") & r(eof)==0 {
				*di "`1':`3'"
				if (`t'==1) qui replace `x_var'=real(subinstr("`1'",",","",.)) in `i'
				qui replace `genvar'=real(subinstr("`3'",",","",.)) in `i'

				local i=`i'+1

				file read tab line
				tokenize `"`line'"', parse("|")
			}

			* Advance by 2 lines: will either hit 'eof' or next 'tab;vartype;byval' line
			file read tab line
			file read tab line
		}

		file close tab

		* If x_r exists, put it on the x_axis
		if ("`x_r'"!="") local x_var `"`x_r'"'

		* Call graph command
		_binsc_graph, y_var(`y_var') y2_var(`y2_var') x_var(`x_var') y_name(`y_name') y2_name(`y2_name') x_name(`x_name') ///
			by(`by') byvals(`"`byvals'"') y_by_varlist(`y_by_varlist') y2_by_varlist(`y2_by_varlist') linetype(`linetype') fit_coefs(`fit_coefs') ///
			colors(`colors') mcolors(`mcolors') lcolors(`lcolors') symbols(`symbols') displaygraphcmd(`displaygraphcmd') reportreg(`reportreg') options(`"`options'"')

		* Save graph
		if `"`savegraph'"'!="" {
			* check file extension using a regular expression
			local temp=regexm(`"`savegraph'"',"\.[a-zA-Z0-9]+$")
			
			if (regexs(0)==".gph") quietly graph save `"`savegraph'"', replace
			else quietly graph export `"`savegraph'"', replace
		}

		* Save data
		if ("`savedata'"!="") save `"`savedata'"', replace

		* Erase tab file
		erase `"`using'"'

	}
end

capture program drop _binsc_graph
program define _binsc_graph
	version 11

	syntax , y_var(string) y_name(string) x_var(string) [y2_var(string) y2_name(string) x_name(string) by(string) byvals(string) y_by_varlist(varlist) y2_by_varlist(varlist) linetype(string) fit_coefs(string) colors(string) mcolors(string) lcolors(string) symbols(string) displaygraphcmd(string) reportreg(string) options(string)]

	* If using tempvars with a by variable, use locals to specify the names of the tempvars
	if "`by'"!="" {

		* if the method was tempvars, so we need to assign the locals to the corresponding tempvars
		if "`y_by_varlist'"!="" {
			local i=1
			foreach var of varlist `y_by_varlist' {
				local `y_var'_by`i' `var'
				local i=`i'+1
			}
		}
		* otherwise, store the name of the local in each local
		else {
			local i=1
			foreach byval in `byvals' {
				local `y_var'_by`i' `y_var'_by`i'
				local i=`i'+1
			}
		}
			
		* repeat for y2
		if "`y2_by_varlist'"!="" {
			local i=1
			foreach var of varlist `y2_by_varlist' {
				local `y2_var'_by`i' `var'
				local i=`i'+1
			}
		}
		else if "`y2_var'"!="" {
			local i=1
			foreach byval in `byvals' {
				local `y2_var'_by`i' `y2_var'_by`i'
				local i=`i'+1
			}
		}
	}


	* Report regressions if "reportreg" and "fit scatter pts" both requested
	if ("`reportreg'"!="") & ("`linetype'"=="lfitscatter" | "`linetype'"=="qfitscatter") {
		if "`linetype'"=="qfitscatter" {
			tempvar x_var2
			gen `x_var2'=`x_var'^2
		}

		if "`by'"=="" {
			reg `y_var' `x_var2' `x_var'
			if ("`y2_var'"!="") reg `y2_var' `x_var2' `x_var'
		}
		else {
			local i=1
			foreach byval in `byvals' {
				* make output readable
				di "{txt}{hline}"
				di "-> `by' = `byval'"

				reg ``y_var'_by`i'' `x_var2' `x_var'
				if ("`y2_var'"!="") reg ``y2_var'_by`i'' `x_var2' `x_var'

				local i=`i'+1
			}
		}
	}

	* Parse line type
	if ("`linetype'"=="connect") local connect `"c(l l l l l l l l l l l) sort(`x_var')"'
	else if ("`linetype'"=="qfitscatter") local typefit qfit
	else if ("`linetype'"=="lfitscatter") local typefit lfit

	* Fill colors if missing
	if "`colors'"=="" local colors ///
		navy maroon forest_green dkorange teal cranberry lavender ///
		khaki sienna emidblue emerald brown erose gold bluishgray ///
		lime magenta cyan pink blue

	if "`mcolors'"=="" local mcolors `colors'
	if "`lcolors'"=="" local lcolors `colors'

	* Prepare symbols
	if "`symbols'"!="" {
		local symbol_prefix "msymbol("
		local symbol_suffix ")"
	}


	* Prepare scatter commands
	if ("`by'"=="") {
		if ("`y2_var'"=="") {
			local scatters (scatter `y_var' `x_var', `connect' mcolor(`: word 1 of `mcolors'') lcolor(`: word 1 of `lcolors'') `symbol_prefix'`: word 1 of `symbols''`symbol_suffix')
			if ("`linetype'"=="lfit") {
				* save the coefs to locals so that the numerical values are stored in the notes if the dataset is saved
				local x_coef=`fit_coefs'[1,1]
				local cons_coef=`fit_coefs'[1,2]
				local fits (function `x_coef'*x+`cons_coef', range(`x_var') lcolor(`: word 1 of `lcolors''))
			}
			else if ("`linetype'"=="qfit") {
				* save the coefs to locals so that the numerical values are stored in the notes if the dataset is saved
				local x2_coef=`fit_coefs'[1,1]
				local x_coef=`fit_coefs'[1,2]
				local cons_coef=`fit_coefs'[1,3]
				local fits (function `x2_coef'*x^2+`x_coef'*x+`cons_coef', range(`x_var') lcolor(`: word 1 of `lcolors''))
			}
			else if ("`typefit'"!="") local fits (`typefit' `y_var' `x_var', lcolor(`: word 1 of `lcolors''))
			local legend_labels "off"
			local order
		}
		else {
			local scatters (scatter `y_var' `y2_var' `x_var', `connect' mcolor(`: word 1 of `mcolors'' `: word 2 of `mcolors'') lcolor(`: word 1 of `lcolors'' `: word 2 of `lcolors'') `symbol_prefix'`: word 1 of `symbols'' `: word 2 of `symbols''`symbol_suffix')
			if ("`linetype'"=="lfit") {
				* save the coefs to locals so that the numerical values are stored in the notes if the dataset is saved
				local yvar_x_coef=`fit_coefs'[1,1]
				local yvar_cons_coef=`fit_coefs'[1,2]
				local y2var_x_coef=`fit_coefs'[2,1]
				local y2var_cons_coef=`fit_coefs'[2,2]
				local fits (function `yvar_x_coef'*x+`yvar_cons_coef', range(`x_var') lcolor(`: word 1 of `lcolors'')) (function `y2var_x_coef'*x+`y2var_cons_coef', range(`x_var') lcolor(`: word 2 of `lcolors''))
			}
			else if ("`linetype'"=="qfit") {
				* save the coefs to locals so that the numerical values are stored in the notes if the dataset is saved
				local yvar_x2_coef=`fit_coefs'[1,1]
				local yvar_x_coef=`fit_coefs'[1,2]
				local yvar_cons_coef=`fit_coefs'[1,3]
				local y2var_x2_coef=`fit_coefs'[2,1]
				local y2var_x_coef=`fit_coefs'[2,2]
				local y2var_cons_coef=`fit_coefs'[2,3]			
				local fits (function `yvar_x2_coef'*x^2+`yvar_x_coef'*x+`yvar_cons_coef', range(`x_var') lcolor(`: word 1 of `lcolors'')) (function `y2var_x2_coef'*x^2+`y2var_x_coef'*x+`y2var_cons_coef', range(`x_var') lcolor(`: word 2 of `lcolors''))
			}
		    	else if ("`typefit'"!="") local fits (`typefit' `y_var' `x_var', lcolor(`: word 1 of `lcolors'')) (`typefit' `y2_var' `x_var', lcolor(`: word 2 of `lcolors''))
			local legend_labels "lab(1 `y_name') lab(2 `y2_name')"
			local order "1 2"
		}
	}
	else {
		local i=1
		local c=1
		foreach byval in `byvals' {
			if ("`y2_var'"=="") {
				local scatters `scatters' (scatter ``y_var'_by`i'' `x_var', `connect' mcolor(`: word `i' of `mcolors'') lcolor(`: word `i' of `lcolors'') `symbol_prefix'`: word `i' of `symbols''`symbol_suffix')
				if ("`linetype'"=="lfit") {
					* save the coefs to locals so that the numerical values are stored in the notes if the dataset is saved
					local x_coef=`fit_coefs'[`i',1]
					local cons_coef=`fit_coefs'[`i',2]
					local fits `fits' (function `x_coef'*x+`cons_coef', range(`x_var') lcolor(`: word `i' of `lcolors''))
				}
				else if ("`linetype'"=="qfit") {
					* save the coefs to locals so that the numerical values are stored in the notes if the dataset is saved
					local x2_coef=`fit_coefs'[`i',1]
					local x_coef=`fit_coefs'[`i',2]
					local cons_coef=`fit_coefs'[`i',3]
					local fits `fits' (function `x2_coef'*x^2+`x_coef'*x+`cons_coef', range(`x_var') lcolor(`: word `i' of `lcolors''))
				}
				else if ("`typefit'"!="") local fits `fits' (`typefit' ``y_var'_by`i'' `x_var', lcolor(`: word `i' of `lcolors''))
				local legend_labels `"`legend_labels' lab(`i' `by'=`byval')"'
				local order "`order' `i'"
			}
			else {
				local cP1=`c'+1
				local scatters `scatters' (scatter ``y_var'_by`i'' ``y2_var'_by`i'' `x_var', `connect' mcolor(`: word `c' of `mcolors'' `: word `cP1' of `mcolors'') lcolor(`: word `c' of `lcolors'' `: word `cP1' of `lcolors'') `symbol_prefix'`: word `c' of `symbols'' `: word `cP1' of `symbols''`symbol_suffix')
				if ("`linetype'"=="lfit") {
					* save the coefs to locals so that the numerical values are stored in the notes if the dataset is saved
					local yvar_x_coef=`fit_coefs'[`c',1]
					local yvar_cons_coef=`fit_coefs'[`c',2]
					local y2var_x_coef=`fit_coefs'[`cP1',1]
					local y2var_cons_coef=`fit_coefs'[`cP1',2]
					local fits `fits' (function `yvar_x_coef'*x+`yvar_cons_coef', range(`x_var') lcolor(`: word `c' of `lcolors'')) (function `y2var_x_coef'*x+`y2var_cons_coef', range(`x_var') lcolor(`: word `cP1' of `lcolors''))
				}
				else if ("`linetype'"=="qfit") {
					* save the coefs to locals so that the numerical values are stored in the notes if the dataset is saved
					local yvar_x2_coef=`fit_coefs'[`c',1]
					local yvar_x_coef=`fit_coefs'[`c',2]
					local yvar_cons_coef=`fit_coefs'[`c',3]
					local y2var_x2_coef=`fit_coefs'[`cP1',1]
					local y2var_x_coef=`fit_coefs'[`cP1',2]
					local y2var_cons_coef=`fit_coefs'[`cP1',3]
					local fits `fits' (function `yvar_x2_coef'*x^2+`yvar_x_coef'*x+`yvar_cons_coef', range(`x_var') lcolor(`: word `c' of `lcolors'')) (function `y2var_x2_coef'*x^2+`y2var_x_coef'*x+`y2var_cons_coef', range(`x_var') lcolor(`: word `cP1' of `lcolors''))
				}
			    	else if ("`typefit'"!="") local fits `fits' (`typefit' ``y_var'_by`i'' `x_var', lcolor(`: word `c' of `lcolors'')) (`typefit' ``y2_var'_by`i'' `x_var', lcolor(`: word `cP1' of `lcolors''))
				local legend_labels `"`legend_labels' lab(`c' `y_name': `by'=`byval') lab(`cP1' `y2_name': `by'=`byval')"'
				local order "`order' `c' `cP1'"
				local c=`c'+2
			}
			local i=`i'+1
		}
	}

	* Prepare y-axis title
	if ("`y2_var'"=="") local ytitle `"`y_name'"'
	else local ytitle `"`y_name' and `y2_name'"'

	* Display graph
	if ("`displaygraphcmd'"!="") di "twoway `scatters' `fits', graphregion(fcolor(white)) xtitle(`x_name') ytitle(`ytitle') legend(`legend_labels' order(`order')) `options'"
	notes: twoway `scatters' `fits', graphregion(fcolor(white)) xtitle(`x_name') ytitle(`ytitle') legend(`legend_labels' order(`order')) `options'
	twoway `scatters' `fits', graphregion(fcolor(white)) xtitle(`x_name') ytitle(`ytitle') legend(`legend_labels' order(`order')) `options'

end
