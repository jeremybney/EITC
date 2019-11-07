/* 	Version 1 discscatter March 2, 2012 
	This program creates a binned scatter plot.
	The command "discscatter" takes two-three variables and has several options.  It can either create a plot of just y vs. x, or two plots
	on the same figure of y1 vs. x and y2 vs. x.
	
	by specifies the categorical variable that the y-variable will be split by
	bynum specifies the number of categories that the 'by' variable takes. If not specified, defaults to 1. Maximum number of categories is 10.
	saving(string asis) specifies that the graph should be saved with the following string.  To specify the format, include
		the extension, i.e. saving(Hello.wmf).  For a title that includes spaces be sure to include quotes.
	
	* Specifies any options for graphs (such as the y scale, y title) can be specified.
	
	Example commands:
	discscatter y1 x if x>4, by(x2) bynum(4) legend(off) title("This is a neat graph!") ylabel(0(3)15)

*/
capture program drop discscatter
program define discscatter
	
	///version 10.1
	
	set more off
	
	syntax varlist [if/] [,by(varname) bynum(integer 1) saving(string) weight(varname) *]
	tokenize `varlist'
	
	if "`if'"!="" {
		local andif `" & (`if')"'
	}
	
	* Set the y-variable as the first variable named in the command, set the x as the second
	local y_var `1'
	
	if "`3'"==""{
		local x_var `2'
	}
	else { 
		local y2_var `2'
		local x_var `3'
		local y2_con `" & `y2_var' ~=."'
	}
	
	tempvar y_r x_r y2_r y2_mean temp

		if "`weight'"!=""{
		local weight `"[w=`weight']"'
		}
	cap log close tab
	*quietly log using "D:\Workdata\702487\RJST\jf_explore\tab.txt", name(tab) replace
	quietly log using "tab.txt", name(tab) replace
	
	if "`if'"!="" {
		tab `x_var' `by' `weight' if (`if'), sum(`y_var') means nost noo nof nol
	}
	
	else {	
		tab `x_var' `by' `weight', sum(`y_var') means nost noo nof nol
	}
	
	log close tab
	cap drop k
	gen k=`bynum' 
	*disclog using "D:\Workdata\702487\RJST\jf_explore\tab.txt"
  	disclog using "tab.txt"

* Create scatter plot of bins
				if `bynum'==1 {
					twoway scatter y_disc1 x_disc, xtitle(`x_var') ytitle(`y_var') c(l) ///
						graphregion(fcolor(white)) legend(label(1 `"`y_var'_1"')) `options'
				}
				if `bynum'==2 {
  		  			twoway scatter y_disc1 y_disc2 x_disc, xtitle(`x_var') ytitle(`y_var') c(l l) ///
  		  				graphregion(fcolor(white)) legend(lab(1 `"`y_var'_1"') lab(2 `"`y_var'_2"')) `options'
				}
				if `bynum'==3 {
  		  			twoway scatter y_disc1 y_disc2 y_disc3 x_disc, xtitle(`x_var') ytitle(`y_var') c(l l l) ///
  		  				graphregion(fcolor(white)) legend(lab(1 `"`y_var'_1"') lab(2 `"`y_var'_2"') lab(3 `"`y_var'_3"')) `options'
				}  	
				if `bynum'==4 {
  		  			twoway scatter y_disc1 y_disc2 y_disc3 y_disc4 x_disc, xtitle(`x_var') ytitle(`y_var') c(l l l l) ///
  		  				graphregion(fcolor(white)) legend(label(1 `"`y_var'_1"') lab(2 `"`y_var'_2"') lab(3 `"`y_var'_3"') lab(4 `"`y_var'_4"')) `options'
				} 
				if `bynum'==5 {
  		  			twoway scatter y_disc1 y_disc2 y_disc3 y_disc4 y_disc5 x_disc, xtitle(`x_var') ytitle(`y_var') c(l l l l l) ///
  		  				graphregion(fcolor(white)) legend(lab(1 `"`y_var'_1"') lab(2 `"`y_var'_2"') lab(3 `"`y_var'_3"') lab(4 `"`y_var'_4"') lab(5 `"`y_var'_5"')) `options'
				} 
				if `bynum'==6 {
  		  			twoway scatter y_disc1 y_disc2 y_disc3 y_disc4 y_disc5 y_disc6 x_disc, xtitle(`x_var') ytitle(`y_var') c(l l l l l l) ///
  		  				graphregion(fcolor(white)) ///
  		  				legend(lab(1 `"`y_var'_1"') lab(2 `"`y_var'_2"') lab(3 `"`y_var'_3"') lab(4 `"`y_var'_4"') lab(5 `"`y_var'_5"') lab(6 `"`y_var'_6"')) `options'
				} 
				if `bynum'==7 {
  		  			twoway scatter y_disc1 y_disc2 y_disc3 y_disc4 y_disc5 y_disc6 y_disc7 x_disc, xtitle(`x_var') ytitle(`y_var') c(l l l l l l l) ///
  		  				graphregion(fcolor(white))  `options' ///
  		  				legend(lab(1 `"`y_var'_1"') lab(2 `"`y_var'_2"') lab(3 `"`y_var'_3"') lab(4 `"`y_var'_4"') lab(5 `"`y_var'_5"') lab(6 `"`y_var'_6"') lab(7 `"`y_var'_7"'))
				} 
				if `bynum'==8 {
  		  			twoway scatter y_disc1 y_disc2 y_disc3 y_disc4 y_disc5 y_disc6 y_disc7 y_disc8 x_disc, xtitle(`x_var') ytitle(`y_var') c(l l l l l l l l) ///
  		  				graphregion(fcolor(white)) `options' ///
  		  				legend(lab(1 `"`y_var'_1"') lab(2 `"`y_var'_2"') lab(3 `"`y_var'_3"') lab(4 `"`y_var'_4"') lab(5 `"`y_var'_5"') lab(6 `"`y_var'_6"') ///
						lab(7 `"`y_var'_7"') lab(8`"`y_var'_8"'))
				} 
				if `bynum'==9 {
  		  			twoway scatter y_disc1 y_disc2 y_disc3 y_disc4 y_disc5 y_disc6 y_disc7 y_disc8 y_disc9 x_disc, xtitle(`x_var') ytitle(`y_var') c(l l l l l l l l l) ///
  		  				graphregion(fcolor(white)) `options' ///
		  				legend(lab(1 `"`y_var'_1"') lab(2 `"`y_var'_2"') lab(3 `"`y_var'_3"') lab(4 `"`y_var'_4"') lab(5 `"`y_var'_5"') lab(6 `"`y_var'_6"') ///
						lab(7 `"`y_var'_7"') lab(8 `"`y_var'_8"') lab(9 `"`y_var'_9"'))
				} 
				if `bynum'==10 {
  		  			twoway scatter y_disc1 y_disc2 y_disc3 y_disc4 y_disc5 y_disc6 y_disc7 y_disc8 y_disc9 y_disc10 x_disc, xtitle(`x_var') ytitle(`y_var') ///
  		  				graphregion(fcolor(white)) `options' c(l l l l l l l l l l) ///
		  				legend(lab(1 `"`y_var'_1"') lab(2 `"`y_var'_2"') lab(3 `"`y_var'_3"') lab(4 `"`y_var'_4"') lab(5 `"`y_var'_5"') lab(6 `"`y_var'_6"') ///
						lab(7 `"`y_var'_7"') lab(8 `"`y_var'_8"') lab(9 `"`y_var'_9"') lab(10 `"`y_var'_10"'))
				} 		
				if `"`saving'"'!=""{
  					quietly graph export `saving', replace
  				}

drop x_disc y_disc* k
	
end
