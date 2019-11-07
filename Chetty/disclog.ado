
capture program drop disclog
program define disclog
syntax using/

tempname myfile
local j=k

file open `myfile' using `"`using'"', read

	forval i=1/`j' {
		qui gen y_disc`i'=""
	}
	qui gen x_disc=""

	file read `myfile' line
	tokenize `line', parse(" ")

	while (`"`1'"'!="{hline" & r(eof)!=1) {
		file read `myfile' line
		tokenize `"`line'"', parse(" ")
	}

	file read `myfile' line
	tokenize `"`line'"', parse(" ")
	qui replace x_disc="`1'" if _n==1
	
	tokenize `"`line'"', parse("|}{res}")
		gettoken right 11: 11, parse(" ")
		qui replace y_disc1="`right'" if _n==1
		
		if `j'>=2 {
			forval i=2/`j' {
				gettoken next 11: 11, parse(" ")
				qui replace y_disc`i'="`next'" if _n==1
			}
		}

file close `myfile'

file open `myfile' using `"`using'"', read

	file read `myfile' line
	tokenize `line', parse(" ")

	while (`"`1'"'!="{hline" & r(eof)!=1) {
		file read `myfile' line
		tokenize `"`line'"', parse(" ")
	}

	file read `myfile' line
	file read `myfile' line
	tokenize `line', parse(" ")
	qui replace x_disc="`2'" if _n==2
	file read `myfile' line
	tokenize `line', parse(" ")
	
	local i=3
	while (`"`1'"'!="{hline 11" & `"`1'"'!="{txt}{hline") {
		cap replace x_disc="`2'" if _n==`i'
		file read `myfile' line
		tokenize `"`line'"', parse(" ")
		local i=`i'+1
	}

file close `myfile'

file open `myfile' using `"`using'"', read

	file read `myfile' line
	tokenize `line', parse(" ")

	while (`"`1'"'!="{hline" & r(eof)!=1) {
		file read `myfile' line
		tokenize `"`line'"', parse(" ")
	}
	file read `myfile' line
	file read `myfile' line
	tokenize `line', parse("|}{res}")
		gettoken right 14: 14, parse(" ")
		qui replace y_disc1="`right'" if _n==2
		
		if `j'>=2 {
			forval i=2/`j' {
				gettoken next 14: 14, parse(" ")
				qui replace y_disc`i'="`next'" if _n==2
			}
		}

	local h=3
	file read `myfile' line
	tokenize `line', parse(" ")

	while (`"`1'"'!="{txt}{hline" & r(eof)!=1) {
		tokenize `"`line'"', parse("|}{res}")
		gettoken right 14: 14, parse(" ")
		qui replace y_disc1="`right'" if _n==`h'
		if `j'>=2 {
			forval i=2/`j' {
				gettoken next 14: 14, parse(" ")
				qui replace y_disc`i'="`next'" if _n==`h'
			}
		}
		local h=`h'+1
		file read `myfile' line
		tokenize `"`line'"', parse(" ")
	}

file close `myfile'

qui destring x_disc, replace
forval i=1/`j' {
	cap destring y_disc`i', replace
}

end






