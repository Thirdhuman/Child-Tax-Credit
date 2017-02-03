setwd("~/Desktop/Poverty:Welfare/Data/R - Survey data/Current Population Survey (CPS)")

library(survey)				# load survey package (analyzes complex design surveys)
library(MonetDBLite)
library(DBI)			# load the DBI package (implements the R-database coding)

dbfolder <- paste0( getwd() , "/MonetDB" )

options( survey.replicates.mse = TRUE )


y <-
	svrepdesign(
		weights = ~marsupwt, 
		repweights = "pwwgt[1-9]", 
		type = "Fay", 
		rho = (1-1/sqrt(4)),
		data = "asec16" ,
		combined.weights = T ,
		dbtype = "MonetDBLite" ,
		dbname = dbfolder
	)

y$mse <- TRUE

f = function(x) if (x < 1000) exp(seq(log(x), log(1000), by=log(1.15))) else 1000
c = function(x) if (x < 2000) exp(seq(log(x), log(2000), by=log(1.45))) else 2000
t <-function(x){length(which(y$a_age < 5))} 

f = function(x) if (x < 1000) exp(seq(log(x), log(1000), by=log(1.15))) else 1000
c = function(x) if (x < 2000) exp(seq(log(x), log(2000), by=log(1.45))) else 2000



Split_Clinton_Plan <-subset( 
		y , 
			(spmu_numkids == 0 & spmu_resources - spmu_actc < spmu_povthreshold) | 
			(spmu_numkids == 1 &  fownu6 == 0 & spmu_resources - spmu_actc + pmin( 1*1000 + 0*2000, 1000) < spmu_povthreshold) | 
			(spmu_numkids == 2 &  fownu6 == 0 & spmu_resources - spmu_actc + pmin( 2*1000 + 0*2000, 2000) < spmu_povthreshold) | 
			(spmu_numkids == 3 &  fownu6 == 0 & spmu_resources - spmu_actc + pmin( 3*1000 + 0*2000, 3000) < spmu_povthreshold) | 
			(spmu_numkids == 4 &  fownu6 == 0 & spmu_resources - spmu_actc + pmin( 4*1000 + 0*2000, 4000) < spmu_povthreshold) | 
			(spmu_numkids == 5 &  fownu6 == 0 & spmu_resources - spmu_actc + pmin( 5*1000 + 0*2000, 5000) < spmu_povthreshold) | 
			(spmu_numkids == 6 &  fownu6 == 0 & spmu_resources - spmu_actc + pmin( 6*1000 + 0*2000, 6000) < spmu_povthreshold) | 
			(spmu_numkids == 7 &  fownu6 == 0 & spmu_resources - spmu_actc + pmin( 7*1000 + 0*2000, 7000) < spmu_povthreshold) | 
			(spmu_numkids == 8 &  fownu6 == 0 & spmu_resources - spmu_actc + pmin( 8*1000 + 0*2000, 8000) < spmu_povthreshold) | 
			(spmu_numkids == 9 &  fownu6 == 0 & spmu_resources - spmu_actc + pmin( 9*1000 + 0*2000, 9000) < spmu_povthreshold) | 
			(spmu_numkids == 10 &  fownu6 == 0 & spmu_resources - spmu_actc + pmin( 10*1000 + 0*2000, 10000) < spmu_povthreshold) | 
			(spmu_numkids == 11 &  fownu6 == 0 & spmu_resources - spmu_actc + pmin( 11*1000 + 0*2000, 11000) < spmu_povthreshold) | 
			(spmu_numkids == 12 &  fownu6 == 0 & spmu_resources - spmu_actc + pmin( 12*1000 + 0*2000, 12000) < spmu_povthreshold) | 
			(spmu_numkids == 13 &  fownu6 == 0 & spmu_resources - spmu_actc + pmin( 13*1000 + 0*2000, 13000) < spmu_povthreshold) | 
			(spmu_numkids == 14 &  fownu6 == 0 & spmu_resources - spmu_actc + pmin( 14*1000 + 0*2000, 14000) < spmu_povthreshold) | 
			(spmu_numkids == 15 &  fownu6 == 0 & spmu_resources - spmu_actc + pmin( 15*1000 + 0*2000, 15000) < spmu_povthreshold) | 
			(spmu_numkids == 16 &  fownu6 == 0 & spmu_resources - spmu_actc + pmin( 16*1000 + 0*2000, 16000) < spmu_povthreshold) | 
			(spmu_numkids == 17 &  fownu6 == 0 & spmu_resources - spmu_actc + pmin( 17*1000 + 0*2000, 17000) < spmu_povthreshold) | 
			(spmu_numkids == 18 &  fownu6 == 0 & spmu_resources - spmu_actc + pmin( 18*1000 + 0*2000, 18000) < spmu_povthreshold) | 
			(spmu_numkids == 19 &  fownu6 == 0 & spmu_resources - spmu_actc + pmin( 19*1000 + 0*2000, 19000) < spmu_povthreshold) |
			(spmu_numkids >= 20 &  fownu6 == 0 & spmu_resources - spmu_actc + pmin( 20*1000 + 0*2000, 20000) < spmu_povthreshold) |
			(spmu_numkids == 1 &  fownu6 == 1 & spmu_resources - spmu_actc + pmin( 1*1000 + 1*2000, 2000) < spmu_povthreshold) | 
			(spmu_numkids == 2 &  fownu6 == 1 & spmu_resources - spmu_actc + pmin( 2*1000 + 1*2000, 3000) < spmu_povthreshold) | 
			(spmu_numkids == 3 &  fownu6 == 1 & spmu_resources - spmu_actc + pmin( 3*1000 + 1*2000, 4000) < spmu_povthreshold) | 
			(spmu_numkids == 4 &  fownu6 == 1 & spmu_resources - spmu_actc + pmin( 4*1000 + 1*2000, 5000) < spmu_povthreshold) | 
			(spmu_numkids == 5 &  fownu6 == 1 & spmu_resources - spmu_actc + pmin( 5*1000 + 1*2000, 6000) < spmu_povthreshold) | 
			(spmu_numkids == 6 &  fownu6 == 1 & spmu_resources - spmu_actc + pmin( 6*1000 + 1*2000, 7000) < spmu_povthreshold) | 
			(spmu_numkids == 7 &  fownu6 == 1 & spmu_resources - spmu_actc + pmin( 7*1000 + 1*2000, 8000) < spmu_povthreshold) | 
			(spmu_numkids == 8 &  fownu6 == 1 & spmu_resources - spmu_actc + pmin( 8*1000 + 1*2000, 9000) < spmu_povthreshold) | 
			(spmu_numkids == 9 &  fownu6 == 1 & spmu_resources - spmu_actc + pmin( 9*1000 + 1*2000, 10000) < spmu_povthreshold) | 
			(spmu_numkids == 10 &  fownu6 == 1 & spmu_resources - spmu_actc + pmin( 10*1000 + 1*2000, 11000) < spmu_povthreshold) | 
			(spmu_numkids == 11 &  fownu6 == 1 & spmu_resources - spmu_actc + pmin( 11*1000 + 1*2000, 12000) < spmu_povthreshold) | 
			(spmu_numkids == 12 &  fownu6 == 1 & spmu_resources - spmu_actc + pmin( 12*1000 + 1*2000, 13000) < spmu_povthreshold) | 
			(spmu_numkids == 13 &  fownu6 == 1 & spmu_resources - spmu_actc + pmin( 13*1000 + 1*2000, 14000) < spmu_povthreshold) | 
			(spmu_numkids == 14 &  fownu6 == 1 & spmu_resources - spmu_actc + pmin( 14*1000 + 1*2000, 15000) < spmu_povthreshold) | 
			(spmu_numkids == 15 &  fownu6 == 1 & spmu_resources - spmu_actc + pmin( 15*1000 + 1*2000, 16000) < spmu_povthreshold) | 
			(spmu_numkids == 16 &  fownu6 == 1 & spmu_resources - spmu_actc + pmin( 16*1000 + 1*2000, 17000) < spmu_povthreshold) | 
			(spmu_numkids == 17 &  fownu6 == 1 & spmu_resources - spmu_actc + pmin( 17*1000 + 1*2000, 18000) < spmu_povthreshold) | 
			(spmu_numkids == 18 &  fownu6 == 1 & spmu_resources - spmu_actc + pmin( 18*1000 + 1*2000, 19000) < spmu_povthreshold) | 
			(spmu_numkids == 19 &  fownu6 == 1 & spmu_resources - spmu_actc + pmin( 19*1000 + 1*2000, 20000) < spmu_povthreshold) |
			(spmu_numkids == 20 &  fownu6 == 1 & spmu_resources - spmu_actc + pmin( 20*1000 + 1*2000, 21000) < spmu_povthreshold)	|		
			(spmu_numkids == 1 &  fownu6 == 2 & spmu_resources - spmu_actc + pmin( 1*1000 + 2*2000, 2000) < spmu_povthreshold) | 
			(spmu_numkids == 2 &  fownu6 == 2 & spmu_resources - spmu_actc + pmin( 2*1000 + 2*2000, 4000) < spmu_povthreshold) | 
			(spmu_numkids == 3 &  fownu6 == 2 & spmu_resources - spmu_actc + pmin( 3*1000 + 2*2000, 5000) < spmu_povthreshold) | 
			(spmu_numkids == 4 &  fownu6 == 2 & spmu_resources - spmu_actc + pmin( 4*1000 + 2*2000, 6000) < spmu_povthreshold) | 
			(spmu_numkids == 5 &  fownu6 == 2 & spmu_resources - spmu_actc + pmin( 5*1000 + 2*2000, 7000) < spmu_povthreshold) | 
			(spmu_numkids == 6 &  fownu6 == 2 & spmu_resources - spmu_actc + pmin( 6*1000 + 2*2000, 8000) < spmu_povthreshold) | 
			(spmu_numkids == 7 &  fownu6 == 2 & spmu_resources - spmu_actc + pmin( 7*1000 + 2*2000, 9000) < spmu_povthreshold) | 
			(spmu_numkids == 8 &  fownu6 == 2 & spmu_resources - spmu_actc + pmin( 8*1000 + 2*2000, 10000) < spmu_povthreshold) | 
			(spmu_numkids == 9 &  fownu6 == 2 & spmu_resources - spmu_actc + pmin( 9*1000 + 2*2000, 11000) < spmu_povthreshold) | 
			(spmu_numkids == 10 &  fownu6 == 2 & spmu_resources - spmu_actc + pmin( 10*1000 + 2*2000, 12000) < spmu_povthreshold) | 
			(spmu_numkids == 11 &  fownu6 == 2 & spmu_resources - spmu_actc + pmin( 11*1000 + 2*2000, 13000) < spmu_povthreshold) | 
			(spmu_numkids == 12 &  fownu6 == 2 & spmu_resources - spmu_actc + pmin( 12*1000 + 2*2000, 14000) < spmu_povthreshold) | 
			(spmu_numkids == 13 &  fownu6 == 2 & spmu_resources - spmu_actc + pmin( 13*1000 + 2*2000, 15000) < spmu_povthreshold) | 
			(spmu_numkids == 14 &  fownu6 == 2 & spmu_resources - spmu_actc + pmin( 14*1000 + 2*2000, 16000) < spmu_povthreshold) | 
			(spmu_numkids == 15 &  fownu6 == 2 & spmu_resources - spmu_actc + pmin( 15*1000 + 2*2000, 17000) < spmu_povthreshold) | 
			(spmu_numkids == 16 &  fownu6 == 2 & spmu_resources - spmu_actc + pmin( 16*1000 + 2*2000, 18000) < spmu_povthreshold) | 
			(spmu_numkids == 17 &  fownu6 == 2 & spmu_resources - spmu_actc + pmin( 17*1000 + 2*2000, 19000) < spmu_povthreshold) | 
			(spmu_numkids == 18 &  fownu6 == 2 & spmu_resources - spmu_actc + pmin( 18*1000 + 2*2000, 20000) < spmu_povthreshold) | 
			(spmu_numkids == 19 &  fownu6 == 2 & spmu_resources - spmu_actc + pmin( 19*1000 + 2*2000, 21000) < spmu_povthreshold) |
			(spmu_numkids == 20 &  fownu6 == 2 & spmu_resources - spmu_actc + pmin( 20*1000 + 2*2000, 22000) < spmu_povthreshold)	|	
			(spmu_numkids == 1 &  fownu6 == 3 & spmu_resources - spmu_actc + pmin( 1*1000 + 3*2000, 2000) < spmu_povthreshold) | 
			(spmu_numkids == 2 &  fownu6 == 3 & spmu_resources - spmu_actc + pmin( 2*1000 + 3*2000, 4000) < spmu_povthreshold) | 
			(spmu_numkids == 3 &  fownu6 == 3 & spmu_resources - spmu_actc + pmin( 3*1000 + 3*2000, 6000) < spmu_povthreshold) | 
			(spmu_numkids == 4 &  fownu6 == 3 & spmu_resources - spmu_actc + pmin( 4*1000 + 3*2000, 7000) < spmu_povthreshold) | 
			(spmu_numkids == 5 &  fownu6 == 3 & spmu_resources - spmu_actc + pmin( 5*1000 + 3*2000, 8000) < spmu_povthreshold) | 
			(spmu_numkids == 6 &  fownu6 == 3 & spmu_resources - spmu_actc + pmin( 6*1000 + 3*2000, 9000) < spmu_povthreshold) | 
			(spmu_numkids == 7 &  fownu6 == 3 & spmu_resources - spmu_actc + pmin( 7*1000 + 3*2000, 10000) < spmu_povthreshold) | 
			(spmu_numkids == 8 &  fownu6 == 3 & spmu_resources - spmu_actc + pmin( 8*1000 + 3*2000, 11000) < spmu_povthreshold) | 
			(spmu_numkids == 9 &  fownu6 == 3 & spmu_resources - spmu_actc + pmin( 9*1000 + 3*2000, 12000) < spmu_povthreshold) | 
			(spmu_numkids == 10 &  fownu6 == 3 & spmu_resources - spmu_actc + pmin( 10*1000 + 3*2000, 13000) < spmu_povthreshold) | 
			(spmu_numkids == 11 &  fownu6 == 3 & spmu_resources - spmu_actc + pmin( 11*1000 + 3*2000, 14000) < spmu_povthreshold) | 
			(spmu_numkids == 12 &  fownu6 == 3 & spmu_resources - spmu_actc + pmin( 12*1000 + 3*2000, 15000) < spmu_povthreshold) | 
			(spmu_numkids == 13 &  fownu6 == 3 & spmu_resources - spmu_actc + pmin( 13*1000 + 3*2000, 16000) < spmu_povthreshold) | 
			(spmu_numkids == 14 &  fownu6 == 3 & spmu_resources - spmu_actc + pmin( 14*1000 + 3*2000, 17000) < spmu_povthreshold) | 
			(spmu_numkids == 15 &  fownu6 == 3 & spmu_resources - spmu_actc + pmin( 15*1000 + 3*2000, 18000) < spmu_povthreshold) | 
			(spmu_numkids == 16 &  fownu6 == 3 & spmu_resources - spmu_actc + pmin( 16*1000 + 3*2000, 19000) < spmu_povthreshold) | 
			(spmu_numkids == 17 &  fownu6 == 3 & spmu_resources - spmu_actc + pmin( 17*1000 + 3*2000, 20000) < spmu_povthreshold) | 
			(spmu_numkids == 18 &  fownu6 == 3 & spmu_resources - spmu_actc + pmin( 18*1000 + 3*2000, 21000) < spmu_povthreshold) | 
			(spmu_numkids == 19 &  fownu6 == 3 & spmu_resources - spmu_actc + pmin( 19*1000 + 3*2000, 22000) < spmu_povthreshold) |
			(spmu_numkids == 20 &  fownu6 == 3 & spmu_resources - spmu_actc + pmin( 20*1000 + 3*2000, 23000) < spmu_povthreshold)	|	
			(spmu_numkids == 1 &  fownu6 == 4 & spmu_resources - spmu_actc + pmin( 1*1000 + 4*2000, 2000) < spmu_povthreshold) | 
			(spmu_numkids == 2 &  fownu6 == 4 & spmu_resources - spmu_actc + pmin( 2*1000 + 4*2000, 4000) < spmu_povthreshold) | 
			(spmu_numkids == 3 &  fownu6 == 4 & spmu_resources - spmu_actc + pmin( 3*1000 + 4*2000, 6000) < spmu_povthreshold) | 
			(spmu_numkids == 4 &  fownu6 == 4 & spmu_resources - spmu_actc + pmin( 4*1000 + 4*2000, 8000) < spmu_povthreshold) | 
			(spmu_numkids == 5 &  fownu6 == 4 & spmu_resources - spmu_actc + pmin( 5*1000 + 4*2000, 9000) < spmu_povthreshold) | 
			(spmu_numkids == 6 &  fownu6 == 4 & spmu_resources - spmu_actc + pmin( 6*1000 + 4*2000, 10000) < spmu_povthreshold) | 
			(spmu_numkids == 7 &  fownu6 == 4 & spmu_resources - spmu_actc + pmin( 7*1000 + 4*2000, 11000) < spmu_povthreshold) | 
			(spmu_numkids == 8 &  fownu6 == 4 & spmu_resources - spmu_actc + pmin( 8*1000 + 4*2000, 12000) < spmu_povthreshold) | 
			(spmu_numkids == 9 &  fownu6 == 4 & spmu_resources - spmu_actc + pmin( 9*1000 + 4*2000, 13000) < spmu_povthreshold) | 
			(spmu_numkids == 10 &  fownu6 == 4 & spmu_resources - spmu_actc + pmin( 10*1000 + 4*2000, 14000) < spmu_povthreshold) | 
			(spmu_numkids == 11 &  fownu6 == 4 & spmu_resources - spmu_actc + pmin( 11*1000 + 4*2000, 15000) < spmu_povthreshold) | 
			(spmu_numkids == 12 &  fownu6 == 4 & spmu_resources - spmu_actc + pmin( 12*1000 + 4*2000, 16000) < spmu_povthreshold) | 
			(spmu_numkids == 13 &  fownu6 == 4 & spmu_resources - spmu_actc + pmin( 13*1000 + 4*2000, 17000) < spmu_povthreshold) | 
			(spmu_numkids == 14 &  fownu6 == 4 & spmu_resources - spmu_actc + pmin( 14*1000 + 4*2000, 18000) < spmu_povthreshold) | 
			(spmu_numkids == 15 &  fownu6 == 4 & spmu_resources - spmu_actc + pmin( 15*1000 + 4*2000, 19000) < spmu_povthreshold) | 
			(spmu_numkids == 16 &  fownu6 == 4 & spmu_resources - spmu_actc + pmin( 16*1000 + 4*2000, 20000) < spmu_povthreshold) | 
			(spmu_numkids == 17 &  fownu6 == 4 & spmu_resources - spmu_actc + pmin( 17*1000 + 4*2000, 21000) < spmu_povthreshold) | 
			(spmu_numkids == 18 &  fownu6 == 4 & spmu_resources - spmu_actc + pmin( 18*1000 + 4*2000, 22000) < spmu_povthreshold) | 
			(spmu_numkids == 19 &  fownu6 == 4 & spmu_resources - spmu_actc + pmin( 19*1000 + 4*2000, 23000) < spmu_povthreshold) |
			(spmu_numkids == 20 &  fownu6 == 4 & spmu_resources - spmu_actc + pmin( 20*1000 + 4*2000, 24000) < spmu_povthreshold)	|				
			(spmu_numkids == 1 &  fownu6 == 5 & spmu_resources - spmu_actc + pmin( 1*1000 + 5*2000, 2000) < spmu_povthreshold) | 
			(spmu_numkids == 2 &  fownu6 == 5 & spmu_resources - spmu_actc + pmin( 2*1000 + 5*2000, 4000) < spmu_povthreshold) | 
			(spmu_numkids == 3 &  fownu6 == 5 & spmu_resources - spmu_actc + pmin( 3*1000 + 5*2000, 6000) < spmu_povthreshold) | 
			(spmu_numkids == 4 &  fownu6 == 5 & spmu_resources - spmu_actc + pmin( 4*1000 + 5*2000, 8000) < spmu_povthreshold) | 
			(spmu_numkids == 5 &  fownu6 == 5 & spmu_resources - spmu_actc + pmin( 5*1000 + 5*2000, 10000) < spmu_povthreshold) | 
			(spmu_numkids == 6 &  fownu6 == 5 & spmu_resources - spmu_actc + pmin( 6*1000 + 5*2000, 11000) < spmu_povthreshold) | 
			(spmu_numkids == 7 &  fownu6 == 5 & spmu_resources - spmu_actc + pmin( 7*1000 + 5*2000, 12000) < spmu_povthreshold) | 
			(spmu_numkids == 8 &  fownu6 == 5 & spmu_resources - spmu_actc + pmin( 8*1000 + 5*2000, 13000) < spmu_povthreshold) | 
			(spmu_numkids == 9 &  fownu6 == 5 & spmu_resources - spmu_actc + pmin( 9*1000 + 5*2000, 14000) < spmu_povthreshold) | 
			(spmu_numkids == 10 &  fownu6 == 5 & spmu_resources - spmu_actc + pmin( 10*1000 + 5*2000, 15000) < spmu_povthreshold) | 
			(spmu_numkids == 11 &  fownu6 == 5 & spmu_resources - spmu_actc + pmin( 11*1000 + 5*2000, 16000) < spmu_povthreshold) | 
			(spmu_numkids == 12 &  fownu6 == 5 & spmu_resources - spmu_actc + pmin( 12*1000 + 5*2000, 17000) < spmu_povthreshold) | 
			(spmu_numkids == 13 &  fownu6 == 5 & spmu_resources - spmu_actc + pmin( 13*1000 + 5*2000, 18000) < spmu_povthreshold) | 
			(spmu_numkids == 14 &  fownu6 == 5 & spmu_resources - spmu_actc + pmin( 14*1000 + 5*2000, 19000) < spmu_povthreshold) | 
			(spmu_numkids == 15 &  fownu6 == 5 & spmu_resources - spmu_actc + pmin( 15*1000 + 5*2000, 20000) < spmu_povthreshold) | 
			(spmu_numkids == 16 &  fownu6 == 5 & spmu_resources - spmu_actc + pmin( 16*1000 + 5*2000, 21000) < spmu_povthreshold) | 
			(spmu_numkids == 17 &  fownu6 == 5 & spmu_resources - spmu_actc + pmin( 17*1000 + 5*2000, 22000) < spmu_povthreshold) | 
			(spmu_numkids == 18 &  fownu6 == 5 & spmu_resources - spmu_actc + pmin( 18*1000 + 5*2000, 23000) < spmu_povthreshold) | 
			(spmu_numkids == 19 &  fownu6 == 5 & spmu_resources - spmu_actc + pmin( 19*1000 + 5*2000, 24000) < spmu_povthreshold) |
			(spmu_numkids == 20 &  fownu6 == 5 & spmu_resources - spmu_actc + pmin( 20*1000 + 5*2000, 25000) < spmu_povthreshold)	|	
			(spmu_numkids == 1 &  fownu6 == 6 & spmu_resources - spmu_actc + pmin( 1*1000 + 6*2000, 2000) < spmu_povthreshold) | 
			(spmu_numkids == 2 &  fownu6 == 6 & spmu_resources - spmu_actc + pmin( 2*1000 + 6*2000, 4000) < spmu_povthreshold) | 
			(spmu_numkids == 3 &  fownu6 == 6 & spmu_resources - spmu_actc + pmin( 3*1000 + 6*2000, 6000) < spmu_povthreshold) | 
			(spmu_numkids == 4 &  fownu6 == 6 & spmu_resources - spmu_actc + pmin( 4*1000 + 6*2000, 8000) < spmu_povthreshold) | 
			(spmu_numkids == 5 &  fownu6 == 6 & spmu_resources - spmu_actc + pmin( 5*1000 + 6*2000, 10000) < spmu_povthreshold) | 
			(spmu_numkids == 6 &  fownu6 == 6 & spmu_resources - spmu_actc + pmin( 6*1000 + 6*2000, 12000) < spmu_povthreshold) | 
			(spmu_numkids == 7 &  fownu6 == 6 & spmu_resources - spmu_actc + pmin( 7*1000 + 6*2000, 13000) < spmu_povthreshold) | 
			(spmu_numkids == 8 &  fownu6 == 6 & spmu_resources - spmu_actc + pmin( 8*1000 + 6*2000, 14000) < spmu_povthreshold) | 
			(spmu_numkids == 9 &  fownu6 == 6 & spmu_resources - spmu_actc + pmin( 9*1000 + 6*2000, 15000) < spmu_povthreshold) | 
			(spmu_numkids == 10 &  fownu6 == 6 & spmu_resources - spmu_actc + pmin( 10*1000 + 6*2000, 16000) < spmu_povthreshold) | 
			(spmu_numkids == 11 &  fownu6 == 6 & spmu_resources - spmu_actc + pmin( 11*1000 + 6*2000, 17000) < spmu_povthreshold) | 
			(spmu_numkids == 12 &  fownu6 == 6 & spmu_resources - spmu_actc + pmin( 12*1000 + 6*2000, 18000) < spmu_povthreshold) | 
			(spmu_numkids == 13 &  fownu6 == 6 & spmu_resources - spmu_actc + pmin( 13*1000 + 6*2000, 19000) < spmu_povthreshold) | 
			(spmu_numkids == 14 &  fownu6 == 6 & spmu_resources - spmu_actc + pmin( 14*1000 + 6*2000, 20000) < spmu_povthreshold) | 
			(spmu_numkids == 15 &  fownu6 == 6 & spmu_resources - spmu_actc + pmin( 15*1000 + 6*2000, 21000) < spmu_povthreshold) | 
			(spmu_numkids == 16 &  fownu6 == 6 & spmu_resources - spmu_actc + pmin( 16*1000 + 6*2000, 22000) < spmu_povthreshold) | 
			(spmu_numkids == 17 &  fownu6 == 6 & spmu_resources - spmu_actc + pmin( 17*1000 + 6*2000, 23000) < spmu_povthreshold) | 
			(spmu_numkids == 18 &  fownu6 == 6 & spmu_resources - spmu_actc + pmin( 18*1000 + 6*2000, 24000) < spmu_povthreshold) | 
			(spmu_numkids == 19 &  fownu6 == 6 & spmu_resources - spmu_actc + pmin( 19*1000 + 6*2000, 25000) < spmu_povthreshold) |
			(spmu_numkids == 20 &  fownu6 == 6 & spmu_resources - spmu_actc + pmin( 20*1000 + 6*2000, 26000) < spmu_povthreshold))

svytotal(
	~one ,
	Split_Clinton_Plan)

svytotal(
	~one ,
	Clinton_Plan)



Pov_wSplit <-subset( 
		y , 
			(spmu_numkids == 0 & spmu_resources < spmu_povthreshold) | 
			(spmu_numkids == 1 & (spmu_resources - spmu_actc + (1000*2) + u(1000) < spmu_povthreshold) | 
			
			(spmu_numkids == 2 & (spmu_resources - spmu_actc + 2000*2) < spmu_povthreshold) | 
			(spmu_numkids == 3 & (spmu_resources - spmu_actc + 3000*2) < spmu_povthreshold) | 
			(spmu_numkids == 3 & (spmu_resources - spmu_actc + 3000*2) < spmu_povthreshold) | 
			(spmu_numkids == 4 & (spmu_resources - spmu_actc + 4000*2) < spmu_povthreshold) | 
			(spmu_numkids == 5 & (spmu_resources - spmu_actc + 5000*2) < spmu_povthreshold) | 
			(spmu_numkids == 6 & (spmu_resources - spmu_actc + 6000*2) < spmu_povthreshold) | 
			(spmu_numkids == 7 & (spmu_resources - spmu_actc + 7000*2) < spmu_povthreshold) | 
			(spmu_numkids == 8 & (spmu_resources - spmu_actc + 8000*2) < spmu_povthreshold) | 
			(spmu_numkids == 9 & (spmu_resources - spmu_actc + 9000*2) < spmu_povthreshold) | 
			(spmu_numkids == 10 & (spmu_resources - spmu_actc + 10000*2) < spmu_povthreshold) | 
			(spmu_numkids == 11 & (spmu_resources - spmu_actc + 11000*2) < spmu_povthreshold) | 
			(spmu_numkids == 12 & (spmu_resources - spmu_actc + 12000*2) < spmu_povthreshold) | 
			(spmu_numkids == 13 & (spmu_resources - spmu_actc + 13000*2) < spmu_povthreshold) | 
			(spmu_numkids == 14 & (spmu_resources - spmu_actc + 14000*2) < spmu_povthreshold) | 
			(spmu_numkids == 15 & (spmu_resources - spmu_actc + 15000*2) < spmu_povthreshold) | 
			(spmu_numkids == 16 & (spmu_resources - spmu_actc + 16000*2) < spmu_povthreshold) | 
			(spmu_numkids == 17 & (spmu_resources - spmu_actc + 17000*2) < spmu_povthreshold) | 
			(spmu_numkids == 18 & (spmu_resources - spmu_actc + 18000*2) < spmu_povthreshold) | 
			(spmu_numkids == 19 & (spmu_resources - spmu_actc + 19000*2) < spmu_povthreshold) |
			(spmu_numkids >= 20 & (spmu_resources - spmu_actc + 20000*2) < spmu_povthreshold))

		
u <-function(x){length(which(y$a_age < 5))} 
o <-function(x){length(which(y$a_age >= 5))} 

f = function(x) if (x < 1000) exp(seq(log(x), log(1000), by=log(1.15))) else 1000
c = function(x) if (x < 2000) exp(seq(log(x), log(2000), by=log(1.45))) else 2000


		Pov_wSplit <-subset( 
		y , 
			(spmu_numkids == 0 & spmu_resources - spmu_actc < spmu_povthreshold) | 
			(spmu_numkids == 1 & u(unique(h_seq)) == 0 & o(unique(h_seq)) == 1 & spmu_resources - spmu_actc + 1000 < spmu_povthreshold) | 
			(spmu_numkids == 2 & u(unique(h_seq)) == 0 & o(unique(h_seq)) == 2 & spmu_resources - spmu_actc + 2000 < spmu_povthreshold) | 
			(spmu_numkids == 3 & u(unique(h_seq)) == 0 & o(unique(h_seq)) == 3 & spmu_resources - spmu_actc + 3000 < spmu_povthreshold) | 
			(spmu_numkids == 4 & u(unique(h_seq)) == 0 & o(unique(h_seq)) == 4 & spmu_resources - spmu_actc + 4000 < spmu_povthreshold) | 
			(spmu_numkids == 5 & u(unique(h_seq)) == 0 & o(unique(h_seq)) == 5 & spmu_resources - spmu_actc + 5000 < spmu_povthreshold) | 
			(spmu_numkids == 6 & u(unique(h_seq)) == 0 & o(unique(h_seq)) == 6 & spmu_resources - spmu_actc + 6000 < spmu_povthreshold) | 
			(spmu_numkids == 7 & u(unique(h_seq)) == 0 & o(unique(h_seq)) == 7 & spmu_resources - spmu_actc + 7000 < spmu_povthreshold) | 
			(spmu_numkids == 8 & u(unique(h_seq)) == 0 & o(unique(h_seq)) == 8 & spmu_resources - spmu_actc + 8000 < spmu_povthreshold) | 
			(spmu_numkids == 9 & u(unique(h_seq)) == 0 & o(unique(h_seq)) == 9 & spmu_resources - spmu_actc + 9000 < spmu_povthreshold) | 
			(spmu_numkids == 10 & u(unique(h_seq)) == 0 & o(unique(h_seq)) == 10 & spmu_resources - spmu_actc +  10000 < spmu_povthreshold) | 
			(spmu_numkids == 11 & u(unique(h_seq)) == 0 & o(unique(h_seq)) == 11 & spmu_resources - spmu_actc +  11000 < spmu_povthreshold) | 
			(spmu_numkids == 12 &  u(unique(h_seq)) == 0 & o(unique(h_seq)) == 12 & spmu_resources - spmu_actc +  12000 < spmu_povthreshold) | 
			(spmu_numkids == 13 &  u(unique(h_seq)) == 0 & o(unique(h_seq)) == 13 & spmu_resources - spmu_actc +  13000 < spmu_povthreshold) | 
			(spmu_numkids == 14 &  u(unique(h_seq)) == 0 & o(unique(h_seq)) == 14 & spmu_resources - spmu_actc +  14000 < spmu_povthreshold) | 
			(spmu_numkids == 15 &  u(unique(h_seq)) == 0 & o(unique(h_seq)) == 15 & spmu_resources - spmu_actc +  15000 < spmu_povthreshold) | 
			(spmu_numkids == 16 &  u(unique(h_seq)) == 0 & o(unique(h_seq)) == 16 & spmu_resources - spmu_actc +  16000 < spmu_povthreshold) | 
			(spmu_numkids == 17 &  u(unique(h_seq)) == 0 & o(unique(h_seq)) == 17 & spmu_resources - spmu_actc +  17000 < spmu_povthreshold) | 
			(spmu_numkids == 18 &  u(unique(h_seq)) == 0 & o(unique(h_seq)) == 18 & spmu_resources - spmu_actc +  18000 < spmu_povthreshold) | 
			(spmu_numkids == 19 &  u(unique(h_seq)) == 0 & o(unique(h_seq)) == 19 & spmu_resources - spmu_actc +  19000 < spmu_povthreshold) |
			(spmu_numkids >= 20 &  u(unique(h_seq)) == 0 & o(unique(h_seq)) == 20 & spmu_resources - spmu_actc +  20000 < spmu_povthreshold) |
			(spmu_numkids == 1 &  u(unique(h_seq)) == 1 & o(unique(h_seq)) == 0 & spmu_resources - spmu_actc +  2000 < spmu_povthreshold) | 
			(spmu_numkids == 2 &  u(unique(h_seq)) == 2 & o(unique(h_seq)) == 0 & spmu_resources - spmu_actc +  4000 < spmu_povthreshold) | 
			(spmu_numkids == 3 &  u(unique(h_seq)) == 3 & o(unique(h_seq)) == 0 & spmu_resources - spmu_actc +  6000 < spmu_povthreshold) | 
			(spmu_numkids == 4 &  u(unique(h_seq)) == 4 & o(unique(h_seq)) == 0 & spmu_resources - spmu_actc +  8000 < spmu_povthreshold) | 
			(spmu_numkids == 5 &  u(unique(h_seq)) == 5 & o(unique(h_seq)) == 0 & spmu_resources - spmu_actc +  10000 < spmu_povthreshold) | 
			(spmu_numkids == 6 &  u(unique(h_seq)) == 6 & o(unique(h_seq)) == 0 & spmu_resources - spmu_actc +  12000 < spmu_povthreshold) | 
			(spmu_numkids == 7 &  u(unique(h_seq)) == 7 & o(unique(h_seq)) == 0 & spmu_resources - spmu_actc +  14000 < spmu_povthreshold) | 
			(spmu_numkids == 8 &  u(unique(h_seq)) == 8 & o(unique(h_seq)) == 0 & spmu_resources - spmu_actc +  16000 < spmu_povthreshold) | 
			(spmu_numkids == 9 &  u(unique(h_seq)) == 9 & o(unique(h_seq)) == 0 & spmu_resources - spmu_actc +  18000 < spmu_povthreshold) | 
			(spmu_numkids == 10 &  u(unique(h_seq)) == 10 & o(unique(h_seq)) == 0 & spmu_resources - spmu_actc + 22000 < spmu_povthreshold) | 
			(spmu_numkids == 11 &  u(unique(h_seq)) == 11 & o(unique(h_seq)) == 0 & spmu_resources - spmu_actc + 24000 < spmu_povthreshold) | 
			(spmu_numkids == 12 &  u(unique(h_seq)) == 12 & o(unique(h_seq)) == 0 & spmu_resources - spmu_actc + 26000 < spmu_povthreshold) | 
			(spmu_numkids == 13 &  u(unique(h_seq)) == 13 & o(unique(h_seq)) == 0 & spmu_resources - spmu_actc + 28000 < spmu_povthreshold) | 
			(spmu_numkids == 14 &  u(unique(h_seq)) == 14 & o(unique(h_seq)) == 0 & spmu_resources - spmu_actc + 30000 < spmu_povthreshold) | 
			(spmu_numkids == 15 &  u(unique(h_seq)) == 15 & o(unique(h_seq)) == 0 & spmu_resources - spmu_actc + 30000 < spmu_povthreshold) | 
			(spmu_numkids == 16 &  u(unique(h_seq)) == 16 & o(unique(h_seq)) == 0 & spmu_resources - spmu_actc + 30000 < spmu_povthreshold) | 
			(spmu_numkids == 17 &  u(unique(h_seq)) == 17 & o(unique(h_seq)) == 0 & spmu_resources - spmu_actc + 30000 < spmu_povthreshold) | 
			(spmu_numkids == 18 &  u(unique(h_seq)) == 18 & o(unique(h_seq)) == 0 & spmu_resources - spmu_actc + 30000 < spmu_povthreshold) | 
			(spmu_numkids == 19 &  u(unique(h_seq)) == 19 & o(unique(h_seq)) == 0 & spmu_resources - spmu_actc + 30000 < spmu_povthreshold) |
			(spmu_numkids == 20 &  u(unique(h_seq)) == 20 & o(unique(h_seq)) == 0 & spmu_resources - spmu_actc + 30000 < spmu_povthreshold)	|		
			(spmu_numkids == 2 &  o(unique(h_seq)) == 1 & u(unique(h_seq)) == 1 & spmu_resources - spmu_actc + 2000 < spmu_povthreshold) | 
			(spmu_numkids == 3 &  o(unique(h_seq)) == 2 & u(unique(h_seq)) == 1 & spmu_resources - spmu_actc + 4000 < spmu_povthreshold) | 
			(spmu_numkids == 4 &  o(unique(h_seq)) == 3 & u(unique(h_seq)) == 1 & spmu_resources - spmu_actc + 5000 < spmu_povthreshold) | 
			(spmu_numkids == 5 &  o(unique(h_seq)) == 4 & u(unique(h_seq)) == 1 & spmu_resources - spmu_actc + 6000 < spmu_povthreshold) | 
			(spmu_numkids == 6 &  o(unique(h_seq)) == 5 & u(unique(h_seq)) == 1 & spmu_resources - spmu_actc + 7000 < spmu_povthreshold) | 
			(spmu_numkids == 7 &  o(unique(h_seq)) == 6 & u(unique(h_seq)) == 1 & spmu_resources - spmu_actc + 8000 < spmu_povthreshold) | 
			(spmu_numkids == 8 &  o(unique(h_seq)) == 7 & u(unique(h_seq)) == 1 & spmu_resources - spmu_actc + 9000 < spmu_povthreshold) | 
			(spmu_numkids == 9 &  o(unique(h_seq)) == 8 & u(unique(h_seq)) == 1 & spmu_resources - spmu_actc + 10000 < spmu_povthreshold) | 
			(spmu_numkids == 10 &  o(unique(h_seq)) == 9 & u(unique(h_seq)) == 1 & spmu_resources - spmu_actc + 11000 < spmu_povthreshold) | 
			(spmu_numkids == 11 &  o(unique(h_seq)) == 10 & u(unique(h_seq)) == 1 & spmu_resources - spmu_actc + 12000 < spmu_povthreshold) | 
			(spmu_numkids == 12 &  o(unique(h_seq)) == 11 & u(unique(h_seq)) == 1 & spmu_resources - spmu_actc + 13000 < spmu_povthreshold) | 
			(spmu_numkids == 13 &  o(unique(h_seq)) == 12 & u(unique(h_seq)) == 1 & spmu_resources - spmu_actc + 14000 < spmu_povthreshold) | 
			(spmu_numkids == 14 &  o(unique(h_seq)) == 13 & u(unique(h_seq)) == 1 & spmu_resources - spmu_actc + 15000 < spmu_povthreshold) | 
			(spmu_numkids == 15 &  o(unique(h_seq)) == 14 & u(unique(h_seq)) == 1 & spmu_resources - spmu_actc + 16000 < spmu_povthreshold) | 
			(spmu_numkids == 16 &  o(unique(h_seq)) == 15 & u(unique(h_seq)) == 1 & spmu_resources - spmu_actc + 17000 < spmu_povthreshold) | 
			(spmu_numkids == 17 &  o(unique(h_seq)) == 16 & u(unique(h_seq)) == 1 & spmu_resources - spmu_actc + 18000 < spmu_povthreshold) | 
			(spmu_numkids == 18 &  o(unique(h_seq)) == 17 & u(unique(h_seq)) == 1 & spmu_resources - spmu_actc + 19000 < spmu_povthreshold) | 
			(spmu_numkids == 19 &  o(unique(h_seq)) == 18 & u(unique(h_seq)) == 1 & spmu_resources - spmu_actc + 20000 < spmu_povthreshold) |
			(spmu_numkids == 20 &  o(unique(h_seq)) == 19 & u(unique(h_seq)) == 1 & spmu_resources - spmu_actc + 21000 < spmu_povthreshold)	|	
			(spmu_numkids == 3 &  o(unique(h_seq)) == 1 & u(unique(h_seq)) == 2 & spmu_resources - spmu_actc + 5000 < spmu_povthreshold) | 
			(spmu_numkids == 4 &  o(unique(h_seq)) == 2 & u(unique(h_seq)) == 2 & spmu_resources - spmu_actc + 6000 < spmu_povthreshold) | 
			(spmu_numkids == 5 &  o(unique(h_seq)) == 3 & u(unique(h_seq)) == 2 & spmu_resources - spmu_actc + 7000 < spmu_povthreshold) | 
			(spmu_numkids == 6 &  o(unique(h_seq)) == 4 & u(unique(h_seq)) == 2 & spmu_resources - spmu_actc + 8000 < spmu_povthreshold) | 
			(spmu_numkids == 7 &  o(unique(h_seq)) == 5 & u(unique(h_seq)) == 2 & spmu_resources - spmu_actc + 9000 < spmu_povthreshold) | 
			(spmu_numkids == 8 &  o(unique(h_seq)) == 6 & u(unique(h_seq)) == 2 & spmu_resources - spmu_actc + 10000 < spmu_povthreshold) | 
			(spmu_numkids == 9 &  o(unique(h_seq)) == 7 & u(unique(h_seq)) == 2 & spmu_resources - spmu_actc + 11000 < spmu_povthreshold) | 
			(spmu_numkids == 10 &  o(unique(h_seq)) == 8 & u(unique(h_seq)) == 2 & spmu_resources - spmu_actc + 12000 < spmu_povthreshold) | 
			(spmu_numkids == 11 &  o(unique(h_seq)) == 9 & u(unique(h_seq)) == 2 & spmu_resources - spmu_actc + 13000 < spmu_povthreshold) | 
			(spmu_numkids == 12 &  o(unique(h_seq)) == 10 & u(unique(h_seq)) == 2 & spmu_resources - spmu_actc + 14000 < spmu_povthreshold) | 
			(spmu_numkids == 13 &  o(unique(h_seq)) == 11 & u(unique(h_seq)) == 2 & spmu_resources - spmu_actc + 15000 < spmu_povthreshold) | 
			(spmu_numkids == 14 &  o(unique(h_seq)) == 12 & u(unique(h_seq)) == 2 & spmu_resources - spmu_actc + 16000 < spmu_povthreshold) | 
			(spmu_numkids == 15 &  o(unique(h_seq)) == 13 & u(unique(h_seq)) == 2 & spmu_resources - spmu_actc + 17000 < spmu_povthreshold) | 
			(spmu_numkids == 16 &  o(unique(h_seq)) == 14 & u(unique(h_seq)) == 2 & spmu_resources - spmu_actc + 18000 < spmu_povthreshold) | 
			(spmu_numkids == 17 &  o(unique(h_seq)) == 15 & u(unique(h_seq)) == 2 & spmu_resources - spmu_actc + 19000 < spmu_povthreshold) | 
			(spmu_numkids == 18 &  o(unique(h_seq)) == 16 & u(unique(h_seq)) == 2 & spmu_resources - spmu_actc + 20000 < spmu_povthreshold) | 
			(spmu_numkids == 19 &  o(unique(h_seq)) == 17 & u(unique(h_seq)) == 2 & spmu_resources - spmu_actc + 21000 < spmu_povthreshold) |
			(spmu_numkids == 20 &  o(unique(h_seq)) == 18 & u(unique(h_seq)) == 2 & spmu_resources - spmu_actc + 22000 < spmu_povthreshold)	|	
			(spmu_numkids == 4 &  o(unique(h_seq)) == 1 & u(unique(h_seq)) == 3 & spmu_resources - spmu_actc + 7000 < spmu_povthreshold) | 
			(spmu_numkids == 5 &  o(unique(h_seq)) == 2 & u(unique(h_seq)) == 3 & spmu_resources - spmu_actc + 8000 < spmu_povthreshold) | 
			(spmu_numkids == 6 &  o(unique(h_seq)) == 3 & u(unique(h_seq)) == 3 & spmu_resources - spmu_actc + 9000 < spmu_povthreshold) | 
			(spmu_numkids == 7 &  o(unique(h_seq)) == 4 & u(unique(h_seq)) == 3 & spmu_resources - spmu_actc + 10000 < spmu_povthreshold) | 
			(spmu_numkids == 8 &  o(unique(h_seq)) == 5 & u(unique(h_seq)) == 3 & spmu_resources - spmu_actc + 11000 < spmu_povthreshold) | 
			(spmu_numkids == 9 &  o(unique(h_seq)) == 6 & u(unique(h_seq)) == 3 & spmu_resources - spmu_actc + 12000 < spmu_povthreshold) | 
			(spmu_numkids == 10 &  o(unique(h_seq)) == 7 & u(unique(h_seq)) == 3 & spmu_resources - spmu_actc + 13000 < spmu_povthreshold) | 
			(spmu_numkids == 11 &  o(unique(h_seq)) == 8 & u(unique(h_seq)) == 3 & spmu_resources - spmu_actc + 14000 < spmu_povthreshold) | 
			(spmu_numkids == 12 &  o(unique(h_seq)) == 9 & u(unique(h_seq)) == 3 & spmu_resources - spmu_actc + 15000 < spmu_povthreshold) | 
			(spmu_numkids == 13 &  o(unique(h_seq)) == 10 & u(unique(h_seq)) == 3 & spmu_resources - spmu_actc + 16000 < spmu_povthreshold) | 
			(spmu_numkids == 14 &  o(unique(h_seq)) == 11 & u(unique(h_seq)) == 3 & spmu_resources - spmu_actc + 17000 < spmu_povthreshold) | 
			(spmu_numkids == 15 &  o(unique(h_seq)) == 12 & u(unique(h_seq)) == 3 & spmu_resources - spmu_actc + 18000 < spmu_povthreshold) | 
			(spmu_numkids == 16 &  o(unique(h_seq)) == 13 & u(unique(h_seq)) == 3 & spmu_resources - spmu_actc + 19000 < spmu_povthreshold) | 
			(spmu_numkids == 17 &  o(unique(h_seq)) == 14 & u(unique(h_seq)) == 3 & spmu_resources - spmu_actc + 20000 < spmu_povthreshold) | 
			(spmu_numkids == 18 &  o(unique(h_seq)) == 15 & u(unique(h_seq)) == 3 & spmu_resources - spmu_actc + 21000 < spmu_povthreshold) | 
			(spmu_numkids == 19 &  o(unique(h_seq)) == 16 & u(unique(h_seq)) == 3 & spmu_resources - spmu_actc + 22000 < spmu_povthreshold) |
			(spmu_numkids == 20 &  o(unique(h_seq)) == 17 & u(unique(h_seq)) == 3 & spmu_resources - spmu_actc + 23000 < spmu_povthreshold)	|				
			(spmu_numkids == 5 &  o(unique(h_seq)) == 1 & u(unique(h_seq)) == 4 & spmu_resources - spmu_actc + 9000 < spmu_povthreshold) | 
			(spmu_numkids == 6 &  o(unique(h_seq)) == 2 & u(unique(h_seq)) == 4 & spmu_resources - spmu_actc + 10000 < spmu_povthreshold) | 
			(spmu_numkids == 7 &  o(unique(h_seq)) == 3 & u(unique(h_seq)) == 4 & spmu_resources - spmu_actc + 11000 < spmu_povthreshold) | 
			(spmu_numkids == 8 &  o(unique(h_seq)) == 4 & u(unique(h_seq)) == 4 & spmu_resources - spmu_actc + 12000 < spmu_povthreshold) | 
			(spmu_numkids == 9 &  o(unique(h_seq)) == 5 & u(unique(h_seq)) == 4 & spmu_resources - spmu_actc + 13000 < spmu_povthreshold) | 
			(spmu_numkids == 10 &  o(unique(h_seq)) == 6 & u(unique(h_seq)) == 4 & spmu_resources - spmu_actc + 14000 < spmu_povthreshold) | 
			(spmu_numkids == 11 &  o(unique(h_seq)) == 7 & u(unique(h_seq)) == 4 & spmu_resources - spmu_actc + 15000 < spmu_povthreshold) | 
			(spmu_numkids == 12 &  o(unique(h_seq)) == 8 & u(unique(h_seq)) == 4 & spmu_resources - spmu_actc + 16000 < spmu_povthreshold) | 
			(spmu_numkids == 13 &  o(unique(h_seq)) == 9 & u(unique(h_seq)) == 4 & spmu_resources - spmu_actc + 17000 < spmu_povthreshold) | 
			(spmu_numkids == 14 &  o(unique(h_seq)) == 10 & u(unique(h_seq)) == 4 & spmu_resources - spmu_actc + 18000 < spmu_povthreshold) | 
			(spmu_numkids == 15 &  o(unique(h_seq)) == 11 & u(unique(h_seq)) == 4 & spmu_resources - spmu_actc + 19000 < spmu_povthreshold) | 
			(spmu_numkids == 16 &  o(unique(h_seq)) == 12 & u(unique(h_seq)) == 4 & spmu_resources - spmu_actc + 20000 < spmu_povthreshold) | 
			(spmu_numkids == 17 &  o(unique(h_seq)) == 13 & u(unique(h_seq)) == 4 & spmu_resources - spmu_actc + 21000 < spmu_povthreshold) | 
			(spmu_numkids == 18 &  o(unique(h_seq)) == 14 & u(unique(h_seq)) == 4 & spmu_resources - spmu_actc + 22000 < spmu_povthreshold) | 
			(spmu_numkids == 19 &  o(unique(h_seq)) == 15 & u(unique(h_seq)) == 4 & spmu_resources - spmu_actc + 23000 < spmu_povthreshold) |
			(spmu_numkids == 20 &  o(unique(h_seq)) == 16 & u(unique(h_seq)) == 4 & spmu_resources - spmu_actc + 24000 < spmu_povthreshold)	|	
			(spmu_numkids == 6 &  o(unique(h_seq)) == 1 & u(unique(h_seq)) == 5 & spmu_resources - spmu_actc + 11000 < spmu_povthreshold) | 
			(spmu_numkids == 7 &  o(unique(h_seq)) == 2 & u(unique(h_seq)) == 5 & spmu_resources - spmu_actc + 12000 < spmu_povthreshold) | 
			(spmu_numkids == 8 &  o(unique(h_seq)) == 3 & u(unique(h_seq)) == 5 & spmu_resources - spmu_actc + 13000 < spmu_povthreshold) | 
			(spmu_numkids == 9 &  o(unique(h_seq)) == 4 & u(unique(h_seq)) == 5 & spmu_resources - spmu_actc + 14000 < spmu_povthreshold) | 
			(spmu_numkids == 10 &  o(unique(h_seq)) == 5 & u(unique(h_seq)) == 5 & spmu_resources - spmu_actc + 15000 < spmu_povthreshold) | 
			(spmu_numkids == 11 &  o(unique(h_seq)) == 6 & u(unique(h_seq)) == 5 & spmu_resources - spmu_actc + 16000 < spmu_povthreshold) | 
			(spmu_numkids == 12 &  o(unique(h_seq)) == 7 & u(unique(h_seq)) == 5 & spmu_resources - spmu_actc + 17000 < spmu_povthreshold) | 
			(spmu_numkids == 13 &  o(unique(h_seq)) == 8 & u(unique(h_seq)) == 5 & spmu_resources - spmu_actc + 18000 < spmu_povthreshold) | 
			(spmu_numkids == 14 &  o(unique(h_seq)) == 9 & u(unique(h_seq)) == 5 & spmu_resources - spmu_actc + 19000 < spmu_povthreshold) | 
			(spmu_numkids == 15 &  o(unique(h_seq)) == 10 & u(unique(h_seq)) == 5 & spmu_resources - spmu_actc + 20000 < spmu_povthreshold) | 
			(spmu_numkids == 16 &  o(unique(h_seq)) == 11 & u(unique(h_seq)) == 5 & spmu_resources - spmu_actc + 21000 < spmu_povthreshold) | 
			(spmu_numkids == 17 &  o(unique(h_seq)) == 12 & u(unique(h_seq)) == 5 & spmu_resources - spmu_actc + 22000 < spmu_povthreshold) | 
			(spmu_numkids == 18 &  o(unique(h_seq)) == 13 & u(unique(h_seq)) == 5 & spmu_resources - spmu_actc + 23000 < spmu_povthreshold) | 
			(spmu_numkids == 19 &  o(unique(h_seq)) == 14 & u(unique(h_seq)) == 5 & spmu_resources - spmu_actc + 24000 < spmu_povthreshold) |
			(spmu_numkids == 20 &  o(unique(h_seq)) == 15 & u(unique(h_seq)) == 5 & spmu_resources - spmu_actc + 25000 < spmu_povthreshold))

svytotal(
	~one ,
	Pov_wSplit)

~hunder18 == 3 & hh5to18 == 3 & fownu6 == 0


Usplit_Clinton_Plan <-subset( 
		y , 
			(spmu_numkids == 0 & spmu_resources - spmu_actc < spmu_povthreshold) | 
			(spmu_numkids == 1 &  t(unique(h_seq)) == 0 & spmu_resources - spmu_actc + pmin( 1*(1000) + 0*(2000), 1000) < spmu_povthreshold) | 
			(spmu_numkids == 2 &  t(unique(h_seq)) == 0 & spmu_resources - spmu_actc + pmin( 2*(1000) + 0*(ftotval)*1.45, 2000) < spmu_povthreshold) | 
			(spmu_numkids == 3 &  t(unique(h_seq)) == 0 & spmu_resources - spmu_actc + pmin( 3*(1000) + 0*(ftotval)*1.45, 3000) < spmu_povthreshold) | 
			(spmu_numkids == 4 &  t(unique(h_seq)) == 0 & spmu_resources - spmu_actc + pmin( 4*(1000) + 0*(ftotval)*1.45, 4000) < spmu_povthreshold) | 
			(spmu_numkids == 5 &  t(unique(h_seq)) == 0 & spmu_resources - spmu_actc + pmin( 5*(1000) + 0*(ftotval)*1.45, 5000) < spmu_povthreshold) | 
			(spmu_numkids == 6 &  t(unique(h_seq)) == 0 & spmu_resources - spmu_actc + pmin( 6*(1000) + 0*(ftotval)*1.45, 6000) < spmu_povthreshold) | 
			(spmu_numkids == 7 &  t(unique(h_seq)) == 0 & spmu_resources - spmu_actc + pmin( 7*(1000) + 0*(ftotval)*1.45, 7000) < spmu_povthreshold) | 
			(spmu_numkids == 8 &  t(unique(h_seq)) == 0 & spmu_resources - spmu_actc + pmin( 8*(1000) + 0*(ftotval)*1.45, 8000) < spmu_povthreshold) | 
			(spmu_numkids == 9 &  t(unique(h_seq)) == 0 & spmu_resources - spmu_actc + pmin( 9*(1000) + 0*(ftotval)*1.45, 9000) < spmu_povthreshold) | 
			(spmu_numkids == 10 &  t(unique(h_seq)) == 0 & spmu_resources - spmu_actc + pmin( 10*(1000) + 0*(ftotval)*1.45, 10000) < spmu_povthreshold) | 
			(spmu_numkids == 11 &  t(unique(h_seq)) == 0 & spmu_resources - spmu_actc + pmin( 11*(1000) + 0*(ftotval)*1.45, 11000) < spmu_povthreshold) | 
			(spmu_numkids == 12 &  t(unique(h_seq)) == 0 & spmu_resources - spmu_actc + pmin( 12*(1000) + 0*(ftotval)*1.45, 12000) < spmu_povthreshold) | 
			(spmu_numkids == 13 &  t(unique(h_seq)) == 0 & spmu_resources - spmu_actc + pmin( 13*(1000) + 0*(ftotval)*1.45, 13000) < spmu_povthreshold) | 
			(spmu_numkids == 14 &  t(unique(h_seq)) == 0 & spmu_resources - spmu_actc + pmin( 14*(1000) + 0*(ftotval)*1.45, 14000) < spmu_povthreshold) | 
			(spmu_numkids == 15 &  t(unique(h_seq)) == 0 & spmu_resources - spmu_actc + pmin( 15*(1000) + 0*(ftotval)*1.45, 15000) < spmu_povthreshold) | 
			(spmu_numkids == 16 &  t(unique(h_seq)) == 0 & spmu_resources - spmu_actc + pmin( 16*(1000) + 0*(ftotval)*1.45, 16000) < spmu_povthreshold) | 
			(spmu_numkids == 17 &  t(unique(h_seq)) == 0 & spmu_resources - spmu_actc + pmin( 17*(1000) + 0*(ftotval)*1.45, 17000) < spmu_povthreshold) | 
			(spmu_numkids == 18 &  t(unique(h_seq)) == 0 & spmu_resources - spmu_actc + pmin( 18*(1000) + 0*(ftotval)*1.45, 18000) < spmu_povthreshold) | 
			(spmu_numkids == 19 &  t(unique(h_seq)) == 0 & spmu_resources - spmu_actc + pmin( 19*(1000) + 0*(ftotval)*1.45, 19000) < spmu_povthreshold) |
			(spmu_numkids >= 20 &  t(unique(h_seq)) == 0 & spmu_resources - spmu_actc + pmin( 20*(1000) + 0*(ftotval)*1.45, 20000) < spmu_povthreshold) |
			(spmu_numkids == 1 &  t(unique(h_seq)) == 1 & spmu_resources - spmu_actc + pmin( 1*(1000) + 1*(ftotval)*1.45, 2000) < spmu_povthreshold) | 
			(spmu_numkids == 2 &  t(unique(h_seq)) == 1 & spmu_resources - spmu_actc + pmin( 2*(1000) + 1*(ftotval)*1.45, 3000) < spmu_povthreshold) | 
			(spmu_numkids == 3 &  t(unique(h_seq)) == 1 & spmu_resources - spmu_actc + pmin( 3*(1000) + 1*(ftotval)*1.45, 4000) < spmu_povthreshold) | 
			(spmu_numkids == 4 &  t(unique(h_seq)) == 1 & spmu_resources - spmu_actc + pmin( 4*(1000) + 1*(ftotval)*1.45, 5000) < spmu_povthreshold) | 
			(spmu_numkids == 5 &  t(unique(h_seq)) == 1 & spmu_resources - spmu_actc + pmin( 5*(1000) + 1*(ftotval)*1.45, 6000) < spmu_povthreshold) | 
			(spmu_numkids == 6 &  t(unique(h_seq)) == 1 & spmu_resources - spmu_actc + pmin( 6*(1000) + 1*(ftotval)*1.45, 7000) < spmu_povthreshold) | 
			(spmu_numkids == 7 &  t(unique(h_seq)) == 1 & spmu_resources - spmu_actc + pmin( 7*(1000) + 1*(ftotval)*1.45, 8000) < spmu_povthreshold) | 
			(spmu_numkids == 8 &  t(unique(h_seq)) == 1 & spmu_resources - spmu_actc + pmin( 8*(1000) + 1*(ftotval)*1.45, 9000) < spmu_povthreshold) | 
			(spmu_numkids == 9 &  t(unique(h_seq)) == 1 & spmu_resources - spmu_actc + pmin( 9*(1000) + 1*(ftotval)*1.45, 10000) < spmu_povthreshold) | 
			(spmu_numkids == 10 &  t(unique(h_seq)) == 1 & spmu_resources - spmu_actc + pmin( 10*(1000) + 1*(ftotval)*1.45, 11000) < spmu_povthreshold) | 
			(spmu_numkids == 11 &  t(unique(h_seq)) == 1 & spmu_resources - spmu_actc + pmin( 11*(1000) + 1*(ftotval)*1.45, 12000) < spmu_povthreshold) | 
			(spmu_numkids == 12 &  t(unique(h_seq)) == 1 & spmu_resources - spmu_actc + pmin( 12*(1000) + 1*(ftotval)*1.45, 13000) < spmu_povthreshold) | 
			(spmu_numkids == 13 &  t(unique(h_seq)) == 1 & spmu_resources - spmu_actc + pmin( 13*(1000) + 1*(ftotval)*1.45, 14000) < spmu_povthreshold) | 
			(spmu_numkids == 14 &  t(unique(h_seq)) == 1 & spmu_resources - spmu_actc + pmin( 14*(1000) + 1*(ftotval)*1.45, 15000) < spmu_povthreshold) | 
			(spmu_numkids == 15 &  t(unique(h_seq)) == 1 & spmu_resources - spmu_actc + pmin( 15*(1000) + 1*(ftotval)*1.45, 16000) < spmu_povthreshold) | 
			(spmu_numkids == 16 &  t(unique(h_seq)) == 1 & spmu_resources - spmu_actc + pmin( 16*(1000) + 1*(ftotval)*1.45, 17000) < spmu_povthreshold) | 
			(spmu_numkids == 17 &  t(unique(h_seq)) == 1 & spmu_resources - spmu_actc + pmin( 17*(1000) + 1*(ftotval)*1.45, 18000) < spmu_povthreshold) | 
			(spmu_numkids == 18 &  t(unique(h_seq)) == 1 & spmu_resources - spmu_actc + pmin( 18*(1000) + 1*(ftotval)*1.45, 19000) < spmu_povthreshold) | 
			(spmu_numkids == 19 &  t(unique(h_seq)) == 1 & spmu_resources - spmu_actc + pmin( 19*(1000) + 1*(ftotval)*1.45, 20000) < spmu_povthreshold) |
			(spmu_numkids == 20 &  t(unique(h_seq)) == 1 & spmu_resources - spmu_actc + pmin( 20*(1000) + 1*(ftotval)*1.45, 21000) < spmu_povthreshold)	|		
			(spmu_numkids == 1 &  t(unique(h_seq)) == 2 & spmu_resources - spmu_actc + pmin( 1*(1000) + 2*(ftotval)*1.45, 2000) < spmu_povthreshold) | 
			(spmu_numkids == 2 &  t(unique(h_seq)) == 2 & spmu_resources - spmu_actc + pmin( 2*(1000) + 2*(ftotval)*1.45, 4000) < spmu_povthreshold) | 
			(spmu_numkids == 3 &  t(unique(h_seq)) == 2 & spmu_resources - spmu_actc + pmin( 3*(1000) + 2*(ftotval)*1.45, 5000) < spmu_povthreshold) | 
			(spmu_numkids == 4 &  t(unique(h_seq)) == 2 & spmu_resources - spmu_actc + pmin( 4*(1000) + 2*(ftotval)*1.45, 6000) < spmu_povthreshold) | 
			(spmu_numkids == 5 &  t(unique(h_seq)) == 2 & spmu_resources - spmu_actc + pmin( 5*(1000) + 2*(ftotval)*1.45, 7000) < spmu_povthreshold) | 
			(spmu_numkids == 6 &  t(unique(h_seq)) == 2 & spmu_resources - spmu_actc + pmin( 6*(1000) + 2*(ftotval)*1.45, 8000) < spmu_povthreshold) | 
			(spmu_numkids == 7 &  t(unique(h_seq)) == 2 & spmu_resources - spmu_actc + pmin( 7*(1000) + 2*(ftotval)*1.45, 9000) < spmu_povthreshold) | 
			(spmu_numkids == 8 &  t(unique(h_seq)) == 2 & spmu_resources - spmu_actc + pmin( 8*(1000) + 2*(ftotval)*1.45, 10000) < spmu_povthreshold) | 
			(spmu_numkids == 9 &  t(unique(h_seq)) == 2 & spmu_resources - spmu_actc + pmin( 9*(1000) + 2*(ftotval)*1.45, 11000) < spmu_povthreshold) | 
			(spmu_numkids == 10 &  t(unique(h_seq)) == 2 & spmu_resources - spmu_actc + pmin( 10*(1000) + 2*(ftotval)*1.45, 12000) < spmu_povthreshold) | 
			(spmu_numkids == 11 &  t(unique(h_seq)) == 2 & spmu_resources - spmu_actc + pmin( 11*(1000) + 2*(ftotval)*1.45, 13000) < spmu_povthreshold) | 
			(spmu_numkids == 12 &  t(unique(h_seq)) == 2 & spmu_resources - spmu_actc + pmin( 12*(1000) + 2*(ftotval)*1.45, 14000) < spmu_povthreshold) | 
			(spmu_numkids == 13 &  t(unique(h_seq)) == 2 & spmu_resources - spmu_actc + pmin( 13*(1000) + 2*(ftotval)*1.45, 15000) < spmu_povthreshold) | 
			(spmu_numkids == 14 &  t(unique(h_seq)) == 2 & spmu_resources - spmu_actc + pmin( 14*(1000) + 2*(ftotval)*1.45, 16000) < spmu_povthreshold) | 
			(spmu_numkids == 15 &  t(unique(h_seq)) == 2 & spmu_resources - spmu_actc + pmin( 15*(1000) + 2*(ftotval)*1.45, 17000) < spmu_povthreshold) | 
			(spmu_numkids == 16 &  t(unique(h_seq)) == 2 & spmu_resources - spmu_actc + pmin( 16*(1000) + 2*(ftotval)*1.45, 18000) < spmu_povthreshold) | 
			(spmu_numkids == 17 &  t(unique(h_seq)) == 2 & spmu_resources - spmu_actc + pmin( 17*(1000) + 2*(ftotval)*1.45, 19000) < spmu_povthreshold) | 
			(spmu_numkids == 18 &  t(unique(h_seq)) == 2 & spmu_resources - spmu_actc + pmin( 18*(1000) + 2*(ftotval)*1.45, 20000) < spmu_povthreshold) | 
			(spmu_numkids == 19 &  t(unique(h_seq)) == 2 & spmu_resources - spmu_actc + pmin( 19*(1000) + 2*(ftotval)*1.45, 21000) < spmu_povthreshold) |
			(spmu_numkids == 20 &  t(unique(h_seq)) == 2 & spmu_resources - spmu_actc + pmin( 20*(1000) + 2*(ftotval)*1.45, 22000) < spmu_povthreshold)	|	
			(spmu_numkids == 1 &  t(unique(h_seq)) == 3 & spmu_resources - spmu_actc + pmin( 1*(1000) + 3*(ftotval)*1.45, 2000) < spmu_povthreshold) | 
			(spmu_numkids == 2 &  t(unique(h_seq)) == 3 & spmu_resources - spmu_actc + pmin( 2*(1000) + 3*(ftotval)*1.45, 4000) < spmu_povthreshold) | 
			(spmu_numkids == 3 &  t(unique(h_seq)) == 3 & spmu_resources - spmu_actc + pmin( 3*(1000) + 3*(ftotval)*1.45, 6000) < spmu_povthreshold) | 
			(spmu_numkids == 4 &  t(unique(h_seq)) == 3 & spmu_resources - spmu_actc + pmin( 4*(1000) + 3*(ftotval)*1.45, 7000) < spmu_povthreshold) | 
			(spmu_numkids == 5 &  t(unique(h_seq)) == 3 & spmu_resources - spmu_actc + pmin( 5*(1000) + 3*(ftotval)*1.45, 8000) < spmu_povthreshold) | 
			(spmu_numkids == 6 &  t(unique(h_seq)) == 3 & spmu_resources - spmu_actc + pmin( 6*(1000) + 3*(ftotval)*1.45, 9000) < spmu_povthreshold) | 
			(spmu_numkids == 7 &  t(unique(h_seq)) == 3 & spmu_resources - spmu_actc + pmin( 7*(1000) + 3*(ftotval)*1.45, 10000) < spmu_povthreshold) | 
			(spmu_numkids == 8 &  t(unique(h_seq)) == 3 & spmu_resources - spmu_actc + pmin( 8*(1000) + 3*(ftotval)*1.45, 11000) < spmu_povthreshold) | 
			(spmu_numkids == 9 &  t(unique(h_seq)) == 3 & spmu_resources - spmu_actc + pmin( 9*(1000) + 3*(ftotval)*1.45, 12000) < spmu_povthreshold) | 
			(spmu_numkids == 10 &  t(unique(h_seq)) == 3 & spmu_resources - spmu_actc + pmin( 10*(1000) + 3*(ftotval)*1.45, 13000) < spmu_povthreshold) | 
			(spmu_numkids == 11 &  t(unique(h_seq)) == 3 & spmu_resources - spmu_actc + pmin( 11*(1000) + 3*(ftotval)*1.45, 14000) < spmu_povthreshold) | 
			(spmu_numkids == 12 &  t(unique(h_seq)) == 3 & spmu_resources - spmu_actc + pmin( 12*(1000) + 3*(ftotval)*1.45, 15000) < spmu_povthreshold) | 
			(spmu_numkids == 13 &  t(unique(h_seq)) == 3 & spmu_resources - spmu_actc + pmin( 13*(1000) + 3*(ftotval)*1.45, 16000) < spmu_povthreshold) | 
			(spmu_numkids == 14 &  t(unique(h_seq)) == 3 & spmu_resources - spmu_actc + pmin( 14*(1000) + 3*(ftotval)*1.45, 17000) < spmu_povthreshold) | 
			(spmu_numkids == 15 &  t(unique(h_seq)) == 3 & spmu_resources - spmu_actc + pmin( 15*(1000) + 3*(ftotval)*1.45, 18000) < spmu_povthreshold) | 
			(spmu_numkids == 16 &  t(unique(h_seq)) == 3 & spmu_resources - spmu_actc + pmin( 16*(1000) + 3*(ftotval)*1.45, 19000) < spmu_povthreshold) | 
			(spmu_numkids == 17 &  t(unique(h_seq)) == 3 & spmu_resources - spmu_actc + pmin( 17*(1000) + 3*(ftotval)*1.45, 20000) < spmu_povthreshold) | 
			(spmu_numkids == 18 &  t(unique(h_seq)) == 3 & spmu_resources - spmu_actc + pmin( 18*(1000) + 3*(ftotval)*1.45, 21000) < spmu_povthreshold) | 
			(spmu_numkids == 19 &  t(unique(h_seq)) == 3 & spmu_resources - spmu_actc + pmin( 19*(1000) + 3*(ftotval)*1.45, 22000) < spmu_povthreshold) |
			(spmu_numkids == 20 &  t(unique(h_seq)) == 3 & spmu_resources - spmu_actc + pmin( 20*(1000) + 3*(ftotval)*1.45, 23000) < spmu_povthreshold)	|	
			(spmu_numkids == 1 &  t(unique(h_seq)) == 4 & spmu_resources - spmu_actc + pmin( 1*(1000) + 4*(ftotval)*1.45, 2000) < spmu_povthreshold) | 
			(spmu_numkids == 2 &  t(unique(h_seq)) == 4 & spmu_resources - spmu_actc + pmin( 2*(1000) + 4*(ftotval)*1.45, 4000) < spmu_povthreshold) | 
			(spmu_numkids == 3 &  t(unique(h_seq)) == 4 & spmu_resources - spmu_actc + pmin( 3*(1000) + 4*(ftotval)*1.45, 6000) < spmu_povthreshold) | 
			(spmu_numkids == 4 &  t(unique(h_seq)) == 4 & spmu_resources - spmu_actc + pmin( 4*(1000) + 4*(ftotval)*1.45, 8000) < spmu_povthreshold) | 
			(spmu_numkids == 5 &  t(unique(h_seq)) == 4 & spmu_resources - spmu_actc + pmin( 5*(1000) + 4*(ftotval)*1.45, 9000) < spmu_povthreshold) | 
			(spmu_numkids == 6 &  t(unique(h_seq)) == 4 & spmu_resources - spmu_actc + pmin( 6*(1000) + 4*(ftotval)*1.45, 10000) < spmu_povthreshold) | 
			(spmu_numkids == 7 &  t(unique(h_seq)) == 4 & spmu_resources - spmu_actc + pmin( 7*(1000) + 4*(ftotval)*1.45, 11000) < spmu_povthreshold) | 
			(spmu_numkids == 8 &  t(unique(h_seq)) == 4 & spmu_resources - spmu_actc + pmin( 8*(1000) + 4*(ftotval)*1.45, 12000) < spmu_povthreshold) | 
			(spmu_numkids == 9 &  t(unique(h_seq)) == 4 & spmu_resources - spmu_actc + pmin( 9*(1000) + 4*(ftotval)*1.45, 13000) < spmu_povthreshold) | 
			(spmu_numkids == 10 &  t(unique(h_seq)) == 4 & spmu_resources - spmu_actc + pmin( 10*(1000) + 4*(ftotval)*1.45, 14000) < spmu_povthreshold) | 
			(spmu_numkids == 11 &  t(unique(h_seq)) == 4 & spmu_resources - spmu_actc + pmin( 11*(1000) + 4*(ftotval)*1.45, 15000) < spmu_povthreshold) | 
			(spmu_numkids == 12 &  t(unique(h_seq)) == 4 & spmu_resources - spmu_actc + pmin( 12*(1000) + 4*(ftotval)*1.45, 16000) < spmu_povthreshold) | 
			(spmu_numkids == 13 &  t(unique(h_seq)) == 4 & spmu_resources - spmu_actc + pmin( 13*(1000) + 4*(ftotval)*1.45, 17000) < spmu_povthreshold) | 
			(spmu_numkids == 14 &  t(unique(h_seq)) == 4 & spmu_resources - spmu_actc + pmin( 14*(1000) + 4*(ftotval)*1.45, 18000) < spmu_povthreshold) | 
			(spmu_numkids == 15 &  t(unique(h_seq)) == 4 & spmu_resources - spmu_actc + pmin( 15*(1000) + 4*(ftotval)*1.45, 19000) < spmu_povthreshold) | 
			(spmu_numkids == 16 &  t(unique(h_seq)) == 4 & spmu_resources - spmu_actc + pmin( 16*(1000) + 4*(ftotval)*1.45, 20000) < spmu_povthreshold) | 
			(spmu_numkids == 17 &  t(unique(h_seq)) == 4 & spmu_resources - spmu_actc + pmin( 17*(1000) + 4*(ftotval)*1.45, 21000) < spmu_povthreshold) | 
			(spmu_numkids == 18 &  t(unique(h_seq)) == 4 & spmu_resources - spmu_actc + pmin( 18*(1000) + 4*(ftotval)*1.45, 22000) < spmu_povthreshold) | 
			(spmu_numkids == 19 &  t(unique(h_seq)) == 4 & spmu_resources - spmu_actc + pmin( 19*(1000) + 4*(ftotval)*1.45, 23000) < spmu_povthreshold) |
			(spmu_numkids == 20 &  t(unique(h_seq)) == 4 & spmu_resources - spmu_actc + pmin( 20*(1000) + 4*(ftotval)*1.45, 24000) < spmu_povthreshold)	|				
			(spmu_numkids == 1 &  t(unique(h_seq)) == 5 & spmu_resources - spmu_actc + pmin( 1*(1000) + 5*(ftotval)*1.45, 2000) < spmu_povthreshold) | 
			(spmu_numkids == 2 &  t(unique(h_seq)) == 5 & spmu_resources - spmu_actc + pmin( 2*(1000) + 5*(ftotval)*1.45, 4000) < spmu_povthreshold) | 
			(spmu_numkids == 3 &  t(unique(h_seq)) == 5 & spmu_resources - spmu_actc + pmin( 3*(1000) + 5*(ftotval)*1.45, 6000) < spmu_povthreshold) | 
			(spmu_numkids == 4 &  t(unique(h_seq)) == 5 & spmu_resources - spmu_actc + pmin( 4*(1000) + 5*(ftotval)*1.45, 8000) < spmu_povthreshold) | 
			(spmu_numkids == 5 &  t(unique(h_seq)) == 5 & spmu_resources - spmu_actc + pmin( 5*(1000) + 5*(ftotval)*1.45, 10000) < spmu_povthreshold) | 
			(spmu_numkids == 6 &  t(unique(h_seq)) == 5 & spmu_resources - spmu_actc + pmin( 6*(1000) + 5*(ftotval)*1.45, 11000) < spmu_povthreshold) | 
			(spmu_numkids == 7 &  t(unique(h_seq)) == 5 & spmu_resources - spmu_actc + pmin( 7*(1000) + 5*(ftotval)*1.45, 12000) < spmu_povthreshold) | 
			(spmu_numkids == 8 &  t(unique(h_seq)) == 5 & spmu_resources - spmu_actc + pmin( 8*(1000) + 5*(ftotval)*1.45, 13000) < spmu_povthreshold) | 
			(spmu_numkids == 9 &  t(unique(h_seq)) == 5 & spmu_resources - spmu_actc + pmin( 9*(1000) + 5*(ftotval)*1.45, 14000) < spmu_povthreshold) | 
			(spmu_numkids == 10 &  t(unique(h_seq)) == 5 & spmu_resources - spmu_actc + pmin( 10*(1000) + 5*(ftotval)*1.45, 15000) < spmu_povthreshold) | 
			(spmu_numkids == 11 &  t(unique(h_seq)) == 5 & spmu_resources - spmu_actc + pmin( 11*(1000) + 5*(ftotval)*1.45, 16000) < spmu_povthreshold) | 
			(spmu_numkids == 12 &  t(unique(h_seq)) == 5 & spmu_resources - spmu_actc + pmin( 12*(1000) + 5*(ftotval)*1.45, 17000) < spmu_povthreshold) | 
			(spmu_numkids == 13 &  t(unique(h_seq)) == 5 & spmu_resources - spmu_actc + pmin( 13*(1000) + 5*(ftotval)*1.45, 18000) < spmu_povthreshold) | 
			(spmu_numkids == 14 &  t(unique(h_seq)) == 5 & spmu_resources - spmu_actc + pmin( 14*(1000) + 5*(ftotval)*1.45, 19000) < spmu_povthreshold) | 
			(spmu_numkids == 15 &  t(unique(h_seq)) == 5 & spmu_resources - spmu_actc + pmin( 15*(1000) + 5*(ftotval)*1.45, 20000) < spmu_povthreshold) | 
			(spmu_numkids == 16 &  t(unique(h_seq)) == 5 & spmu_resources - spmu_actc + pmin( 16*(1000) + 5*(ftotval)*1.45, 21000) < spmu_povthreshold) | 
			(spmu_numkids == 17 &  t(unique(h_seq)) == 5 & spmu_resources - spmu_actc + pmin( 17*(1000) + 5*(ftotval)*1.45, 22000) < spmu_povthreshold) | 
			(spmu_numkids == 18 &  t(unique(h_seq)) == 5 & spmu_resources - spmu_actc + pmin( 18*(1000) + 5*(ftotval)*1.45, 23000) < spmu_povthreshold) | 
			(spmu_numkids == 19 &  t(unique(h_seq)) == 5 & spmu_resources - spmu_actc + pmin( 19*(1000) + 5*(ftotval)*1.45, 24000) < spmu_povthreshold) |
			(spmu_numkids == 20 &  t(unique(h_seq)) == 5 & spmu_resources - spmu_actc + pmin( 20*(1000) + 5*(ftotval)*1.45, 25000) < spmu_povthreshold)	|	
			(spmu_numkids == 1 &  t(unique(h_seq)) == 6 & spmu_resources - spmu_actc + pmin( 1*(1000) + 6*(ftotval)*1.45, 2000) < spmu_povthreshold) | 
			(spmu_numkids == 2 &  t(unique(h_seq)) == 6 & spmu_resources - spmu_actc + pmin( 2*(1000) + 6*(ftotval)*1.45, 4000) < spmu_povthreshold) | 
			(spmu_numkids == 3 &  t(unique(h_seq)) == 6 & spmu_resources - spmu_actc + pmin( 3*(1000) + 6*(ftotval)*1.45, 6000) < spmu_povthreshold) | 
			(spmu_numkids == 4 &  t(unique(h_seq)) == 6 & spmu_resources - spmu_actc + pmin( 4*(1000) + 6*(ftotval)*1.45, 8000) < spmu_povthreshold) | 
			(spmu_numkids == 5 &  t(unique(h_seq)) == 6 & spmu_resources - spmu_actc + pmin( 5*(1000) + 6*(ftotval)*1.45, 10000) < spmu_povthreshold) | 
			(spmu_numkids == 6 &  t(unique(h_seq)) == 6 & spmu_resources - spmu_actc + pmin( 6*(1000) + 6*(ftotval)*1.45, 12000) < spmu_povthreshold) | 
			(spmu_numkids == 7 &  t(unique(h_seq)) == 6 & spmu_resources - spmu_actc + pmin( 7*(1000) + 6*(ftotval)*1.45, 13000) < spmu_povthreshold) | 
			(spmu_numkids == 8 &  t(unique(h_seq)) == 6 & spmu_resources - spmu_actc + pmin( 8*(1000) + 6*(ftotval)*1.45, 14000) < spmu_povthreshold) | 
			(spmu_numkids == 9 &  t(unique(h_seq)) == 6 & spmu_resources - spmu_actc + pmin( 9*(1000) + 6*(ftotval)*1.45, 15000) < spmu_povthreshold) | 
			(spmu_numkids == 10 &  t(unique(h_seq)) == 6 & spmu_resources - spmu_actc + pmin( 10*(1000) + 6*(ftotval)*1.45, 16000) < spmu_povthreshold) | 
			(spmu_numkids == 11 &  t(unique(h_seq)) == 6 & spmu_resources - spmu_actc + pmin( 11*(1000) + 6*(ftotval)*1.45, 17000) < spmu_povthreshold) | 
			(spmu_numkids == 12 &  t(unique(h_seq)) == 6 & spmu_resources - spmu_actc + pmin( 12*(1000) + 6*(ftotval)*1.45, 18000) < spmu_povthreshold) | 
			(spmu_numkids == 13 &  t(unique(h_seq)) == 6 & spmu_resources - spmu_actc + pmin( 13*(1000) + 6*(ftotval)*1.45, 19000) < spmu_povthreshold) | 
			(spmu_numkids == 14 &  t(unique(h_seq)) == 6 & spmu_resources - spmu_actc + pmin( 14*(1000) + 6*(ftotval)*1.45, 20000) < spmu_povthreshold) | 
			(spmu_numkids == 15 &  t(unique(h_seq)) == 6 & spmu_resources - spmu_actc + pmin( 15*(1000) + 6*(ftotval)*1.45, 21000) < spmu_povthreshold) | 
			(spmu_numkids == 16 &  t(unique(h_seq)) == 6 & spmu_resources - spmu_actc + pmin( 16*(1000) + 6*(ftotval)*1.45, 22000) < spmu_povthreshold) | 
			(spmu_numkids == 17 &  t(unique(h_seq)) == 6 & spmu_resources - spmu_actc + pmin( 17*(1000) + 6*(ftotval)*1.45, 23000) < spmu_povthreshold) | 
			(spmu_numkids == 18 &  t(unique(h_seq)) == 6 & spmu_resources - spmu_actc + pmin( 18*(1000) + 6*(ftotval)*1.45, 24000) < spmu_povthreshold) | 
			(spmu_numkids == 19 &  t(unique(h_seq)) == 6 & spmu_resources - spmu_actc + pmin( 19*(1000) + 6*(ftotval)*1.45, 25000) < spmu_povthreshold) |
			(spmu_numkids == 20 &  t(unique(h_seq)) == 6 & spmu_resources - spmu_actc + pmin( 20*(1000) + 6*(ftotval)*1.45, 26000) < spmu_povthreshold))

svytotal(
	~one ,
	Usplit_Clinton_Plan)

Clinton_Plan <-subset( 
		y , 
			(spmu_numkids == 0 & spmu_resources - spmu_actc < spmu_povthreshold) | 
			(spmu_numkids == 1 &  t(unique(h_seq)) == 0 & spmu_resources - spmu_actc + pmin( 1*(ftotval*1.15) + 0*(ftotval*1.45), 1000) < spmu_povthreshold) | 
			(spmu_numkids == 2 &  t(unique(h_seq)) == 0 & spmu_resources - spmu_actc + pmin( 2*(ftotval*1.15) + 0*(ftotval)*1.45, 2000) < spmu_povthreshold) | 
			(spmu_numkids == 3 &  t(unique(h_seq)) == 0 & spmu_resources - spmu_actc + pmin( 3*(ftotval*1.15) + 0*(ftotval)*1.45, 3000) < spmu_povthreshold) | 
			(spmu_numkids == 4 &  t(unique(h_seq)) == 0 & spmu_resources - spmu_actc + pmin( 4*(ftotval*1.15) + 0*(ftotval)*1.45, 4000) < spmu_povthreshold) | 
			(spmu_numkids == 5 &  t(unique(h_seq)) == 0 & spmu_resources - spmu_actc + pmin( 5*(ftotval*1.15) + 0*(ftotval)*1.45, 5000) < spmu_povthreshold) | 
			(spmu_numkids == 6 &  t(unique(h_seq)) == 0 & spmu_resources - spmu_actc + pmin( 6*(ftotval*1.15) + 0*(ftotval)*1.45, 6000) < spmu_povthreshold) | 
			(spmu_numkids == 7 &  t(unique(h_seq)) == 0 & spmu_resources - spmu_actc + pmin( 7*(ftotval*1.15) + 0*(ftotval)*1.45, 7000) < spmu_povthreshold) | 
			(spmu_numkids == 8 &  t(unique(h_seq)) == 0 & spmu_resources - spmu_actc + pmin( 8*(ftotval*1.15) + 0*(ftotval)*1.45, 8000) < spmu_povthreshold) | 
			(spmu_numkids == 9 &  t(unique(h_seq)) == 0 & spmu_resources - spmu_actc + pmin( 9*(ftotval*1.15) + 0*(ftotval)*1.45, 9000) < spmu_povthreshold) | 
			(spmu_numkids == 10 &  t(unique(h_seq)) == 0 & spmu_resources - spmu_actc + pmin( 10*(ftotval*1.15) + 0*(ftotval)*1.45, 10000) < spmu_povthreshold) | 
			(spmu_numkids == 11 &  t(unique(h_seq)) == 0 & spmu_resources - spmu_actc + pmin( 11*(ftotval*1.15) + 0*(ftotval)*1.45, 11000) < spmu_povthreshold) | 
			(spmu_numkids == 12 &  t(unique(h_seq)) == 0 & spmu_resources - spmu_actc + pmin( 12*(ftotval*1.15) + 0*(ftotval)*1.45, 12000) < spmu_povthreshold) | 
			(spmu_numkids == 13 &  t(unique(h_seq)) == 0 & spmu_resources - spmu_actc + pmin( 13*(ftotval*1.15) + 0*(ftotval)*1.45, 13000) < spmu_povthreshold) | 
			(spmu_numkids == 14 &  t(unique(h_seq)) == 0 & spmu_resources - spmu_actc + pmin( 14*(ftotval*1.15) + 0*(ftotval)*1.45, 14000) < spmu_povthreshold) | 
			(spmu_numkids == 15 &  t(unique(h_seq)) == 0 & spmu_resources - spmu_actc + pmin( 15*(ftotval*1.15) + 0*(ftotval)*1.45, 15000) < spmu_povthreshold) | 
			(spmu_numkids == 16 &  t(unique(h_seq)) == 0 & spmu_resources - spmu_actc + pmin( 16*(ftotval*1.15) + 0*(ftotval)*1.45, 16000) < spmu_povthreshold) | 
			(spmu_numkids == 17 &  t(unique(h_seq)) == 0 & spmu_resources - spmu_actc + pmin( 17*(ftotval*1.15) + 0*(ftotval)*1.45, 17000) < spmu_povthreshold) | 
			(spmu_numkids == 18 &  t(unique(h_seq)) == 0 & spmu_resources - spmu_actc + pmin( 18*(ftotval*1.15) + 0*(ftotval)*1.45, 18000) < spmu_povthreshold) | 
			(spmu_numkids == 19 &  t(unique(h_seq)) == 0 & spmu_resources - spmu_actc + pmin( 19*(ftotval*1.15) + 0*(ftotval)*1.45, 19000) < spmu_povthreshold) |
			(spmu_numkids >= 20 &  t(unique(h_seq)) == 0 & spmu_resources - spmu_actc + pmin( 20*(ftotval*1.15) + 0*(ftotval)*1.45, 20000) < spmu_povthreshold) |
			(spmu_numkids == 1 &  t(unique(h_seq)) == 1 & spmu_resources - spmu_actc + pmin( 1*(ftotval*1.15) + 1*(ftotval)*1.45, 2000) < spmu_povthreshold) | 
			(spmu_numkids == 2 &  t(unique(h_seq)) == 1 & spmu_resources - spmu_actc + pmin( 2*(ftotval*1.15) + 1*(ftotval)*1.45, 3000) < spmu_povthreshold) | 
			(spmu_numkids == 3 &  t(unique(h_seq)) == 1 & spmu_resources - spmu_actc + pmin( 3*(ftotval*1.15) + 1*(ftotval)*1.45, 4000) < spmu_povthreshold) | 
			(spmu_numkids == 4 &  t(unique(h_seq)) == 1 & spmu_resources - spmu_actc + pmin( 4*(ftotval*1.15) + 1*(ftotval)*1.45, 5000) < spmu_povthreshold) | 
			(spmu_numkids == 5 &  t(unique(h_seq)) == 1 & spmu_resources - spmu_actc + pmin( 5*(ftotval*1.15) + 1*(ftotval)*1.45, 6000) < spmu_povthreshold) | 
			(spmu_numkids == 6 &  t(unique(h_seq)) == 1 & spmu_resources - spmu_actc + pmin( 6*(ftotval*1.15) + 1*(ftotval)*1.45, 7000) < spmu_povthreshold) | 
			(spmu_numkids == 7 &  t(unique(h_seq)) == 1 & spmu_resources - spmu_actc + pmin( 7*(ftotval*1.15) + 1*(ftotval)*1.45, 8000) < spmu_povthreshold) | 
			(spmu_numkids == 8 &  t(unique(h_seq)) == 1 & spmu_resources - spmu_actc + pmin( 8*(ftotval*1.15) + 1*(ftotval)*1.45, 9000) < spmu_povthreshold) | 
			(spmu_numkids == 9 &  t(unique(h_seq)) == 1 & spmu_resources - spmu_actc + pmin( 9*(ftotval*1.15) + 1*(ftotval)*1.45, 10000) < spmu_povthreshold) | 
			(spmu_numkids == 10 &  t(unique(h_seq)) == 1 & spmu_resources - spmu_actc + pmin( 10*(ftotval*1.15) + 1*(ftotval)*1.45, 11000) < spmu_povthreshold) | 
			(spmu_numkids == 11 &  t(unique(h_seq)) == 1 & spmu_resources - spmu_actc + pmin( 11*(ftotval*1.15) + 1*(ftotval)*1.45, 12000) < spmu_povthreshold) | 
			(spmu_numkids == 12 &  t(unique(h_seq)) == 1 & spmu_resources - spmu_actc + pmin( 12*(ftotval*1.15) + 1*(ftotval)*1.45, 13000) < spmu_povthreshold) | 
			(spmu_numkids == 13 &  t(unique(h_seq)) == 1 & spmu_resources - spmu_actc + pmin( 13*(ftotval*1.15) + 1*(ftotval)*1.45, 14000) < spmu_povthreshold) | 
			(spmu_numkids == 14 &  t(unique(h_seq)) == 1 & spmu_resources - spmu_actc + pmin( 14*(ftotval*1.15) + 1*(ftotval)*1.45, 15000) < spmu_povthreshold) | 
			(spmu_numkids == 15 &  t(unique(h_seq)) == 1 & spmu_resources - spmu_actc + pmin( 15*(ftotval*1.15) + 1*(ftotval)*1.45, 16000) < spmu_povthreshold) | 
			(spmu_numkids == 16 &  t(unique(h_seq)) == 1 & spmu_resources - spmu_actc + pmin( 16*(ftotval*1.15) + 1*(ftotval)*1.45, 17000) < spmu_povthreshold) | 
			(spmu_numkids == 17 &  t(unique(h_seq)) == 1 & spmu_resources - spmu_actc + pmin( 17*(ftotval*1.15) + 1*(ftotval)*1.45, 18000) < spmu_povthreshold) | 
			(spmu_numkids == 18 &  t(unique(h_seq)) == 1 & spmu_resources - spmu_actc + pmin( 18*(ftotval*1.15) + 1*(ftotval)*1.45, 19000) < spmu_povthreshold) | 
			(spmu_numkids == 19 &  t(unique(h_seq)) == 1 & spmu_resources - spmu_actc + pmin( 19*(ftotval*1.15) + 1*(ftotval)*1.45, 20000) < spmu_povthreshold) |
			(spmu_numkids == 20 &  t(unique(h_seq)) == 1 & spmu_resources - spmu_actc + pmin( 20*(ftotval*1.15) + 1*(ftotval)*1.45, 21000) < spmu_povthreshold)	|		
			(spmu_numkids == 1 &  t(unique(h_seq)) == 2 & spmu_resources - spmu_actc + pmin( 1*(ftotval*1.15) + 2*(ftotval)*1.45, 2000) < spmu_povthreshold) | 
			(spmu_numkids == 2 &  t(unique(h_seq)) == 2 & spmu_resources - spmu_actc + pmin( 2*(ftotval*1.15) + 2*(ftotval)*1.45, 4000) < spmu_povthreshold) | 
			(spmu_numkids == 3 &  t(unique(h_seq)) == 2 & spmu_resources - spmu_actc + pmin( 3*(ftotval*1.15) + 2*(ftotval)*1.45, 5000) < spmu_povthreshold) | 
			(spmu_numkids == 4 &  t(unique(h_seq)) == 2 & spmu_resources - spmu_actc + pmin( 4*(ftotval*1.15) + 2*(ftotval)*1.45, 6000) < spmu_povthreshold) | 
			(spmu_numkids == 5 &  t(unique(h_seq)) == 2 & spmu_resources - spmu_actc + pmin( 5*(ftotval*1.15) + 2*(ftotval)*1.45, 7000) < spmu_povthreshold) | 
			(spmu_numkids == 6 &  t(unique(h_seq)) == 2 & spmu_resources - spmu_actc + pmin( 6*(ftotval*1.15) + 2*(ftotval)*1.45, 8000) < spmu_povthreshold) | 
			(spmu_numkids == 7 &  t(unique(h_seq)) == 2 & spmu_resources - spmu_actc + pmin( 7*(ftotval*1.15) + 2*(ftotval)*1.45, 9000) < spmu_povthreshold) | 
			(spmu_numkids == 8 &  t(unique(h_seq)) == 2 & spmu_resources - spmu_actc + pmin( 8*(ftotval*1.15) + 2*(ftotval)*1.45, 10000) < spmu_povthreshold) | 
			(spmu_numkids == 9 &  t(unique(h_seq)) == 2 & spmu_resources - spmu_actc + pmin( 9*(ftotval*1.15) + 2*(ftotval)*1.45, 11000) < spmu_povthreshold) | 
			(spmu_numkids == 10 &  t(unique(h_seq)) == 2 & spmu_resources - spmu_actc + pmin( 10*(ftotval*1.15) + 2*(ftotval)*1.45, 12000) < spmu_povthreshold) | 
			(spmu_numkids == 11 &  t(unique(h_seq)) == 2 & spmu_resources - spmu_actc + pmin( 11*(ftotval*1.15) + 2*(ftotval)*1.45, 13000) < spmu_povthreshold) | 
			(spmu_numkids == 12 &  t(unique(h_seq)) == 2 & spmu_resources - spmu_actc + pmin( 12*(ftotval*1.15) + 2*(ftotval)*1.45, 14000) < spmu_povthreshold) | 
			(spmu_numkids == 13 &  t(unique(h_seq)) == 2 & spmu_resources - spmu_actc + pmin( 13*(ftotval*1.15) + 2*(ftotval)*1.45, 15000) < spmu_povthreshold) | 
			(spmu_numkids == 14 &  t(unique(h_seq)) == 2 & spmu_resources - spmu_actc + pmin( 14*(ftotval*1.15) + 2*(ftotval)*1.45, 16000) < spmu_povthreshold) | 
			(spmu_numkids == 15 &  t(unique(h_seq)) == 2 & spmu_resources - spmu_actc + pmin( 15*(ftotval*1.15) + 2*(ftotval)*1.45, 17000) < spmu_povthreshold) | 
			(spmu_numkids == 16 &  t(unique(h_seq)) == 2 & spmu_resources - spmu_actc + pmin( 16*(ftotval*1.15) + 2*(ftotval)*1.45, 18000) < spmu_povthreshold) | 
			(spmu_numkids == 17 &  t(unique(h_seq)) == 2 & spmu_resources - spmu_actc + pmin( 17*(ftotval*1.15) + 2*(ftotval)*1.45, 19000) < spmu_povthreshold) | 
			(spmu_numkids == 18 &  t(unique(h_seq)) == 2 & spmu_resources - spmu_actc + pmin( 18*(ftotval*1.15) + 2*(ftotval)*1.45, 20000) < spmu_povthreshold) | 
			(spmu_numkids == 19 &  t(unique(h_seq)) == 2 & spmu_resources - spmu_actc + pmin( 19*(ftotval*1.15) + 2*(ftotval)*1.45, 21000) < spmu_povthreshold) |
			(spmu_numkids == 20 &  t(unique(h_seq)) == 2 & spmu_resources - spmu_actc + pmin( 20*(ftotval*1.15) + 2*(ftotval)*1.45, 22000) < spmu_povthreshold)	|	
			(spmu_numkids == 1 &  t(unique(h_seq)) == 3 & spmu_resources - spmu_actc + pmin( 1*(ftotval*1.15) + 3*(ftotval)*1.45, 2000) < spmu_povthreshold) | 
			(spmu_numkids == 2 &  t(unique(h_seq)) == 3 & spmu_resources - spmu_actc + pmin( 2*(ftotval*1.15) + 3*(ftotval)*1.45, 4000) < spmu_povthreshold) | 
			(spmu_numkids == 3 &  t(unique(h_seq)) == 3 & spmu_resources - spmu_actc + pmin( 3*(ftotval*1.15) + 3*(ftotval)*1.45, 6000) < spmu_povthreshold) | 
			(spmu_numkids == 4 &  t(unique(h_seq)) == 3 & spmu_resources - spmu_actc + pmin( 4*(ftotval*1.15) + 3*(ftotval)*1.45, 7000) < spmu_povthreshold) | 
			(spmu_numkids == 5 &  t(unique(h_seq)) == 3 & spmu_resources - spmu_actc + pmin( 5*(ftotval*1.15) + 3*(ftotval)*1.45, 8000) < spmu_povthreshold) | 
			(spmu_numkids == 6 &  t(unique(h_seq)) == 3 & spmu_resources - spmu_actc + pmin( 6*(ftotval*1.15) + 3*(ftotval)*1.45, 9000) < spmu_povthreshold) | 
			(spmu_numkids == 7 &  t(unique(h_seq)) == 3 & spmu_resources - spmu_actc + pmin( 7*(ftotval*1.15) + 3*(ftotval)*1.45, 10000) < spmu_povthreshold) | 
			(spmu_numkids == 8 &  t(unique(h_seq)) == 3 & spmu_resources - spmu_actc + pmin( 8*(ftotval*1.15) + 3*(ftotval)*1.45, 11000) < spmu_povthreshold) | 
			(spmu_numkids == 9 &  t(unique(h_seq)) == 3 & spmu_resources - spmu_actc + pmin( 9*(ftotval*1.15) + 3*(ftotval)*1.45, 12000) < spmu_povthreshold) | 
			(spmu_numkids == 10 &  t(unique(h_seq)) == 3 & spmu_resources - spmu_actc + pmin( 10*(ftotval*1.15) + 3*(ftotval)*1.45, 13000) < spmu_povthreshold) | 
			(spmu_numkids == 11 &  t(unique(h_seq)) == 3 & spmu_resources - spmu_actc + pmin( 11*(ftotval*1.15) + 3*(ftotval)*1.45, 14000) < spmu_povthreshold) | 
			(spmu_numkids == 12 &  t(unique(h_seq)) == 3 & spmu_resources - spmu_actc + pmin( 12*(ftotval*1.15) + 3*(ftotval)*1.45, 15000) < spmu_povthreshold) | 
			(spmu_numkids == 13 &  t(unique(h_seq)) == 3 & spmu_resources - spmu_actc + pmin( 13*(ftotval*1.15) + 3*(ftotval)*1.45, 16000) < spmu_povthreshold) | 
			(spmu_numkids == 14 &  t(unique(h_seq)) == 3 & spmu_resources - spmu_actc + pmin( 14*(ftotval*1.15) + 3*(ftotval)*1.45, 17000) < spmu_povthreshold) | 
			(spmu_numkids == 15 &  t(unique(h_seq)) == 3 & spmu_resources - spmu_actc + pmin( 15*(ftotval*1.15) + 3*(ftotval)*1.45, 18000) < spmu_povthreshold) | 
			(spmu_numkids == 16 &  t(unique(h_seq)) == 3 & spmu_resources - spmu_actc + pmin( 16*(ftotval*1.15) + 3*(ftotval)*1.45, 19000) < spmu_povthreshold) | 
			(spmu_numkids == 17 &  t(unique(h_seq)) == 3 & spmu_resources - spmu_actc + pmin( 17*(ftotval*1.15) + 3*(ftotval)*1.45, 20000) < spmu_povthreshold) | 
			(spmu_numkids == 18 &  t(unique(h_seq)) == 3 & spmu_resources - spmu_actc + pmin( 18*(ftotval*1.15) + 3*(ftotval)*1.45, 21000) < spmu_povthreshold) | 
			(spmu_numkids == 19 &  t(unique(h_seq)) == 3 & spmu_resources - spmu_actc + pmin( 19*(ftotval*1.15) + 3*(ftotval)*1.45, 22000) < spmu_povthreshold) |
			(spmu_numkids == 20 &  t(unique(h_seq)) == 3 & spmu_resources - spmu_actc + pmin( 20*(ftotval*1.15) + 3*(ftotval)*1.45, 23000) < spmu_povthreshold)	|	
			(spmu_numkids == 1 &  t(unique(h_seq)) == 4 & spmu_resources - spmu_actc + pmin( 1*(ftotval*1.15) + 4*(ftotval)*1.45, 2000) < spmu_povthreshold) | 
			(spmu_numkids == 2 &  t(unique(h_seq)) == 4 & spmu_resources - spmu_actc + pmin( 2*(ftotval*1.15) + 4*(ftotval)*1.45, 4000) < spmu_povthreshold) | 
			(spmu_numkids == 3 &  t(unique(h_seq)) == 4 & spmu_resources - spmu_actc + pmin( 3*(ftotval*1.15) + 4*(ftotval)*1.45, 6000) < spmu_povthreshold) | 
			(spmu_numkids == 4 &  t(unique(h_seq)) == 4 & spmu_resources - spmu_actc + pmin( 4*(ftotval*1.15) + 4*(ftotval)*1.45, 8000) < spmu_povthreshold) | 
			(spmu_numkids == 5 &  t(unique(h_seq)) == 4 & spmu_resources - spmu_actc + pmin( 5*(ftotval*1.15) + 4*(ftotval)*1.45, 9000) < spmu_povthreshold) | 
			(spmu_numkids == 6 &  t(unique(h_seq)) == 4 & spmu_resources - spmu_actc + pmin( 6*(ftotval*1.15) + 4*(ftotval)*1.45, 10000) < spmu_povthreshold) | 
			(spmu_numkids == 7 &  t(unique(h_seq)) == 4 & spmu_resources - spmu_actc + pmin( 7*(ftotval*1.15) + 4*(ftotval)*1.45, 11000) < spmu_povthreshold) | 
			(spmu_numkids == 8 &  t(unique(h_seq)) == 4 & spmu_resources - spmu_actc + pmin( 8*(ftotval*1.15) + 4*(ftotval)*1.45, 12000) < spmu_povthreshold) | 
			(spmu_numkids == 9 &  t(unique(h_seq)) == 4 & spmu_resources - spmu_actc + pmin( 9*(ftotval*1.15) + 4*(ftotval)*1.45, 13000) < spmu_povthreshold) | 
			(spmu_numkids == 10 &  t(unique(h_seq)) == 4 & spmu_resources - spmu_actc + pmin( 10*(ftotval*1.15) + 4*(ftotval)*1.45, 14000) < spmu_povthreshold) | 
			(spmu_numkids == 11 &  t(unique(h_seq)) == 4 & spmu_resources - spmu_actc + pmin( 11*(ftotval*1.15) + 4*(ftotval)*1.45, 15000) < spmu_povthreshold) | 
			(spmu_numkids == 12 &  t(unique(h_seq)) == 4 & spmu_resources - spmu_actc + pmin( 12*(ftotval*1.15) + 4*(ftotval)*1.45, 16000) < spmu_povthreshold) | 
			(spmu_numkids == 13 &  t(unique(h_seq)) == 4 & spmu_resources - spmu_actc + pmin( 13*(ftotval*1.15) + 4*(ftotval)*1.45, 17000) < spmu_povthreshold) | 
			(spmu_numkids == 14 &  t(unique(h_seq)) == 4 & spmu_resources - spmu_actc + pmin( 14*(ftotval*1.15) + 4*(ftotval)*1.45, 18000) < spmu_povthreshold) | 
			(spmu_numkids == 15 &  t(unique(h_seq)) == 4 & spmu_resources - spmu_actc + pmin( 15*(ftotval*1.15) + 4*(ftotval)*1.45, 19000) < spmu_povthreshold) | 
			(spmu_numkids == 16 &  t(unique(h_seq)) == 4 & spmu_resources - spmu_actc + pmin( 16*(ftotval*1.15) + 4*(ftotval)*1.45, 20000) < spmu_povthreshold) | 
			(spmu_numkids == 17 &  t(unique(h_seq)) == 4 & spmu_resources - spmu_actc + pmin( 17*(ftotval*1.15) + 4*(ftotval)*1.45, 21000) < spmu_povthreshold) | 
			(spmu_numkids == 18 &  t(unique(h_seq)) == 4 & spmu_resources - spmu_actc + pmin( 18*(ftotval*1.15) + 4*(ftotval)*1.45, 22000) < spmu_povthreshold) | 
			(spmu_numkids == 19 &  t(unique(h_seq)) == 4 & spmu_resources - spmu_actc + pmin( 19*(ftotval*1.15) + 4*(ftotval)*1.45, 23000) < spmu_povthreshold) |
			(spmu_numkids == 20 &  t(unique(h_seq)) == 4 & spmu_resources - spmu_actc + pmin( 20*(ftotval*1.15) + 4*(ftotval)*1.45, 24000) < spmu_povthreshold)	|				
			(spmu_numkids == 1 &  t(unique(h_seq)) == 5 & spmu_resources - spmu_actc + pmin( 1*(ftotval*1.15) + 5*(ftotval)*1.45, 2000) < spmu_povthreshold) | 
			(spmu_numkids == 2 &  t(unique(h_seq)) == 5 & spmu_resources - spmu_actc + pmin( 2*(ftotval*1.15) + 5*(ftotval)*1.45, 4000) < spmu_povthreshold) | 
			(spmu_numkids == 3 &  t(unique(h_seq)) == 5 & spmu_resources - spmu_actc + pmin( 3*(ftotval*1.15) + 5*(ftotval)*1.45, 6000) < spmu_povthreshold) | 
			(spmu_numkids == 4 &  t(unique(h_seq)) == 5 & spmu_resources - spmu_actc + pmin( 4*(ftotval*1.15) + 5*(ftotval)*1.45, 8000) < spmu_povthreshold) | 
			(spmu_numkids == 5 &  t(unique(h_seq)) == 5 & spmu_resources - spmu_actc + pmin( 5*(ftotval*1.15) + 5*(ftotval)*1.45, 10000) < spmu_povthreshold) | 
			(spmu_numkids == 6 &  t(unique(h_seq)) == 5 & spmu_resources - spmu_actc + pmin( 6*(ftotval*1.15) + 5*(ftotval)*1.45, 11000) < spmu_povthreshold) | 
			(spmu_numkids == 7 &  t(unique(h_seq)) == 5 & spmu_resources - spmu_actc + pmin( 7*(ftotval*1.15) + 5*(ftotval)*1.45, 12000) < spmu_povthreshold) | 
			(spmu_numkids == 8 &  t(unique(h_seq)) == 5 & spmu_resources - spmu_actc + pmin( 8*(ftotval*1.15) + 5*(ftotval)*1.45, 13000) < spmu_povthreshold) | 
			(spmu_numkids == 9 &  t(unique(h_seq)) == 5 & spmu_resources - spmu_actc + pmin( 9*(ftotval*1.15) + 5*(ftotval)*1.45, 14000) < spmu_povthreshold) | 
			(spmu_numkids == 10 &  t(unique(h_seq)) == 5 & spmu_resources - spmu_actc + pmin( 10*(ftotval*1.15) + 5*(ftotval)*1.45, 15000) < spmu_povthreshold) | 
			(spmu_numkids == 11 &  t(unique(h_seq)) == 5 & spmu_resources - spmu_actc + pmin( 11*(ftotval*1.15) + 5*(ftotval)*1.45, 16000) < spmu_povthreshold) | 
			(spmu_numkids == 12 &  t(unique(h_seq)) == 5 & spmu_resources - spmu_actc + pmin( 12*(ftotval*1.15) + 5*(ftotval)*1.45, 17000) < spmu_povthreshold) | 
			(spmu_numkids == 13 &  t(unique(h_seq)) == 5 & spmu_resources - spmu_actc + pmin( 13*(ftotval*1.15) + 5*(ftotval)*1.45, 18000) < spmu_povthreshold) | 
			(spmu_numkids == 14 &  t(unique(h_seq)) == 5 & spmu_resources - spmu_actc + pmin( 14*(ftotval*1.15) + 5*(ftotval)*1.45, 19000) < spmu_povthreshold) | 
			(spmu_numkids == 15 &  t(unique(h_seq)) == 5 & spmu_resources - spmu_actc + pmin( 15*(ftotval*1.15) + 5*(ftotval)*1.45, 20000) < spmu_povthreshold) | 
			(spmu_numkids == 16 &  t(unique(h_seq)) == 5 & spmu_resources - spmu_actc + pmin( 16*(ftotval*1.15) + 5*(ftotval)*1.45, 21000) < spmu_povthreshold) | 
			(spmu_numkids == 17 &  t(unique(h_seq)) == 5 & spmu_resources - spmu_actc + pmin( 17*(ftotval*1.15) + 5*(ftotval)*1.45, 22000) < spmu_povthreshold) | 
			(spmu_numkids == 18 &  t(unique(h_seq)) == 5 & spmu_resources - spmu_actc + pmin( 18*(ftotval*1.15) + 5*(ftotval)*1.45, 23000) < spmu_povthreshold) | 
			(spmu_numkids == 19 &  t(unique(h_seq)) == 5 & spmu_resources - spmu_actc + pmin( 19*(ftotval*1.15) + 5*(ftotval)*1.45, 24000) < spmu_povthreshold) |
			(spmu_numkids == 20 &  t(unique(h_seq)) == 5 & spmu_resources - spmu_actc + pmin( 20*(ftotval*1.15) + 5*(ftotval)*1.45, 25000) < spmu_povthreshold)	|	
			(spmu_numkids == 1 &  t(unique(h_seq)) == 6 & spmu_resources - spmu_actc + pmin( 1*(ftotval*1.15) + 6*(ftotval)*1.45, 2000) < spmu_povthreshold) | 
			(spmu_numkids == 2 &  t(unique(h_seq)) == 6 & spmu_resources - spmu_actc + pmin( 2*(ftotval*1.15) + 6*(ftotval)*1.45, 4000) < spmu_povthreshold) | 
			(spmu_numkids == 3 &  t(unique(h_seq)) == 6 & spmu_resources - spmu_actc + pmin( 3*(ftotval*1.15) + 6*(ftotval)*1.45, 6000) < spmu_povthreshold) | 
			(spmu_numkids == 4 &  t(unique(h_seq)) == 6 & spmu_resources - spmu_actc + pmin( 4*(ftotval*1.15) + 6*(ftotval)*1.45, 8000) < spmu_povthreshold) | 
			(spmu_numkids == 5 &  t(unique(h_seq)) == 6 & spmu_resources - spmu_actc + pmin( 5*(ftotval*1.15) + 6*(ftotval)*1.45, 10000) < spmu_povthreshold) | 
			(spmu_numkids == 6 &  t(unique(h_seq)) == 6 & spmu_resources - spmu_actc + pmin( 6*(ftotval*1.15) + 6*(ftotval)*1.45, 12000) < spmu_povthreshold) | 
			(spmu_numkids == 7 &  t(unique(h_seq)) == 6 & spmu_resources - spmu_actc + pmin( 7*(ftotval*1.15) + 6*(ftotval)*1.45, 13000) < spmu_povthreshold) | 
			(spmu_numkids == 8 &  t(unique(h_seq)) == 6 & spmu_resources - spmu_actc + pmin( 8*(ftotval*1.15) + 6*(ftotval)*1.45, 14000) < spmu_povthreshold) | 
			(spmu_numkids == 9 &  t(unique(h_seq)) == 6 & spmu_resources - spmu_actc + pmin( 9*(ftotval*1.15) + 6*(ftotval)*1.45, 15000) < spmu_povthreshold) | 
			(spmu_numkids == 10 &  t(unique(h_seq)) == 6 & spmu_resources - spmu_actc + pmin( 10*(ftotval*1.15) + 6*(ftotval)*1.45, 16000) < spmu_povthreshold) | 
			(spmu_numkids == 11 &  t(unique(h_seq)) == 6 & spmu_resources - spmu_actc + pmin( 11*(ftotval*1.15) + 6*(ftotval)*1.45, 17000) < spmu_povthreshold) | 
			(spmu_numkids == 12 &  t(unique(h_seq)) == 6 & spmu_resources - spmu_actc + pmin( 12*(ftotval*1.15) + 6*(ftotval)*1.45, 18000) < spmu_povthreshold) | 
			(spmu_numkids == 13 &  t(unique(h_seq)) == 6 & spmu_resources - spmu_actc + pmin( 13*(ftotval*1.15) + 6*(ftotval)*1.45, 19000) < spmu_povthreshold) | 
			(spmu_numkids == 14 &  t(unique(h_seq)) == 6 & spmu_resources - spmu_actc + pmin( 14*(ftotval*1.15) + 6*(ftotval)*1.45, 20000) < spmu_povthreshold) | 
			(spmu_numkids == 15 &  t(unique(h_seq)) == 6 & spmu_resources - spmu_actc + pmin( 15*(ftotval*1.15) + 6*(ftotval)*1.45, 21000) < spmu_povthreshold) | 
			(spmu_numkids == 16 &  t(unique(h_seq)) == 6 & spmu_resources - spmu_actc + pmin( 16*(ftotval*1.15) + 6*(ftotval)*1.45, 22000) < spmu_povthreshold) | 
			(spmu_numkids == 17 &  t(unique(h_seq)) == 6 & spmu_resources - spmu_actc + pmin( 17*(ftotval*1.15) + 6*(ftotval)*1.45, 23000) < spmu_povthreshold) | 
			(spmu_numkids == 18 &  t(unique(h_seq)) == 6 & spmu_resources - spmu_actc + pmin( 18*(ftotval*1.15) + 6*(ftotval)*1.45, 24000) < spmu_povthreshold) | 
			(spmu_numkids == 19 &  t(unique(h_seq)) == 6 & spmu_resources - spmu_actc + pmin( 19*(ftotval*1.15) + 6*(ftotval)*1.45, 25000) < spmu_povthreshold) |
			(spmu_numkids == 20 &  t(unique(h_seq)) == 6 & spmu_resources - spmu_actc + pmin( 20*(ftotval*1.15) + 6*(ftotval)*1.45, 26000) < spmu_povthreshold))

svytotal(
	~one ,
	Clinton_Plan)


