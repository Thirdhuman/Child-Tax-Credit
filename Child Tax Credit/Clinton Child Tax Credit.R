setwd("~/Desktop/Poverty:Welfare/Data/R - Survey data/Current Population Survey (CPS)")

f = function(x) if (x < 1000) exp(seq(log(x), log(1000), by=log(1.15))) else 1000
c = function(x) if (x < 2000) exp(seq(log(x), log(2000), by=log(1.45))) else 2000
t <-function(x){length(which(y$a_age < 5))} 

Clinton_Plan <-subset( 
		y , 
			(spmu_numkids == 0 & spmu_resources - spmu_actc < spmu_povthreshold) | 
			(spmu_numkids == 1 &  t(unique(h_seq)) == 0 & spmu_resources - spmu_actc + pmin( 1*f(ftotval) + 0*c(ftotval), 1000) < spmu_povthreshold) | 
			(spmu_numkids == 2 &  t(unique(h_seq)) == 0 & spmu_resources - spmu_actc + pmin( 2*f(ftotval) + 0*c(ftotval), 2000) < spmu_povthreshold) | 
			(spmu_numkids == 3 &  t(unique(h_seq)) == 0 & spmu_resources - spmu_actc + pmin( 3*f(ftotval) + 0*c(ftotval), 3000) < spmu_povthreshold) | 
			(spmu_numkids == 4 &  t(unique(h_seq)) == 0 & spmu_resources - spmu_actc + pmin( 4*f(ftotval) + 0*c(ftotval), 4000) < spmu_povthreshold) | 
			(spmu_numkids == 5 &  t(unique(h_seq)) == 0 & spmu_resources - spmu_actc + pmin( 5*f(ftotval) + 0*c(ftotval), 5000) < spmu_povthreshold) | 
			(spmu_numkids == 6 &  t(unique(h_seq)) == 0 & spmu_resources - spmu_actc + pmin( 6*f(ftotval) + 0*c(ftotval), 6000) < spmu_povthreshold) | 
			(spmu_numkids == 7 &  t(unique(h_seq)) == 0 & spmu_resources - spmu_actc + pmin( 7*f(ftotval) + 0*c(ftotval), 7000) < spmu_povthreshold) | 
			(spmu_numkids == 8 &  t(unique(h_seq)) == 0 & spmu_resources - spmu_actc + pmin( 8*f(ftotval) + 0*c(ftotval), 8000) < spmu_povthreshold) | 
			(spmu_numkids == 9 &  t(unique(h_seq)) == 0 & spmu_resources - spmu_actc + pmin( 9*f(ftotval) + 0*c(ftotval), 9000) < spmu_povthreshold) | 
			(spmu_numkids == 10 &  t(unique(h_seq)) == 0 & spmu_resources - spmu_actc + pmin( 10*f(ftotval) + 0*c(ftotval), 10000) < spmu_povthreshold) | 
			(spmu_numkids == 11 &  t(unique(h_seq)) == 0 & spmu_resources - spmu_actc + pmin( 11*f(ftotval) + 0*c(ftotval), 11000) < spmu_povthreshold) | 
			(spmu_numkids == 12 &  t(unique(h_seq)) == 0 & spmu_resources - spmu_actc + pmin( 12*f(ftotval) + 0*c(ftotval), 12000) < spmu_povthreshold) | 
			(spmu_numkids == 13 &  t(unique(h_seq)) == 0 & spmu_resources - spmu_actc + pmin( 13*f(ftotval) + 0*c(ftotval), 13000) < spmu_povthreshold) | 
			(spmu_numkids == 14 &  t(unique(h_seq)) == 0 & spmu_resources - spmu_actc + pmin( 14*f(ftotval) + 0*c(ftotval), 14000) < spmu_povthreshold) | 
			(spmu_numkids == 15 &  t(unique(h_seq)) == 0 & spmu_resources - spmu_actc + pmin( 15*f(ftotval) + 0*c(ftotval), 15000) < spmu_povthreshold) | 
			(spmu_numkids == 16 &  t(unique(h_seq)) == 0 & spmu_resources - spmu_actc + pmin( 16*f(ftotval) + 0*c(ftotval), 16000) < spmu_povthreshold) | 
			(spmu_numkids == 17 &  t(unique(h_seq)) == 0 & spmu_resources - spmu_actc + pmin( 17*f(ftotval) + 0*c(ftotval), 17000) < spmu_povthreshold) | 
			(spmu_numkids == 18 &  t(unique(h_seq)) == 0 & spmu_resources - spmu_actc + pmin( 18*f(ftotval) + 0*c(ftotval), 18000) < spmu_povthreshold) | 
			(spmu_numkids == 19 &  t(unique(h_seq)) == 0 & spmu_resources - spmu_actc + pmin( 19*f(ftotval) + 0*c(ftotval), 19000) < spmu_povthreshold) |
			(spmu_numkids >= 20 &  t(unique(h_seq)) == 0 & spmu_resources - spmu_actc + pmin( 20*f(ftotval) + 0*c(ftotval), 20000) < spmu_povthreshold) |
			(spmu_numkids == 1 &  t(unique(h_seq)) == 1 & spmu_resources - spmu_actc + pmin( 1*f(ftotval) + 1*c(ftotval), 2000) < spmu_povthreshold) | 
			(spmu_numkids == 2 &  t(unique(h_seq)) == 1 & spmu_resources - spmu_actc + pmin( 2*f(ftotval) + 1*c(ftotval), 3000) < spmu_povthreshold) | 
			(spmu_numkids == 3 &  t(unique(h_seq)) == 1 & spmu_resources - spmu_actc + pmin( 3*f(ftotval) + 1*c(ftotval), 4000) < spmu_povthreshold) | 
			(spmu_numkids == 4 &  t(unique(h_seq)) == 1 & spmu_resources - spmu_actc + pmin( 4*f(ftotval) + 1*c(ftotval), 5000) < spmu_povthreshold) | 
			(spmu_numkids == 5 &  t(unique(h_seq)) == 1 & spmu_resources - spmu_actc + pmin( 5*f(ftotval) + 1*c(ftotval), 6000) < spmu_povthreshold) | 
			(spmu_numkids == 6 &  t(unique(h_seq)) == 1 & spmu_resources - spmu_actc + pmin( 6*f(ftotval) + 1*c(ftotval), 7000) < spmu_povthreshold) | 
			(spmu_numkids == 7 &  t(unique(h_seq)) == 1 & spmu_resources - spmu_actc + pmin( 7*f(ftotval) + 1*c(ftotval), 8000) < spmu_povthreshold) | 
			(spmu_numkids == 8 &  t(unique(h_seq)) == 1 & spmu_resources - spmu_actc + pmin( 8*f(ftotval) + 1*c(ftotval), 9000) < spmu_povthreshold) | 
			(spmu_numkids == 9 &  t(unique(h_seq)) == 1 & spmu_resources - spmu_actc + pmin( 9*f(ftotval) + 1*c(ftotval), 10000) < spmu_povthreshold) | 
			(spmu_numkids == 10 &  t(unique(h_seq)) == 1 & spmu_resources - spmu_actc + pmin( 10*f(ftotval) + 1*c(ftotval), 11000) < spmu_povthreshold) | 
			(spmu_numkids == 11 &  t(unique(h_seq)) == 1 & spmu_resources - spmu_actc + pmin( 11*f(ftotval) + 1*c(ftotval), 12000) < spmu_povthreshold) | 
			(spmu_numkids == 12 &  t(unique(h_seq)) == 1 & spmu_resources - spmu_actc + pmin( 12*f(ftotval) + 1*c(ftotval), 13000) < spmu_povthreshold) | 
			(spmu_numkids == 13 &  t(unique(h_seq)) == 1 & spmu_resources - spmu_actc + pmin( 13*f(ftotval) + 1*c(ftotval), 14000) < spmu_povthreshold) | 
			(spmu_numkids == 14 &  t(unique(h_seq)) == 1 & spmu_resources - spmu_actc + pmin( 14*f(ftotval) + 1*c(ftotval), 15000) < spmu_povthreshold) | 
			(spmu_numkids == 15 &  t(unique(h_seq)) == 1 & spmu_resources - spmu_actc + pmin( 15*f(ftotval) + 1*c(ftotval), 16000) < spmu_povthreshold) | 
			(spmu_numkids == 16 &  t(unique(h_seq)) == 1 & spmu_resources - spmu_actc + pmin( 16*f(ftotval) + 1*c(ftotval), 17000) < spmu_povthreshold) | 
			(spmu_numkids == 17 &  t(unique(h_seq)) == 1 & spmu_resources - spmu_actc + pmin( 17*f(ftotval) + 1*c(ftotval), 18000) < spmu_povthreshold) | 
			(spmu_numkids == 18 &  t(unique(h_seq)) == 1 & spmu_resources - spmu_actc + pmin( 18*f(ftotval) + 1*c(ftotval), 19000) < spmu_povthreshold) | 
			(spmu_numkids == 19 &  t(unique(h_seq)) == 1 & spmu_resources - spmu_actc + pmin( 19*f(ftotval) + 1*c(ftotval), 20000) < spmu_povthreshold) |
			(spmu_numkids == 20 &  t(unique(h_seq)) == 1 & spmu_resources - spmu_actc + pmin( 20*f(ftotval) + 1*c(ftotval), 21000) < spmu_povthreshold)	|		
			(spmu_numkids == 1 &  t(unique(h_seq)) == 2 & spmu_resources - spmu_actc + pmin( 1*f(ftotval) + 2*c(ftotval), 2000) < spmu_povthreshold) | 
			(spmu_numkids == 2 &  t(unique(h_seq)) == 2 & spmu_resources - spmu_actc + pmin( 2*f(ftotval) + 2*c(ftotval), 4000) < spmu_povthreshold) | 
			(spmu_numkids == 3 &  t(unique(h_seq)) == 2 & spmu_resources - spmu_actc + pmin( 3*f(ftotval) + 2*c(ftotval), 5000) < spmu_povthreshold) | 
			(spmu_numkids == 4 &  t(unique(h_seq)) == 2 & spmu_resources - spmu_actc + pmin( 4*f(ftotval) + 2*c(ftotval), 6000) < spmu_povthreshold) | 
			(spmu_numkids == 5 &  t(unique(h_seq)) == 2 & spmu_resources - spmu_actc + pmin( 5*f(ftotval) + 2*c(ftotval), 7000) < spmu_povthreshold) | 
			(spmu_numkids == 6 &  t(unique(h_seq)) == 2 & spmu_resources - spmu_actc + pmin( 6*f(ftotval) + 2*c(ftotval), 8000) < spmu_povthreshold) | 
			(spmu_numkids == 7 &  t(unique(h_seq)) == 2 & spmu_resources - spmu_actc + pmin( 7*f(ftotval) + 2*c(ftotval), 9000) < spmu_povthreshold) | 
			(spmu_numkids == 8 &  t(unique(h_seq)) == 2 & spmu_resources - spmu_actc + pmin( 8*f(ftotval) + 2*c(ftotval), 10000) < spmu_povthreshold) | 
			(spmu_numkids == 9 &  t(unique(h_seq)) == 2 & spmu_resources - spmu_actc + pmin( 9*f(ftotval) + 2*c(ftotval), 11000) < spmu_povthreshold) | 
			(spmu_numkids == 10 &  t(unique(h_seq)) == 2 & spmu_resources - spmu_actc + pmin( 10*f(ftotval) + 2*c(ftotval), 12000) < spmu_povthreshold) | 
			(spmu_numkids == 11 &  t(unique(h_seq)) == 2 & spmu_resources - spmu_actc + pmin( 11*f(ftotval) + 2*c(ftotval), 13000) < spmu_povthreshold) | 
			(spmu_numkids == 12 &  t(unique(h_seq)) == 2 & spmu_resources - spmu_actc + pmin( 12*f(ftotval) + 2*c(ftotval), 14000) < spmu_povthreshold) | 
			(spmu_numkids == 13 &  t(unique(h_seq)) == 2 & spmu_resources - spmu_actc + pmin( 13*f(ftotval) + 2*c(ftotval), 15000) < spmu_povthreshold) | 
			(spmu_numkids == 14 &  t(unique(h_seq)) == 2 & spmu_resources - spmu_actc + pmin( 14*f(ftotval) + 2*c(ftotval), 16000) < spmu_povthreshold) | 
			(spmu_numkids == 15 &  t(unique(h_seq)) == 2 & spmu_resources - spmu_actc + pmin( 15*f(ftotval) + 2*c(ftotval), 17000) < spmu_povthreshold) | 
			(spmu_numkids == 16 &  t(unique(h_seq)) == 2 & spmu_resources - spmu_actc + pmin( 16*f(ftotval) + 2*c(ftotval), 18000) < spmu_povthreshold) | 
			(spmu_numkids == 17 &  t(unique(h_seq)) == 2 & spmu_resources - spmu_actc + pmin( 17*f(ftotval) + 2*c(ftotval), 19000) < spmu_povthreshold) | 
			(spmu_numkids == 18 &  t(unique(h_seq)) == 2 & spmu_resources - spmu_actc + pmin( 18*f(ftotval) + 2*c(ftotval), 20000) < spmu_povthreshold) | 
			(spmu_numkids == 19 &  t(unique(h_seq)) == 2 & spmu_resources - spmu_actc + pmin( 19*f(ftotval) + 2*c(ftotval), 21000) < spmu_povthreshold) |
			(spmu_numkids == 20 &  t(unique(h_seq)) == 2 & spmu_resources - spmu_actc + pmin( 20*f(ftotval) + 2*c(ftotval), 22000) < spmu_povthreshold)	|	
			(spmu_numkids == 1 &  t(unique(h_seq)) == 3 & spmu_resources - spmu_actc + pmin( 1*f(ftotval) + 3*c(ftotval), 2000) < spmu_povthreshold) | 
			(spmu_numkids == 2 &  t(unique(h_seq)) == 3 & spmu_resources - spmu_actc + pmin( 2*f(ftotval) + 3*c(ftotval), 4000) < spmu_povthreshold) | 
			(spmu_numkids == 3 &  t(unique(h_seq)) == 3 & spmu_resources - spmu_actc + pmin( 3*f(ftotval) + 3*c(ftotval), 6000) < spmu_povthreshold) | 
			(spmu_numkids == 4 &  t(unique(h_seq)) == 3 & spmu_resources - spmu_actc + pmin( 4*f(ftotval) + 3*c(ftotval), 7000) < spmu_povthreshold) | 
			(spmu_numkids == 5 &  t(unique(h_seq)) == 3 & spmu_resources - spmu_actc + pmin( 5*f(ftotval) + 3*c(ftotval), 8000) < spmu_povthreshold) | 
			(spmu_numkids == 6 &  t(unique(h_seq)) == 3 & spmu_resources - spmu_actc + pmin( 6*f(ftotval) + 3*c(ftotval), 9000) < spmu_povthreshold) | 
			(spmu_numkids == 7 &  t(unique(h_seq)) == 3 & spmu_resources - spmu_actc + pmin( 7*f(ftotval) + 3*c(ftotval), 10000) < spmu_povthreshold) | 
			(spmu_numkids == 8 &  t(unique(h_seq)) == 3 & spmu_resources - spmu_actc + pmin( 8*f(ftotval) + 3*c(ftotval), 11000) < spmu_povthreshold) | 
			(spmu_numkids == 9 &  t(unique(h_seq)) == 3 & spmu_resources - spmu_actc + pmin( 9*f(ftotval) + 3*c(ftotval), 12000) < spmu_povthreshold) | 
			(spmu_numkids == 10 &  t(unique(h_seq)) == 3 & spmu_resources - spmu_actc + pmin( 10*f(ftotval) + 3*c(ftotval), 13000) < spmu_povthreshold) | 
			(spmu_numkids == 11 &  t(unique(h_seq)) == 3 & spmu_resources - spmu_actc + pmin( 11*f(ftotval) + 3*c(ftotval), 14000) < spmu_povthreshold) | 
			(spmu_numkids == 12 &  t(unique(h_seq)) == 3 & spmu_resources - spmu_actc + pmin( 12*f(ftotval) + 3*c(ftotval), 15000) < spmu_povthreshold) | 
			(spmu_numkids == 13 &  t(unique(h_seq)) == 3 & spmu_resources - spmu_actc + pmin( 13*f(ftotval) + 3*c(ftotval), 16000) < spmu_povthreshold) | 
			(spmu_numkids == 14 &  t(unique(h_seq)) == 3 & spmu_resources - spmu_actc + pmin( 14*f(ftotval) + 3*c(ftotval), 17000) < spmu_povthreshold) | 
			(spmu_numkids == 15 &  t(unique(h_seq)) == 3 & spmu_resources - spmu_actc + pmin( 15*f(ftotval) + 3*c(ftotval), 18000) < spmu_povthreshold) | 
			(spmu_numkids == 16 &  t(unique(h_seq)) == 3 & spmu_resources - spmu_actc + pmin( 16*f(ftotval) + 3*c(ftotval), 19000) < spmu_povthreshold) | 
			(spmu_numkids == 17 &  t(unique(h_seq)) == 3 & spmu_resources - spmu_actc + pmin( 17*f(ftotval) + 3*c(ftotval), 20000) < spmu_povthreshold) | 
			(spmu_numkids == 18 &  t(unique(h_seq)) == 3 & spmu_resources - spmu_actc + pmin( 18*f(ftotval) + 3*c(ftotval), 21000) < spmu_povthreshold) | 
			(spmu_numkids == 19 &  t(unique(h_seq)) == 3 & spmu_resources - spmu_actc + pmin( 19*f(ftotval) + 3*c(ftotval), 22000) < spmu_povthreshold) |
			(spmu_numkids == 20 &  t(unique(h_seq)) == 3 & spmu_resources - spmu_actc + pmin( 20*f(ftotval) + 3*c(ftotval), 23000) < spmu_povthreshold)	|	
			(spmu_numkids == 1 &  t(unique(h_seq)) == 4 & spmu_resources - spmu_actc + pmin( 1*f(ftotval) + 4*c(ftotval), 2000) < spmu_povthreshold) | 
			(spmu_numkids == 2 &  t(unique(h_seq)) == 4 & spmu_resources - spmu_actc + pmin( 2*f(ftotval) + 4*c(ftotval), 4000) < spmu_povthreshold) | 
			(spmu_numkids == 3 &  t(unique(h_seq)) == 4 & spmu_resources - spmu_actc + pmin( 3*f(ftotval) + 4*c(ftotval), 6000) < spmu_povthreshold) | 
			(spmu_numkids == 4 &  t(unique(h_seq)) == 4 & spmu_resources - spmu_actc + pmin( 4*f(ftotval) + 4*c(ftotval), 8000) < spmu_povthreshold) | 
			(spmu_numkids == 5 &  t(unique(h_seq)) == 4 & spmu_resources - spmu_actc + pmin( 5*f(ftotval) + 4*c(ftotval), 9000) < spmu_povthreshold) | 
			(spmu_numkids == 6 &  t(unique(h_seq)) == 4 & spmu_resources - spmu_actc + pmin( 6*f(ftotval) + 4*c(ftotval), 10000) < spmu_povthreshold) | 
			(spmu_numkids == 7 &  t(unique(h_seq)) == 4 & spmu_resources - spmu_actc + pmin( 7*f(ftotval) + 4*c(ftotval), 11000) < spmu_povthreshold) | 
			(spmu_numkids == 8 &  t(unique(h_seq)) == 4 & spmu_resources - spmu_actc + pmin( 8*f(ftotval) + 4*c(ftotval), 12000) < spmu_povthreshold) | 
			(spmu_numkids == 9 &  t(unique(h_seq)) == 4 & spmu_resources - spmu_actc + pmin( 9*f(ftotval) + 4*c(ftotval), 13000) < spmu_povthreshold) | 
			(spmu_numkids == 10 &  t(unique(h_seq)) == 4 & spmu_resources - spmu_actc + pmin( 10*f(ftotval) + 4*c(ftotval), 14000) < spmu_povthreshold) | 
			(spmu_numkids == 11 &  t(unique(h_seq)) == 4 & spmu_resources - spmu_actc + pmin( 11*f(ftotval) + 4*c(ftotval), 15000) < spmu_povthreshold) | 
			(spmu_numkids == 12 &  t(unique(h_seq)) == 4 & spmu_resources - spmu_actc + pmin( 12*f(ftotval) + 4*c(ftotval), 16000) < spmu_povthreshold) | 
			(spmu_numkids == 13 &  t(unique(h_seq)) == 4 & spmu_resources - spmu_actc + pmin( 13*f(ftotval) + 4*c(ftotval), 17000) < spmu_povthreshold) | 
			(spmu_numkids == 14 &  t(unique(h_seq)) == 4 & spmu_resources - spmu_actc + pmin( 14*f(ftotval) + 4*c(ftotval), 18000) < spmu_povthreshold) | 
			(spmu_numkids == 15 &  t(unique(h_seq)) == 4 & spmu_resources - spmu_actc + pmin( 15*f(ftotval) + 4*c(ftotval), 19000) < spmu_povthreshold) | 
			(spmu_numkids == 16 &  t(unique(h_seq)) == 4 & spmu_resources - spmu_actc + pmin( 16*f(ftotval) + 4*c(ftotval), 20000) < spmu_povthreshold) | 
			(spmu_numkids == 17 &  t(unique(h_seq)) == 4 & spmu_resources - spmu_actc + pmin( 17*f(ftotval) + 4*c(ftotval), 21000) < spmu_povthreshold) | 
			(spmu_numkids == 18 &  t(unique(h_seq)) == 4 & spmu_resources - spmu_actc + pmin( 18*f(ftotval) + 4*c(ftotval), 22000) < spmu_povthreshold) | 
			(spmu_numkids == 19 &  t(unique(h_seq)) == 4 & spmu_resources - spmu_actc + pmin( 19*f(ftotval) + 4*c(ftotval), 23000) < spmu_povthreshold) |
			(spmu_numkids == 20 &  t(unique(h_seq)) == 4 & spmu_resources - spmu_actc + pmin( 20*f(ftotval) + 4*c(ftotval), 24000) < spmu_povthreshold)	|				
			(spmu_numkids == 1 &  t(unique(h_seq)) == 5 & spmu_resources - spmu_actc + pmin( 1*f(ftotval) + 5*c(ftotval), 2000) < spmu_povthreshold) | 
			(spmu_numkids == 2 &  t(unique(h_seq)) == 5 & spmu_resources - spmu_actc + pmin( 2*f(ftotval) + 5*c(ftotval), 4000) < spmu_povthreshold) | 
			(spmu_numkids == 3 &  t(unique(h_seq)) == 5 & spmu_resources - spmu_actc + pmin( 3*f(ftotval) + 5*c(ftotval), 6000) < spmu_povthreshold) | 
			(spmu_numkids == 4 &  t(unique(h_seq)) == 5 & spmu_resources - spmu_actc + pmin( 4*f(ftotval) + 5*c(ftotval), 8000) < spmu_povthreshold) | 
			(spmu_numkids == 5 &  t(unique(h_seq)) == 5 & spmu_resources - spmu_actc + pmin( 5*f(ftotval) + 5*c(ftotval), 10000) < spmu_povthreshold) | 
			(spmu_numkids == 6 &  t(unique(h_seq)) == 5 & spmu_resources - spmu_actc + pmin( 6*f(ftotval) + 5*c(ftotval), 11000) < spmu_povthreshold) | 
			(spmu_numkids == 7 &  t(unique(h_seq)) == 5 & spmu_resources - spmu_actc + pmin( 7*f(ftotval) + 5*c(ftotval), 12000) < spmu_povthreshold) | 
			(spmu_numkids == 8 &  t(unique(h_seq)) == 5 & spmu_resources - spmu_actc + pmin( 8*f(ftotval) + 5*c(ftotval), 13000) < spmu_povthreshold) | 
			(spmu_numkids == 9 &  t(unique(h_seq)) == 5 & spmu_resources - spmu_actc + pmin( 9*f(ftotval) + 5*c(ftotval), 14000) < spmu_povthreshold) | 
			(spmu_numkids == 10 &  t(unique(h_seq)) == 5 & spmu_resources - spmu_actc + pmin( 10*f(ftotval) + 5*c(ftotval), 15000) < spmu_povthreshold) | 
			(spmu_numkids == 11 &  t(unique(h_seq)) == 5 & spmu_resources - spmu_actc + pmin( 11*f(ftotval) + 5*c(ftotval), 16000) < spmu_povthreshold) | 
			(spmu_numkids == 12 &  t(unique(h_seq)) == 5 & spmu_resources - spmu_actc + pmin( 12*f(ftotval) + 5*c(ftotval), 17000) < spmu_povthreshold) | 
			(spmu_numkids == 13 &  t(unique(h_seq)) == 5 & spmu_resources - spmu_actc + pmin( 13*f(ftotval) + 5*c(ftotval), 18000) < spmu_povthreshold) | 
			(spmu_numkids == 14 &  t(unique(h_seq)) == 5 & spmu_resources - spmu_actc + pmin( 14*f(ftotval) + 5*c(ftotval), 19000) < spmu_povthreshold) | 
			(spmu_numkids == 15 &  t(unique(h_seq)) == 5 & spmu_resources - spmu_actc + pmin( 15*f(ftotval) + 5*c(ftotval), 20000) < spmu_povthreshold) | 
			(spmu_numkids == 16 &  t(unique(h_seq)) == 5 & spmu_resources - spmu_actc + pmin( 16*f(ftotval) + 5*c(ftotval), 21000) < spmu_povthreshold) | 
			(spmu_numkids == 17 &  t(unique(h_seq)) == 5 & spmu_resources - spmu_actc + pmin( 17*f(ftotval) + 5*c(ftotval), 22000) < spmu_povthreshold) | 
			(spmu_numkids == 18 &  t(unique(h_seq)) == 5 & spmu_resources - spmu_actc + pmin( 18*f(ftotval) + 5*c(ftotval), 23000) < spmu_povthreshold) | 
			(spmu_numkids == 19 &  t(unique(h_seq)) == 5 & spmu_resources - spmu_actc + pmin( 19*f(ftotval) + 5*c(ftotval), 24000) < spmu_povthreshold) |
			(spmu_numkids == 20 &  t(unique(h_seq)) == 5 & spmu_resources - spmu_actc + pmin( 20*f(ftotval) + 5*c(ftotval), 25000) < spmu_povthreshold)	|	
			(spmu_numkids == 1 &  t(unique(h_seq)) == 6 & spmu_resources - spmu_actc + pmin( 1*f(ftotval) + 6*c(ftotval), 2000) < spmu_povthreshold) | 
			(spmu_numkids == 2 &  t(unique(h_seq)) == 6 & spmu_resources - spmu_actc + pmin( 2*f(ftotval) + 6*c(ftotval), 4000) < spmu_povthreshold) | 
			(spmu_numkids == 3 &  t(unique(h_seq)) == 6 & spmu_resources - spmu_actc + pmin( 3*f(ftotval) + 6*c(ftotval), 6000) < spmu_povthreshold) | 
			(spmu_numkids == 4 &  t(unique(h_seq)) == 6 & spmu_resources - spmu_actc + pmin( 4*f(ftotval) + 6*c(ftotval), 8000) < spmu_povthreshold) | 
			(spmu_numkids == 5 &  t(unique(h_seq)) == 6 & spmu_resources - spmu_actc + pmin( 5*f(ftotval) + 6*c(ftotval), 10000) < spmu_povthreshold) | 
			(spmu_numkids == 6 &  t(unique(h_seq)) == 6 & spmu_resources - spmu_actc + pmin( 6*f(ftotval) + 6*c(ftotval), 12000) < spmu_povthreshold) | 
			(spmu_numkids == 7 &  t(unique(h_seq)) == 6 & spmu_resources - spmu_actc + pmin( 7*f(ftotval) + 6*c(ftotval), 13000) < spmu_povthreshold) | 
			(spmu_numkids == 8 &  t(unique(h_seq)) == 6 & spmu_resources - spmu_actc + pmin( 8*f(ftotval) + 6*c(ftotval), 14000) < spmu_povthreshold) | 
			(spmu_numkids == 9 &  t(unique(h_seq)) == 6 & spmu_resources - spmu_actc + pmin( 9*f(ftotval) + 6*c(ftotval), 15000) < spmu_povthreshold) | 
			(spmu_numkids == 10 &  t(unique(h_seq)) == 6 & spmu_resources - spmu_actc + pmin( 10*f(ftotval) + 6*c(ftotval), 16000) < spmu_povthreshold) | 
			(spmu_numkids == 11 &  t(unique(h_seq)) == 6 & spmu_resources - spmu_actc + pmin( 11*f(ftotval) + 6*c(ftotval), 17000) < spmu_povthreshold) | 
			(spmu_numkids == 12 &  t(unique(h_seq)) == 6 & spmu_resources - spmu_actc + pmin( 12*f(ftotval) + 6*c(ftotval), 18000) < spmu_povthreshold) | 
			(spmu_numkids == 13 &  t(unique(h_seq)) == 6 & spmu_resources - spmu_actc + pmin( 13*f(ftotval) + 6*c(ftotval), 19000) < spmu_povthreshold) | 
			(spmu_numkids == 14 &  t(unique(h_seq)) == 6 & spmu_resources - spmu_actc + pmin( 14*f(ftotval) + 6*c(ftotval), 20000) < spmu_povthreshold) | 
			(spmu_numkids == 15 &  t(unique(h_seq)) == 6 & spmu_resources - spmu_actc + pmin( 15*f(ftotval) + 6*c(ftotval), 21000) < spmu_povthreshold) | 
			(spmu_numkids == 16 &  t(unique(h_seq)) == 6 & spmu_resources - spmu_actc + pmin( 16*f(ftotval) + 6*c(ftotval), 22000) < spmu_povthreshold) | 
			(spmu_numkids == 17 &  t(unique(h_seq)) == 6 & spmu_resources - spmu_actc + pmin( 17*f(ftotval) + 6*c(ftotval), 23000) < spmu_povthreshold) | 
			(spmu_numkids == 18 &  t(unique(h_seq)) == 6 & spmu_resources - spmu_actc + pmin( 18*f(ftotval) + 6*c(ftotval), 24000) < spmu_povthreshold) | 
			(spmu_numkids == 19 &  t(unique(h_seq)) == 6 & spmu_resources - spmu_actc + pmin( 19*f(ftotval) + 6*c(ftotval), 25000) < spmu_povthreshold) |
			(spmu_numkids == 20 &  t(unique(h_seq)) == 6 & spmu_resources - spmu_actc + pmin( 20*f(ftotval) + 6*c(ftotval), 26000) < spmu_povthreshold))

svytotal(
	~one ,
	Clinton_Plan)



