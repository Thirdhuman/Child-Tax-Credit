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


y <-
	svrepdesign(
		weights = ~marsupwt, 
		repweights = "pwwgt[1-9]", 
		type = "Fay", 
		rho = (1-1/sqrt(4)),
		data = "asec15" ,
		combined.weights = T ,
		dbtype = "MonetDBLite" ,
		dbname = dbfolder
	)

y$mse <- TRUE

Own children in family under 6
& fownu6 > 0

pov_total <-subset( 
		y , 
		(
			(spmu_numkids == 0 & spmu_resources < spmu_povthreshold) | 
			(spmu_numkids == 1 & spmu_resources < spmu_povthreshold) | 
			(spmu_numkids == 2 & spmu_resources < spmu_povthreshold) | 
			(spmu_numkids == 3 & spmu_resources < spmu_povthreshold) | 
			(spmu_numkids == 4 & spmu_resources < spmu_povthreshold) | 
			(spmu_numkids == 5 & spmu_resources < spmu_povthreshold) | 
			(spmu_numkids == 6 & spmu_resources < spmu_povthreshold) | 
			(spmu_numkids == 7 & spmu_resources < spmu_povthreshold) | 
			(spmu_numkids == 8 & spmu_resources < spmu_povthreshold) | 
			(spmu_numkids == 9 & spmu_resources < spmu_povthreshold) | 
			(spmu_numkids == 10 & spmu_resources < spmu_povthreshold) | 
			(spmu_numkids == 11 & spmu_resources < spmu_povthreshold) | 
			(spmu_numkids == 12 & spmu_resources < spmu_povthreshold) | 
			(spmu_numkids == 13 & spmu_resources < spmu_povthreshold) | 
			(spmu_numkids == 14 & spmu_resources < spmu_povthreshold) | 
			(spmu_numkids == 16 & spmu_resources < spmu_povthreshold) | 
			(spmu_numkids == 17 & spmu_resources < spmu_povthreshold) | 
			(spmu_numkids == 18 & spmu_resources < spmu_povthreshold) | 
			(spmu_numkids == 19 & spmu_resources < spmu_povthreshold) | 
			(spmu_numkids >= 20 & spmu_resources < spmu_povthreshold)))


#	Clinton Increses the child tax credit from $1,000 to $2,000 and increasing the phasein rate from 15 percent to 45 percent 
#for the refundable portion of the credit, for families with eligible children under age 5. 

#She would also eliminate the minimum earnings requirement
#(currently $3,000) for all families with eligible children (under age 17) so that tax relief would
#begin with the first dollar earned.

	
f = function(x) if (x < 1000) exp(seq(log(x), log(1000), by=log(1.15))) else 1000
c = function(x) if (x < 2000) exp(seq(log(x), log(2000), by=log(1.45))) else 2000




Pov_w2000 <-subset( 
		y , 
			(spmu_numkids == 0 & spmu_resources < spmu_povthreshold) | 
			(spmu_numkids == 1 & (spmu_resources - spmu_actc + 1000*2) < spmu_povthreshold) | 
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
			


Pov_w2000

#################

pov_total <-subset( 
		y , 
		(
			workyn == 1,
			agi > 15500,
			agi > 14500,

			WEXP
			))


pov_total <-subset( 
		y , 
		(spmu_poor == 1))


	update(
		under_5 = factor( spmu_numkids ) ,
		pov_total_u6
	)



clinton_kids_under5 <-subset( 
		pov_total , 
		(a_age < 5 & unique()))


pov_total_u6f <-
	update(
		spmu_numkids = factor( spmu_numkids ) ,
		pov_total_u6
	)


pov_total_u6f <-
	update(
		spmu_numkids = factor( spmu_numkids ) ,
		pov_total_u6
	)



pov_kids_5p <-
	svyby(
		~a_age < 5,
		~spmu_numkids ,
		design = pov_total_u6f ,
		svymean
	)

pov_kids_5t <-
	svyby(
		~a_age < 5,
		~spmu_numkids ,
		design = pov_total_u6f ,
		svytotal
	)

pov_kids_5t
pov_kids_5p

pov_kids_6 <-
	svyby(
		~spmu_numkids,
		~a_age,
		design = pov_total_u6 ,
		svymean
	)

pov_kids_6 <-
	svyby(
		~spmu_numkids < 6,
		~a_age ,
		design = pov_total ,
		svymean
	)

pov_kids_5 <-
	svyby(
		~spmu_numkids < 5,
		~a_age ,
		design = pov_total ,
		svymean
	)



# percent married - nationwide
svyby(
	~a_age,
	~spmu_numkids,
	design = pov_total,
	svymean
)


Age_6 <- svyby(
	~ ,
	~,
	design = pov_total,
	svytotal
)



svyby(
	~a_age,
	~spmu_numkids,
	design = Age_6,
	svytotal
)



svytotal(
	~one ,
	Pov_w2000)
		
svytotal(
	~one ,
	pov_total)

svytotal(
	~one ,
	Clinton_Plan)

svytotal(
	~one ,
	Pov_w2000_4max)



svytotal(
	~one ,
	y
)




Pov_w2000_4max <-subset( 
		y , 
			(spmu_numkids == 0 & spmu_resources < spmu_povthreshold) | 
			(spmu_numkids == 1 & (spmu_resources - spmu_actc + 1000*2) < spmu_povthreshold) | 
			(spmu_numkids == 2 & (spmu_resources - spmu_actc + 2000*2) < spmu_povthreshold) | 
			(spmu_numkids == 3 & (spmu_resources - spmu_actc + 3000*2) < spmu_povthreshold) | 
			(spmu_numkids == 3 & (spmu_resources - spmu_actc + 3000*2) < spmu_povthreshold) | 
			(spmu_numkids == 4 & (spmu_resources - spmu_actc + 4000*2) < spmu_povthreshold) | 
			(spmu_numkids == 5 & (spmu_resources - spmu_actc + 4000*2) < spmu_povthreshold) | 
			(spmu_numkids == 6 & (spmu_resources - spmu_actc + 4000*2) < spmu_povthreshold) | 
			(spmu_numkids == 7 & (spmu_resources - spmu_actc + 4000*2) < spmu_povthreshold) | 
			(spmu_numkids == 8 & (spmu_resources - spmu_actc + 4000*2) < spmu_povthreshold) | 
			(spmu_numkids == 9 & (spmu_resources - spmu_actc + 4000*2) < spmu_povthreshold) | 
			(spmu_numkids == 10 & (spmu_resources - spmu_actc + 4000*2) < spmu_povthreshold) | 
			(spmu_numkids == 11 & (spmu_resources - spmu_actc + 4000*2) < spmu_povthreshold) | 
			(spmu_numkids == 12 & (spmu_resources - spmu_actc + 4000*2) < spmu_povthreshold) | 
			(spmu_numkids == 13 & (spmu_resources - spmu_actc + 4000*2) < spmu_povthreshold) | 
			(spmu_numkids == 14 & (spmu_resources - spmu_actc + 4000*2) < spmu_povthreshold) | 
			(spmu_numkids == 15 & (spmu_resources - spmu_actc + 4000*2) < spmu_povthreshold) | 
			(spmu_numkids == 16 & (spmu_resources - spmu_actc + 4000*2) < spmu_povthreshold) | 
			(spmu_numkids == 17 & (spmu_resources - spmu_actc + 4000*2) < spmu_povthreshold) | 
			(spmu_numkids == 18 & (spmu_resources - spmu_actc + 4000*2) < spmu_povthreshold) | 
			(spmu_numkids == 19 & (spmu_resources - spmu_actc + 4000*2) < spmu_povthreshold) |
			(spmu_numkids >= 20 & (spmu_resources - spmu_actc + 4000*2) < spmu_povthreshold))
			


Pov_w2000


