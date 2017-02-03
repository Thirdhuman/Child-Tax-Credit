#https://cps.ipums.org/cps/poverty_notes.shtml

setwd("~/Desktop/Poverty:Welfare/Data/R - Survey data/Current Population Survey (CPS)")

library(survey)				# load survey package (analyzes complex design surveys)
library(MonetDBLite)
library(DBI)			# load the DBI package (implements the R-database coding)

dbfolder <- paste0( getwd() , "/MonetDB" )

options( survey.replicates.mse = TRUE )

y16 <-
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

y16$mse <- TRUE


y15 <-
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

y15$mse <- TRUE


y14 <-
	svrepdesign(
		weights = ~marsupwt, 
		repweights = "pwwgt[1-9]", 
		type = "Fay", 
		rho = (1-1/sqrt(4)),
		data = "asec14" ,
		combined.weights = T ,
		dbtype = "MonetDBLite" ,
		dbname = dbfolder
	)

y14$mse <- TRUE


y13 <-
	svrepdesign(
		weights = ~marsupwt, 
		repweights = "pwwgt[1-9]", 
		type = "Fay", 
		rho = (1-1/sqrt(4)),
		data = "asec13" ,
		combined.weights = T ,
		dbtype = "MonetDBLite" ,
		dbname = dbfolder
	)

y13$mse <- TRUE


y12 <-
	svrepdesign(
		weights = ~marsupwt, 
		repweights = "pwwgt[1-9]", 
		type = "Fay", 
		rho = (1-1/sqrt(4)),
		data = "asec12" ,
		combined.weights = T ,
		dbtype = "MonetDBLite" ,
		dbname = dbfolder
	)

y12$mse <- TRUE


y11 <-
	svrepdesign(
		weights = ~marsupwt, 
		repweights = "pwwgt[1-9]", 
		type = "Fay", 
		rho = (1-1/sqrt(4)),
		data = "asec11" ,
		combined.weights = T ,
		dbtype = "MonetDBLite" ,
		dbname = dbfolder
	)

y11$mse <- TRUE

y10 <-
	svrepdesign(
		weights = ~marsupwt, 
		repweights = "pwwgt[1-9]", 
		type = "Fay", 
		rho = (1-1/sqrt(4)),
		data = "asec10" ,
		combined.weights = T ,
		dbtype = "MonetDBLite" ,
		dbname = dbfolder
	)

y10$mse <- TRUE

y9 <-
	svrepdesign(
		weights = ~marsupwt, 
		repweights = "pwwgt[1-9]", 
		type = "Fay", 
		rho = (1-1/sqrt(4)),
		data = "asec09" ,
		combined.weights = T ,
		dbtype = "MonetDBLite" ,
		dbname = dbfolder
	)

y9$mse <- TRUE

y8 <-
	svrepdesign(
		weights = ~marsupwt, 
		repweights = "pwwgt[1-9]", 
		type = "Fay", 
		rho = (1-1/sqrt(4)),
		data = "asec08" ,
		combined.weights = T ,
		dbtype = "MonetDBLite" ,
		dbname = dbfolder
	)

y8$mse <- TRUE

y7 <-
	svrepdesign(
		weights = ~marsupwt, 
		repweights = "pwwgt[1-9]", 
		type = "Fay", 
		rho = (1-1/sqrt(4)),
		data = "asec07" ,
		combined.weights = T ,
		dbtype = "MonetDBLite" ,
		dbname = dbfolder
	)

y7$mse <- TRUE

y6 <-
	svrepdesign(
		weights = ~marsupwt, 
		repweights = "pwwgt[1-9]", 
		type = "Fay", 
		rho = (1-1/sqrt(4)),
		data = "asec06" ,
		combined.weights = T ,
		dbtype = "MonetDBLite" ,
		dbname = dbfolder
	)

y6$mse <- TRUE

y5 <-
	svrepdesign(
		weights = ~marsupwt, 
		repweights = "pwwgt[1-9]", 
		type = "Fay", 
		rho = (1-1/sqrt(4)),
		data = "asec05" ,
		combined.weights = T ,
		dbtype = "MonetDBLite" ,
		dbname = dbfolder
	)

y5$mse <- TRUE


childpov16 <-subset( 
		y16 , 
  a_age < 19 & a_age >= 5 
	& povll < 4
		)

childpov_free16 <-subset( 
		y16 , 
	 a_age < 19& a_age >= 5 & a_hscol < 2 & povll < 6)

childpov_reduced16 <-subset( 
		y16 , 
	 a_age < 19& a_age >= 5 & a_hscol < 2 & povll < 7 & povll > 5)


childpov15 <-subset( 
		y15 , 
  a_age < 19 & a_age >= 5 
	& povll < 4
		)

childpov_free15 <-subset( 
		y15 , 
	 a_age < 19& a_age >= 5 & a_hscol < 2 & povll < 6)

childpov_reduced15 <-subset( 
		y15 , 
	 a_age < 19& a_age >= 5 & a_hscol < 2 & povll < 7 & povll > 5)


childpov14 <-subset( 
		y14 , 
  a_age < 19& a_age >= 5 
	& povll < 4)

childpov_free14 <-subset( 
		y14 , 
	 a_age < 19& a_age >= 5 & a_hscol < 2 & povll < 6)

childpov_reduced14 <-subset( 
		y14 , 
	 a_age < 19& a_age >= 5 & a_hscol < 2 & povll < 7 & povll > 5)

childpov13 <-subset( 
		y13 , 
  a_age < 19& a_age >= 5 
	& povll < 4)

childpov_free13 <-subset( 
		y13 , 
	 a_age < 19& a_age >= 5 & a_hscol < 2 & povll < 6)

childpov_reduced13 <-subset( 
		y13 , 
	 a_age < 19& a_age >= 5 & a_hscol < 2 & povll < 7 & povll > 5)

childpov12 <-subset( 
		y12 , 
  a_age < 19& a_age >= 5 
	& povll < 4)

childpov_free12 <-subset( 
		y12 , 
	 a_age < 19& a_age >= 5 & a_hscol < 2 & povll < 6)

childpov_reduced12 <-subset( 
		y12 , 
	 a_age < 19& a_age >= 5 & a_hscol < 2 & povll < 7 & povll > 5)


childpov11 <-subset( 
		y11 , 
  a_age < 19& a_age >= 5 
	& povll < 4)

childpov_free11 <-subset( 
		y11 , 
	 a_age < 19& a_age >= 5 & a_hscol < 2 & povll < 6)

childpov_reduced11 <-subset( 
		y11 , 
	 a_age < 19& a_age >= 5 & a_hscol < 2 & povll < 7 & povll > 5)

childpov10 <-subset( 
		y10 , 
  a_age < 19& a_age >= 5 
	& povll < 4)

childpov_free10 <-subset( 
		y10 , 
	 a_age < 19& a_age >= 5 & a_hscol < 2 & povll < 6)

childpov_reduced10 <-subset( 
		y10 , 
	 a_age < 19& a_age >= 5 & a_hscol < 2 & povll < 7 & povll > 5)

childpov9 <-subset( 
		y9 , 
  a_age < 19& a_age >= 5 
	& povll < 4)

childpov_free9 <-subset( 
		y9 , 
	 a_age < 19& a_age >= 5 & a_hscol < 2 & povll < 6)

childpov_reduced9 <-subset( 
		y9 , 
	 a_age < 19& a_age >= 5 & a_hscol < 2 & povll < 7 & povll > 5)

childpov8 <-subset( 
		y8 , 
  a_age < 19& a_age >= 5 
	& povll < 4)

childpov_free8 <-subset( 
		y8 , 
	 a_age < 19& a_age >= 5 & a_hscol < 2 & povll < 6)

childpov_reduced8 <-subset( 
		y8 , 
	 a_age < 19& a_age >= 5 & a_hscol < 2 & povll < 7 & povll > 5)

childpov7 <-subset( 
		y7 , 
  a_age < 19& a_age >= 5 
	& povll < 4)

childpov_free7 <-subset( 
		y7 , 
	 a_age < 19& a_age >= 5 & a_hscol < 2 & povll < 6)

childpov_reduced7 <-subset( 
		y7 , 
	 a_age < 19& a_age >= 5 & a_hscol < 2 & povll < 7 & povll > 5)

childpov6 <-subset( 
		y6 , 
  a_age < 19& a_age >= 5 
	& povll < 4)

childpov_free6 <-subset( 
		y6 , 
	 a_age < 19& a_age >= 5 & a_hscol < 2 & povll < 6)

childpov_reduced6 <-subset( 
		y6 , 
	 a_age < 19& a_age >= 5 & a_hscol < 2 & povll < 7 & povll > 5)


childpov5 <-subset( 
		y5 , 
  a_age < 19& a_age >= 5 
	& povll < 4)

childpov_free5 <-subset( 
		y5 , 
	 a_age < 19& a_age >= 5 & a_hscol < 2 & povll < 6)

childpov_reduced5 <-subset( 
		y5 , 
	 a_age < 19& a_age >= 5 & a_hscol < 2 & povll < 7 & povll > 5)


###
svytotal(
	~one ,
childpov16)

svytotal(
	~one ,
	childpov_free16)

svytotal(
	~one ,
	childpov_reduced16)


svytotal(
	~one ,
childpov15)

svytotal(
	~one ,
	childpov_free15)

svytotal(
	~one ,
	childpov_reduced15)


svytotal(
	~one ,
childpov14)

svytotal(
	~one ,
	childpov_free14)

svytotal(
	~one ,
	childpov_reduced14)


svytotal(
	~one ,
childpov13)

svytotal(
	~one ,
	childpov_free13)


svytotal(
	~one ,
	childpov_reduced13)


svytotal(
	~one ,
childpov12)

svytotal(
	~one ,
	childpov_free12)

svytotal(
	~one ,
	childpov_reduced12)


svytotal(
	~one ,
childpov11)

svytotal(
	~one ,
	childpov_free11)

svytotal(
	~one ,
	childpov_reduced11)

svytotal(
	~one ,
childpov10)

svytotal(
	~one ,
	childpov_free10)

svytotal(
	~one ,
	childpov_reduced10)

svytotal(
	~one ,
childpov9)

svytotal(
	~one ,
	childpov_free9)

svytotal(
	~one ,
	childpov_reduced9)

svytotal(
	~one ,
childpov8)

svytotal(
	~one ,
	childpov_free8)

svytotal(
	~one ,
	childpov_reduced8)

svytotal(
	~one ,
childpov7)

svytotal(
	~one ,
	childpov_free7)

svytotal(
	~one ,
	childpov_reduced7)

svytotal(
	~one ,
childpov6)

svytotal(
	~one ,
	childpov_free6)

svytotal(
	~one ,
	childpov_reduced6)


svytotal(
	~one ,
childpov5)

svytotal(
	~one ,
	childpov_free5)

svytotal(
	~one ,
	childpov_reduced5)



#################


#https://cps.ipums.org/cps/poverty_notes.shtml



#Children receiving free lunch HFLUNNO 73
#Children receiving free or reduced price lunches HFLUNCH 72
#Hot lunch eaten by children at school HHOTLUN 70 
#Hot lunch, number of children who ate at school HHOTNO 71

hflunch


childpov_lunch0 <-subset( 
		y16 , 
  a_age < 19 & a_age >= 5 
	& povll < 5 
		)

childpov_lunch <-subset( 
		y16 , 
  a_age < 19 & a_age >= 5 
	& povll < 5 &
	hflunno > 0
		)

HFLUNNO

svytotal(
	~one ,
childpov_lunch)

svytotal(
	~one ,
childpov_lunch0)

####

childpov_wL16 <-subset( 
		y16 , 
  a_age < 19& a_age >= 5 & a_hscol < 2 & hflunno > 0 
	& povll < 4
		)

childpov_wL_free16 <-subset( 
		y16 , 
	 a_age < 19 & a_age >= 5 & a_hscol < 2 & hflunno > 0 & povll < 5)

childpov_wOut_free16 <-subset( 
		y16 , 
	 a_age < 19 & a_age >= 5 & a_hscol < 2 & hflunno < 1 & povll < 5)

childpov_wL_reduced16 <-subset( 
		y16 , 
	 a_age < 19& a_age >= 5 & a_hscol < 2 & hflunno > 0 & povll < 7 & povll > 5)


childpov_wL15 <-subset( 
		y15 , 
  a_age < 19& a_age >= 5 & a_hscol < 2 & hflunno > 0 
	& povll < 4
		)

childpov_wL_free15 <-subset( 
		y15 , 
	 a_age < 19& a_age >= 5 & a_hscol < 2 & hflunno > 0 & povll < 5)

childpov_wOut_free15 <-subset( 
		y15 , 
	 a_age < 19 & a_age >= 5 & a_hscol < 2 & hflunno < 1 & povll < 5)


childpov_wL_reduced15 <-subset( 
		y15 , 
	 a_age < 19& a_age >= 5 & a_hscol < 2 & hflunno > 0 & povll < 7 & povll > 5)


childpov_wL14 <-subset( 
		y14 , 
  a_age < 19& a_age >= 5 & a_hscol < 2 & hflunno > 0 
	& povll < 4)

childpov_wL_free14 <-subset( 
		y14 , 
	 a_age < 19& a_age >= 5 & a_hscol < 2 & hflunno > 0 & povll < 5)

childpov_wOut_free14 <-subset( 
		y14 , 
	 a_age < 19 & a_age >= 5 & a_hscol < 2 & hflunno < 1 & povll < 5)


childpov_wL_reduced14 <-subset( 
		y14 , 
	 a_age < 19& a_age >= 5 & a_hscol < 2 & hflunno > 0 & povll < 7 & povll > 5)

childpov_wL13 <-subset( 
		y13 , 
  a_age < 19& a_age >= 5 & a_hscol < 2 & hflunno > 0 
	& povll < 4)

childpov_wL_free13 <-subset( 
		y13 , 
	 a_age < 19& a_age >= 5 & a_hscol < 2 & hflunno > 0 & povll < 5)

childpov_wOut_free13 <-subset( 
		y13 , 
	 a_age < 19 & a_age >= 5 & a_hscol < 2 & hflunno < 1 & povll < 5)


childpov_wL_reduced13 <-subset( 
		y13 , 
	 a_age < 19& a_age >= 5 & a_hscol < 2 & hflunno > 0 & povll < 7 & povll > 5)

childpov_wL12 <-subset( 
		y12 , 
  a_age < 19& a_age >= 5 & a_hscol < 2 & hflunno > 0 
	& povll < 4)

childpov_wL_free12 <-subset( 
		y12 , 
	 a_age < 19& a_age >= 5 & a_hscol < 2 & hflunno > 0 & povll < 5)

childpov_wOut_free12 <-subset( 
		y12 , 
	 a_age < 19 & a_age >= 5 & a_hscol < 2 & hflunno < 1 & povll < 5)


childpov_wL_reduced12 <-subset( 
		y12 , 
	 a_age < 19& a_age >= 5 & a_hscol < 2 & hflunno > 0 & povll < 7 & povll > 5)

childpov_wL11 <-subset( 
		y11 , 
  a_age < 19& a_age >= 5 & a_hscol < 2 & hflunno > 0 
	& povll < 4)

childpov_wL_free11 <-subset( 
		y11 , 
	 a_age < 19& a_age >= 5 & a_hscol < 2 & hflunno > 0 & povll < 5)

childpov_wOut_free11 <-subset( 
		y11 , 
	 a_age < 19 & a_age >= 5 & a_hscol < 2 & hflunno < 1 & povll < 5)


childpov_wL_reduced11 <-subset( 
		y11 , 
	 a_age < 19& a_age >= 5 & a_hscol < 2 & hflunno > 0 & povll < 7 & povll > 5)

childpov_wL10 <-subset( 
		y10 , 
  a_age < 19& a_age >= 5 & a_hscol < 2 & hflunno > 0 
	& povll < 4)

childpov_wL_free10 <-subset( 
		y10 , 
	 a_age < 19& a_age >= 5 & a_hscol < 2 & hflunno > 0 & povll < 5)

childpov_wOut_free10 <-subset( 
		y10 , 
	 a_age < 19 & a_age >= 5 & a_hscol < 2 & hflunno < 1 & povll < 5)


childpov_wL_reduced10 <-subset( 
		y10 , 
	 a_age < 19& a_age >= 5 & a_hscol < 2 & hflunno > 0 & povll < 7 & povll > 5)

childpov_wL9 <-subset( 
		y9 , 
  a_age < 19& a_age >= 5 & a_hscol < 2 & hflunno > 0 
	& povll < 4)

childpov_wL_free9 <-subset( 
		y9 , 
	 a_age < 19& a_age >= 5 & a_hscol < 2 & hflunno > 0 & povll < 5)

childpov_wOut_free9 <-subset( 
		y9 , 
	 a_age < 19 & a_age >= 5 & a_hscol < 2 & hflunno < 1 & povll < 5)


childpov_wL_reduced9 <-subset( 
		y9 , 
	 a_age < 19& a_age >= 5 & a_hscol < 2 & hflunno > 0 & povll < 7 & povll > 5)

childpov_wL8 <-subset( 
		y8 , 
  a_age < 19& a_age >= 5 & a_hscol < 2 & hflunno > 0 
	& povll < 4)

childpov_wL_free8 <-subset( 
		y8 , 
	 a_age < 19& a_age >= 5 & a_hscol < 2 & hflunno > 0 & povll < 5)

childpov_wOut_free8 <-subset( 
		y8 , 
	 a_age < 19 & a_age >= 5 & a_hscol < 2 & hflunno < 1 & povll < 5)


childpov_wL_reduced8 <-subset( 
		y8 , 
	 a_age < 19& a_age >= 5 & a_hscol < 2 & hflunno > 0 & povll < 7 & povll > 5)

childpov_wL7 <-subset( 
		y7 , 
  a_age < 19& a_age >= 5 & a_hscol < 2 & hflunno > 0 
	& povll < 4)

childpov_wL_free7 <-subset( 
		y7 , 
	 a_age < 19& a_age >= 5 & a_hscol < 2 & hflunno > 0 & povll < 5)

childpov_wOut_free7 <-subset( 
		y7 , 
	 a_age < 19 & a_age >= 5 & a_hscol < 2 & hflunno < 1 & povll < 5)


childpov_wL_reduced7 <-subset( 
		y7 , 
	 a_age < 19& a_age >= 5 & a_hscol < 2 & hflunno > 0 & povll < 7 & povll > 5)

childpov_wL6 <-subset( 
		y6 , 
  a_age < 19& a_age >= 5 & a_hscol < 2 & hflunno > 0 
	& povll < 4)

childpov_wL_free6 <-subset( 
		y6 , 
	 a_age < 19& a_age >= 5 & a_hscol < 2 & hflunno > 0 & povll < 5)

childpov_wOut_free6 <-subset( 
		y6 , 
	 a_age < 19 & a_age >= 5 & a_hscol < 2 & hflunno < 1 & povll < 5)


childpov_wL_reduced6 <-subset( 
		y6 , 
	 a_age < 19& a_age >= 5 & a_hscol < 2 & hflunno > 0 & povll < 7 & povll > 5)


childpov_wL5 <-subset( 
		y5 , 
  a_age < 19& a_age >= 5 & a_hscol < 2 & hflunno > 0 
	& povll < 4)

childpov_wL_free5 <-subset( 
		y5 , 
	 a_age < 19& a_age >= 5 & a_hscol < 2 & hflunno > 0 & povll < 5)

childpov_wOut_free5 <-subset( 
		y5 , 
	 a_age < 19 & a_age >= 5 & a_hscol < 2 & hflunno < 1 & povll < 5)


childpov_wL_reduced5 <-subset( 
		y5 , 
	 a_age < 19& a_age >= 5 & a_hscol < 2 & hflunno > 0 & povll < 7 & povll > 5)

#### Cold ####

prnlfsch

childpov_wOut_cold5 <-subset( 
		y5 , 
	 a_age < 19& a_age >= 5 & a_hscol < 2 & hflunno < 1 & povll < 5 & hhotlun == 2)

childpov_wOut_cold6 <-subset( 
		y6 , 
	 a_age < 19& a_age >= 5 & a_hscol < 2 & hflunno < 1 & povll < 5 & hhotlun == 2)

childpov_wOut_cold7 <-subset( 
		y7 , 
	 a_age < 19& a_age >= 5 & a_hscol < 2 & hflunno < 1 & povll < 5 & hhotlun == 2)

childpov_wOut_cold8 <-subset( 
		y8 , 
	 a_age < 19& a_age >= 5 & a_hscol < 2 & hflunno < 1 & povll < 5 & hhotlun == 2)

childpov_wOut_cold9 <-subset( 
		y9 , 
	 a_age < 19& a_age >= 5 & a_hscol < 2 & hflunno < 1 & povll < 5 & hhotlun == 2)

childpov_wOut_cold10 <-subset( 
		y10 , 
	 a_age < 19& a_age >= 5 & a_hscol < 2 & hflunno < 1 & povll < 5 & hhotlun == 2)

childpov_wOut_cold11 <-subset( 
		y11 , 
	 a_age < 19& a_age >= 5 & a_hscol < 2 & hflunno < 1 & povll < 5 & hhotlun == 2)

childpov_wOut_cold12 <-subset( 
		y12 , 
	 a_age < 19& a_age >= 5 & a_hscol < 2 & hflunno < 1 & povll < 5 & hhotlun == 2)

childpov_wOut_cold13 <-subset( 
		y13 , 
	 a_age < 19& a_age >= 5 & a_hscol < 2 & hflunno < 1 & povll < 5 & hhotlun == 2)

childpov_wOut_cold14 <-subset( 
		y14 , 
	 a_age < 19& a_age >= 5 & a_hscol < 2 & hflunno < 1 & povll < 5 & hhotlun == 2)

childpov_wOut_cold15 <-subset( 
		y15 , 
	 a_age < 19& a_age >= 5 & a_hscol < 2 & hflunno < 1 & povll < 5 & hhotlun == 2)

childpov_wOut_cold16 <-subset( 
		y16 , 
	 a_age < 19& a_age >= 5 & a_hscol < 2 & hflunno < 1 & povll < 5 & hhotlun == 2)


#### Free or Reduced Price ####

childpov_wOutFoR5 <-subset( 
		y5 , 
	 a_age < 19& a_age >= 5 & a_hscol < 2 & hflunch != 1 & povll < 7 & hhotlun == 2)

childpov_wOutFoR6 <-subset( 
		y6 , 
	 a_age < 19& a_age >= 5 & a_hscol < 2 & hflunch != 1 & povll < 7 & hhotlun == 2)

childpov_wOutFoR7 <-subset( 
		y7 , 
	 a_age < 19& a_age >= 5 & a_hscol < 2 & hflunch != 1 & povll < 7 & hhotlun == 2)

childpov_wOutFoR8 <-subset( 
		y8 , 
	 a_age < 19& a_age >= 5 & a_hscol < 2 & hflunch != 1 & povll < 7 & hhotlun == 2)

childpov_wOutFoR9 <-subset( 
		y9 , 
	 a_age < 19& a_age >= 5 & a_hscol < 2 & hflunch != 1 & povll < 7 & hhotlun == 2)

childpov_wOutFoR10 <-subset( 
		y10 , 
	 a_age < 19& a_age >= 5 & a_hscol < 2 & hflunch != 1 & povll < 7 & hhotlun == 2)

childpov_wOutFoR11 <-subset( 
		y11 , 
	 a_age < 19& a_age >= 5 & a_hscol < 2 & hflunch != 1 & povll < 7 & hhotlun == 2)

childpov_wOutFoR12 <-subset( 
		y12 , 
	 a_age < 19& a_age >= 5 & a_hscol < 2 & hflunch != 1 & povll < 7 & hhotlun == 2)

childpov_wOutFoR13 <-subset( 
		y13 , 
	 a_age < 19& a_age >= 5 & a_hscol < 2 & hflunch != 1 & povll < 7 & hhotlun == 2)

childpov_wOutFoR14 <-subset( 
		y14 , 
	 a_age < 19& a_age >= 5 & a_hscol < 2 & hflunch != 1 & povll < 7 & hhotlun == 2)

childpov_wOutFoR15 <-subset( 
		y15 , 
	 a_age < 19& a_age >= 5 & a_hscol < 2 & hflunch != 1 & povll < 7 & hhotlun == 2)

childpov_wOutFoR16 <-subset( 
		y16 , 
	 a_age < 19& a_age >= 5 & a_hscol < 2 & hflunch != 1 & povll < 7 & hhotlun == 2)



###
svytotal(
	~one ,
childpov_wL16)

svytotal(
	~one ,
childpov_wL15)

svytotal(
	~one ,
childpov_wL14)

svytotal(
	~one ,
childpov_wL13)

svytotal(
	~one ,
childpov_wL12)

svytotal(
	~one ,
childpov_wL11)

svytotal(
	~one ,
childpov_wL10)

svytotal(
	~one ,
childpov_wL9)

svytotal(
	~one ,
childpov_wL8)

svytotal(
	~one ,
childpov_wL7)

svytotal(
	~one ,
childpov_wL6)

svytotal(
	~one ,
childpov_wL5)


#####
svytotal(
	~one ,
	childpov_wL_free16)


svytotal(
	~one ,
	childpov_wL_free15)

svytotal(
	~one ,
	childpov_wL_free14)


svytotal(
	~one ,
	childpov_wL_free13)

svytotal(
	~one ,
	childpov_wL_free12)

svytotal(
	~one ,
	childpov_wL_free11)

svytotal(
	~one ,
	childpov_wL_free10)

svytotal(
	~one ,
	childpov_wL_free9)

svytotal(
	~one ,
	childpov_wL_free8)

svytotal(
	~one ,
	childpov_wL_free7)

svytotal(
	~one ,
	childpov_wL_free6)

svytotal(
	~one ,
	childpov_wL_free5)



###########

svytotal(
	~one ,
	childpov_wL_reduced16)

svytotal(
	~one ,
	childpov_wL_reduced15)

svytotal(
	~one ,
	childpov_wL_reduced14)

svytotal(
	~one ,
	childpov_wL_reduced13)

svytotal(
	~one ,
	childpov_wL_reduced12)

svytotal(
	~one ,
	childpov_wL_reduced11)

svytotal(
	~one ,
	childpov_wL_reduced10)

svytotal(
	~one ,
	childpov_wL_reduced9)

svytotal(
	~one ,
	childpov_wL_reduced8)

svytotal(
	~one ,
	childpov_wL_reduced7)

svytotal(
	~one ,
	childpov_wL_reduced6)

svytotal(
	~one ,
	childpov_wL_reduced5)

####### Abstain or Unable #####

svytotal(
	~one ,
	childpov_wOut_free16)

svytotal(
	~one ,
	childpov_wOut_free15)

svytotal(
	~one ,
	childpov_wOut_free14)

svytotal(
	~one ,
	childpov_wOut_free13)

svytotal(
	~one ,
	childpov_wOut_free12)

svytotal(
	~one ,
	childpov_wOut_free11)

svytotal(
	~one ,
	childpov_wOut_free10)

svytotal(
	~one ,
	childpov_wOut_free9)

svytotal(
	~one ,
	childpov_wOut_free8)

svytotal(
	~one ,
	childpov_wOut_free7)

svytotal(
	~one ,
	childpov_wOut_free6)

svytotal(
	~one ,
	childpov_wOut_free5)

######## Cold ######

svytotal(
	~one ,
	childpov_wOut_cold16)

svytotal(
	~one ,
	childpov_wOut_cold15)

svytotal(
	~one ,
	childpov_wOut_cold14)


svytotal(
	~one ,
	childpov_wOut_cold13)

svytotal(
	~one ,
	childpov_wOut_cold12)

svytotal(
	~one ,
	childpov_wOut_cold11)

svytotal(
	~one ,
	childpov_wOut_cold10)

svytotal(
	~one ,
	childpov_wOut_cold9)

svytotal(
	~one ,
	childpov_wOut_cold8)

svytotal(
	~one ,
	childpov_wOut_cold7)

svytotal(
	~one ,
	childpov_wOut_cold6)

svytotal(
	~one ,
	childpov_wOut_cold5)

######## Free or Reduced ######

svytotal(
	~one ,
	childpov_wOutFoR16)

svytotal(
	~one ,
	childpov_wOutFoR15)

svytotal(
	~one ,
	childpov_wOutFoR14)


svytotal(
	~one ,
	childpov_wOutFoR13)

svytotal(
	~one ,
	childpov_wOutFoR12)

svytotal(
	~one ,
	childpov_wOutFoR11)

svytotal(
	~one ,
	childpov_wOutFoR10)

svytotal(
	~one ,
	childpov_wOutFoR9)

svytotal(
	~one ,
	childpov_wOutFoR8)

svytotal(
	~one ,
	childpov_wOutFoR7)

svytotal(
	~one ,
	childpov_wOutFoR6)

svytotal(
	~one ,
	childpov_wOutFoR5)


