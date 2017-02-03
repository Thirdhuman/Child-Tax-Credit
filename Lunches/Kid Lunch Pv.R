#https://cps.ipums.org/cps/poverty_notes.shtml

setwd("~/Desktop/Poverty:Welfare/Data/R - Survey data/Current Population Survey (CPS)")

library(survey)				# load survey package (analyzes complex design surveys)
library(MonetDBLite)
library(DBI)			# load the DBI package (implements the R-database coding)

dbfolder <- paste0( getwd() , "/MonetDB" )

options( survey.replicates.mse = TRUE )

#### CPS Surveys ####
cps16 <-
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

cps16 <- update(cps16,		hfftotinc = ifelse(ftype == 1, ftotval, 0))
cps16 <- update(cps16,		ototinc = ifelse(ftype == 3, ftotval, 0))

cps16 <- update(cps16,		hftotinc = ifelse(duplicated(h_idnum1) == TRUE & duplicated(h_idnum2) == TRUE & ftype == 1,
																																			 ftotval + (ototinc),
																																			ifelse(duplicated(h_idnum1) == TRUE & duplicated(h_idnum2) == TRUE & ftype == 3,
																																			 ftotval + (hfftotinc),
																																								ftotval)))

cps16 <- update(cps16,		hftotinc = ifelse((ftype != 3 | ftype != 1), ftotval, htotval))

cps16 <- update(cps16,		hftotinc1 = ifelse(ftype == 3 | 1, htotval, 
																																							ifelse(ftype != 3 | ftype !=  1, ftotval, ftotval)))

cps16 <- update(cps16,		hftotinc3 = ifelse( ftype == 3 | ftype == 1 , (hfftotinc) + (ototinc), 
																																							ifelse(ftype != 3 | ftype != 1, ftotval, 0)))

cps16 <- update(cps16, hftotinc2 =	ifelse(htotval == ftotval, ftotval,
	ifelse((ftype != 3 & ftype != 1), htotval - ftotval, ftotval)))


svyby( ~ftotval , by = ~ftype , design = cps16, FUN = svyquantile ,
	quantiles = c(0, .25, 0.5, .75, .90, 1))

svyby( ~htotval , by = ~ftype , design = cps16, FUN = svyquantile ,
	quantiles = c(0, .25, 0.5, .75, .90, 1))

svyby( ~hftotinc , by = ~ftype , design = cps16, FUN = svyquantile ,
	quantiles = c(0, .25, 0.5, .75, .90, 1))
svyby( ~hftotinc1 , by = ~ftype , design = cps16, FUN = svyquantile ,
	quantiles = c(0, .25, 0.5, .75, .90, 1))
svyby( ~hftotinc2 , by = ~ftype , design = cps16, FUN = svyquantile ,
	quantiles = c(0, .25, 0.5, .75, .90, 1))
svyby( ~hftotinc3 , by = ~ftype , design = cps16, FUN = svyquantile ,
	quantiles = c(0, .25, 0.5, .75, .90, 1))

#svytotal(~hfftotinc, design = cps16)
svytotal(~hftotinc3, design = cps16)
svytotal(~hftotinc2, design = cps16)
svytotal(~hftotinc1, design = cps16)
svytotal(~hftotinc, design = cps16)
svytotal(~ftotval, design = cps16)
svytotal(~htotval, design = cps16)


cps15 <-
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

cps15$mse <- TRUE

cps15 <- update(cps15,		hfftotinc = ifelse(ftype == 3, ftotval, 0))
cps15 <- update(cps15,		ototinc = ifelse(ftype == 1, ftotval, 0))
cps15 <- update(cps15,		hftotinc = ifelse((ftype != 3 & ftype != 1), htotval - ftotval, htotval))
cps15 <- update(cps15,		hftotinc2 = ifelse( identical(h_seq) == TRUE & ftype == 3 | 1, (hfftotinc) + (ototinc), 
																																							ifelse(ftype != 3 | 1, ftotval, 0)))
#cps15 <- update(cps15,		hftotinc3 = ifelse( ftype == 3 | 1 , (hfftotinc) + (ototinc), 
						#																																					ifelse(ftype != 3 | 1, ftotval, 0)))


svytotal(~hfftotinc, design = cps15)
svytotal(~ototinc, design = cps15)
svytotal(~hftotinc1, design = cps15)


cps14 <-
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

cps14$mse <- TRUE

cps14 <- update(cps14,		hfftotinc = ifelse(ftype == 3, ftotval, 0))
cps14 <- update(cps14,		ototinc = ifelse(ftype == 1, ftotval, 0))
cps14 <- update(cps14,		hftotinc = ifelse((ftype != 3 & ftype != 1), htotval - ftotval, htotval))


cps13 <-
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

cps13$mse <- TRUE

cps13 <- update(cps13,		hfftotinc = ifelse(ftype == 3, ftotval, 0))
cps13 <- update(cps13,		ototinc = ifelse(ftype == 1, ftotval, 0))
cps13 <- update(cps13,		hftotinc = ifelse((ftype != 3 & ftype != 1), htotval - ftotval, htotval))


cps12 <-
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

cps12$mse <- TRUE

cps12 <- update(cps12,		hfftotinc = ifelse(ftype == 3, ftotval, 0))
cps12 <- update(cps12,		ototinc = ifelse(ftype == 1, ftotval, 0))
cps12 <- update(cps12,		hftotinc = ifelse((ftype != 3 & ftype != 1), htotval - ftotval, htotval))


cps11 <-
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

cps11$mse <- TRUE

cps11 <- update(cps11,		hfftotinc = ifelse(ftype == 3, ftotval, 0))
cps11 <- update(cps11,		ototinc = ifelse(ftype == 1, ftotval, 0))
cps11 <- update(cps11,		hftotinc = ifelse((ftype != 3 & ftype != 1), htotval - ftotval, htotval))


cps10 <-
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

cps10$mse <- TRUE

cps10 <- update(cps10,		hfftotinc = ifelse(ftype == 3, ftotval, 0))
cps10 <- update(cps10,		ototinc = ifelse(ftype == 1, ftotval, 0))
cps10 <- update(cps10,		hftotinc = ifelse((ftype != 3 & ftype != 1), htotval - ftotval, htotval))


cps9 <-
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

cps9$mse <- TRUE

cps9 <- update(cps9,		hfftotinc = ifelse(ftype == 3, ftotval, 0))
cps9 <- update(cps9,		ototinc = ifelse(ftype == 1, ftotval, 0))
cps9 <- update(cps9,		hftotinc = ifelse((ftype != 3 & ftype != 1), htotval - ftotval, htotval))


cps8 <-
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

cps8$mse <- TRUE

cps8 <- update(cps8,		hfftotinc = ifelse(ftype == 3, ftotval, 0))
cps8 <- update(cps8,		ototinc = ifelse(ftype == 1, ftotval, 0))
cps8 <- update(cps8,		hftotinc = ifelse((ftype != 3 & ftype != 1), htotval - ftotval, htotval))

cps7 <-
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

cps7$mse <- TRUE

cps7 <- update(cps7,		hfftotinc = ifelse(ftype == 3, ftotval, 0))
cps7 <- update(cps7,		ototinc = ifelse(ftype == 1, ftotval, 0))
cps7 <- update(cps7,		hftotinc = ifelse((ftype != 3 & ftype != 1), htotval - ftotval, htotval))

cps6 <-
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

cps6$mse <- TRUE

cps6 <- update(cps6,		hfftotinc = ifelse(ftype == 3, ftotval, 0))
cps6 <- update(cps6,		ototinc = ifelse(ftype == 1, ftotval, 0))
cps6 <- update(cps6,		hftotinc = ifelse((ftype != 3 & ftype != 1), htotval - ftotval, htotval))

cps5 <-
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

cps5$mse <- TRUE

cps5 <- update(cps5,		hfftotinc = ifelse(ftype == 3, ftotval, 0))
cps5 <- update(cps5,		ototinc = ifelse(ftype == 1, ftotval, 0))
cps5 <- update(cps5,		hftotinc = ifelse((ftype != 3 & ftype != 1), htotval - ftotval, htotval))

#####  Subsets #####
#### subsets - Kids ####
kids15 <-subset( 
		cps16 , 
  (a_age < 19 & a_hscol == 1) | (a_age >= 5 & a_age <= 17)
		)

kids14 <-subset( 
		cps15 , 
  (a_age < 19 & a_hscol == 1) | (a_age >= 5 & a_age <= 17) 
		)

kids13 <-subset( 
		cps14 , 
  (a_age < 19 & a_hscol == 1) | (a_age >= 5 & a_age <= 17) 
	)

kids12 <-subset( 
		cps13 , 
  (a_age < 19 & a_hscol == 1) | (a_age >= 5 & a_age <= 17) 
	)

kids11 <-subset( 
		cps12 , 
  (a_age < 19 & a_hscol == 1) | (a_age >= 5 & a_age <= 17) 
	)

kids10 <-subset( 
		cps11 , 
  (a_age < 19 & a_hscol == 1) | (a_age >= 5 & a_age <= 17) 
	)

kids9 <-subset( 
		cps10 , 
  (a_age < 19 & a_hscol == 1) | (a_age >= 5 & a_age <= 17) 
	)

kids8 <-subset( 
		cps9 , 
  (a_age < 19 & a_hscol == 1) | (a_age >= 5 & a_age <= 17)
	)

kids7 <-subset( 
		cps8 , 
  (a_age < 19 & a_hscol == 1) | (a_age >= 5 & a_age <= 17) 
	)

kids6 <-subset( 
		cps7 , 
  (a_age < 19 & a_hscol == 1) | (a_age >= 5 & a_age <= 17) 
	)

kids5 <-subset( 
		cps6 , 
  (a_age < 19 & a_hscol == 1) | (a_age >= 5 & a_age <= 17) 
	)

kids4 <-subset( 
		cps5 , 
  (a_age < 19 & a_hscol == 1) | (a_age >= 5 & a_age <= 17) 
	)
#### Qualified subsets- Total/Free/Reduced/Fullp Lunch - Subsidized ####
kids_qual15<- subset(kids15, hflunch == 1)
kids_qual14<- subset( kids14, hflunch == 1)
kids_qual13<- subset( kids13, hflunch == 1)
kids_qual12<- subset( kids12, hflunch == 1)
kids_qual11<- subset( kids11, hflunch == 1)
kids_qual10<- subset( kids10, hflunch == 1)
kids_qual9<- subset( kids9, hflunch == 1)
kids_qual8<- subset( kids8, hflunch == 1)
kids_qual7<- subset( kids7, hflunch == 1)
kids_qual6<- subset( kids6, hflunch == 1)
kids_qual5<- subset( kids5, hflunch == 1)
kids_qual_free15<- subset(kids_free15, hflunch == 1)
kids_qual_free14<- subset(kids_free14, hflunch == 1)
kids_qual_free13<- subset(kids_free13, hflunch == 1)
kids_qual_free12<- subset(kids_free12, hflunch == 1)
kids_qual_free11<- subset(kids_free11, hflunch == 1)
kids_qual_free10<- subset(kids_free10, hflunch == 1)
kids_qual_free9<- subset(kids_free9, hflunch == 1)
kids_qual_free8<- subset(kids_free8, hflunch == 1)
kids_qual_free7<- subset(kids_free7, hflunch == 1)
kids_qual_free6<- subset(kids_free6, hflunch == 1)
kids_qual_free5<- subset(kids_free5, hflunch == 1)
kids_qual_reduced15<- subset(kids_reduced15, hflunch == 1)
kids_qual_reduced14<- subset(kids_reduced14, hflunch == 1)
kids_qual_reduced13<- subset(kids_reduced13, hflunch == 1)
kids_qual_reduced12<- subset(kids_reduced12, hflunch == 1)
kids_qual_reduced11<- subset(kids_reduced11, hflunch == 1)
kids_qual_reduced10<- subset(kids_reduced10, hflunch == 1)
kids_qual_reduced9<- subset(kids_reduced9, hflunch == 1)
kids_qual_reduced8<- subset(kids_reduced8, hflunch == 1)
kids_qual_reduced7<- subset(kids_reduced7, hflunch == 1)
kids_qual_reduced6<- subset(kids_reduced6, hflunch == 1)
kids_qual_reduced5<- subset(kids_reduced5, hflunch == 1)
kids_qual_full15<- subset(kids_full15, hflunch == 1)
kids_qual_full14<- subset(kids_full14, hflunch == 1)
kids_qual_full13<- subset(kids_full13, hflunch == 1)
kids_qual_full12<- subset(kids_full12, hflunch == 1)
kids_qual_full11<- subset(kids_full11, hflunch == 1)
kids_qual_full10<- subset(kids_full10, hflunch == 1)
kids_qual_full9<- subset(kids_full9, hflunch == 1)
kids_qual_full8<- subset(kids_full8, hflunch == 1)
kids_qual_full7<- subset(kids_full7, hflunch == 1)
kids_qual_full6<- subset(kids_full6, hflunch == 1)
kids_qual_full5<- subset(kids_full5, hflunch == 1)

kids_unqual15<- subset(kids15, hflunch == 2)
kids_unqual14<- subset( kids14, hflunch == 2)
kids_unqual13<- subset( kids13, hflunch == 2)
kids_unqual12<- subset( kids12, hflunch == 2)
kids_unqual11<- subset( kids11, hflunch == 2)
kids_unqual10<- subset( kids10, hflunch == 2)
kids_unqual9<- subset( kids9, hflunch == 2)
kids_unqual8<- subset( kids8, hflunch == 2)
kids_unqual7<- subset( kids7, hflunch == 2)
kids_unqual6<- subset( kids6, hflunch == 2)
kids_unqual5<- subset( kids5, hflunch == 2)
kids_unqual_free15<- subset(kids_free15, hflunch == 2)
kids_unqual_free14<- subset(kids_free14, hflunch == 2)
kids_unqual_free13<- subset(kids_free13, hflunch == 2)
kids_unqual_free12<- subset(kids_free12, hflunch == 2)
kids_unqual_free11<- subset(kids_free11, hflunch == 2)
kids_unqual_free10<- subset(kids_free10, hflunch == 2)
kids_unqual_free9<- subset(kids_free9, hflunch == 2)
kids_unqual_free8<- subset(kids_free8, hflunch == 2)
kids_unqual_free7<- subset(kids_free7, hflunch == 2)
kids_unqual_free6<- subset(kids_free6, hflunch == 2)
kids_unqual_free5<- subset(kids_free5, hflunch == 2)
kids_unqual_reduced15<- subset(kids_reduced15, hflunch == 2)
kids_unqual_reduced14<- subset(kids_reduced14, hflunch == 2)
kids_unqual_reduced13<- subset(kids_reduced13, hflunch == 2)
kids_unqual_reduced12<- subset(kids_reduced12, hflunch == 2)
kids_unqual_reduced11<- subset(kids_reduced11, hflunch == 2)
kids_unqual_reduced10<- subset(kids_reduced10, hflunch == 2)
kids_unqual_reduced9<- subset(kids_reduced9, hflunch == 2)
kids_unqual_reduced8<- subset(kids_reduced8, hflunch == 2)
kids_unqual_reduced7<- subset(kids_reduced7, hflunch == 2)
kids_unqual_reduced6<- subset(kids_reduced6, hflunch == 2)
kids_unqual_reduced5<- subset(kids_reduced5, hflunch == 2)
kids_unqual_full15<- subset(kids_full15, hflunch == 2)
kids_unqual_full14<- subset(kids_full14, hflunch == 2)
kids_unqual_full13<- subset(kids_full13, hflunch == 2)
kids_unqual_full12<- subset(kids_full12, hflunch == 2)
kids_unqual_full11<- subset(kids_full11, hflunch == 2)
kids_unqual_full10<- subset(kids_full10, hflunch == 2)
kids_unqual_full9<- subset(kids_full9, hflunch == 2)
kids_unqual_full8<- subset(kids_full8, hflunch == 2)
kids_unqual_full7<- subset(kids_full7, hflunch == 2)
kids_unqual_full6<- subset(kids_full6, hflunch == 2)
kids_unqual_full5<- subset(kids_full5, hflunch == 2)

kids_niu15<- subset(kids15, hflunch == 0)
kids_niu14<- subset( kids14, hflunch == 0)
kids_niu13<- subset( kids13, hflunch == 0)
kids_niu12<- subset( kids12, hflunch == 0)
kids_niu11<- subset( kids11, hflunch == 0)
kids_niu10<- subset( kids10, hflunch == 0)
kids_niu9<- subset( kids9, hflunch == 0)
kids_niu8<- subset( kids8, hflunch == 0)
kids_niu7<- subset( kids7, hflunch == 0)
kids_niu6<- subset( kids6, hflunch == 0)
kids_niu5<- subset( kids5, hflunch == 0)
kids_niu_free15<- subset(kids_free15, hflunch == 0)
kids_niu_free14<- subset(kids_free14, hflunch == 0)
kids_niu_free13<- subset(kids_free13, hflunch == 0)
kids_niu_free12<- subset(kids_free12, hflunch == 0)
kids_niu_free11<- subset(kids_free11, hflunch == 0)
kids_niu_free10<- subset(kids_free10, hflunch == 0)
kids_niu_free9<- subset(kids_free9, hflunch == 0)
kids_niu_free8<- subset(kids_free8, hflunch == 0)
kids_niu_free7<- subset(kids_free7, hflunch == 0)
kids_niu_free6<- subset(kids_free6, hflunch == 0)
kids_niu_free5<- subset(kids_free5, hflunch == 0)
kids_niu_reduced15<- subset(kids_reduced15, hflunch == 0)
kids_niu_reduced14<- subset(kids_reduced14, hflunch == 0)
kids_niu_reduced13<- subset(kids_reduced13, hflunch == 0)
kids_niu_reduced12<- subset(kids_reduced12, hflunch == 0)
kids_niu_reduced11<- subset(kids_reduced11, hflunch == 0)
kids_niu_reduced10<- subset(kids_reduced10, hflunch == 0)
kids_niu_reduced9<- subset(kids_reduced9, hflunch == 0)
kids_niu_reduced8<- subset(kids_reduced8, hflunch == 0)
kids_niu_reduced7<- subset(kids_reduced7, hflunch == 0)
kids_niu_reduced6<- subset(kids_reduced6, hflunch == 0)
kids_niu_reduced5<- subset(kids_reduced5, hflunch == 0)
kids_niu_full15<- subset(kids_full15, hflunch == 0)
kids_niu_full14<- subset(kids_full14, hflunch == 0)
kids_niu_full13<- subset(kids_full13, hflunch == 0)
kids_niu_full12<- subset(kids_full12, hflunch == 0)
kids_niu_full11<- subset(kids_full11, hflunch == 0)
kids_niu_full10<- subset(kids_full10, hflunch == 0)
kids_niu_full9<- subset(kids_full9, hflunch == 0)
kids_niu_full8<- subset(kids_full8, hflunch == 0)
kids_niu_full7<- subset(kids_full7, hflunch == 0)
kids_niu_full6<- subset(kids_full6, hflunch == 0)
kids_niu_full5<- subset(kids_full5, hflunch == 0)


######### CPS Lunch Variables #########

#Children receiving free lunch HFLUNNO 73
#Children receiving free or reduced price lunches HFLUNCH 72
#Hot lunch eaten by children at school HHOTLUN 70 
#Hot lunch, number of children who ate at school HHOTNO 71

#### Subs Lunch - Total Kids ####

kids_povl15<- subset(kids15, povll < 6)
svyby( ~one , by = ~hflunch ,	FUN =  svytotal, design = kids_povl15)
kids15

Hot_Lunch_TKids <- c(
svyby( ~one , by = ~hflunch ,FUN =  svytotal, design = kids15),
svyby( ~one , by = ~hflunch ,FUN =  svytotal, design = kids14),
svyby( ~one , by = ~hflunch ,FUN =  svytotal, design = kids13),
svyby( ~one , by = ~hflunch ,FUN =  svytotal, design = kids12),
svyby( ~one , by = ~hflunch ,FUN =  svytotal, design = kids11),
svyby( ~one , by = ~hflunch ,FUN =  svytotal, design = kids10),
svyby( ~one , by = ~hflunch ,FUN =  svytotal, design = kids9),
svyby( ~one , by = ~hflunch ,FUN =  svytotal, design = kids8),
svyby( ~one , by = ~hflunch ,FUN =  svytotal, design = kids7),
svyby( ~one , by = ~hflunch ,FUN =  svytotal, design = kids6),
svyby( ~one , by = ~hflunch ,FUN =  svytotal, design = kids5))

as.vector(Hot_Lunch_TKids)
Hot_Lunch_TKids
#### Hot Lunch - Kids - Free ####
Hot_Lunch_FreeKids <- c(
svyby( ~one , by = ~hflunch ,	FUN =  svytotal, design = kids_free15),
svyby( ~one , by = ~hflunch ,	FUN =  svytotal, design = kids_free14),
svyby( ~one , by = ~hflunch ,	FUN =  svytotal, design = kids_free13),
svyby( ~one , by = ~hflunch ,	FUN =  svytotal, design = kids_free12),
svyby( ~one , by = ~hflunch ,	FUN =  svytotal, design = kids_free11),
svyby( ~one , by = ~hflunch ,	FUN =  svytotal, design = kids_free10),
svyby( ~one , by = ~hflunch ,	FUN =  svytotal, design = kids_free9),
svyby( ~one , by = ~hflunch , FUN =  svytotal, design = kids_free8),
svyby( ~one , by = ~hflunch ,	FUN =  svytotal, design = kids_free7),
svyby( ~one , by = ~hflunch ,	FUN =  svytotal, design = kids_free6),
svyby( ~one , by = ~hflunch ,	FUN =  svytotal, design = kids_free5))

#### Hot Lunch - Kids - Reduced Price ####
Hot_Lunch_rPrice_Kids <- c(
svyby( ~one , by = ~hflunch ,	FUN =  svytotal, design = kids_reduced15),
svyby( ~one , by = ~hflunch ,	FUN =  svytotal, design = kids_reduced14),
svyby( ~one , by = ~hflunch ,	FUN =  svytotal, design = kids_reduced13),
svyby( ~one , by = ~hflunch ,	FUN =  svytotal, design = kids_reduced12),
svyby( ~one , by = ~hflunch ,	FUN =  svytotal, design = kids_reduced11),
svyby( ~one , by = ~hflunch ,	FUN =  svytotal, design = kids_reduced10),
svyby( ~one , by = ~hflunch ,	FUN =  svytotal, design = kids_reduced9),
svyby( ~one , by = ~hflunch ,	FUN =  svytotal, design = kids_reduced8),
svyby( ~one , by = ~hflunch ,	FUN =  svytotal, design = kids_reduced7),
svyby( ~one , by = ~hflunch ,	FUN =  svytotal, design = kids_reduced6),
svyby( ~one , by = ~hflunch ,	FUN =  svytotal, design = kids_reduced5))
#### Hot Lunch - Kids - Full Price ####
Hot_Lunch_fPrice_Kids <- c(
svyby( ~one , by = ~hflunch ,	FUN =  svytotal, design = kids_full15),
svyby( ~one , by = ~hflunch ,	FUN =  svytotal, design = kids_full14),
svyby( ~one , by = ~hflunch ,	FUN =  svytotal, design = kids_full13),
svyby( ~one , by = ~hflunch ,	FUN =  svytotal, design = kids_full12),
svyby( ~one , by = ~hflunch ,	FUN =  svytotal, design = kids_full11),
svyby( ~one , by = ~hflunch ,	FUN =  svytotal, design = kids_full10),
svyby( ~one , by = ~hflunch ,	FUN =  svytotal, design = kids_full9),
svyby( ~one , by = ~hflunch , FUN =  svytotal, design = kids_full8),
svyby( ~one , by = ~hflunch ,	FUN =  svytotal, design = kids_full7),
svyby( ~one , by = ~hflunch ,	FUN =  svytotal, design = kids_full6),
svyby( ~one , by = ~hflunch ,	FUN =  svytotal, design = kids_full5))



##### Print #####
Hot_Lunch_TKids

Hot_Lunch_FreeKids

Hot_Lunch_rPrice_Kids

Hot_Lunch_fPrice_Kids


#### Subsidies -  Kids - Free ####
Subs_free_Kids <- c(
svyby( ~one , by = ~hflunch ,	FUN =  svytotal, design = kids_qual_free15),
svyby( ~one , by = ~hflunch ,	FUN =  svytotal, design = kids_qual_free14),
svyby( ~one , by = ~hflunch ,	FUN =  svytotal, design = kids_qual_free13),
svyby( ~one , by = ~hflunch ,	FUN =  svytotal, design = kids_qual_free12),
svyby( ~one , by = ~hflunch ,	FUN =  svytotal, design = kids_qual_free11),
svyby( ~one , by = ~hflunch ,	FUN =  svytotal, design = kids_qual_free10),
svyby( ~one , by = ~hflunch ,	FUN =  svytotal, design = kids_qual_free9),
svyby( ~one , by = ~hflunch , FUN =  svytotal, design = kids_qual_free8),
svyby( ~one , by = ~hflunch ,	FUN =  svytotal, design = kids_qual_free7),
svyby( ~one , by = ~hflunch ,	FUN =  svytotal, design = kids_qual_free6),
svyby( ~one , by = ~hflunch ,	FUN =  svytotal, design = kids_qual_free5))
Subs_free_Kids
#### Subsidies -  Kids - Reduced Price ####
Subs_rPrice_Kids <- c(
svyby( ~one , by = ~hflunch ,	FUN =  svytotal, design = kids_qual_reduced15),
svyby( ~one , by = ~hflunch ,	FUN =  svytotal, design = kids_qual_reduced14),
svyby( ~one , by = ~hflunch ,	FUN =  svytotal, design = kids_qual_reduced13),
svyby( ~one , by = ~hflunch ,	FUN =  svytotal, design = kids_qual_reduced12),
svyby( ~one , by = ~hflunch ,	FUN =  svytotal, design = kids_qual_reduced11),
svyby( ~one , by = ~hflunch ,	FUN =  svytotal, design = kids_qual_reduced10),
svyby( ~one , by = ~hflunch ,	FUN =  svytotal, design = kids_qual_reduced9),
svyby( ~one , by = ~hflunch ,	FUN =  svytotal, design = kids_qual_reduced8),
svyby( ~one , by = ~hflunch ,	FUN =  svytotal, design = kids_qual_reduced7),
svyby( ~one , by = ~hflunch ,	FUN =  svytotal, design = kids_qual_reduced6),
svyby( ~one , by = ~hflunch ,	FUN =  svytotal, design = kids_qual_reduced5))
Subs_rPrice_Kids
#### Subsidies -  Kids - Full Price ####
Subs_fPrice_Kids <- c(
svyby( ~one , by = ~hflunch ,	FUN =  svytotal, design = kids_qual_full15),
svyby( ~one , by = ~hflunch ,	FUN =  svytotal, design = kids_qual_full14),
svyby( ~one , by = ~hflunch ,	FUN =  svytotal, design = kids_qual_full13),
svyby( ~one , by = ~hflunch ,	FUN =  svytotal, design = kids_qual_full12),
svyby( ~one , by = ~hflunch ,	FUN =  svytotal, design = kids_qual_full11),
svyby( ~one , by = ~hflunch ,	FUN =  svytotal, design = kids_qual_full10),
svyby( ~one , by = ~hflunch ,	FUN =  svytotal, design = kids_qual_full9),
svyby( ~one , by = ~hflunch , FUN =  svytotal, design = kids_qual_full8),
svyby( ~one , by = ~hflunch ,	FUN =  svytotal, design = kids_qual_full7),
svyby( ~one , by = ~hflunch ,	FUN =  svytotal, design = kids_qual_full6),
svyby( ~one , by = ~hflunch ,	FUN =  svytotal, design = kids_qual_full5))
Subs_fPrice_Kids
#### Income Analyses ####

svyby( 
	~hftotinc , 
	# choose the grouping variable
	by = ~hflunch ,
	# specify the same survey design
	design = kids_qual_full15 ,
	# run the svyquantile() function across groups
	FUN = svyquantile ,
	# calculate the median (the 50th percentile)
	quantiles = c(0, .25, 0.5, .75, .90, 1),
	# remove missing values
	na.rm = TRUE
)


#### State - Unqualified ####

svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_unqual_full5)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_unqual_full6)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_unqual_full7)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_unqual_full8)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_unqual_full9)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_unqual_full10)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_unqual_full11)
svyby( ~one , by = ~gestfips , FUN =  svytotal, design = kids_unqual_full12)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_unqual_full13)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_unqual_full14)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_unqual_full15)


svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_unqual_reduced5)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_unqual_reduced6)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_unqual_reduced7)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_unqual_reduced8)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_unqual_reduced9)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_unqual_reduced10)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_unqual_reduced11)
svyby( ~one , by = ~gestfips , FUN =  svytotal, design = kids_unqual_reduced12)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_unqual_reduced13)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_unqual_reduced14)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_unqual_reduced15)


svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_unqual_free5)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_unqual_free6)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_unqual_free7)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_unqual_free8)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_unqual_free9)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_unqual_free10)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_unqual_free11)
svyby( ~one , by = ~gestfips , FUN =  svytotal, design = kids_unqual_free12)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_unqual_free13)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_unqual_free14)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_unqual_free15)

#### State - Qual ####

svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_qual_full5)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_qual_full6)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_qual_full7)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_qual_full8)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_qual_full9)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_qual_full10)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_qual_full11)
svyby( ~one , by = ~gestfips , FUN =  svytotal, design = kids_qual_full12)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_qual_full13)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_qual_full14)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_qual_full15)

svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_qual_reduced5)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_qual_reduced6)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_qual_reduced7)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_qual_reduced8)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_qual_reduced9)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_qual_reduced10)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_qual_reduced11)
svyby( ~one , by = ~gestfips , FUN =  svytotal, design = kids_qual_reduced12)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_qual_reduced13)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_qual_reduced14)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_qual_reduced15)

svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_qual_free5)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_qual_free6)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_qual_free7)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_qual_free8)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_qual_free9)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_qual_free10)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_qual_free11)
svyby( ~one , by = ~gestfips , FUN =  svytotal, design = kids_qual_free12)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_qual_free13)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_qual_free14)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_qual_free15)

#### State - NIU ####

svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_niu_full5)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_niu_full6)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_niu_full7)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_niu_full8)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_niu_full9)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_niu_full10)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_niu_full11)
svyby( ~one , by = ~gestfips , FUN =  svytotal, design = kids_niu_full12)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_niu_full13)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_niu_full14)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_niu_full15)

svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_niu_reduced5)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_niu_reduced6)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_niu_reduced7)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_niu_reduced8)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_niu_reduced9)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_niu_reduced10)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_niu_reduced11)
svyby( ~one , by = ~gestfips , FUN =  svytotal, design = kids_niu_reduced12)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_niu_reduced13)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_niu_reduced14)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_niu_reduced15)

svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_niu_free5)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_niu_free6)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_niu_free7)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_niu_free8)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_niu_free9)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_niu_free10)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_niu_free11)
svyby( ~one , by = ~gestfips , FUN =  svytotal, design = kids_niu_free12)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_niu_free13)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_niu_free14)
svyby( ~one , by = ~gestfips ,	FUN =  svytotal, design = kids_niu_free15)

