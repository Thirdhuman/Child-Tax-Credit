setwd("~/Desktop/Poverty:Welfare/Data/R - Survey data/Current Population Survey (CPS)")

library(survey)				# load survey package (analyzes complex design surveys)
library(MonetDBLite)
library(DBI)			# load the DBI package (implements the R-database coding)
library(emdbook)

dbfolder <- paste0( getwd() , "/MonetDB" )

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

		children_fullb <-subset( y , 
		(agi <= 110000 & filestat <= 3 & fownu18 == 1 & hunder18 == 1 & actc_crd + ctc_crd <= 1000) |
		(agi <= 110000 & filestat <= 3 & fownu18 == 2 & hunder18 == 2 & actc_crd + ctc_crd <= 2000) |
		(agi <= 110000 & filestat <= 3 & fownu18 == 3 & hunder18 == 3 & actc_crd + ctc_crd <= 3000) |
		(agi <= 110000 & filestat <= 3 & fownu18 == 4 & hunder18 == 4 & actc_crd + ctc_crd <= 4000) |
		(agi <= 110000 & filestat <= 3 & fownu18 == 5 & hunder18 == 5 & actc_crd + ctc_crd <= 5000) |
		(agi <= 110000 & filestat <= 3 & fownu18 == 6 & hunder18 == 6 & actc_crd + ctc_crd <= 6000) |
		(agi <= 110000 & filestat <= 3 & fownu18 == 7 & hunder18 == 7 & actc_crd + ctc_crd <= 7000) |
		(agi <= 110000 & filestat <= 3 & fownu18 == 8 & hunder18 == 8 & actc_crd + ctc_crd <= 8000) |
		(agi <= 110000 & filestat <= 3 & fownu18 == 9 & hunder18 == 9 & actc_crd + ctc_crd <= 9000) |
		(agi <= 110000 & filestat <= 3 & fownu18 == 10 & hunder18 == 10 & actc_crd + ctc_crd <= 10000) |
		(agi <= 75000 & (filestat == 4 | filestat == 5) & fownu18 == 1 & hunder18 == 1 & actc_crd + ctc_crd <= 1000) |
		(agi <= 75000 & (filestat == 4 | filestat == 5) & fownu18 == 2 & hunder18 == 2 & actc_crd + ctc_crd <= 2000) |
		(agi <= 75000 & (filestat == 4 | filestat == 5) & fownu18 == 3 & hunder18 == 3 & actc_crd + ctc_crd <= 3000) |
		(agi <= 75000 & (filestat == 4 | filestat == 5) & fownu18 == 4 & hunder18 == 4 & actc_crd + ctc_crd <= 4000) |
		(agi <= 75000 & (filestat == 4 | filestat == 5) & fownu18 == 5 & hunder18 == 5 & actc_crd + ctc_crd <= 5000) |
		(agi <= 75000 & (filestat == 4 | filestat == 5) & fownu18 == 6 & hunder18 == 6 & actc_crd + ctc_crd <= 6000) |
		(agi <= 75000 & (filestat == 4 | filestat == 5) & fownu18 == 7 & hunder18 == 7 & actc_crd + ctc_crd <= 7000) |
		(agi <= 75000 & (filestat == 4 | filestat == 5) & fownu18 == 8 & hunder18 == 8 & actc_crd + ctc_crd <= 8000) |
		(agi <= 75000 & (filestat == 4 | filestat == 5) & fownu18 == 9 & hunder18 == 9 & actc_crd + ctc_crd <= 9000) |
		(agi <= 75000 & (filestat == 4 | filestat == 5) & fownu18 == 10 & hunder18 == 10 & actc_crd + ctc_crd <= 10000)
		& a_age > 17 & dep_stat == 0)

Fullbenefit_single <- subset(children_fullb, duplicated(h_seq) == TRUE)
Fullbenefit_dual <- subset(children_fullb, duplicated(h_seq) == FALSE)

		Fullbenefit_single <-
	update(Fullbenefit_single,
		cost_single = (2000 * hunder18))

				Fullbenefit_dual <-
	update(Fullbenefit_dual,
		cost_dual = (2000 * hunder18))
				
half	<-	svytotal(
	   ~cost_dual,
	Fullbenefit_dual)
		
full	<-	svytotal(
	   ~cost_single,
		Fullbenefit_single)



###### 1000 + 2000

		children_fullb_split <-subset( y , 
		(agi <= 110000 & filestat <= 3 & fownu18 == 1  & hunder18 == 1 & actc_crd + ctc_crd <= 1000) |
		(agi <= 110000 & filestat <= 3 & fownu18 == 2  & hunder18 == 2 & actc_crd + ctc_crd <= 2000) |
		(agi <= 110000 & filestat <= 3 & fownu18 == 3  & hunder18 == 3 & actc_crd + ctc_crd <= 3000) |
		(agi <= 110000 & filestat <= 3 & fownu18 == 4  & hunder18 == 4 & actc_crd + ctc_crd <= 4000) |
		(agi <= 110000 & filestat <= 3 & fownu18 == 5  & hunder18 == 5 & actc_crd + ctc_crd <= 5000) |
		(agi <= 110000 & filestat <= 3 & fownu18 == 6  & hunder18 == 6 & actc_crd + ctc_crd <= 6000) |
		(agi <= 110000 & filestat <= 3 & fownu18 == 7  & hunder18 == 7 & actc_crd + ctc_crd <= 7000) |
		(agi <= 110000 & filestat <= 3 & fownu18 == 8  & hunder18 == 8 & actc_crd + ctc_crd <= 8000) |
		(agi <= 110000 & filestat <= 3 & fownu18 == 9  & hunder18 == 9 & actc_crd + ctc_crd <= 9000) |
		(agi <= 110000 & filestat <= 3 & fownu18 == 10  & hunder18 == 10 & actc_crd + ctc_crd <= 10000) |
		(agi <= 75000 & (filestat == 4 | filestat == 5) & fownu18 == 1 & hunder18 == 1 & actc_crd + ctc_crd <= 1000) |
		(agi <= 75000 & (filestat == 4 | filestat == 5) & fownu18 == 2 & hunder18 == 2 & actc_crd + ctc_crd <= 2000) |
		(agi <= 75000 & (filestat == 4 | filestat == 5) & fownu18 == 3 & hunder18 == 3 & actc_crd + ctc_crd <= 3000) |
		(agi <= 75000 & (filestat == 4 | filestat == 5) & fownu18 == 4 & hunder18 == 4 & actc_crd + ctc_crd <= 4000) |
		(agi <= 75000 & (filestat == 4 | filestat == 5) & fownu18 == 5 & hunder18 == 5 & actc_crd + ctc_crd <= 5000) |
		(agi <= 75000 & (filestat == 4 | filestat == 5) & fownu18 == 6 & hunder18 == 6 & actc_crd + ctc_crd <= 6000) |
		(agi <= 75000 & (filestat == 4 | filestat == 5) & fownu18 == 7 & hunder18 == 7 & actc_crd + ctc_crd <= 7000) |
		(agi <= 75000 & (filestat == 4 | filestat == 5) & fownu18 == 8 & hunder18 == 8 & actc_crd + ctc_crd <= 8000) |
		(agi <= 75000 & (filestat == 4 | filestat == 5) & fownu18 == 9 & hunder18 == 9 & actc_crd + ctc_crd <= 9000) |
		(agi <= 75000 & (filestat == 4 | filestat == 5) & fownu18 == 10 & hunder18 == 10 & actc_crd + ctc_crd <= 10000)
		& a_age > 17 & dep_stat == 0)

Fullbenefit_single_split <- subset(children_fullb_split, duplicated(h_seq) == TRUE)
Fullbenefit_dual_split <- subset(children_fullb_split, duplicated(h_seq) == FALSE)

		Fullbenefit_single_split <-
	update(Fullbenefit_single_split,
		cost_single_split = (2000 * pmin(fownu6, 3) + hh5to18 * 1000))

				Fullbenefit_dual_split <-
	update(Fullbenefit_dual_split,
		cost_dual_split = (2000 * pmin(fownu6, 3) + hh5to18 * 1000))
				
half_split	<-	svytotal(
	   ~cost_dual_split,
	Fullbenefit_dual_split)
		
full_split	<-	svytotal(
	   ~cost_single_split,
		Fullbenefit_single_split)


######  Reduced #######
#### Reduced Benefit - 2000 ####

mchild_reduceb <-subset( y , 
		(agi >= 110000 & agi < 130000 & filestat <= 3 & fownu18 == 1 & hunder18 == 1 & actc_crd + ctc_crd <= 1000) |
		(agi >= 110000 & agi < 150000 & filestat <= 3 & fownu18 == 2 & hunder18 == 2 & actc_crd + ctc_crd <= 2000) |
		(agi >= 110000 & agi < 170000 & filestat <= 3 & fownu18 == 3 & hunder18 == 3 & actc_crd + ctc_crd <= 3000) |
		(agi >= 110000 & agi < 190000 & filestat <= 3 & fownu18 == 4 & hunder18 == 4 & actc_crd + ctc_crd <= 4000) |
		(agi >= 110000 & agi < 210000 & filestat <= 3 & fownu18 == 5 & hunder18 == 5 & actc_crd + ctc_crd <= 5000) |
		(agi >= 110000 & agi < 230000 & filestat <= 3 & fownu18 == 6 & hunder18 == 6 & actc_crd + ctc_crd <= 6000) |
		(agi >= 110000 & agi < 250000 & filestat <= 3 & fownu18 == 7 & hunder18 == 7 & actc_crd + ctc_crd <= 7000) |
		(agi >= 110000 & agi < 270000 & filestat <= 3 & fownu18 == 8 & hunder18 == 8 & actc_crd + ctc_crd <= 8000) |
		(agi >= 110000 & agi < 290000 & filestat <= 3 & fownu18 == 9 & hunder18 == 9 & actc_crd + ctc_crd <= 9000) |
		(agi >= 110000 & agi < 310000 & filestat <= 3 & fownu18 == 10 & hunder18 == 10 & actc_crd + ctc_crd <= 10000) 
		& a_age > 17 & dep_stat == 0)

mchild_reduceb_single <- subset(mchild_reduceb, duplicated(h_seq) == TRUE)
mchild_reduceb_dual <- subset(mchild_reduceb, duplicated(h_seq) == FALSE)

mchild_reduceb_single <-
	update(mchild_reduceb_single,
		mnew_credit_single =  (2000 * fownu18) - ((agi - 110000) * (.05)))

mchild_reduceb_dual <-
	update(mchild_reduceb_dual,
		mnew_credit_dual =  (2000 * fownu18) - ((agi - 110000) * (.05)))

half_r	<-	svytotal(
	   ~mnew_credit_dual,
	mchild_reduceb_dual)
		
full_r	<-	svytotal(
	   ~mnew_credit_single,
		mchild_reduceb_single)


schild_reduceb <- subset( y,
		(agi >= 75000 & agi < 95000 &(filestat == 4 | filestat == 5) & fownu18 == 1 & hunder18 == 1 & actc_crd + ctc_crd <= 1000) |
		(agi >= 75000 & agi < 115000 &(filestat == 4 | filestat == 5) & fownu18 == 2 & hunder18 == 2 & actc_crd + ctc_crd <= 2000) |
		(agi >= 75000 & agi < 135000 &(filestat == 4 | filestat == 5) & fownu18 == 3 & hunder18 == 3 & actc_crd + ctc_crd <= 3000) |
		(agi >= 75000 & agi < 155000 &(filestat == 4 | filestat == 5) & fownu18 == 4 & hunder18 == 4 & actc_crd + ctc_crd <= 4000) |
		(agi >= 75000 & agi < 175000 &(filestat == 4 | filestat == 5) & fownu18 == 5 & hunder18 == 5 & actc_crd + ctc_crd <= 5000) |
		(agi >= 75000 & agi < 195000 &(filestat == 4 | filestat == 5) & fownu18 == 6 & hunder18 == 6 & actc_crd + ctc_crd <= 6000) |
		(agi >= 75000 & agi < 215000 &(filestat == 4 | filestat == 5) & fownu18 == 7 & hunder18 == 7 & actc_crd + ctc_crd <= 7000) |
		(agi >= 75000 & agi < 235000 &(filestat == 4 | filestat == 5) & fownu18 == 8 & hunder18 == 8 & actc_crd + ctc_crd <= 8000) |
		(agi >= 75000 & agi < 255000 &(filestat == 4 | filestat == 5) & fownu18 == 9 & hunder18 == 9 & actc_crd + ctc_crd <= 9000) |
		(agi >= 75000 & agi < 275000 &(filestat == 4 | filestat == 5) & fownu18 == 10 & hunder18 == 10 & actc_crd + ctc_crd <= 10000) 
			& a_age > 17 & dep_stat == 0)

schild_reduceb_single <- subset(schild_reduceb, duplicated(h_seq) == TRUE)
schild_reduceb_dual <- subset(schild_reduceb, duplicated(h_seq) == FALSE)

schild_reduceb_single <-
	update(schild_reduceb_single,
		snew_credit_single =  (2000 * fownu18) - ((agi - 75000) * (.05)))

schild_reduceb_dual <-
	update(schild_reduceb_dual,
		snew_credit_dual =  (2000 * fownu18) - ((agi - 75000) * (.05)))

s_half_r	<-	svytotal(
	   ~snew_credit_dual,
	schild_reduceb_dual)
		
s_full_r	<-	svytotal(
	   ~snew_credit_single,
		schild_reduceb_single)


#### Reduced Benefit 1000 & 2000 - split ####

mchild_reduceb_split <-subset( y , 
		(agi >= 110000 & agi < 130000 & filestat <= 3 & fownu18 == 1 & hunder18 == 1 & actc_crd + ctc_crd <= 1000) |
		(agi >= 110000 & agi < 150000 & filestat <= 3 & fownu18 == 2 & hunder18 == 2 & actc_crd + ctc_crd <= 2000) |
		(agi >= 110000 & agi < 170000 & filestat <= 3 & fownu18 == 3 & hunder18 == 3 & actc_crd + ctc_crd <= 3000) |
		(agi >= 110000 & agi < 190000 & filestat <= 3 & fownu18 == 4 & hunder18 == 4 & actc_crd + ctc_crd <= 4000) |
		(agi >= 110000 & agi < 210000 & filestat <= 3 & fownu18 == 5 & hunder18 == 5 & actc_crd + ctc_crd <= 5000) |
		(agi >= 110000 & agi < 230000 & filestat <= 3 & fownu18 == 6 & hunder18 == 6 & actc_crd + ctc_crd <= 6000) |
		(agi >= 110000 & agi < 250000 & filestat <= 3 & fownu18 == 7 & hunder18 == 7 & actc_crd + ctc_crd <= 7000) |
		(agi >= 110000 & agi < 270000 & filestat <= 3 & fownu18 == 8 & hunder18 == 8 & actc_crd + ctc_crd <= 8000) |
		(agi >= 110000 & agi < 290000 & filestat <= 3 & fownu18 == 9 & hunder18 == 9 & actc_crd + ctc_crd <= 9000) |
		(agi >= 110000 & agi < 310000 & filestat <= 3 & fownu18 == 10 & hunder18 == 10 & actc_crd + ctc_crd <= 10000) 	
		& a_age > 17 & dep_stat == 0)

mchild_reduceb_single_split <- subset(mchild_reduceb_split, duplicated(h_seq) == TRUE)
mchild_reduceb_dual_split <- subset(mchild_reduceb_split, duplicated(h_seq) == FALSE)

mchild_reduceb_single_split <-
	update(mchild_reduceb_single_split,
		mnew_credit_single_split =  (1000 * hh5to18) + (2000 * pmin(fownu6, 3)) - ((agi - 110000) * (.05)))

mchild_reduceb_dual_split <-
	update(mchild_reduceb_dual_split,
		mnew_credit_dual_split =  (1000 * hh5to18) + (2000 * pmin(fownu6, 3)) - ((agi - 110000) * (.05)))

half_r_split	<-	svytotal(
	   ~mnew_credit_dual_split,
	mchild_reduceb_dual_split)
		
full_r_split	<-	svytotal(
	   ~mnew_credit_single_split,
		mchild_reduceb_single_split)


schild_reduceb_split <- subset( y,
		(agi >= 75000 & agi < 95000 &(filestat == 4 | filestat == 5) & fownu18 == 1 & hunder18 == 1 & actc_crd + ctc_crd <= 1000) |
		(agi >= 75000 & agi < 115000 &(filestat == 4 | filestat == 5) & fownu18 == 2 & hunder18 == 2 & actc_crd + ctc_crd <= 2000) |
		(agi >= 75000 & agi < 135000 &(filestat == 4 | filestat == 5) & fownu18 == 3& hunder18 == 3 & actc_crd + ctc_crd <= 3000) |
		(agi >= 75000 & agi < 155000 &(filestat == 4 | filestat == 5) & fownu18 == 4 & hunder18 == 4 & actc_crd + ctc_crd <= 4000) |
		(agi >= 75000 & agi < 175000 &(filestat == 4 | filestat == 5) & fownu18 == 5  & hunder18 == 5  & actc_crd + ctc_crd <= 5000) |
		(agi >= 75000 & agi < 195000 &(filestat == 4 | filestat == 5) & fownu18 == 6 & hunder18 == 6  & actc_crd + ctc_crd <= 6000) |
		(agi >= 75000 & agi < 215000 &(filestat == 4 | filestat == 5) & fownu18 == 7 & hunder18 == 7  & actc_crd + ctc_crd <= 7000) |
		(agi >= 75000 & agi < 235000 &(filestat == 4 | filestat == 5) & fownu18 == 8 & hunder18 == 8  & actc_crd + ctc_crd <= 8000) |
		(agi >= 75000 & agi < 255000 &(filestat == 4 | filestat == 5) & fownu18 == 9 & hunder18 == 9  & actc_crd + ctc_crd <= 9000) |
		(agi >= 75000 & agi < 275000 &(filestat == 4 | filestat == 5) & fownu18 == 10 & hunder18 == 10  & actc_crd + ctc_crd <= 10000) 
			& a_age > 17 & dep_stat == 0)

schild_reduceb_single_split <- subset(schild_reduceb, duplicated(h_seq) == TRUE)
schild_reduceb_dual_split <- subset(schild_reduceb, duplicated(h_seq) == FALSE)

schild_reduceb_single_split <-
	update(schild_reduceb_single_split,
		snew_credit_single_split =  (2000 * pmin(fownu6, 3) + 1000 * hh5to18) - ((agi - 75000) * (.05)))

schild_reduceb_dual_split <-
	update(schild_reduceb_dual_split,
		snew_credit_dual_split =  (2000 * pmin(fownu6, 3) + 1000 * hh5to18) - ((agi - 75000) * (.05)))

s_half_r_split	<-	svytotal(
	   ~snew_credit_dual_split,
	schild_reduceb_dual_split)
		
s_full_r_split	<-	svytotal(
	   ~snew_credit_single_split,
		schild_reduceb_single_split)


schild_reduceb_split <-
	update(schild_reduceb_split,
		snew_credit =  (2000 * fownu6) - ((agi - 75000) * (.05))))

mchild_reduceb2_split <-
	update(mchild_reduceb2,
		mnew_credit2_split =  (2000 * fownu6) +  - ((agi - 110000) * (.05))))

schild_reduceb2 <-
	update(schild_reduceb2_split,
		snew_credit2_split =  (2000 * fownu6) - ((agi - 75000) * (.05))))

