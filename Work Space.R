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
		data = "asec15" ,
		combined.weights = T ,
		dbtype = "MonetDBLite" ,
		dbname = dbfolder
	)

A_FNLWGT
a_fnlwgt

WTFINL
wtfinl
wtsupp

y$mse <- TRUE


		num_fullb_0u5<-
	svyby(
		~one,
		design = children_fullb_0u5 ,
		svytotal
	)
		
		y <- update( y , totcost =  actc_crd + ctc_crd) 


		num_fullb_0u5
		
		children_fullb <-subset( y , 
		(agi <= 110000 & filestat <= 3 & fownu18 == 1 & hh5to18 == 0 & fownu6 == 1 & actc_crd + ctc_crd <= 1000) |
		(agi <= 110000 & filestat <= 3 & fownu18 == 2 & hh5to18 == 0 & fownu6 == 2  & actc_crd + ctc_crd <= 2000) |
		(agi <= 110000 & filestat <= 3 & fownu18 == 3 & hh5to18 == 0 & fownu6 == 3  & actc_crd + ctc_crd <= 3000) |
		(agi <= 110000 & filestat <= 3 & fownu18 == 4 & hh5to18 == 0 & fownu6 == 4  & actc_crd + ctc_crd <= 4000) |
		(agi <= 110000 & filestat <= 3 & fownu18 == 5 & hh5to18 == 0 & fownu6 == 5  & actc_crd + ctc_crd <= 5000) |
		(agi <= 110000 & filestat <= 3 & fownu18 == 6 & hh5to18 == 0 & fownu6 == 6  & actc_crd + ctc_crd <= 6000) |
		(agi <= 110000 & filestat <= 3 & fownu18 == 7 & hh5to18 == 0 & fownu6 == 7  & actc_crd + ctc_crd <= 7000) |
		(agi <= 110000 & filestat <= 3 & fownu18 == 8 & hh5to18 == 0 & fownu6 == 8  & actc_crd + ctc_crd <= 8000) |
		(agi <= 110000 & filestat <= 3 & fownu18 == 9 & hh5to18 == 0 & fownu6 == 9  & actc_crd + ctc_crd <= 9000) |
		(agi <= 110000 & filestat <= 3 & fownu18 == 10 &  hh5to18 == 0 & fownu6 == 0 & actc_crd + ctc_crd <= 10000) |
		(agi <= 75000 & (filestat == 4 | filestat == 5) & fownu18 == 1 &  actc_crd + ctc_crd <= 1000) |
		(agi <= 75000 & (filestat == 4 | filestat == 5) & fownu18 == 2  &  actc_crd + ctc_crd <= 2000) |
		(agi <= 75000 & (filestat == 4 | filestat == 5) & fownu18 == 3   & actc_crd + ctc_crd <= 3000) |
		(agi <= 75000 & (filestat == 4 | filestat == 5) & fownu18 == 4  & actc_crd + ctc_crd <= 4000) |
		(agi <= 75000 & (filestat == 4 | filestat == 5) & fownu18 == 5    & actc_crd + ctc_crd <= 5000) |
		(agi <= 75000 & (filestat == 4 | filestat == 5) & fownu18 == 6   & actc_crd + ctc_crd <= 6000) |
		(agi <= 75000 & (filestat == 4 | filestat == 5) & fownu18 == 7    & actc_crd + ctc_crd <= 7000) |
		(agi <= 75000 & (filestat == 4 | filestat == 5) & fownu18 == 8  & actc_crd + ctc_crd <= 8000) |
		(agi <= 75000 & (filestat == 4 | filestat == 5) & fownu18 == 9    & actc_crd + ctc_crd <= 9000) |
		(agi <= 75000 & (filestat == 4 | filestat == 5) & fownu18 == 10    & actc_crd + ctc_crd <= 10000) 
		& filestat != 0 & length(unique((h_seq)) == 1 )) 
		
		children_redb <-subset( y , 
		(agi >= 110000 & filestat <= 3 & fownu18 == 1 & actc_crd + ctc_crd <= 1000) |
		(agi >= 110000 & filestat <= 3 & fownu18 == 2 & actc_crd + ctc_crd <= 2000) |
		(agi >= 110000 & filestat <= 3 & fownu18 == 3 & actc_crd + ctc_crd <= 3000) |
		(agi >= 110000 & filestat <= 3 & fownu18 == 4 & actc_crd + ctc_crd <= 4000) |
		(agi >= 110000 & filestat <= 3 & fownu18 == 5 & actc_crd + ctc_crd <= 5000) |
		(agi >= 110000 & filestat <= 3 & fownu18 == 6 & actc_crd + ctc_crd <= 6000) |
		(agi >= 110000 & filestat <= 3 & fownu18 == 7 & actc_crd + ctc_crd <= 7000) |
		(agi >= 110000 & filestat <= 3 & fownu18 == 8 & actc_crd + ctc_crd <= 8000) |
		(agi >= 110000 & filestat <= 3 & fownu18 == 9 & actc_crd + ctc_crd <= 9000) |
		(agi >= 110000 & filestat <= 3 & fownu18 == 10 & actc_crd + ctc_crd <= 10000) |
		(agi >= 75000 & (filestat == 4 | filestat == 5) & fownu18 == 1 & actc_crd + ctc_crd <= 1000) |
		(agi >= 75000 & (filestat == 4 | filestat == 5) & fownu18 == 2 & actc_crd + ctc_crd <= 2000) |
		(agi >= 75000 & (filestat == 4 | filestat == 5) & fownu18 == 3 & actc_crd + ctc_crd <= 3000) |
		(agi >= 75000 & (filestat == 4 | filestat == 5) & fownu18 == 4 & actc_crd + ctc_crd <= 4000) |
		(agi >= 75000 & (filestat == 4 | filestat == 5) & fownu18 == 5 & actc_crd + ctc_crd <= 5000) |
		(agi >= 75000 & (filestat == 4 | filestat == 5) & fownu18 == 6 & actc_crd + ctc_crd <= 6000) |
		(agi >= 75000 & (filestat == 4 | filestat == 5) & fownu18 == 7 & actc_crd + ctc_crd <= 7000) |
		(agi >= 75000 & (filestat == 4 | filestat == 5) & fownu18 == 8 & actc_crd + ctc_crd <= 8000) |
		(agi >= 75000 & (filestat == 4 | filestat == 5) & fownu18 == 9 & actc_crd + ctc_crd <= 9000) |
		(agi >= 75000 & (filestat == 4 | filestat == 5) & fownu18 == 10 & actc_crd + ctc_crd <= 10000) 
			& filestat != 0 & h_seq[!duplicated(h_seq)])
		
				children_redb <- update( children_redb , totcost =  actc_crd + ctc_crd) 

						Overthresh<-
	svytotal(
		~totcost ,
		design = children_fullb 
	)
				children_fullb <-
	update(children_fullb,
	cost = (1000 * pmin(hh5to18, 4) + (2000 * pmin(fownu6, 4) - (agi - 110000) * (.05))))
				
				
				
				
children_fullb <- update(children_fullb,
			ifelse(length(duplicated(hseq)) == 1,  cost_full = 1, 
			ifelse(length(duplicated(hseq)) == 2,  cost_full = 2, 					
		 ifelse(length(duplicated(hseq)) == 3,  cost_full = 3, 
												cost_full = 0)))))
								
									cost_full =  2000 * pmin(fownu18, 20) - (agi - 110000) * (.05))
					
	Underthresh<-
	svytotal(
		~cost_full ,
		design = children_fullb )
			
Underthresh
						
				children_redb<-
	svyby(
		~one ,
		~hunder18 & fownu6 = 0,
		design = children_redb ,
		svytotal
	)

		
		
				num_fullb_1u5<-
	svyby(
		~one ,
		~hunder18 & fownu6 = 0,
		design = children_fullb_0u5 ,
		svytotal
	)

		num_fullb_1u5

		children_fullb_1u5 <-
	update(children_fullb_0u5,
		mnew_credit2 =  (2000 * pmin(fownu18, 4) - ((agi - 110000) * (.05))))

				children_fullb_1u5 <-
	update(children_fullb_0o5,
		mnew_credit2 =  (1000 * pmin(fownu18, 4) - ((agi - 110000) * (.05))))

	
x <-subset( y , length(unique((h_seq)) == 1 ))				
				
mchild_reduceb_0u5 <-subset( x , 
		(agi >= 110000 & agi < 130000 & filestat <= 3 & fownu18 == 1 & hh5to18 == 1 & actc_crd + ctc_crd <= 1000) |
		(agi >= 110000 & agi < 150000 & filestat <= 3 & fownu18 == 2 & hh5to18 == 2 & actc_crd + ctc_crd <= 2000) |
		(agi >= 110000 & agi < 170000 & filestat <= 3 & fownu18 == 3 & hh5to18 == 3 & actc_crd + ctc_crd <= 3000) |
		(agi >= 110000 & agi < 190000 & filestat <= 3 & fownu18 == 4 & hh5to18 == 4 & actc_crd + ctc_crd <= 4000) |
		(agi >= 110000 & agi < 190000 & filestat <= 3 & fownu18 == 5 & hh5to18 == 5 & actc_crd + ctc_crd <= 5000) |
		(agi >= 110000 & agi < 190000 & filestat <= 3 & fownu18 == 6 & hh5to18 == 6 & actc_crd + ctc_crd <= 6000) |
		(agi >= 110000 & agi < 190000 & filestat <= 3 & fownu18 == 7 & hh5to18 == 7 & actc_crd + ctc_crd <= 7000) |
		(agi >= 110000 & agi < 190000 & filestat <= 3 & fownu18 == 8 & hh5to18 == 8 & actc_crd + ctc_crd <= 8000) |
		(agi >= 110000 & agi < 190000 & filestat <= 3 & fownu18 == 9 & hh5to18 == 9 & actc_crd + ctc_crd <= 9000) |
		(agi >= 110000 & agi < 190000 & filestat <= 3 & fownu18 == 10 & hh5to18 == 10 & actc_crd + ctc_crd <= 10000))

mchild_reduceb_0u5 <-
	update(mchild_reduceb_0u5,
		mnew_credit_0u5 =  (1000 * pmin(hh5to18, 4) - ((agi - 110000) * (.05))))


mchild_reduceb_1u5 <-subset( x , 
		(agi >= 110000 & agi < 130000 & filestat <= 3 & fownu18 == 1 & hh5to18 <= 1 & fownu6 == 1 & actc_crd + ctc_crd <= 1000) |
		(agi >= 110000 & agi < 150000 & filestat <= 3 & fownu18 == 2 & hh5to18 <= 2 & fownu6 == 1 & actc_crd + ctc_crd <= 2000) |
		(agi >= 110000 & agi < 170000 & filestat <= 3 & fownu18 == 3 & hh5to18 <= 3 & fownu6 == 1 & actc_crd + ctc_crd <= 3000) |
		(agi >= 110000 & agi < 190000 & filestat <= 3 & fownu18 == 4 & hh5to18 <= 4 & fownu6 == 1 & actc_crd + ctc_crd <= 4000) |
		(agi >= 110000 & agi < 190000 & filestat <= 3 & fownu18 == 5 & hh5to18 <= 5 & fownu6 == 1 & actc_crd + ctc_crd <= 5000) |
		(agi >= 110000 & agi < 190000 & filestat <= 3 & fownu18 == 6 & hh5to18 <= 6 & fownu6 == 1 & actc_crd + ctc_crd <= 6000) |
		(agi >= 110000 & agi < 190000 & filestat <= 3 & fownu18 == 7 & hh5to18 <= 7 & fownu6 == 1 & actc_crd + ctc_crd <= 7000) |
		(agi >= 110000 & agi < 190000 & filestat <= 3 & fownu18 == 8 & hh5to18 <= 8 & fownu6 == 1 & actc_crd + ctc_crd <= 8000) |
		(agi >= 110000 & agi < 190000 & filestat <= 3 & fownu18 == 9 & hh5to18 <= 9 & fownu6 == 1 & actc_crd + ctc_crd <= 9000) |
		(agi >= 110000 & agi < 190000 & filestat <= 3 & fownu18 == 10 & hh5to18 <= 10 & fownu6 == 1 & actc_crd + ctc_crd <= 10000))

mchild_reduceb_1u5 <-
	update(mchild_reduceb_1u5,
		mnew_credit_1u5 =  (1000 * pmin(hh5to18, 3) + (2000 * pmin(fownu6, 1) - ((agi  - 110000) * (.05)))))
		
svytotal(
	~mnew_credit_1u5,
	mchild_reduceb_1u5)

mchild_reduceb_2u5 <-subset( x , 
		(agi >= 110000 & agi < 130000 & filestat <= 3 & fownu18 == 1 & hh5to18 <= 1 & fownu6 == 2 & actc_crd + ctc_crd <= 1000) |
		(agi >= 110000 & agi < 150000 & filestat <= 3 & fownu18 == 2 & hh5to18 <= 2 & fownu6 == 2 & actc_crd + ctc_crd <= 2000) |
		(agi >= 110000 & agi < 170000 & filestat <= 3 & fownu18 == 3 & hh5to18 <= 3 & fownu6 == 2 & actc_crd + ctc_crd <= 3000) |
		(agi >= 110000 & agi < 190000 & filestat <= 3 & fownu18 == 4 & hh5to18 <= 4 & fownu6 == 2 & actc_crd + ctc_crd <= 4000) |
		(agi >= 110000 & agi < 190000 & filestat <= 3 & fownu18 == 5 & hh5to18 <= 5 & fownu6 == 2 & actc_crd + ctc_crd <= 5000) |
		(agi >= 110000 & agi < 190000 & filestat <= 3 & fownu18 == 6 & hh5to18 <= 6 & fownu6 == 2 & actc_crd + ctc_crd <= 6000) |
		(agi >= 110000 & agi < 190000 & filestat <= 3 & fownu18 == 7 & hh5to18 <= 7 & fownu6 == 2 & actc_crd + ctc_crd <= 7000) |
		(agi >= 110000 & agi < 190000 & filestat <= 3 & fownu18 == 8 & hh5to18 <= 8 & fownu6 == 2 & actc_crd + ctc_crd <= 8000) |
		(agi >= 110000 & agi < 190000 & filestat <= 3 & fownu18 == 9 & hh5to18 <= 9 & fownu6 == 2 & actc_crd + ctc_crd <= 9000) |
		(agi >= 110000 & agi < 190000 & filestat <= 3 & fownu18 >= 10 & hh5to18 >= 10 & fownu6 == 2 & actc_crd + ctc_crd <= 10000))

mchild_reduceb_2u5 <-
	update(mchild_reduceb_2u5,
		mnew_credit_2u5 =  (1000 * pmin(hh5to18, 2) + (2000 * pmin(fownu6, 2) - ((agi - 110000) * (.05)))))
		

mchild_reduceb_3u5 <-subset( x , 
		(agi >= 110000 & agi < 130000 & filestat <= 3 & fownu18 == 3 & hh5to18 >= 1 & fownu6 == 3 & actc_crd + ctc_crd <= 1000) |
		(agi >= 110000 & agi < 150000 & filestat <= 3 & fownu18 == 3 & hh5to18 >= 2 & fownu6 == 3 & actc_crd + ctc_crd <= 2000) |
		(agi >= 110000 & agi < 170000 & filestat <= 3 & fownu18 == 3 & hh5to18 >= 3 & fownu6 == 3 & actc_crd + ctc_crd <= 3000) |
		(agi >= 110000 & agi < 190000 & filestat <= 3 & fownu18 == 4 & hh5to18 >= 4 & fownu6 == 3 & actc_crd + ctc_crd <= 4000) |
		(agi >= 110000 & agi < 190000 & filestat <= 3 & fownu18 == 5 & hh5to18 >= 5 & fownu6 == 3 & actc_crd + ctc_crd <= 5000) |
		(agi >= 110000 & agi < 190000 & filestat <= 3 & fownu18 == 6 & hh5to18 >= 6 & fownu6 == 3 & actc_crd + ctc_crd <= 6000) |
		(agi >= 110000 & agi < 190000 & filestat <= 3 & fownu18 == 7 & hh5to18 >= 7 & fownu6 == 3 & actc_crd + ctc_crd <= 7000) |
		(agi >= 110000 & agi < 190000 & filestat <= 3 & fownu18 == 8 & hh5to18 >= 8 & fownu6 == 3 & actc_crd + ctc_crd <= 8000) |
		(agi >= 110000 & agi < 190000 & filestat <= 3 & fownu18 == 9 & hh5to18 >= 9 & fownu6 == 3 & actc_crd + ctc_crd <= 9000) |
		(agi >= 110000 & agi < 190000 & filestat <= 3 & fownu18 >= 10 & hh5to18 >= 10 & fownu6 == 3 & actc_crd + ctc_crd <= 10000))

mchild_reduceb_3u5 <-
	update(mchild_reduceb_3u5,
		mnew_credit_3u5 =  (1000 * pmin(hh5to18, 1) + (2000 * pmin(fownu6, 3) - ((agi - 110000) * (.05)))))
		

mchild_reduceb_4u5 <-subset( x , 
		(agi >= 110000 & agi < 130000 & filestat <= 3 & fownu18 == 4 & hh5to18 >= 0 & fownu6 >= 4 & actc_crd + ctc_crd <= 1000) |
		(agi >= 110000 & agi < 150000 & filestat <= 3 & fownu18 == 4 & hh5to18 >= 2 & fownu6 >= 4 & actc_crd + ctc_crd <= 2000) |
		(agi >= 110000 & agi < 170000 & filestat <= 3 & fownu18 == 4 & hh5to18 >= 3 & fownu6 >= 4 & actc_crd + ctc_crd <= 3000) |
		(agi >= 110000 & agi < 190000 & filestat <= 3 & fownu18 == 4 & hh5to18 >= 4 & fownu6 >= 4 & actc_crd + ctc_crd <= 4000) |
		(agi >= 110000 & agi < 190000 & filestat <= 3 & fownu18 == 5 & hh5to18 >= 5 & fownu6 >= 4 & actc_crd + ctc_crd <= 5000) |
		(agi >= 110000 & agi < 190000 & filestat <= 3 & fownu18 == 6 & hh5to18 >= 6 & fownu6 >= 4 & actc_crd + ctc_crd <= 6000) |
		(agi >= 110000 & agi < 190000 & filestat <= 3 & fownu18 == 7 & hh5to18 >= 7 & fownu6 >= 4 & actc_crd + ctc_crd <= 7000) |
		(agi >= 110000 & agi < 190000 & filestat <= 3 & fownu18 == 8 & hh5to18 >= 8 & fownu6 >= 4 & actc_crd + ctc_crd <= 8000) |
		(agi >= 110000 & agi < 190000 & filestat <= 3 & fownu18 == 9 & hh5to18 >= 9 & fownu6 >= 4 & actc_crd + ctc_crd <= 9000) |
		(agi >= 110000 & agi < 190000 & filestat <= 3 & fownu18 == 10 & hh5to18 >= 10 & fownu6 >= 4 & actc_crd + ctc_crd <= 10000))

mchild_reduceb_4u5 <-
	update(mchild_reduceb_4u5,
		mnew_credit_4u5 =  (1000 * pmin(hh5to18, 0) + (2000 * pmin(fownu6, 4) - ((agi - 110000) * (.05)))))
		

#Cost_UT	= (num_fullb_1o5 * 1000) + (num_fullb_2o5 * 2000) + (num_fullb_3o5 * 3000) + (num_fullb_4o5 * 4000)
#+ (num_fullb_1u5 * 2000) + (num_fullb_1u5 * 4000) + (num_fullb_3u5 * 6000) + (num_fullb_4u5 * 8000)
#+ (num_fullb_2u_2o * 6000) (num_fullb_1u_1o * 3000) + (num_fullb_2u_1o * 5000) + (num_fullb_1u_2o * 4000)
#+ (num_fullb_3u_1o * 7000) + (num_fullb_1u_3o * 7000)
																
schild_reduceb_0u5 <-subset( x , 
		(agi >= 75000 & agi < 95000 &(filestat == 4 | filestat == 5) & fownu18 == 1 & hh5to18 <= 1 & fownu6 == 0 & actc_crd + ctc_crd <= 1000) |
		(agi >= 75000 & agi < 115000 &(filestat == 4 | filestat == 5) & fownu18 == 2 & hh5to18 <= 2 & fownu6 == 0 & actc_crd + ctc_crd <= 2000) |
		(agi >= 75000 & agi < 135000 &(filestat == 4 | filestat == 5) & fownu18 == 3 & hh5to18 <= 3 & fownu6 == 0 & actc_crd + ctc_crd <= 3000) |
		(agi >= 75000 & agi < 155000 &(filestat == 4 | filestat == 5) & fownu18 == 4 & hh5to18 <= 4 & fownu6 == 0 & actc_crd + ctc_crd <= 4000) |
		(agi >= 75000 & agi < 155000 &(filestat == 4 | filestat == 5) & fownu18 == 5 & hh5to18 <= 5 & fownu6 == 0 & actc_crd + ctc_crd <= 5000) |
		(agi >= 75000 & agi < 155000 &(filestat == 4 | filestat == 5) & fownu18 == 6 & hh5to18 <= 6 & fownu6 == 0 & actc_crd + ctc_crd <= 6000) |
		(agi >= 75000 & agi < 155000 &(filestat == 4 | filestat == 5) & fownu18 == 7 & hh5to18 <= 7 & fownu6 == 0 & actc_crd + ctc_crd <= 7000) |
		(agi >= 75000 & agi < 155000 &(filestat == 4 | filestat == 5) & fownu18 == 8 & hh5to18 <= 8 & fownu6 == 0 & actc_crd + ctc_crd <= 8000) |
		(agi >= 75000 & agi < 155000 &(filestat == 4 | filestat == 5) & fownu18 == 9 & hh5to18 <= 9 & fownu6 == 0 & actc_crd + ctc_crd <= 9000) |
		(agi >= 75000 & agi < 155000 &(filestat == 4 | filestat == 5) & fownu18 == 10 & hh5to18 <= 10 & fownu6 == 0 & actc_crd + ctc_crd <= 10000))

schild_reduceb_0u5 <-
	update(schild_reduceb_0u5,
		snew_credit_0u5 =  (1000 * pmin(hh5to18, 4) - ((agi - 75000) * (.05))))

schild_reduceb_1u5 <-subset( x , 
		(agi >= 75000 & agi < 95000 &(filestat == 4 | filestat == 5) & fownu18 == 1 & hh5to18 <= 1 & fownu6 == 1 & actc_crd + ctc_crd <= 1000) |
		(agi >= 75000 & agi < 115000 &(filestat == 4 | filestat == 5) & fownu18 == 2 & hh5to18 <= 2 & fownu6 == 1 & actc_crd + ctc_crd <= 2000) |
		(agi >= 75000 & agi < 135000 &(filestat == 4 | filestat == 5) & fownu18 == 3 & hh5to18 <= 3 & fownu6 == 1 & actc_crd + ctc_crd <= 3000) |
		(agi >= 75000 & agi < 155000 &(filestat == 4 | filestat == 5) & fownu18 == 4 & hh5to18 <= 4 & fownu6 == 1 & actc_crd + ctc_crd <= 4000) |
		(agi >= 75000 & agi < 155000 &(filestat == 4 | filestat == 5) & fownu18 == 5 & hh5to18 <= 5 & fownu6 == 1 & actc_crd + ctc_crd <= 5000) |
		(agi >= 75000 & agi < 155000 &(filestat == 4 | filestat == 5) & fownu18 == 6 & hh5to18 <= 6 & fownu6 == 1 & actc_crd + ctc_crd <= 6000) |
		(agi >= 75000 & agi < 155000 &(filestat == 4 | filestat == 5) & fownu18 == 7 & hh5to18 <= 7 & fownu6 == 1 & actc_crd + ctc_crd <= 7000) |
		(agi >= 75000 & agi < 155000 &(filestat == 4 | filestat == 5) & fownu18 == 8 & hh5to18 <= 8 & fownu6 == 1 & actc_crd + ctc_crd <= 8000) |
		(agi >= 75000 & agi < 155000 &(filestat == 4 | filestat == 5) & fownu18 == 9 & hh5to18 >= 9 & fownu6 == 1 & actc_crd + ctc_crd <= 9000) |
		(agi >= 75000 & agi < 155000 &(filestat == 4 | filestat == 5) & fownu18 >= 10 & hh5to18 >= 10 & fownu6 == 1 & actc_crd + ctc_crd <= 10000))

schild_reduceb_1u5 <-
	update(schild_reduceb_1u5,
		snew_credit_1u5 =  (1000 * pmin(hh5to18, 3) + (2000 * pmin(fownu6, 1) - ((agi - 75000) * (.05)))))
		

schild_reduceb_2u5 <-subset( x , 
		(agi >= 75000 & agi < 95000 &(filestat == 4 | filestat == 5) & fownu18 == 2 & hh5to18 <= 1 & fownu6 == 2 & actc_crd + ctc_crd <= 1000) |
		(agi >= 75000 & agi < 115000 &(filestat == 4 | filestat == 5) & fownu18 == 2 & hh5to18 <= 2 & fownu6 == 2 & actc_crd + ctc_crd <= 2000) |
		(agi >= 75000 & agi < 135000 &(filestat == 4 | filestat == 5) & fownu18 == 3 & hh5to18 <= 3 & fownu6 == 2 & actc_crd + ctc_crd <= 3000) |
		(agi >= 75000 & agi < 155000 &(filestat == 4 | filestat == 5) & fownu18 == 4 & hh5to18 <= 4 & fownu6 == 2 & actc_crd + ctc_crd <= 4000) |
		(agi >= 75000 & agi < 155000 &(filestat == 4 | filestat == 5) & fownu18 == 5 & hh5to18 <= 5 & fownu6 == 2 & actc_crd + ctc_crd <= 5000) |
		(agi >= 75000 & agi < 155000 &(filestat == 4 | filestat == 5) & fownu18 == 6 & hh5to18 <= 6 & fownu6 == 2 & actc_crd + ctc_crd <= 6000) |
		(agi >= 75000 & agi < 155000 &(filestat == 4 | filestat == 5) & fownu18 == 7 & hh5to18 <= 7 & fownu6 == 2 & actc_crd + ctc_crd <= 7000) |
		(agi >= 75000 & agi < 155000 &(filestat == 4 | filestat == 5) & fownu18 == 8 & hh5to18 <= 8 & fownu6 == 2 & actc_crd + ctc_crd <= 8000) |
		(agi >= 75000 & agi < 155000 &(filestat == 4 | filestat == 5) & fownu18 == 9 & hh5to18 <= 9 & fownu6 == 2 & actc_crd + ctc_crd <= 9000) |
		(agi >= 75000 & agi < 155000 &(filestat == 4 | filestat == 5) & fownu18 == 10 & hh5to18 <= 10 & fownu6 == 2 & actc_crd + ctc_crd <= 10000))


schild_reduceb_2u5 <-
	update(schild_reduceb_2u5,
		snew_credit_2u5 =  (1000 * pmin(fownu18, 2) + (2000 * pmin(fownu6, 2) - ((agi - 75000) * (.05)))))


schild_reduceb_3u5 <-subset( x , 
		(agi >= 75000 & agi < 95000 &(filestat == 4 | filestat == 5) & fownu18 == 3 & hh5to18 <= 1 & fownu6 == 3 & actc_crd + ctc_crd <= 1000) |
		(agi >= 75000 & agi < 115000 &(filestat == 4 | filestat == 5) & fownu18 == 3 & hh5to18 <= 2 & fownu6 == 3 & actc_crd + ctc_crd <= 2000) |
		(agi >= 75000 & agi < 135000 &(filestat == 4 | filestat == 5) & fownu18 == 3 & hh5to18 <= 3 & fownu6 == 3 & actc_crd + ctc_crd <= 3000) |
		(agi >= 75000 & agi < 155000 &(filestat == 4 | filestat == 5) & fownu18 == 4 & hh5to18 <= 4 & fownu6 == 3 & actc_crd + ctc_crd <= 4000) |
		(agi >= 75000 & agi < 155000 &(filestat == 4 | filestat == 5) & fownu18 == 5 & hh5to18 <= 5 & fownu6 == 3 & actc_crd + ctc_crd <= 5000) |
		(agi >= 75000 & agi < 155000 &(filestat == 4 | filestat == 5) & fownu18 == 6 & hh5to18 <= 6 & fownu6 == 3 & actc_crd + ctc_crd <= 6000) |
		(agi >= 75000 & agi < 155000 &(filestat == 4 | filestat == 5) & fownu18 == 7 & hh5to18 <= 7 & fownu6 == 3 & actc_crd + ctc_crd <= 7000) |
		(agi >= 75000 & agi < 155000 &(filestat == 4 | filestat == 5) & fownu18 == 8 & hh5to18 <= 8 & fownu6 == 3 & actc_crd + ctc_crd <= 8000) |
		(agi >= 75000 & agi < 155000 &(filestat == 4 | filestat == 5) & fownu18 == 9 & hh5to18 <= 9 & fownu6 == 3 & actc_crd + ctc_crd <= 9000) |
		(agi >= 75000 & agi < 155000 &(filestat == 4 | filestat == 5) & fownu18 == 10 & hh5to18 <= 10 & fownu6 == 3 & actc_crd + ctc_crd <= 10000))


schild_reduceb_3u5 <-
	update(schild_reduceb_3u5,
		snew_credit_3u5 =  (1000 * pmin(fownu18, 1) + (2000 * pmin(fownu6, 3) - ((agi - 75000) * (.05)))))



schild_reduceb_4u5 <-subset( x , 
		(agi >= 75000 & agi < 95000 &(filestat == 4 | filestat == 5) & fownu18 == 4 & hh5to18 <= 1 & fownu6 >= 4 & actc_crd + ctc_crd <= 1000) |
		(agi >= 75000 & agi < 115000 &(filestat == 4 | filestat == 5) & fownu18 == 4 & hh5to18 <= 2 & fownu6 >= 4 & actc_crd + ctc_crd <= 2000) |
		(agi >= 75000 & agi < 135000 &(filestat == 4 | filestat == 5) & fownu18 == 4 & hh5to18 == 3 & fownu6 >= 4 & actc_crd + ctc_crd <= 3000) |
		(agi >= 75000 & agi < 155000 &(filestat == 4 | filestat == 5) & fownu18 == 4 & hh5to18 == 4 & fownu6 >= 4 & actc_crd + ctc_crd <= 4000) |
		(agi >= 75000 & agi < 155000 &(filestat == 4 | filestat == 5) & fownu18 == 5 & hh5to18 == 5 & fownu6 >= 4 & actc_crd + ctc_crd <= 5000) |
		(agi >= 75000 & agi < 155000 &(filestat == 4 | filestat == 5) & fownu18 == 6 & hh5to18 == 6 & fownu6 >= 4 & actc_crd + ctc_crd <= 6000) |
		(agi >= 75000 & agi < 155000 &(filestat == 4 | filestat == 5) & fownu18 == 7 & hh5to18 == 7 & fownu6 >= 4 & actc_crd + ctc_crd <= 7000) |
		(agi >= 75000 & agi < 155000 &(filestat == 4 | filestat == 5) & fownu18 == 8 & hh5to18 == 8 & fownu6 >= 4 & actc_crd + ctc_crd <= 8000) |
		(agi >= 75000 & agi < 155000 &(filestat == 4 | filestat == 5) & fownu18 == 9 & hh5to18 == 9 & fownu6 >= 4 & actc_crd + ctc_crd <= 9000) |
		(agi >= 75000 & agi < 155000 &(filestat == 4 | filestat == 5) & fownu18 == 10 & hh5to18 == 10 & fownu6 >= 4 & actc_crd + ctc_crd <= 10000))


schild_reduceb_4u5 <-
	update(schild_reduceb_4u5,
		snew_credit_4u5 =  (1000 * pmin(fownu18, 0) + (2000 * pmin(fownu6, 4) - ((agi - 75000) * (.05)))))

#################

svytotal(
	~mnew_credit_0u5 ,
	mchild_reduceb_0u5)

svytotal(
	~mnew_credit_1u5,
	mchild_reduceb_1u5)


svytotal(
	~mnew_credit_2u5,
	mchild_reduceb_2u5)


svytotal(
	~mnew_credit_3u5,
	mchild_reduceb_3u5)


svytotal(
	~mnew_credit_4u5,
	mchild_reduceb_4u5)


svytotal(
	~snew_credit_4u5 ,
	schild_reduceb_4u5)


svytotal(
	~snew_credit_3u5 ,
	schild_reduceb_3u5)


svytotal(
	~snew_credit_2u5 ,
	schild_reduceb_2u5)


svytotal(
	~snew_credit_1u5 ,
	schild_reduceb_1u5)


svytotal(
	~snew_credit_0u5 ,
	schild_reduceb_0u5)



#####

		
								num_fullb_1o5<-
	print(svyby(
		~one ,
		~hunder18 == 1 & hh5to18 == 1 & fownu6 == 0,
		design = children_fullb ,
		svytotal
	))
13324080

						num_fullb_2o5<-
	svyby(
		~one ,
		~hunder18 == 2 & hh5to18 == 2 & fownu6 == 0,
		design = children_fullb ,
		svytotal
	)
						num_fullb_2o5
						10788590
						
num_fullb_3o5<- print(svyby(
		~one ,
		~hunder18 == 3 & hh5to18 == 3 & fownu6 == 0,
		design = children_fullb ,
		svytotal
	))
			3390632
		
		num_fullb_4o5<- print(svyby(
		~one ,
		~hunder18 >= 4 & hh5to18 >= 4 & fownu6 == 0,
		design = children_fullb ,
		svytotal
	))
		1279477
		
				num_fullb_add_for6_1<- print(svyby(
		~one ,
		~hunder18 == 1 & hh5to18 <= 1 & fownu6 == 1,
		design = children_fullb ,
		svytotal
	))
		
				num_fullb_add_for6_2<- print(svyby(
		~one ,
		~hunder18 <= 2 & hh5to18 == 2 & fownu6 == 2,
		design = children_fullb ,
		svytotal
	))
		
								num_fullb_add_for6_3<- print(svyby(
		~one ,
		~hunder18 <= 3 & hh5to18 <= 3 & fownu6 == 3,
		design = children_fullb ,
		svytotal
	))
		
		

				num_fullb_1u5<- print(svyby(
		~one ,
		~hunder18 == 1 & hh5to18 == 0 & fownu6 == 1,
		design = children_fullb ,
		svytotal
	))
6509068

				num_fullb_2u5<- print(svyby(
		~one ,
		~hunder18 == 2 & hh5to18 == 0 & fownu6 == 2,
		design = children_fullb ,
		svytotal
	))

								num_fullb_3u5<- print(svyby(
		~one ,
		~hunder18 == 3 & hh5to18 == 0 & fownu6 == 3,
		design = children_fullb ,
		svytotal
	))
273447

								num_fullb_4u5<- print(svyby(
		~one ,
		~hunder18 >= 4 & hh5to18 == 0 & fownu6 >= 4,
		design = children_fullb ,
		svytotal
	))
17515.59

								num_fullb_1u_1o<- print(svyby(
		~one ,
		~hunder18 == 2 & hh5to18 == 1 & fownu6 == 1,
		design = children_fullb ,
		svytotal
	))
3477236

								num_fullb_1u_2o<- print(svyby(
		~one ,
		~hunder18 == 3 & hh5to18 == 2 & fownu6 == 1,
		design = children_fullb ,
		svytotal
	))
 1964081
																num_fullb_1u_3o<- print(svyby(
		~one ,
		~hunder18 >= 4 & hh5to18 >= 3 & fownu6 == 1,
		design = children_fullb ,
		svytotal
	))
1086378
																num_fullb_2u_1o<- print(svyby(
		~one ,
		~hunder18 == 3 & hh5to18 == 1 & fownu6 == 2,
		design = children_fullb ,
		svytotal
	))
751167.4

																num_fullb_3u_1o<- print(svyby(
		~one ,
		~hunder18 >= 4 & hh5to18 >= 1 & fownu6 == 3,
		design = children_fullb ,
		svytotal
	))
241027.9

																num_fullb_2u_2o<- print(svyby(
		~one ,
		~hunder18 >= 4 & hh5to18 >= 2 & fownu6 == 2,
		design = children_fullb ,
		svytotal
	))
																
#Cost_UT	= (num_fullb_1o5 * 1000) + (num_fullb_2o5 * 2000) + (num_fullb_3o5 * 3000) + (num_fullb_4o5 * 4000)
#+ (num_fullb_1u5 * 2000) + (num_fullb_1u5 * 4000) + (num_fullb_3u5 * 6000) + (num_fullb_4u5 * 8000)
#+ (num_fullb_2u_2o * 6000) (num_fullb_1u_1o * 3000) + (num_fullb_2u_1o * 5000) + (num_fullb_1u_2o * 4000)
#+ (num_fullb_3u_1o * 7000) + (num_fullb_1u_3o * 7000)
																
																
x <- 13324080 * 1000 + 10788590 * 2000 + 3390632 * 3000 + 1279477 * 4000 + 6509068 * 2000 + 6509068 * 4000 + 273447 * 6000 + 17515.59 * 8000 + 852896.4 * 6000 + 3477236 * 3000 + 751167.4 * 5000 + 1964081 * 4000 + 404075.7 * 7000 + 1086378 * 7000
x

		
		
		
		
		
		
		
		
		
		
		
		
	