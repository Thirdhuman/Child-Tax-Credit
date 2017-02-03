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

y <-
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


y$mse <- TRUE


#Child - UBI Cost

#so for our $2000 proposal, fully refundable for kids 4 and under
#if we phase it out in the same way Clinton has proposed what does that cost

#FTOTVAL
ftotval

#Child tax credit 
#CTC_CRD 
ctc_rd

#Child tax credit additional 
#ACTC_CRD 
actc_crd

children_fullb <-subset( 
		y , 
		())

y<-update(y,	cost = 0)
											
											sc1(ftotval))

svytotal(
	~ cost,
	y)


t <-function(x){length(which(y$fownu18 > 0))} 

#s <-function(x){length(which(y$agi <= 75000 & y$filestat == 4 | y$filestat == 5))} 
#m <-function(x){length(which(y$agi <= 110000 & y$filestat <= 3))}

#s2 <-function(x){length(which(y$agi < 75000))} 
#m2 <-function(x){length(which(y$agi < 110000))} 

s2 <-function(x){length(which(y$ftotval < 75000))} 
m2 <-function(x){length(which(y$ftotval < 110000))} 

s <-function(x){length(which(y$filestat == 4 | y$filestat ==  5))}
m <-function(x){length(which(y$filestat <= 3))}
	
s2 <-function(x){length(which(y$agi < 75000))} 
m2 <-function(x){length(which(y$agi < 110000))} 

s <-function(x){length(which(y$filestat == 4 | y$filestat ==  5))}
m <-function(x){length(which(y$filestat <= 3))}

#if(cond) expr

y<-update(y,	cost = 0)

y <- update(y, cost =	cost + length(y$dep_stat)
)

y <- update( y , totcost =  actc_crd + ctc_crd) 

 y$filestat == 4 | y$filestat ==  5

svytotal(
	~ a_age,
	y)

marital.status.by.employment <-
	svyby(
		~age ,
		design = y ,
		svytotal
	)

marital.status.by.employment

marital.status.by.employment2 <-
	svyby(
		~agi ,
		~h_seq ,
		design = y ,
		svytotal
	)
marital.status.by.employment2

# now you have the results saved into a new object of type "svyby"
class( marital.status.by.employment )

# this object can be coerced (converted) to a data frame..
marital.status.by.employment <- data.frame( marital.status.by.employment )

length(y$dep_stat)

PERIDNUM
peridnum

		children_fullb <-subset( y , 
		(agi <= 110000 & filestat <= 3 & fownu18 == 1 & actc_crd + ctc_crd <= 1000) |
		(agi <= 110000 & filestat <= 3 & fownu18 == 2 & actc_crd + ctc_crd <= 2000) |
		(agi <= 110000 & filestat <= 3 & fownu18 == 3 & actc_crd + ctc_crd <= 3000) |
		(agi <= 110000 & filestat <= 3 & fownu18 == 4 & actc_crd + ctc_crd <= 4000) |
		(agi <= 110000 & filestat <= 3 & fownu18 == 5 & actc_crd + ctc_crd <= 5000) |
		(agi <= 110000 & filestat <= 3 & fownu18 == 6 & actc_crd + ctc_crd <= 6000) |
		(agi <= 110000 & filestat <= 3 & fownu18 == 7 & actc_crd + ctc_crd <= 7000) |
		(agi <= 110000 & filestat <= 3 & fownu18 == 8 & actc_crd + ctc_crd <= 8000) |
		(agi <= 110000 & filestat <= 3 & fownu18 == 9 & actc_crd + ctc_crd <= 9000) |
		(agi <= 110000 & filestat <= 3 & fownu18 == 10 & actc_crd + ctc_crd <= 10000) |
		(agi <= 75000 & (filestat == 4 | filestat == 5) & fownu18 == 1 & actc_crd + ctc_crd <= 1000) |
		(agi <= 75000 & (filestat == 4 | filestat == 5) & fownu18 == 2 & actc_crd + ctc_crd <= 2000) |
		(agi <= 75000 & (filestat == 4 | filestat == 5) & fownu18 == 3 & actc_crd + ctc_crd <= 3000) |
		(agi <= 75000 & (filestat == 4 | filestat == 5) & fownu18 == 4 & actc_crd + ctc_crd <= 4000) |
		(agi <= 75000 & (filestat == 4 | filestat == 5) & fownu18 == 5 & actc_crd + ctc_crd <= 5000) |
		(agi <= 75000 & (filestat == 4 | filestat == 5) & fownu18 == 6 & actc_crd + ctc_crd <= 6000) |
		(agi <= 75000 & (filestat == 4 | filestat == 5) & fownu18 == 7 & actc_crd + ctc_crd <= 7000) |
		(agi <= 75000 & (filestat == 4 | filestat == 5) & fownu18 == 8 & actc_crd + ctc_crd <= 8000) |
		(agi <= 75000 & (filestat == 4 | filestat == 5) & fownu18 == 9 & actc_crd + ctc_crd <= 9000) |
		(agi <= 75000 & (filestat == 4 | filestat == 5) & fownu18 == 10 & actc_crd + ctc_crd <= 10000) 
		& filestat != 0)
		
		
		children_fullb <- update( children_fullb , totcost =  actc_crd + ctc_crd) 

		svytotal(
	~totcost ,
	children_fullb)

		totcost
		
		
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
	
	
children_fullb_younger <-subset( y , 
		(agi <= 110000 & filestat <= 3 & fownu18 == 1 & hh5to18 == 1 & actc_crd + ctc_crd <= 1000) |
		(agi <= 110000 & filestat <= 3 & fownu18 == 2 & actc_crd + ctc_crd <= 2000) |
		(agi <= 110000 & filestat <= 3 & fownu18 == 3 & actc_crd + ctc_crd <= 3000) |
		(agi <= 110000 & filestat <= 3 & fownu18 == 4 & actc_crd + ctc_crd <= 4000) |
		(agi <= 110000 & filestat <= 3 & fownu18 == 5 & actc_crd + ctc_crd <= 5000) |
		(agi <= 110000 & filestat <= 3 & fownu18 == 6 & actc_crd + ctc_crd <= 6000) |
		(agi <= 110000 & filestat <= 3 & fownu18 == 7 & actc_crd + ctc_crd <= 7000) |
		(agi <= 110000 & filestat <= 3 & fownu18 == 8 & actc_crd + ctc_crd <= 8000) |
		(agi <= 110000 & filestat <= 3 & fownu18 == 9 & actc_crd + ctc_crd <= 9000) |
		(agi <= 110000 & filestat <= 3 & fownu18 == 10 & actc_crd + ctc_crd <= 10000) |
		(agi <= 75000 & (filestat == 4 | filestat == 5) & fownu18 == 1  & actc_crd + ctc_crd <= 1000) |
		(agi <= 75000 & (filestat == 4 | filestat == 5) & fownu18 == 2  & actc_crd + ctc_crd <= 2000) |
		(agi <= 75000 & (filestat == 4 | filestat == 5) & fownu18 == 3  & actc_crd + ctc_crd <= 3000) |
		(agi <= 75000 & (filestat == 4 | filestat == 5) & fownu18 == 4  & actc_crd + ctc_crd <= 4000) |
		(agi <= 75000 & (filestat == 4 | filestat == 5) & fownu18 == 5  & actc_crd + ctc_crd <= 5000) |
		(agi <= 75000 & (filestat == 4 | filestat == 5) & fownu18 == 6  & actc_crd + ctc_crd <= 6000) |
		(agi <= 75000 & (filestat == 4 | filestat == 5) & fownu18 == 7  & actc_crd + ctc_crd <= 7000) |
		(agi <= 75000 & (filestat == 4 | filestat == 5) & fownu18 == 8  & actc_crd + ctc_crd <= 8000) |
		(agi <= 75000 & (filestat == 4 | filestat == 5) & fownu18 == 9  & actc_crd + ctc_crd <= 9000) |
		(agi <= 75000 & (filestat == 4 | filestat == 5) & fownu18 == 10  & actc_crd + ctc_crd <= 10000) 
			& filestat != 0)

children_fullb <-
	transform(children_fullb, under_5 = tapply(as.factor() )
								)

tot_num_children_f<-
	svyby(
		~filestat,
		~a_age ,
		design = children_fullb ,
		svytotal
	)

tot_num_children_f

tot_num_children_h<-
	svyby(
		~one ,
		~hunder18,
		design = children_fullb ,
		svytotal
	)

tot_num_children_u6<-
	svyby(
		 ~one,
			~hh5to18,
		design = children_fullb ,
		svytotal
	)

#hunder18

tot_num_children_u6

tot_num_children_h

tot_num_children_f


hh5to18

tot_num_children<-
	svyby(
		~one ,
		~a_age < 5 ,
		design = children_fullb ,
		svytotal
	)

tot_num_children

t <-function(x){length(children_fullb$a_age < 5)} 


tot_num_children

children_fullb <-
	transform(children_fullb, under_5 = t(unique(h_seq))
								)

children_fullb <-
	transform(children_fullb, under_5 = factor(under_5)
								)

tot_num_children_u5<-
	svyby(
		~one ,
		~under_5 ,
		design = children_fullb ,
		svytotal
	)

children_fullb <-
	update(children_fullb,
		under_5 = t(unique(ph_seq)))

children_fullb <-
		update(y, under_5 = factor(under_5)
								)

tot_num_children_u5<-
	svyby(
		~one ,
		~under_5 ,
		design = children_fullb ,
		svytotal
	)

tot_num_children_u5

actc<-
	svytotal(
		~ actc_crd,
		design = children_fullb ,
	)

ctc<-
	svytotal(
		~ ctc_crd,
		design = children_fullb ,
	)

ctc
actc

child_reduceb1 <-subset( y , 
		(agi >= 110000 & agi < 130000 & filestat <= 3 & fownu18 == 1 & actc_crd + ctc_crd <= 1000) |
		
		(agi >= 110000 & agi < 150000 & filestat <= 3 & fownu18 == 2 & actc_crd + ctc_crd <= 2000) |
		(agi >= 110000 & agi < 170000 & filestat <= 3 & fownu18 == 3 & actc_crd + ctc_crd <= 3000) |
		(agi >= 110000 & agi < 190000 & filestat <= 3 & fownu18 == 4 & actc_crd + ctc_crd <= 4000) |
		(agi >= 110000 & agi < 210000 & filestat <= 3 & fownu18 == 5 & actc_crd + ctc_crd <= 5000) |
		(agi >= 110000 & agi < 230000 & filestat <= 3 & fownu18 == 6 & actc_crd + ctc_crd <= 6000) |
		(agi >= 110000 & agi < 250000 & filestat <= 3 & fownu18 == 7 & actc_crd + ctc_crd <= 7000) |
		(agi >= 110000 & agi < 270000 & filestat <= 3 & fownu18 == 8 & actc_crd + ctc_crd <= 8000) |
		(agi >= 110000 & agi < 290000 & filestat <= 3 & fownu18 == 9 & actc_crd + ctc_crd <= 9000) |
		(agi >= 110000 & agi < 290000 & filestat <= 3 & fownu18 == 10 & actc_crd + ctc_crd <= 10000) 	& filestat != 0)

child_reduceb2 <- subset( y,
		(agi >= 75000 & agi < 95000 &(filestat == 4 | filestat == 5) & fownu18 == 1 & actc_crd + ctc_crd <= 1000) |
		(agi >= 75000 & agi < 115000 &(filestat == 4 | filestat == 5) & fownu18 == 2 & actc_crd + ctc_crd <= 2000) |
		(agi >= 75000 & agi < 135000 &(filestat == 4 | filestat == 5) & fownu18 == 3 & actc_crd + ctc_crd <= 3000) |
		(agi >= 75000 & agi < 155000 &(filestat == 4 | filestat == 5) & fownu18 == 4 & actc_crd + ctc_crd <= 4000) |
		(agi >= 75000 & agi < 175000 &(filestat == 4 | filestat == 5) & fownu18 == 5 & actc_crd + ctc_crd <= 5000) |
		(agi >= 75000 & agi < 195000 &(filestat == 4 | filestat == 5) & fownu18 == 6 & actc_crd + ctc_crd <= 6000) |
		(agi >= 75000 & agi < 215000 &(filestat == 4 | filestat == 5) & fownu18 == 7 & actc_crd + ctc_crd <= 7000) |
		(agi >= 75000 & agi < 235000 &(filestat == 4 | filestat == 5) & fownu18 == 8 & actc_crd + ctc_crd <= 8000) |
		(agi >= 75000 & agi < 255000 &(filestat == 4 | filestat == 5) & fownu18 == 9 & actc_crd + ctc_crd <= 9000) |
		(agi >= 75000 & agi < 255000 &(filestat == 4 | filestat == 5) & fownu18 == 10 & actc_crd + ctc_crd <= 10000) 
			& filestat != 0)

child_reduceb1 <-
	update(child_reduceb1,
		Mnew_credit =  2000 - ((agi - 110000) * (.05) * ))


		
svytotal(
	~Mnew_credit ,
	child_reduceb)

child_reduceb2 <-
	update(child_reduceb2,
		Snew_credit =  (2000 * fownu18) -  * .05))

svytotal(
	~Snew_credit ,
	child_reduceb)

		
		
		fownu18)


MultiplyUntil(10000, .45, 1000)


svytotal(
	~new_credit ,
	child_reduceb)


			exp(seq(log(agi)), log(130000), by=log(1.05)))
	
y$new_credit



		(agi <= 110000 & filestat <= 3 & fownu18 == 1 & actc_crd + ctc_crd <= 10000) |
		(agi <= 75000 & (filestat == 4 | filestat == 5) & fownu18 == 1 & actc_crd + ctc_crd <= 1000) |


y <-
	update(		y,
		new_credit = 0
	)

y <-
	update( y, new_credit <- 0 + ifelse(filestat <= 3 & fownu18 == 1, (mc1(agi) - agi),
		   0))



#if(client=='private'){
 #   tot.price <- net.price * 1.12
#} else if(client=='public'){
#    tot.price <- net.price * 1.06
#} else {
#    tot.price <- net.price
#}

new_credit_tot<-
	svyby(
		~new_credit ,
		design = y ,
		svytotal
	)

f = function(x) if (x < 1000) exp(seq(log(x), log(1000), by=log(1.15))) else 1000

k <-function(x){with(unique(y$h_seq))} 

new_credit = sum(
																						ifelse(y$filestat <= 3 & y$fownu18 == 1, mc1(unique(y$agi)) - (unique(y$agi)), 0 +
																						ifelse(y$filestat <= 3 & y$fownu18 == 2, mc2(y$agi) - y$agi, 0 +
																						ifelse(y$filestat <= 3 & y$fownu18 == 3, mc3(y$agi) - y$agi, 0 +
																						ifelse(y$filestat <= 3 & y$fownu18 == 4, mc4(y$agi) - y$agi, 0 +
																						ifelse(y$filestat <= 3 & y$fownu18 == 5, mc5(y$agi) - y$agi, 0 +
																						ifelse(y$filestat <= 3 & y$fownu18 == 6, mc6(y$agi) - y$agi, 0 +
																						ifelse(y$filestat <= 3 & y$fownu18 == 7, mc7(y$agi) - y$agi, 0 +
																						ifelse(y$filestat <= 3 & y$fownu18 == 8, mc8(y$agi) - y$agi, 0 +
																						ifelse(y$filestat <= 3 & y$fownu18 == 9, mc9(y$agi) - y$agi, 0 +
																						ifelse(y$filestat == 4 | y$filestat == 5) & y$fownu18 == 1, sc1(y$agi) - y$agi, 0 +
																						ifelse(y$filestat == 4 | y$filestat == 5) & y$fownu18 == 2, sc2(y$agi) - y$agi, 0 +
																						ifelse(y$filestat == 4 | y$filestat == 5) & y$fownu18 == 3, sc3(y$agi) - y$agi, 0 +
																						ifelse(y$filestat == 4 | y$filestat == 5) & y$fownu18 == 4, sc4(y$agi) - y$agi, 0 +
																						ifelse(y$filestat == 4 | y$filestat == 5) & y$fownu18 == 5, sc5(y$agi) - y$agi, 0 +
																						ifelse(y$filestat == 4 | y$filestat == 5) & y$fownu18 == 6, sc6(y$agi) - y$agi, 0 +
																						ifelse(y$filestat == 4 | y$filestat == 5) & y$fownu18 == 7, sc7(y$agi) - y$agi, 0 +
																						ifelse(y$filestat == 4 | y$filestat == 5) & y$fownu18 == 8, sc8(y$agi) - y$agi, 0 +
																						ifelse(y$filestat == 4 | y$filestat == 5) & y$fownu18 == 9, sc9(y$agi) - y$agi, 0 
																						))))))))))

new_credit																											

																							(mc1(y$agi(which(unique(y$h_seq))))))

new_credit
	
	2000*y$fownu18 - (mc1(y$agi))

	
	
sc1 = function(x) if (x > 75000 & x < 95000) exp(seq(log(x), log(95000), by=log(1.05))) else 0
sc2 = function(x) if (x > 75000 & x < 115000) exp(seq(log(x), log(115000), by=log(1.05))) else 0
sc3 = function(x) if (x > 75000 & x < 135000) exp(seq(log(x), log(135000), by=log(1.05))) else 0
sc4 = function(x) if (x > 75000 & x < 155000) exp(seq(log(x), log(155000), by=log(1.05))) else 0
sc5 = function(x) if (x > 75000 & x < 175000) exp(seq(log(x), log(175000), by=log(1.05))) else 0
sc6 = function(x) if (x > 75000 & x < 195000) exp(seq(log(x), log(195000), by=log(1.05))) else 0
sc7 = function(x) if (x > 75000 & x < 215000) exp(seq(log(x), log(215000), by=log(1.05))) else 0
sc8 = function(x) if (x > 75000 & x < 235000) exp(seq(log(x), log(235000), by=log(1.05))) else 0
sc9 = function(x) if (x > 75000 & x < 255000) exp(seq(log(x), log(255000), by=log(1.05))) else 0

mc1 = function(x) if (x >= 110000 & x < 130000) exp(seq(log(x), log(130000), by=log(1.05))) else 0
mc2 = function(x) if (x > 110000 &  x < 150000) exp(seq(log(x), log(150000), by=log(1.05))) else 0
mc3 = function(x) if (x > 110000 &  x < 170000) exp(seq(log(x), log(170000), by=log(1.05))) else 0
mc4 = function(x) if (x > 110000 &  x < 190000) exp(seq(log(x), log(190000), by=log(1.05))) else 0
mc5 = function(x) if (x > 110000 &  x < 210000) exp(seq(log(x), log(210000), by=log(1.05))) else 0
mc6 = function(x) if (x > 110000 &  x < 230000) exp(seq(log(x), log(230000), by=log(1.05))) else 0
mc7 = function(x) if (x > 110000 &  x < 250000) exp(seq(log(x), log(250000), by=log(1.05))) else 0
mc8 = function(x) if (x > 110000 &  x < 270000) exp(seq(log(x), log(270000), by=log(1.05))) else 0
mc9 = function(x) if (x > 110000 &  x < 290000) exp(seq(log(x), log(290000), by=log(1.05))) else 0


t <-function(x){length(which(y$fownu18 > 0))} 



svytotal(
	~filestat ,
	y)


ftotval


phaseout <-subset( y , 
		(
		(agi > 110000 & a_maritl <= 3) |
	 (agi > 75000 & a_maritl > 3))) 

svytotal(
	~ctc_crd ,
	phaseout)

svytotal(
	~actc_crd ,
	phaseout)

fownu18

f = function(x) if (x < 1000) exp(seq(log(x), log(1000), by=log(1.15))) else 1000


children_130000 <-subset( 
		y , 
		(a_age < 18 & ftotval < 110000))

svytotal(
	~ctc_crd ,
	phaseout)


total

ftotval - (actc_crd + ctc_rd)

actc_crd + ctc_rd

total_ctc <- svyby(
	~ctc_crd,
	design = y,
	svytotal
)

total_actc <- svyby(
	~actc_crd,
	design = y,
	svytotal
)


svytotal(
	~one ,
	actc_crd)

svytotal(
	~one ,
	ctc_crd)

svytotal(
	~one ,
	children)

svytotal(
	~actc_crd ,
	y)



svytotal(
	~one ,
	y
)





* length(which(y$ftotval < 110000 & y$filestat <= 3)))

s <-function(x){length(which(y$filestat == 4 | y$filestat ==  5))}
m <-function(x){length(which(y$filestat <= 3))}



# & y$filestat == 4 | 5)
# & y$filestat <= 3)

#peridnum
#fheadidx

children_fullb <-subset( y , 
  (s(unique(ph_seq) >= 0) & (s2(unique(ph_seq) >= 0) |
		(m(unique(ph_seq) >= 0) & (m2(unique(ph_seq) >= 0 )))))) 

children_fullb <-subset( y , 
		(if  which(unique(fheadidx) (ftotval <= 110000 & filestat <= 3) |
		(ftotval <= 75000 & (filestat == 4 | filestat == 5))
		& a_age < 18))


svytotal(
	~ fownu18,
	children_fullb)

svytotal(
	~ (dep_stat),
	children_fullb)

svytotal(
	~ (one),
	children_fullb)


FFPOS
ffpos

h_idnum1
h_idnum2

HRHHID
hrhhid


t(unique(h_seq)	< 


		h_seq 	agi < 110000 & filestat <= 3) |
		(agi < 75000 & filestat == 4 | filestat == 5))
		& fownu18 > 0)
		
		& t(unique(fh_seq)) > 0)