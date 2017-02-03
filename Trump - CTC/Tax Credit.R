### Trump Stuff

setwd("~/Desktop/Poverty:Welfare/Data/R - Survey data/Current Population Survey (CPS)")

library(survey)				# load survey package (analyzes complex design surveys)
library(MonetDBLite)
library(DBI)			# load the DBI package (implements the R-database coding)

dbfolder <- paste0( getwd() , "/MonetDB" )

options( survey.replicates.mse = TRUE )

#hwtsupp

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
#FOWNU6 -Own children in family under 6
#FRELU18  -Related persons in family under 18
#tax_inc = taxable amount
# 71% of children under 17 are under 13
#care_val = annual ammount

#Alabama	AL	01	
#Alaska	AK	02	
#Arizona	AZ	04	
#Arkansas	AR	05	
#Baker Island		81	
#California	CA	06	
#Colorado	CO	08
#Connecticut	CT	09
#Delaware	DE	10
#District of Columbia	DC	11
#Florida	FL	12
#Georgia	GA	13
#Hawaii	HI	15
#Idaho	ID	16
#Illinois	IL	17
#Indiana	IN	18
#Iowa	IA	19
#Kansas	KS	20
#Kentucky	KY	21
#Louisiana	LA	22
#Maine	ME	23
#Maryland	MD	24
#Massachusetts	MA	25
#Michigan	MI	26
#Minnesota	MN	27
#Mississippi	MS	28
#Missouri	MO	29
#Montana	MT	30
#Nebraska	NE	31
#Nevada	NV	32
#New Hampshire	NH	33
#New Jersey	NJ	34
#New Mexico	NM	35
#New York	NY	36
#North Carolina	NC	37
#North Dakota	ND	38
#Ohio	OH	39
#Oklahoma	OK	40
#Oregon	OR	41
#Pennsylvania	PA	42
#Rhode Island	RI	44
#South Carolina	SC	45
#South Dakota	SD	46
#Tennessee	TN	47
#Texas	TX	48
#Utah	UT	49
#Vermont	VT	50
#Virginia	VA	51
#Washington	WA	53
#West Virginia	WV	54
#Wisconsin	WI	55
#Wyoming	WY	56

library("xlsx")

hrpaidcc

svyby( ~care_val, by = ~ gestfips, design =  core, 	FUN = svyquantile, c(.25, .5, .75, 1))
svyby( ~one, by = ~ gestfips, design =  y, 	FUN = svytotal)
svyby( ~hrpaidcc, by = ~ gestfips, design =  core, 	FUN = svytotal)
svyby( ~one, by = ~ gestfips, design =  core, 	FUN = svytotal)

svytotal(~ one, design =  core)

ctcc <- read.xlsx("~/Desktop/Poverty:Welfare/Childcare_Costs.xlsx", 1)

core <-subset( y , 
	((hwsval <= 500000 & filestat <= 3 & hunder15 == 1) |
		(hwsval <= 500000 & filestat <= 3 & hunder15 == 2) |
		(hwsval <= 500000 & filestat <= 3 & hunder15 == 3) |
		(hwsval <= 500000 & filestat <= 3 & hunder15 >= 4) |
		(hwsval <= 250000 & (filestat == 4 | filestat == 5) & hunder15 == 1) |
		(hwsval <= 250000 & (filestat == 4 | filestat == 5) & hunder15 == 2) |
		(hwsval <= 250000 & (filestat == 4 | filestat == 5) & hunder15 == 3) |
		(hwsval <= 250000 & (filestat == 4 | filestat == 5) & hunder15 >= 4)) & (a_age > 17  & dep_stat == 0 & fownu18 > 0 &
	fedtax_ac > 0))

	core1 <-	update(core,
low_est = (pmax(fedtax_ac - (pmin (care_val * pmin(hunder15, 4) , 5644 * 4)), 0)))
		 		
		svyby( ~low_est, by = ~ duplicated(h_seq) == FALSE, design =  core1, 	FUN = svytotal)


#####

#####

	AL <-subset( core , gestfips ==01)
			AL <-	update(AL,
		low_est = (pmax(fedtax_ac - (pmin (pmin(care_val, 5644) * pmin(hunder15, 4) , 5644 * 4)), 0)))
		 			svyby( ~low_est, by = ~ duplicated(h_seq) == FALSE, design =  AL, 	FUN = svytotal)
			AL <-	update(AL,
			low_est2 = pmax( fedtax_ac - low_est, 0) )
			svyby( ~low_est2, by = ~ duplicated(h_seq) == FALSE, design =  AL, 	FUN = svytotal)
		
	AK<-subset( core , gestfips ==02)
				AK <-	update(AK,
		low_est = (pmax(fedtax_ac - (pmin (pmin(care_val, 11700) * pmin(hunder15, 4) , 11700 * 4)), 0)))
						AK <-	update(AK,	low_est2 = pmax( fedtax_ac - low_est, 0) )
			svyby( ~low_est2 , by = ~ duplicated(h_seq) == FALSE, design =  AK, 	FUN = svytotal)

	AZ<-subset( core , gestfips ==04)
				AZ <-	update(AZ,
		low_est = (pmax(fedtax_ac - (pmin (pmin(care_val, 9993) * pmin(hunder15, 4) , 9993*4)), 0)))
						AZ <-	update(AZ, low_est2 = pmax( fedtax_ac - low_est, 0) )
						svyby( ~low_est2 , by = ~ duplicated(h_seq) == FALSE, design =  AZ, 	FUN = svytotal)

	AR<-subset( core , gestfips ==05)
				AR <-	update(AR,
		low_est = (pmax(fedtax_ac - (pmin (pmin(care_val, 6074) * pmin(hunder15, 4) , 6074*4)), 0)))
						AR <-	update(AR,low_est2 = pmax( fedtax_ac - low_est, 0) )
						svyby( ~low_est2 , by = ~ duplicated(h_seq) == FALSE, design =  AR, 	FUN = svytotal)

CA<-subset( core , gestfips ==06)
				CA <-	update(CA,
		low_est = (pmax(fedtax_ac - (pmin( pmin(care_val, 13343) * pmin(hunder15, 4) , 13343*4)), 0)))
						CA <-	update(CA,	low_est2 = pmax( fedtax_ac - low_est, 0) )
						svyby( ~low_est2 , by = ~ duplicated(h_seq) == FALSE, design =  CA, 	FUN = svytotal)

	CO<-subset( core , gestfips ==08)
				CO <-	update(CO,
		low_est = (pmax(fedtax_ac - (pmin( pmin(care_val, 14950) * pmin(hunder15, 4) , 14950 * 4)), 0)))
						CO <-	update(CO,	low_est2 = pmax( fedtax_ac - low_est, 0) )
			svyby( ~low_est2 , by = ~ duplicated(h_seq) == FALSE, design =  CO, 	FUN = svytotal)

	CT<-subset( core , gestfips ==09)
				CT <-	update(CT,
		low_est = (pmax(fedtax_ac - (pmin( pmin(care_val, 14079) * pmin(hunder15, 4) , 14079 * 4)), 0)))
									CT <-	update(CT,	low_est2 = pmax( fedtax_ac - low_est, 0) )
			svyby( ~low_est2 , by = ~ duplicated(h_seq) == FALSE, design =  CT, 	FUN = svytotal)

	DE<-subset( core , gestfips ==10)
				DE <-	update(DE,
		low_est = (pmax(fedtax_ac - (pmin( pmin(care_val, 10396) * pmin(hunder15, 4) , 10396 * 4)), 0)))
									DE <-	update(DE,	low_est2 = pmax( fedtax_ac - low_est, 0) )
			svyby( ~low_est2 , by = ~ duplicated(h_seq) == FALSE, design =  DE, 	FUN = svytotal)

	DC<-subset( core , gestfips ==11)			
	DC <-	update(DC,
		low_est = (pmax(fedtax_ac - (pmin( pmin(care_val, 22658) * pmin(hunder15, 4) , 22658 * 4)), 0)))
												DC <-	update( DC ,	low_est2 = pmax( fedtax_ac - low_est, 0) )
						svyby( ~low_est2 , by = ~ duplicated(h_seq) == FALSE, design =  DC, 	FUN = svytotal)

	FL<-subset( core , gestfips ==12)
				FL <-	update(FL ,
		low_est = (pmax(fedtax_ac - (pmin( pmin(care_val, 8719) * pmin(hunder15, 4) , 8719 * 4)), 0)))
				FL <-	update( FL ,low_est2 = pmax( fedtax_ac - low_est, 0) )
						svyby( ~low_est2 , by = ~ duplicated(h_seq) == FALSE, design =  FL, 	FUN = svytotal)

	GA<-subset( core , gestfips ==13)
				GA <-	update( GA ,
		low_est = (pmax(fedtax_ac - (pmin( pmin(care_val, 7597) * pmin(hunder15, 4) , 7597 * 4)), 0)))
					GA <-	update( GA ,	low_est2 = pmax( fedtax_ac - low_est, 0) )
						svyby( ~low_est2 , by = ~ duplicated(h_seq) == FALSE, design =  GA, 	FUN = svytotal)

	HI<-subset( core , gestfips ==15)
				HI <-	update( HI ,
		low_est = (pmax(fedtax_ac - (pmin( pmin(care_val, 13584) * pmin(hunder15, 4) , 13584 * 4)), 0)))
				HI <-	update( HI ,
			low_est2 = pmax( fedtax_ac - low_est, 0) )
			svyby( ~low_est2 , by = ~ duplicated(h_seq) == FALSE, design =  HI, 	FUN = svytotal)

	ID<-subset( core , gestfips ==16)
				ID <-	update( ID ,
		low_est = (pmax(fedtax_ac - (pmin( pmin(care_val, 7385) * pmin(hunder15, 4) , 7385 * 4)), 0)))
				ID <-	update( ID ,
			low_est2 = pmax( fedtax_ac - low_est, 0) )
			svyby( ~low_est2 , by = ~ duplicated(h_seq) == FALSE, design =  ID, 	FUN = svytotal)

	IL<-subset( core , gestfips ==17)
				IL <-	update( IL ,
		low_est = (pmax(fedtax_ac - (pmin( pmin(care_val, 13176) * pmin(hunder15, 4) , 13176 * 4)), 0)))
				IL <-	update( IL ,low_est2 = pmax( fedtax_ac - low_est, 0) )
			svyby( ~low_est2 , by = ~ duplicated(h_seq) == FALSE, design =  IL, 	FUN = svytotal)

	IN<-subset( core , gestfips ==18)
				estlow_AL <-	update( AL ,
		low_est = (pmax(fedtax_ac - (pmin( pmin(care_val, 8929) * pmin(hunder15, 4) , 8929 * 4)), 0)))
				IN <-	update( IN ,
			low_est2 = pmax( fedtax_ac - low_est, 0) )
			svyby( ~low_est2 , by = ~ duplicated(h_seq) == FALSE, design =  estlow_AL, 	FUN = svytotal)

	IA<-subset( core , gestfips ==19)
				IA <-	update( IA ,
		low_est = (pmax(fedtax_ac - (pmin( pmin(care_val, 10015) * pmin(hunder15, 4) , 10015 * 4)), 0)))
				IA <-	update( IA ,
			low_est2 = pmax( fedtax_ac - low_est, 0) )
			svyby( ~low_est2 , by = ~ duplicated(h_seq) == FALSE, design =  IA, 	FUN = svytotal)

	KS<-subset( core , gestfips ==20)
				KS <-	update( KS ,
		low_est = (pmax(fedtax_ac - (pmin( pmin(care_val, 11482) * pmin(hunder15, 4) , 11482 * 4)), 0)))
				KS <-	update( KS ,
			low_est2 = pmax( fedtax_ac - low_est, 0) )
			svyby( ~low_est2 , by = ~ duplicated(h_seq) == FALSE, design =  KS, 	FUN = svytotal)

	KY<-subset( core , gestfips ==21)
				KY <-	update( KY ,
		low_est = (pmax(fedtax_ac - (pmin( pmin(care_val, 7800) * pmin(hunder15, 4) , 7800 * 4)), 0)))
				KY <-	update( KY ,	low_est2 = pmax( fedtax_ac - low_est, 0) )
			svyby( ~low_est2 , by = ~ duplicated(h_seq) == FALSE, design =  KY, 	FUN = svytotal)

	LA<-subset( core , gestfips ==22)
				LA <-	update( LA ,
		low_est = (pmax(fedtax_ac - (pmin( pmin(care_val, 5754) * pmin(hunder15, 4) , 5754 * 4)), 0)))
				LA <-	update( LA ,low_est2 = pmax( fedtax_ac - low_est, 0) )
			svyby( ~low_est2 , by = ~ duplicated(h_seq) == FALSE, design =  LA, 	FUN = svytotal)

	ME<-subset( core , gestfips ==23)
				ME <-	update( ME ,
		low_est = (pmax(fedtax_ac - (pmin( pmin(care_val, 9677) * pmin(hunder15, 4) , 9677 * 4)), 0)))
				ME <-	update( ME ,low_est2 = pmax( fedtax_ac - low_est, 0) )
			svyby( ~low_est2 , by = ~ duplicated(h_seq) == FALSE, design =  ME, 	FUN = svytotal)

	MD<-subset( core , gestfips ==24)
				MD <-	update( MD ,
		low_est = (pmax(fedtax_ac - (pmin( pmin(care_val, 14726) * pmin(hunder15, 4) , 14726 * 4)), 0)))
				MD <-	update( MD ,		low_est2 = pmax( fedtax_ac - low_est, 0) )
			svyby( ~low_est2 , by = ~ duplicated(h_seq) == FALSE, design =  MD, 	FUN = svytotal)

	MA<-subset( core , gestfips ==25)
				MA <-	update( MA ,
		low_est = (pmax(fedtax_ac - (pmin( pmin(care_val, 17082) * pmin(hunder15, 4) , 17082 * 4)), 0)))
				MA <-	update( MA ,
			low_est2 = pmax( fedtax_ac - low_est, 0) )
			svyby( ~low_est2 , by = ~ duplicated(h_seq) == FALSE, design =  MA, 	FUN = svytotal)

	MI<-subset( core , gestfips ==26)
				MI <-	update( MI ,
		low_est = (pmax(fedtax_ac - (pmin( pmin(care_val, 10178) * pmin(hunder15, 4) , 10178 * 4)), 0)))
				MI <-	update( MI ,
			low_est2 = pmax( fedtax_ac - low_est, 0) )
			svyby( ~low_est2 , by = ~ duplicated(h_seq) == FALSE, design =  MI, 	FUN = svytotal)

	MN<-subset( core , gestfips ==27)
				MN <-	update( MN ,
		low_est = (pmax(fedtax_ac - (pmin( pmin(care_val, 14826) * pmin(hunder15, 4) , 14826 * 4)), 0)))
				MN <-	update( MN ,
			low_est2 = pmax( fedtax_ac - low_est, 0) )
			svyby( ~low_est2 , by = ~ duplicated(h_seq) == FALSE, design =  MN, 	FUN = svytotal)

	MS<-subset( core , gestfips ==28)
				MS <-	update( MS ,
		low_est = (pmax(fedtax_ac - (pmin( pmin(care_val, 5045) * pmin(hunder15, 4) , 5045 * 4)), 0)))
				MS <-	update( MS ,
			low_est2 = pmax( fedtax_ac - low_est, 0) )
			svyby( ~low_est2 , by = ~ duplicated(h_seq) == FALSE, design =  MS, 	FUN = svytotal)

	MO<-subset( core , gestfips ==29)
				MO <-	update( MO ,
		low_est = (pmax(fedtax_ac - (pmin( pmin(care_val, 9100) * pmin(hunder15, 4) , 9100 * 4)), 0)))
				MO <-	update( MO ,
			low_est2 = pmax( fedtax_ac - low_est, 0) )
			svyby( ~low_est2 , by = ~ duplicated(h_seq) == FALSE, design =  MO, 	FUN = svytotal)

	MT<-subset( core , gestfips ==30)
				MT <-	update( MT ,
		low_est = (pmax(fedtax_ac - (pmin( pmin(care_val, 9383) * pmin(hunder15, 4) , 9383 * 4)), 0)))
				MT <-	update( MT ,
			low_est2 = pmax( fedtax_ac - low_est, 0) )
			svyby( ~low_est2 , by = ~ duplicated(h_seq) == FALSE, design =  MT, 	FUN = svytotal)

	NE<-subset( core , gestfips ==31)
				NE <-	update( NE ,
		low_est = (pmax(fedtax_ac - (pmin( pmin(care_val, 9043) * pmin(hunder15, 4) , 9043 * 4)), 0)))
				NE <-	update( NE ,
			low_est2 = pmax( fedtax_ac - low_est, 0) )
			svyby( ~low_est2 , by = ~ duplicated(h_seq) == FALSE, design =  NE, 	FUN = svytotal)

	NV<-subset( core , gestfips ==32)
				NV <-	update( NV ,
		low_est = (pmax(fedtax_ac - (pmin( pmin(care_val, 10317) * pmax(hunder15, 4), 10317 * 4)), 0)))
				NV <-	update( NV ,
			low_est2 = pmax( fedtax_ac - low_est, 0) )
			svyby( ~low_est2 , by = ~ duplicated(h_seq) == FALSE, design =  NV, 	FUN = svytotal)

	NH<-subset( core , gestfips ==33)
				NH <-	update( NH ,
		low_est = (pmax(fedtax_ac - (pmin( pmin(care_val, 12399) * pmin(hunder15, 4) , 12399 * 4)), 0)))
				NH <-	update( NH ,	low_est2 = pmax( fedtax_ac - low_est, 0) )
				svyby( ~low_est2 , by = ~ duplicated(h_seq) == FALSE, design =  NH, 	FUN = svytotal)

	NJ<-subset( core , gestfips ==34)
				NJ <-	update( NJ ,
		low_est = (pmax(fedtax_ac - (pmin( pmin(care_val, 11548) * pmin(hunder15, 4) , 11548 * 4)), 0)))
				NJ <-	update( NJ ,		low_est2 = pmax( fedtax_ac - low_est, 0) )
			svyby( ~low_est2 , by = ~ duplicated(h_seq) == FALSE, design =  NJ, 	FUN = svytotal)

	NM<-subset( core , gestfips ==35)
				NM <-	update( NM ,
		low_est = (pmax(fedtax_ac - (pmin( pmin(care_val, 7802) * pmin(hunder15, 4) , 7802 * 4)), 0)))
				NM <-	update( NM ,low_est2 = pmax( fedtax_ac - low_est, 0) )
			svyby( ~low_est2 , by = ~ duplicated(h_seq) == FALSE, design =  NM, 	FUN = svytotal)

	NY<-subset( core , gestfips ==36)
				NY <-	update( NY ,
		low_est = (pmax(fedtax_ac - (pmin( pmin(care_val, 14144) * pmin(hunder15, 4) , 14144 * 4)), 0)))
				NY <-	update( NY ,	low_est2 = pmax( fedtax_ac - low_est, 0) )
			svyby( ~low_est2 , by = ~ duplicated(h_seq) == FALSE, design =  NY, 	FUN = svytotal)

	NC<-subset( core , gestfips ==37)
				NC <-	update( NC ,
		low_est = (pmax(fedtax_ac - (pmin( pmin(care_val, 9254) * pmin(hunder15, 4) , 9254 * 4)), 0)))
				NC <-	update( NC ,	low_est2 = pmax( fedtax_ac - low_est, 0) )
			svyby( ~low_est2 , by = ~ duplicated(h_seq) == FALSE, design =  NC, 	FUN = svytotal)

	ND<-subset( core , gestfips ==38)
				ND <-	update( ND ,
		low_est = (pmax(fedtax_ac - (pmin( pmin(care_val, 8431) * pmin(hunder15, 4) , 8431 * 4)), 0)))
				ND <-	update( ND ,	low_est2 = pmax( fedtax_ac - low_est, 0) )
			svyby( ~low_est2 , by = ~ duplicated(h_seq) == FALSE, design =  ND, 	FUN = svytotal)

	OH<-subset( core , gestfips ==39)
				OH <-	update( OH ,
		low_est = (pmax(fedtax_ac - (pmin( pmin(care_val, 8985) * pmin(hunder15, 4) , 8985 * 4)), 0)))
				OH <-	update( OH ,low_est2 = pmax( fedtax_ac - low_est, 0) )
			svyby( ~low_est2 , by = ~ duplicated(h_seq) == FALSE, design =  OH, 	FUN = svytotal)

	OK<-subset( core , gestfips ==40)
				OK <-	update( OK ,
		low_est = (pmax(fedtax_ac - (pmin( pmin(care_val, 6572) * pmin(hunder15, 4) , 6572 * 4)), 0)))
				OK <-	update( OK ,	low_est2 = pmax( fedtax_ac - low_est, 0) )
			svyby( ~low_est2 , by = ~ duplicated(h_seq) == FALSE, design =  OK, 	FUN = svytotal)

	OR<-subset( core , gestfips ==41)
				OR <-	update( OR ,
		low_est = (pmax(fedtax_ac - (pmin( pmin(care_val, 11964) * pmin(hunder15, 4) , 11964 * 4)), 0)))
				OR <-	update( OR ,		low_est2 = pmax( fedtax_ac - low_est, 0) )
			svyby( ~low_est2 , by = ~ duplicated(h_seq) == FALSE, design =  OR, 	FUN = svytotal)

	PA<-subset( core , gestfips ==42)
				PA <-	update( PA ,
		low_est = (pmax(fedtax_ac - (pmin( pmin(care_val, 11978) * pmin(hunder15, 4) , 11978 * 4)), 0)))
				PA <-	update( PA ,	low_est2 = pmax( fedtax_ac - low_est, 0) )
			svyby( ~low_est2 , by = ~ duplicated(h_seq) == FALSE, design =  PA, 	FUN = svytotal)

	RI<-subset( core , gestfips ==44)
				RI <-	update( RI ,
		low_est = (pmax(fedtax_ac - (pmin( pmin(care_val, 12882) * pmin(hunder15, 4) , 12882 * 4)), 0)))
				RI <-	update( RI ,	low_est2 = pmax( fedtax_ac - low_est, 0) )
			svyby( ~low_est2 , by = ~ duplicated(h_seq) == FALSE, design =  RI, 	FUN = svytotal)

	SC<-subset( core , gestfips ==45)
				SC <-	update( SC ,
		low_est = (pmax(fedtax_ac - (pmin( pmin(care_val, 6483) * pmin(hunder15, 4) , 6483 * 4)), 0)))
				SC <-	update( SC ,	low_est2 = pmax( fedtax_ac - low_est, 0) )
			svyby( ~low_est2 , by = ~ duplicated(h_seq) == FALSE, design =  SC, 	FUN = svytotal)

	SD<-subset( core , gestfips ==46)
				SD <-	update( SD ,
		low_est = (pmax(fedtax_ac - (pmin( pmin(care_val, 6143) * pmin(hunder15, 4) , 6143 * 4)), 0)))
				SD <-	update( SD ,	low_est2 = pmax( fedtax_ac - low_est, 0) )
			svyby( ~low_est2 , by = ~ duplicated(h_seq) == FALSE, design =  SD, 	FUN = svytotal)

	TN<-subset( core , gestfips ==47)
				TN <-	update( TN ,
		low_est = (pmax(fedtax_ac - (pmin( pmin(care_val, 8378) * pmin(hunder15, 4) , 8378 * 4)), 0)))
				TN <-	update( TN ,low_est2 = pmax( fedtax_ac - low_est, 0) )
			svyby( ~low_est2 , by = ~ duplicated(h_seq) == FALSE, design =  TN, 	FUN = svytotal)

	TX<-subset( core , gestfips ==48)
				TX <-	update( TX ,
		low_est = (pmax(fedtax_ac - (pmin( pmin(care_val, 9207) * pmin(hunder15, 4) , 9207 * 4)), 0)))
				TX <-	update( TX ,	low_est2 = pmax( fedtax_ac - low_est, 0) )
			svyby( ~low_est2 , by = ~ duplicated(h_seq) == FALSE, design =  TX, 	FUN = svytotal)

	UT<-subset( core , gestfips ==49)
				UT <-	update( UT ,
		low_est = (pmax(fedtax_ac - (pmin( pmin(care_val, 9183) * pmin(hunder15, 4) , 9183 * 4)), 0)))
				UT <-	update( UT ,		low_est2 = pmax( fedtax_ac - low_est, 0) )
			svyby( ~low_est2 , by = ~ duplicated(h_seq) == FALSE, design =  UT, 	FUN = svytotal)

	VT<-subset( core , gestfips ==50)
				VT <-	update( VT ,
		low_est = (pmax(fedtax_ac - (pmin( pmin(care_val, 11513) * pmin(hunder15, 4) , 11513 * 4)), 0)))
				VT <-	update( VT ,low_est2 = pmax( fedtax_ac - low_est, 0) )
			svyby( ~low_est2 , by = ~ duplicated(h_seq) == FALSE, design =  VT, 	FUN = svytotal)

	VA<-subset( core , gestfips ==51)
				VA <-	update( VA ,
		low_est = (pmax(fedtax_ac - (pmin( pmin(care_val, 12220) * pmin(hunder15, 4) , 12220 * 4)), 0)))
				VA <-	update( VA ,	low_est2 = pmax( fedtax_ac - low_est, 0) )
			svyby( ~low_est2 , by = ~ duplicated(h_seq) == FALSE, design =  VA, 	FUN = svytotal)

	WA<-subset( core , gestfips ==53)
				WA <-	update( WA ,
		low_est = (pmax(fedtax_ac - (pmin( pmin(care_val, 13110) * pmin(hunder15, 4) , 13110 * 4)), 0)))
				WA <-	update( WA ,	low_est2 = pmax( fedtax_ac - low_est, 0) )
			svyby( ~low_est2 , by = ~ duplicated(h_seq) == FALSE, design =  WA, 	FUN = svytotal)

	WV<-subset( core , gestfips ==54)
				WV <-	update( WV ,
		low_est = (pmax(fedtax_ac - (pmin( pmin(care_val, 8580) * pmin(hunder15, 4) , 8580 * 4)), 0)))
				WV <-	update( WV ,	low_est2 = pmax( fedtax_ac - low_est, 0) )
						svyby( ~low_est2 , by = ~ duplicated(h_seq) == FALSE, design =  WV, 	FUN = svytotal)

	WI<-subset( core , gestfips ==55)
				WI <-	update( WI ,
		low_est = (pmax(fedtax_ac - (pmin( pmin(care_val, 11750) * pmin(hunder15, 4) , 11750 * 4)), 0)))
				WI <-	update( WI ,	low_est2 = pmax( fedtax_ac - low_est, 0) )
			svyby( ~low_est2 , by = ~ duplicated(h_seq) == FALSE, design =  WI, 	FUN = svytotal)

	WY<-subset( core , gestfips ==56)
				WY <-	update( WY ,
		low_est = (pmax(fedtax_ac - (pmin( pmin(care_val, 9110) * pmin(hunder15, 4) , 9110 * 4)), 0)))
						WY <-	update( WY ,	low_est2 = pmax( fedtax_ac - low_est, 0) )
			svyby( ~low_est2, by = ~ duplicated(h_seq) == FALSE, design = WY, 	FUN = svytotal)

		
####		

								
		####
				svyby( ~low_est2 , by = ~ duplicated(h_seq) == FALSE, design =  WY )

 duplicated(h_seq) == FALSE
			
str(x)
	
		low_1 <- subset(core, duplicated(h_seq) == 1 )
		low_1 <- subset(core, duplicated(h_seq) == 0 )
		
		low_est1 <-	function( k,){
		low_est <-	update(low_1,
		low_est = (pmin( pmax(care_val, k) * pmax(hh5to18, 4) , lololol * 4)), 0))
			
			z <- svyby( ~low_est, by = ~ gestfips, design = low_est, 	FUN = svytotal)
				z
				
}
						low_est1(8985)

	
	pmin( k)
		pmin()
		

		est1 <-	function( k ,  ){
			update(low_1,
		high_est = (pmin( k * pmax(hh5to18, 4) , lololol * 4)), 0))

core(8985)

cost = (x * fownu18)

low_est_pt1 <-
			update(low_est_yes,
		cost_full = (x * fownu18))
			
	low_est_pt2 <-
					update(low_est_no,
		cost_split = (x * fownu18))
}

	svyby(
	   ~cost_full,
	low_est_yes)
		
		svyby(
	   ~cost_split,
		low_est_no)
}

core(14950) 

				Fullbenefit_yes <-
	update(Fullbenefit_yes,
		cost_single = (2000 * fownu18 ))

				Fullbenefit_no <-
	update(Fullbenefit_no,
		cost_dual = (2000 * fownu18))
				
		svyby(
	   ~cost_single,
	Fullbenefit_yes)
		
		svyby(
	   ~cost_dual,
		Fullbenefit_no)
