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
#FOWNU6 -Own children in family under 6
#FRELU18  -Related persons in family under 18

16317 = 16337
19055	= 19078
19073	=	19096				
24817	= 24847
24008 = 24036
24091	=	24120			
29875 = 29911
28960	= 28995
28252	= 28286
27820	= 27853
34004	= 34044
33303	= 33342
32631	= 32670
31633	= 31670
31041	=	31078
39214	= 39260
38375	= 38421
37791	= 37835
36701	= 36745
35431	= 35473
34036	=	34077
43970	= 44023
43179	= 43230
42485	= 42536
41501	= 41551
40252	= 40300
38953	= 38999
38622	= 38668
52685	= 52747
51984	= 52046
51396	= 51457
50430	= 50490
49101	= 49159
47899	= 47956
47601	= 47658
45768 = 45822



childpov_XTR <-subset( 
		y , 
		((h_numper == 2 & fownu18 == 1 & hwsval<=	16337/2) |
			(h_numper == 3 & fownu18 == 1 & hwsval<= 19078/2) |
			(h_numper == 3 & fownu18 == 2 & hwsval<= 19096/2) |
		 (h_numper == 4 & fownu18 == 1 & hwsval<= 24847/2) |
			(h_numper == 4 & fownu18 == 2 & hwsval<= 24036/2) |
		 (h_numper == 4 & fownu18 == 3 & hwsval<= 24120/2) |
		 (h_numper == 5 & fownu18 == 1 & hwsval<= 29911/2) |
		 (h_numper == 5 & fownu18 == 2 & hwsval<= 28995/2) |
		 (h_numper == 5 & fownu18 == 3 & hwsval<= 28286/2) |
			(h_numper == 5 & fownu18 == 4 & hwsval<= 27853/2) |
		 (h_numper == 6 & fownu18 == 1 & hwsval<=	34044/2) |
			(h_numper == 6 & fownu18 == 2 & hwsval<=	33342/2) |
		 (h_numper == 6 & fownu18 == 3 & hwsval<=	32670/2) |
		 (h_numper == 6 & fownu18 == 4 & hwsval<=	31670/2) |
		 (h_numper == 6 & fownu18 == 5 & hwsval<=	31078/2) |
		 (h_numper == 7 & fownu18 == 1 & hwsval<=	39260/2) |
		 (h_numper == 7 & fownu18 == 2 & hwsval<= 38421/2) |
			(h_numper == 7 & fownu18 == 3 & hwsval<= 37835/2) |
		 (h_numper == 7 & fownu18 == 4 & hwsval<= 36745/2) |
		 (h_numper == 7 & fownu18 == 5 & hwsval<= 35473/2) |
		 (h_numper == 7 & fownu18 == 6 & hwsval<= 34077/2) |
		 (h_numper == 8 & fownu18 == 1 & hwsval<= 44023/2) |
		 (h_numper == 8 & fownu18 == 2 & hwsval<= 43230/2) |
		 (h_numper == 8 & fownu18 == 3 & hwsval<= 42536/2) |
		 (h_numper == 8 & fownu18 == 4 & hwsval<= 41551/2) |
		 (h_numper == 8 & fownu18 == 5 & hwsval<= 40300/2) |
		 (h_numper == 8 & fownu18 == 6 & hwsval<= 38999/2) |
		 (h_numper == 8 & fownu18 == 7 & hwsval<= 38668/2) |
		 (h_numper == 9 & fownu18 == 1 & hwsval<= 52747/2) |
			(h_numper == 9 & fownu18 == 2 & hwsval<= 52046/2) |
		 (h_numper == 9 & fownu18 == 3 & hwsval<= 51457/2) |
		 (h_numper == 9 & fownu18 == 4 & hwsval<= 50490/2) |
		 (h_numper == 9 & fownu18 == 5 & hwsval<= 49159/2) |
		 (h_numper == 9 & fownu18 == 6 & hwsval<= 47956/2) |
		 (h_numper == 9 & fownu18 == 7 & hwsval<= 47658/2) |
		 (h_numper == 9 & fownu18 == 8 & hwsval<= 45822/2)) & fownu18 >= 0 & a_age < 19 
		& povll < 2
	)

ychildpov_XTR <-subset( 
		y , 
		((h_numper == 2 & fownu18 == 1 & hwsval<=	16337/2) |
			(h_numper == 3 & fownu18 == 1 & hwsval<= 19078/2) |
			(h_numper == 3 & fownu18 == 2 & hwsval<= 19096/2) |
		 (h_numper == 4 & fownu18 == 1 & hwsval<= 24847/2) |
			(h_numper == 4 & fownu18 == 2 & hwsval<= 24036/2) |
		 (h_numper == 4 & fownu18 == 3 & hwsval<= 24120/2) |
		 (h_numper == 5 & fownu18 == 1 & hwsval<= 29911/2) |
		 (h_numper == 5 & fownu18 == 2 & hwsval<= 28995/2) |
		 (h_numper == 5 & fownu18 == 3 & hwsval<= 28286/2) |
			(h_numper == 5 & fownu18 == 4 & hwsval<= 27853/2) |
		 (h_numper == 6 & fownu18 == 1 & hwsval<=	34044/2) |
			(h_numper == 6 & fownu18 == 2 & hwsval<=	33342/2) |
		 (h_numper == 6 & fownu18 == 3 & hwsval<=	32670/2) |
		 (h_numper == 6 & fownu18 == 4 & hwsval<=	31670/2) |
		 (h_numper == 6 & fownu18 == 5 & hwsval<=	31078/2) |
		 (h_numper == 7 & fownu18 == 1 & hwsval<=	39260/2) |
		 (h_numper == 7 & fownu18 == 2 & hwsval<= 38421/2) |
			(h_numper == 7 & fownu18 == 3 & hwsval<= 37835/2) |
		 (h_numper == 7 & fownu18 == 4 & hwsval<= 36745/2) |
		 (h_numper == 7 & fownu18 == 5 & hwsval<= 35473/2) |
		 (h_numper == 7 & fownu18 == 6 & hwsval<= 34077/2) |
		 (h_numper == 8 & fownu18 == 1 & hwsval<= 44023/2) |
		 (h_numper == 8 & fownu18 == 2 & hwsval<= 43230/2) |
		 (h_numper == 8 & fownu18 == 3 & hwsval<= 42536/2) |
		 (h_numper == 8 & fownu18 == 4 & hwsval<= 41551/2) |
		 (h_numper == 8 & fownu18 == 5 & hwsval<= 40300/2) |
		 (h_numper == 8 & fownu18 == 6 & hwsval<= 38999/2) |
		 (h_numper == 8 & fownu18 == 7 & hwsval<= 38668/2) |
		 (h_numper == 9 & fownu18 == 1 & hwsval<= 52747/2) |
			(h_numper == 9 & fownu18 == 2 & hwsval<= 52046/2) |
		 (h_numper == 9 & fownu18 == 3 & hwsval<= 51457/2) |
		 (h_numper == 9 & fownu18 == 4 & hwsval<= 50490/2) |
		 (h_numper == 9 & fownu18 == 5 & hwsval<= 49159/2) |
		 (h_numper == 9 & fownu18 == 6 & hwsval<= 47956/2) |
		 (h_numper == 9 & fownu18 == 7 & hwsval<= 47658/2) |
		 (h_numper == 9 & fownu18 == 8 & hwsval<= 45822/2)) & fownu6 > 0 & a_age < 6 
		#& povll < 2
	)


childpov <-subset( 
		y , 
		((h_numper == 2 & fownu18 == 1 & hwsval<=	16337) |
			(h_numper == 3 & fownu18 == 1 & hwsval<= 19078) |
			(h_numper == 3 & fownu18 == 2 & hwsval<= 19096) |
		 (h_numper == 4 & fownu18 == 1 & hwsval<= 24847) |
			(h_numper == 4 & fownu18 == 2 & hwsval<= 24036) |
		 (h_numper == 4 & fownu18 == 3 & hwsval<= 24120) |
		 (h_numper == 5 & fownu18 == 1 & hwsval<= 29911) |
		 (h_numper == 5 & fownu18 == 2 & hwsval<= 28995) |
		 (h_numper == 5 & fownu18 == 3 & hwsval<= 28286) |
			(h_numper == 5 & fownu18 == 4 & hwsval<= 27853) |
		 (h_numper == 6 & fownu18 == 1 & hwsval<=	34044) |
			(h_numper == 6 & fownu18 == 2 & hwsval<=	33342) |
		 (h_numper == 6 & fownu18 == 3 & hwsval<=	32670) |
		 (h_numper == 6 & fownu18 == 4 & hwsval<=	31670) |
		 (h_numper == 6 & fownu18 == 5 & hwsval<=	31078) |
		 (h_numper == 7 & fownu18 == 1 & hwsval<=	39260) |
		 (h_numper == 7 & fownu18 == 2 & hwsval<= 38421) |
			(h_numper == 7 & fownu18 == 3 & hwsval<= 37835) |
		 (h_numper == 7 & fownu18 == 4 & hwsval<= 36745) |
		 (h_numper == 7 & fownu18 == 5 & hwsval<= 35473) |
		 (h_numper == 7 & fownu18 == 6 & hwsval<= 34077) |
		 (h_numper == 8 & fownu18 == 1 & hwsval<= 44023) |
		 (h_numper == 8 & fownu18 == 2 & hwsval<= 43230) |
		 (h_numper == 8 & fownu18 == 3 & hwsval<= 42536) |
		 (h_numper == 8 & fownu18 == 4 & hwsval<= 41551) |
		 (h_numper == 8 & fownu18 == 5 & hwsval<= 40300) |
		 (h_numper == 8 & fownu18 == 6 & hwsval<= 38999) |
		 (h_numper == 8 & fownu18 == 7 & hwsval<= 38668) |
		 (h_numper == 9 & fownu18 == 1 & hwsval<= 52747) |
			(h_numper == 9 & fownu18 == 2 & hwsval<= 52046) |
		 (h_numper == 9 & fownu18 == 3 & hwsval<= 51457) |
		 (h_numper == 9 & fownu18 == 4 & hwsval<= 50490) |
		 (h_numper == 9 & fownu18 == 5 & hwsval<= 49159) |
		 (h_numper == 9 & fownu18 == 6 & hwsval<= 47956) |
		 (h_numper == 9 & fownu18 == 7 & hwsval<= 47658) |
		 (h_numper == 9 & fownu18 == 8 & hwsval<= 45822)) & fownu18 > 0 & a_age < 19 
		#& povll < 4
		)


ychildpov <-subset( 	
	y , 
		((h_numper == 2 & fownu18 == 1 & hwsval<=	16337) |
			(h_numper == 3 & fownu18 == 1 & hwsval<= 19078) |
			(h_numper == 3 & fownu18 == 2 & hwsval<= 19096) |
		 (h_numper == 4 & fownu18 == 1 & hwsval<= 24847) |
			(h_numper == 4 & fownu18 == 2 & hwsval<= 24036) |
		 (h_numper == 4 & fownu18 == 3 & hwsval<= 24120) |
		 (h_numper == 5 & fownu18 == 1 & hwsval<= 29911) |
		 (h_numper == 5 & fownu18 == 2 & hwsval<= 28995) |
		 (h_numper == 5 & fownu18 == 3 & hwsval<= 28286) |
			(h_numper == 5 & fownu18 == 4 & hwsval<= 27853) |
		 (h_numper == 6 & fownu18 == 1 & hwsval<=	34044) |
			(h_numper == 6 & fownu18 == 2 & hwsval<=	33342) |
		 (h_numper == 6 & fownu18 == 3 & hwsval<=	32670) |
		 (h_numper == 6 & fownu18 == 4 & hwsval<=	31670) |
		 (h_numper == 6 & fownu18 == 5 & hwsval<=	31078) |
		 (h_numper == 7 & fownu18 == 1 & hwsval<=	39260) |
		 (h_numper == 7 & fownu18 == 2 & hwsval<= 38421) |
			(h_numper == 7 & fownu18 == 3 & hwsval<= 37835) |
		 (h_numper == 7 & fownu18 == 4 & hwsval<= 36745) |
		 (h_numper == 7 & fownu18 == 5 & hwsval<= 35473) |
		 (h_numper == 7 & fownu18 == 6 & hwsval<= 34077) |
		 (h_numper == 8 & fownu18 == 1 & hwsval<= 44023) |
		 (h_numper == 8 & fownu18 == 2 & hwsval<= 43230) |
		 (h_numper == 8 & fownu18 == 3 & hwsval<= 42536) |
		 (h_numper == 8 & fownu18 == 4 & hwsval<= 41551) |
		 (h_numper == 8 & fownu18 == 5 & hwsval<= 40300) |
		 (h_numper == 8 & fownu18 == 6 & hwsval<= 38999) |
		 (h_numper == 8 & fownu18 == 7 & hwsval<= 38668) |
		 (h_numper == 9 & fownu18 == 1 & hwsval<= 52747) |
			(h_numper == 9 & fownu18 == 2 & hwsval<= 52046) |
		 (h_numper == 9 & fownu18 == 3 & hwsval<= 51457) |
		 (h_numper == 9 & fownu18 == 4 & hwsval<= 50490) |
		 (h_numper == 9 & fownu18 == 5 & hwsval<= 49159) |
		 (h_numper == 9 & fownu18 == 6 & hwsval<= 47956) |
		 (h_numper == 9 & fownu18 == 7 & hwsval<= 47658) |
		 (h_numper == 9 & fownu18 == 8 & hwsval<= 45822)) & fownu6 > 0 &	a_age < 6 
	#& povll < 4
	)


childpov_wPayment <-subset( 
		y , 
		((h_numper == 2 & fownu18 == 1 & hwsval<=	16337 - 5000) |
			(h_numper == 3 & fownu18 == 1 & hwsval<= 19078 - 5000 ) |
			(h_numper == 3 & fownu18 == 2 & hwsval<= 19096 - 10000) |
		 (h_numper == 4 & fownu18 == 1 & hwsval<= 24847 - 5000) |
			(h_numper == 4 & fownu18 == 2 & hwsval<= 24036 - 10000) |
		 (h_numper == 4 & fownu18 == 3 & hwsval<= 24120 - 15000) |
		 (h_numper == 5 & fownu18 == 1 & hwsval<= 29911 - 5000) |
		 (h_numper == 5 & fownu18 == 2 & hwsval<= 28995 - 10000) |
		 (h_numper == 5 & fownu18 == 3 & hwsval<= 28286 - 15000) |
			(h_numper == 5 & fownu18 == 4 & hwsval<= 27853 - 20000) |
		 (h_numper == 6 & fownu18 == 1 & hwsval<=	34044 - 5000) |
			(h_numper == 6 & fownu18 == 2 & hwsval<=	33342 - 10000) |
		 (h_numper == 6 & fownu18 == 3 & hwsval<=	32670 - 15000) |
		 (h_numper == 6 & fownu18 == 4 & hwsval<=	31670 - 20000) |
		 (h_numper == 6 & fownu18 == 5 & hwsval<=	31078 - 25000) |
		 (h_numper == 7 & fownu18 == 1 & hwsval<=	39260 - 5000) |
		 (h_numper == 7 & fownu18 == 2 & hwsval<= 38421 - 10000) |
			(h_numper == 7 & fownu18 == 3 & hwsval<= 37835 - 15000) |
		 (h_numper == 7 & fownu18 == 4 & hwsval<= 36745 - 20000) |
		 (h_numper == 7 & fownu18 == 5 & hwsval<= 35473 - 25000) |
		 (h_numper == 7 & fownu18 == 6 & hwsval<= 34077 - 30000) |
		 (h_numper == 8 & fownu18 == 1 & hwsval<= 44023 - 5000) |
		 (h_numper == 8 & fownu18 == 2 & hwsval<= 43230 - 10000) |
		 (h_numper == 8 & fownu18 == 3 & hwsval<= 42536 - 15000) |
		 (h_numper == 8 & fownu18 == 4 & hwsval<= 41551 - 20000) |
		 (h_numper == 8 & fownu18 == 5 & hwsval<= 40300 - 25000) |
		 (h_numper == 8 & fownu18 == 6 & hwsval<= 38999 - 30000) |
		 (h_numper == 8 & fownu18 == 7 & hwsval<= 38668 - 35000) |
		 (h_numper == 9 & fownu18 == 1 & hwsval<= 52747 - 5000) |
			(h_numper == 9 & fownu18 == 2 & hwsval<= 52046 - 10000) |
		 (h_numper == 9 & fownu18 == 3 & hwsval<= 51457 - 15000) |
		 (h_numper == 9 & fownu18 == 4 & hwsval<= 50490 - 20000) |
		 (h_numper == 9 & fownu18 == 5 & hwsval<= 49159 - 25000) |
		 (h_numper == 9 & fownu18 == 6 & hwsval<= 47956 - 30000) |
		 (h_numper == 9 & fownu18 == 7 & hwsval<= 47658 - 35000) |
		 (h_numper == 9 & fownu18 == 8 & hwsval<= 45822 - 40000)) &
		fownu18 > 0 &
		a_age < 19 
		#& povll < 4
		)


ychildpov_wPayment <-subset( 
		y , 
		((h_numper == 2 & fownu18 == 1 & hwsval<=	16337 - 5000) |
			(h_numper == 3 & fownu18 == 1 & hwsval<= 19078 - 5000 ) |
			(h_numper == 3 & fownu18 == 2 & hwsval<= 19096 - 10000) |
		 (h_numper == 4 & fownu18 == 1 & hwsval<= 24847 - 5000) |
			(h_numper == 4 & fownu18 == 2 & hwsval<= 24036 - 10000) |
		 (h_numper == 4 & fownu18 == 3 & hwsval<= 24120 - 15000) |
		 (h_numper == 5 & fownu18 == 1 & hwsval<= 29911 - 5000) |
		 (h_numper == 5 & fownu18 == 2 & hwsval<= 28995 - 10000) |
		 (h_numper == 5 & fownu18 == 3 & hwsval<= 28286 - 15000) |
			(h_numper == 5 & fownu18 == 4 & hwsval<= 27853 - 20000) |
		 (h_numper == 6 & fownu18 == 1 & hwsval<=	34044 - 5000) |
			(h_numper == 6 & fownu18 == 2 & hwsval<=	33342 - 10000) |
		 (h_numper == 6 & fownu18 == 3 & hwsval<=	32670 - 15000) |
		 (h_numper == 6 & fownu18 == 4 & hwsval<=	31670 - 20000) |
		 (h_numper == 6 & fownu18 == 5 & hwsval<=	31078 - 25000) |
		 (h_numper == 7 & fownu18 == 1 & hwsval<=	39260 - 5000) |
		 (h_numper == 7 & fownu18 == 2 & hwsval<= 38421 - 10000) |
			(h_numper == 7 & fownu18 == 3 & hwsval<= 37835 - 15000) |
		 (h_numper == 7 & fownu18 == 4 & hwsval<= 36745 - 20000) |
		 (h_numper == 7 & fownu18 == 5 & hwsval<= 35473 - 25000) |
		 (h_numper == 7 & fownu18 == 6 & hwsval<= 34077 - 30000) |
		 (h_numper == 8 & fownu18 == 1 & hwsval<= 44023 - 5000) |
		 (h_numper == 8 & fownu18 == 2 & hwsval<= 43230 - 10000) |
		 (h_numper == 8 & fownu18 == 3 & hwsval<= 42536 - 15000) |
		 (h_numper == 8 & fownu18 == 4 & hwsval<= 41551 - 20000) |
		 (h_numper == 8 & fownu18 == 5 & hwsval<= 40300 - 25000) |
		 (h_numper == 8 & fownu18 == 6 & hwsval<= 38999 - 30000) |
		 (h_numper == 8 & fownu18 == 7 & hwsval<= 38668 - 35000) |
		 (h_numper == 9 & fownu18 == 1 & hwsval<= 52747 - 5000) |
			(h_numper == 9 & fownu18 == 2 & hwsval<= 52046 - 10000) |
		 (h_numper == 9 & fownu18 == 3 & hwsval<= 51457 - 15000) |
		 (h_numper == 9 & fownu18 == 4 & hwsval<= 50490 - 20000) |
		 (h_numper == 9 & fownu18 == 5 & hwsval<= 49159 - 25000) |
		 (h_numper == 9 & fownu18 == 6 & hwsval<= 47956 - 30000) |
		 (h_numper == 9 & fownu18 == 7 & hwsval<= 47658 - 35000) |
		 (h_numper == 9 & fownu18 == 8 & hwsval<= 45822 - 40000)) &
		fownu6 > 0 &	a_age < 6 
		#&	povll < 4
		)

childpov_XTR_wPayment <-subset( 
		y , 
		((h_numper == 2 & fownu18 == 1 & hwsval<=	16337/2 - 5000) |
			(h_numper == 3 & fownu18 == 1 & hwsval<= 19078/2 - 5000) |
			(h_numper == 3 & fownu18 == 2 & hwsval<= 19096/2 - 10000) |
		 (h_numper == 4 & fownu18 == 1 & hwsval<= 24847/2 - 5000) |
			(h_numper == 4 & fownu18 == 2 & hwsval<= 24036/2 - 10000) |
		 (h_numper == 4 & fownu18 == 3 & hwsval<= 24120/2 - 15000) |
		 (h_numper == 5 & fownu18 == 1 & hwsval<= 29911/2 - 5000) |
		 (h_numper == 5 & fownu18 == 2 & hwsval<= 28995/2 - 10000) |
		 (h_numper == 5 & fownu18 == 3 & hwsval<= 28286/2 - 15000) |
			(h_numper == 5 & fownu18 == 4 & hwsval<= 27853/2 - 20000) |
		 (h_numper == 6 & fownu18 == 1 & hwsval<=	34044/2 - 5000) |
			(h_numper == 6 & fownu18 == 2 & hwsval<=	33342/2 - 10000) |
		 (h_numper == 6 & fownu18 == 3 & hwsval<=	32670/2 - 15000) |
		 (h_numper == 6 & fownu18 == 4 & hwsval<=	31670/2 - 20000) |
		 (h_numper == 6 & fownu18 == 5 & hwsval<=	31078/2 - 25000) |
		 (h_numper == 7 & fownu18 == 1 & hwsval<=	39260/2 - 5000) |
		 (h_numper == 7 & fownu18 == 2 & hwsval<= 38421/2 - 10000) |
			(h_numper == 7 & fownu18 == 3 & hwsval<= 37835/2 - 15000) |
		 (h_numper == 7 & fownu18 == 4 & hwsval<= 36745/2 - 20000) |
		 (h_numper == 7 & fownu18 == 5 & hwsval<= 35473/2 - 25000) |
		 (h_numper == 7 & fownu18 == 6 & hwsval<= 34077/2 - 30000) |
		 (h_numper == 8 & fownu18 == 1 & hwsval<= 44023/2 - 5000) |
		 (h_numper == 8 & fownu18 == 2 & hwsval<= 43230/2 - 10000) |
		 (h_numper == 8 & fownu18 == 3 & hwsval<= 42536/2 - 15000) |
		 (h_numper == 8 & fownu18 == 4 & hwsval<= 41551/2 - 20000) |
		 (h_numper == 8 & fownu18 == 5 & hwsval<= 40300/2 - 25000) |
		 (h_numper == 8 & fownu18 == 6 & hwsval<= 38999/2 - 30000) |
		 (h_numper == 8 & fownu18 == 7 & hwsval<= 38668/2 - 35000) |
		 (h_numper == 9 & fownu18 == 1 & hwsval<= 52747/2 - 5000) |
			(h_numper == 9 & fownu18 == 2 & hwsval<= 52046/2 - 10000) |
		 (h_numper == 9 & fownu18 == 3 & hwsval<= 51457/2 - 15000) |
		 (h_numper == 9 & fownu18 == 4 & hwsval<= 50490/2 - 20000) |
		 (h_numper == 9 & fownu18 == 5 & hwsval<= 49159/2 - 25000) |
		 (h_numper == 9 & fownu18 == 6 & hwsval<= 47956/2 - 30000) |
		 (h_numper == 9 & fownu18 == 7 & hwsval<= 47658/2 - 35000) |
		 (h_numper == 9 & fownu18 == 8 & hwsval<= 45822/2 - 40000)) & fownu18 > 0 &	a_age < 19 
		#& povll < 2
		)

ychildpov_XTR_wPayment <-subset( 
		y , 
		((h_numper == 2 & fownu18 == 1 & hwsval<=	16337/2 - 5000) |
			(h_numper == 3 & fownu18 == 1 & hwsval<= 19078/2 - 5000 ) |
			(h_numper == 3 & fownu18 == 2 & hwsval<= 19096/2 - 10000) |
		 (h_numper == 4 & fownu18 == 1 & hwsval<= 24847/2 - 5000) |
			(h_numper == 4 & fownu18 == 2 & hwsval<= 24036/2 - 10000) |
		 (h_numper == 4 & fownu18 == 3 & hwsval<= 24120/2 - 15000) |
		 (h_numper == 5 & fownu18 == 1 & hwsval<= 29911/2 - 5000) |
		 (h_numper == 5 & fownu18 == 2 & hwsval<= 28995/2 - 10000) |
		 (h_numper == 5 & fownu18 == 3 & hwsval<= 28286/2 - 15000) |
			(h_numper == 5 & fownu18 == 4 & hwsval<= 27853/2 - 20000) |
		 (h_numper == 6 & fownu18 == 1 & hwsval<=	34044/2 - 5000) |
			(h_numper == 6 & fownu18 == 2 & hwsval<=	33342/2 - 10000) |
		 (h_numper == 6 & fownu18 == 3 & hwsval<=	32670/2 - 15000) |
		 (h_numper == 6 & fownu18 == 4 & hwsval<=	31670/2 - 20000) |
		 (h_numper == 6 & fownu18 == 5 & hwsval<=	31078/2 - 25000) |
		 (h_numper == 7 & fownu18 == 1 & hwsval<=	39260/2 - 5000) |
		 (h_numper == 7 & fownu18 == 2 & hwsval<= 38421/2 - 10000) |
			(h_numper == 7 & fownu18 == 3 & hwsval<= 37835/2 - 15000) |
		 (h_numper == 7 & fownu18 == 4 & hwsval<= 36745/2 - 20000) |
		 (h_numper == 7 & fownu18 == 5 & hwsval<= 35473/2 - 25000) |
		 (h_numper == 7 & fownu18 == 6 & hwsval<= 34077/2 - 30000) |
		 (h_numper == 8 & fownu18 == 1 & hwsval<= 44023/2 - 5000) |
		 (h_numper == 8 & fownu18 == 2 & hwsval<= 43230/2 - 10000) |
		 (h_numper == 8 & fownu18 == 3 & hwsval<= 42536/2 - 15000) |
		 (h_numper == 8 & fownu18 == 4 & hwsval<= 41551/2 - 20000) |
		 (h_numper == 8 & fownu18 == 5 & hwsval<= 40300/2 - 25000) |
		 (h_numper == 8 & fownu18 == 6 & hwsval<= 38999/2 - 30000) |
		 (h_numper == 8 & fownu18 == 7 & hwsval<= 38668/2 - 35000) |
		 (h_numper == 9 & fownu18 == 1 & hwsval<= 52747/2 - 5000) |
			(h_numper == 9 & fownu18 == 2 & hwsval<= 52046/2 - 10000) |
		 (h_numper == 9 & fownu18 == 3 & hwsval<= 51457/2 - 15000) |
		 (h_numper == 9 & fownu18 == 4 & hwsval<= 50490/2 - 20000) |
		 (h_numper == 9 & fownu18 == 5 & hwsval<= 49159/2 - 25000) |
		 (h_numper == 9 & fownu18 == 6 & hwsval<= 47956/2 - 30000) |
		 (h_numper == 9 & fownu18 == 7 & hwsval<= 47658/2 - 35000) |
		 (h_numper == 9 & fownu18 == 8 & hwsval<= 45822/2 - 40000)) & fownu6 > 0 &	a_age < 6 
			#& povll < 2
		)

childpov_wPayment_s <-subset( 
		y , 
		((h_numper == 2 & fownu18 == 1 & hwsval<=	16337 - 1000) |
			(h_numper == 3 & fownu18 == 1 & hwsval<= 19078 - 1000) |
			(h_numper == 3 & fownu18 == 2 & hwsval<= 19096 - 2000) |
		 (h_numper == 4 & fownu18 == 1 & hwsval<= 24847 - 1000) |
			(h_numper == 4 & fownu18 == 2 & hwsval<= 24036 - 2000) |
		 (h_numper == 4 & fownu18 == 3 & hwsval<= 24120 - 3000) |
		 (h_numper == 5 & fownu18 == 1 & hwsval<= 29911 - 1000) |
		 (h_numper == 5 & fownu18 == 2 & hwsval<= 28995 - 2000) |
		 (h_numper == 5 & fownu18 == 3 & hwsval<= 28286 - 3000) |
			(h_numper == 5 & fownu18 == 4 & hwsval<= 27853 - 4000) |
		 (h_numper == 6 & fownu18 == 1 & hwsval<=	34044 - 1000) |
			(h_numper == 6 & fownu18 == 2 & hwsval<=	33342 - 2000) |
		 (h_numper == 6 & fownu18 == 3 & hwsval<=	32670 - 3000) |
		 (h_numper == 6 & fownu18 == 4 & hwsval<=	31670 - 4000) |
		 (h_numper == 6 & fownu18 == 5 & hwsval<=	31078 - 5000) |
		 (h_numper == 7 & fownu18 == 1 & hwsval<=	39260 - 1000) |
		 (h_numper == 7 & fownu18 == 2 & hwsval<= 38421 - 2000) |
			(h_numper == 7 & fownu18 == 3 & hwsval<= 37835 - 3000) |
		 (h_numper == 7 & fownu18 == 4 & hwsval<= 36745 - 4000) |
		 (h_numper == 7 & fownu18 == 5 & hwsval<= 35473 - 5000) |
		 (h_numper == 7 & fownu18 == 6 & hwsval<= 34077 - 6000) |
		 (h_numper == 8 & fownu18 == 1 & hwsval<= 44023 - 1000) |
		 (h_numper == 8 & fownu18 == 2 & hwsval<= 43230 - 2000) |
		 (h_numper == 8 & fownu18 == 3 & hwsval<= 42536 - 3000) |
		 (h_numper == 8 & fownu18 == 4 & hwsval<= 41551 - 4000) |
		 (h_numper == 8 & fownu18 == 5 & hwsval<= 40300 - 5000) |
		 (h_numper == 8 & fownu18 == 6 & hwsval<= 38999 - 6000) |
		 (h_numper == 8 & fownu18 == 7 & hwsval<= 38668 - 7000) |
		 (h_numper == 9 & fownu18 == 1 & hwsval<= 52747 - 1000) |
			(h_numper == 9 & fownu18 == 2 & hwsval<= 52046 - 2000) |
		 (h_numper == 9 & fownu18 == 3 & hwsval<= 51457 - 3000) |
		 (h_numper == 9 & fownu18 == 4 & hwsval<= 50490 - 4000) |
		 (h_numper == 9 & fownu18 == 5 & hwsval<= 49159 - 5000) |
		 (h_numper == 9 & fownu18 == 6 & hwsval<= 47956 - 6000) |
		 (h_numper == 9 & fownu18 == 7 & hwsval<= 47658 - 7000) |
		 (h_numper == 9 & fownu18 == 8 & hwsval<= 45822 - 8000)) &
		fownu18 > 0 &	a_age < 19 
		#& povll < 4
		)

ychildpov_wPayment_s <-subset( 
		y , 
		((h_numper == 2 & fownu18 == 1 & hwsval<=	16337 - 1000) |
			(h_numper == 3 & fownu18 == 1 & hwsval<= 19078 - 1000) |
			(h_numper == 3 & fownu18 == 2 & hwsval<= 19096 - 2000) |
		 (h_numper == 4 & fownu18 == 1 & hwsval<= 24847 - 1000) |
			(h_numper == 4 & fownu18 == 2 & hwsval<= 24036 - 2000) |
		 (h_numper == 4 & fownu18 == 3 & hwsval<= 24120 - 3000) |
		 (h_numper == 5 & fownu18 == 1 & hwsval<= 29911 - 1000) |
		 (h_numper == 5 & fownu18 == 2 & hwsval<= 28995 - 2000) |
		 (h_numper == 5 & fownu18 == 3 & hwsval<= 28286 - 3000) |
			(h_numper == 5 & fownu18 == 4 & hwsval<= 27853 - 4000) |
		 (h_numper == 6 & fownu18 == 1 & hwsval<=	34044 - 1000) |
			(h_numper == 6 & fownu18 == 2 & hwsval<=	33342 - 2000) |
		 (h_numper == 6 & fownu18 == 3 & hwsval<=	32670 - 3000) |
		 (h_numper == 6 & fownu18 == 4 & hwsval<=	31670 - 4000) |
		 (h_numper == 6 & fownu18 == 5 & hwsval<=	31078 - 5000) |
		 (h_numper == 7 & fownu18 == 1 & hwsval<=	39260 - 1000) |
		 (h_numper == 7 & fownu18 == 2 & hwsval<= 38421 - 2000) |
			(h_numper == 7 & fownu18 == 3 & hwsval<= 37835 - 3000) |
		 (h_numper == 7 & fownu18 == 4 & hwsval<= 36745 - 4000) |
		 (h_numper == 7 & fownu18 == 5 & hwsval<= 35473 - 5000) |
		 (h_numper == 7 & fownu18 == 6 & hwsval<= 34077 - 6000) |
		 (h_numper == 8 & fownu18 == 1 & hwsval<= 44023 - 1000) |
		 (h_numper == 8 & fownu18 == 2 & hwsval<= 43230 - 2000) |
		 (h_numper == 8 & fownu18 == 3 & hwsval<= 42536 - 3000) |
		 (h_numper == 8 & fownu18 == 4 & hwsval<= 41551 - 4000) |
		 (h_numper == 8 & fownu18 == 5 & hwsval<= 40300 - 5000) |
		 (h_numper == 8 & fownu18 == 6 & hwsval<= 38999 - 6000) |
		 (h_numper == 8 & fownu18 == 7 & hwsval<= 38668 - 7000) |
		 (h_numper == 9 & fownu18 == 1 & hwsval<= 52747 - 1000) |
			(h_numper == 9 & fownu18 == 2 & hwsval<= 52046 - 2000) |
		 (h_numper == 9 & fownu18 == 3 & hwsval<= 51457 - 3000) |
		 (h_numper == 9 & fownu18 == 4 & hwsval<= 50490 - 4000) |
		 (h_numper == 9 & fownu18 == 5 & hwsval<= 49159 - 5000) |
		 (h_numper == 9 & fownu18 == 6 & hwsval<= 47956 - 6000) |
		 (h_numper == 9 & fownu18 == 7 & hwsval<= 47658 - 7000) |
		 (h_numper == 9 & fownu18 == 8 & hwsval<= 45822 - 8000)) &
		fownu6 > 0 & a_age < 6 
		#& povll < 4
		)

childpov_XTR_wPayment_s <-subset( 
		y , 
		((h_numper == 2 & fownu18 == 1 & hwsval<=	16337/2 - 1000) |
			(h_numper == 3 & fownu18 == 1 & hwsval<= 19078/2 - 1000) |
			(h_numper == 3 & fownu18 == 2 & hwsval<= 19096/2 - 2000) |
		 (h_numper == 4 & fownu18 == 1 & hwsval<= 24847/2 - 1000) |
			(h_numper == 4 & fownu18 == 2 & hwsval<= 24036/2 - 2000) |
		 (h_numper == 4 & fownu18 == 3 & hwsval<= 24120/2 - 3000) |
		 (h_numper == 5 & fownu18 == 1 & hwsval<= 29911/2 - 1000) |
		 (h_numper == 5 & fownu18 == 2 & hwsval<= 28995/2 - 2000) |
		 (h_numper == 5 & fownu18 == 3 & hwsval<= 28286/2 - 3000) |
			(h_numper == 5 & fownu18 == 4 & hwsval<= 27853/2 - 4000) |
		 (h_numper == 6 & fownu18 == 1 & hwsval<=	34044/2 - 1000) |
			(h_numper == 6 & fownu18 == 2 & hwsval<=	33342/2 - 2000) |
		 (h_numper == 6 & fownu18 == 3 & hwsval<=	32670/2 - 3000) |
		 (h_numper == 6 & fownu18 == 4 & hwsval<=	31670/2 - 4000) |
		 (h_numper == 6 & fownu18 == 5 & hwsval<=	31078/2 - 5000) |
		 (h_numper == 7 & fownu18 == 1 & hwsval<=	39260/2 - 1000) |
		 (h_numper == 7 & fownu18 == 2 & hwsval<= 38421/2 - 2000) |
			(h_numper == 7 & fownu18 == 3 & hwsval<= 37835/2 - 3000) |
		 (h_numper == 7 & fownu18 == 4 & hwsval<= 36745/2 - 4000) |
		 (h_numper == 7 & fownu18 == 5 & hwsval<= 35473/2 - 5000) |
		 (h_numper == 7 & fownu18 == 6 & hwsval<= 34077/2 - 6000) |
		 (h_numper == 8 & fownu18 == 1 & hwsval<= 44023/2 - 1000) |
		 (h_numper == 8 & fownu18 == 2 & hwsval<= 43230/2 - 2000) |
		 (h_numper == 8 & fownu18 == 3 & hwsval<= 42536/2 - 3000) |
		 (h_numper == 8 & fownu18 == 4 & hwsval<= 41551/2 - 4000) |
		 (h_numper == 8 & fownu18 == 5 & hwsval<= 40300/2 - 5000) |
		 (h_numper == 8 & fownu18 == 6 & hwsval<= 38999/2 - 6000) |
		 (h_numper == 8 & fownu18 == 7 & hwsval<= 38668/2 - 7000) |
		 (h_numper == 9 & fownu18 == 1 & hwsval<= 52747/2 - 1000) |
			(h_numper == 9 & fownu18 == 2 & hwsval<= 52046/2 - 2000) |
		 (h_numper == 9 & fownu18 == 3 & hwsval<= 51457/2 - 3000) |
		 (h_numper == 9 & fownu18 == 4 & hwsval<= 50490/2 - 4000) |
		 (h_numper == 9 & fownu18 == 5 & hwsval<= 49159/2 - 5000) |
		 (h_numper == 9 & fownu18 == 6 & hwsval<= 47956/2 - 6000) |
		 (h_numper == 9 & fownu18 == 7 & hwsval<= 47658/2 - 7000) |
		 (h_numper == 9 & fownu18 == 8 & hwsval<= 45822/2 - 8000)) &
		fownu18 > 0 &
		a_age < 19 
		#& povll < 2
	)


ychildpov_XTR_wPayment_s <-subset( 
		y , 
		((h_numper == 2 & fownu18 == 1 & hwsval<=	16337/2 - 1000) |
			(h_numper == 3 & fownu18 == 1 & hwsval<= 19078/2 - 1000) |
			(h_numper == 3 & fownu18 == 2 & hwsval<= 19096/2 - 2000) |
		 (h_numper == 4 & fownu18 == 1 & hwsval<= 24847/2 - 1000) |
			(h_numper == 4 & fownu18 == 2 & hwsval<= 24036/2 - 2000) |
		 (h_numper == 4 & fownu18 == 3 & hwsval<= 24120/2 - 3000) |
		 (h_numper == 5 & fownu18 == 1 & hwsval<= 29911/2 - 1000) |
		 (h_numper == 5 & fownu18 == 2 & hwsval<= 28995/2 - 2000) |
		 (h_numper == 5 & fownu18 == 3 & hwsval<= 28286/2 - 3000) |
			(h_numper == 5 & fownu18 == 4 & hwsval<= 27853/2 - 4000) |
		 (h_numper == 6 & fownu18 == 1 & hwsval<=	34044/2 - 1000) |
			(h_numper == 6 & fownu18 == 2 & hwsval<=	33342/2 - 2000) |
		 (h_numper == 6 & fownu18 == 3 & hwsval<=	32670/2 - 3000) |
		 (h_numper == 6 & fownu18 == 4 & hwsval<=	31670/2 - 4000) |
		 (h_numper == 6 & fownu18 == 5 & hwsval<=	31078/2 - 5000) |
		 (h_numper == 7 & fownu18 == 1 & hwsval<=	39260/2 - 1000) |
		 (h_numper == 7 & fownu18 == 2 & hwsval<= 38421/2 - 2000) |
			(h_numper == 7 & fownu18 == 3 & hwsval<= 37835/2 - 3000) |
		 (h_numper == 7 & fownu18 == 4 & hwsval<= 36745/2 - 4000) |
		 (h_numper == 7 & fownu18 == 5 & hwsval<= 35473/2 - 5000) |
		 (h_numper == 7 & fownu18 == 6 & hwsval<= 34077/2 - 6000) |
		 (h_numper == 8 & fownu18 == 1 & hwsval<= 44023/2 - 1000) |
		 (h_numper == 8 & fownu18 == 2 & hwsval<= 43230/2 - 2000) |
		 (h_numper == 8 & fownu18 == 3 & hwsval<= 42536/2 - 3000) |
		 (h_numper == 8 & fownu18 == 4 & hwsval<= 41551/2 - 4000) |
		 (h_numper == 8 & fownu18 == 5 & hwsval<= 40300/2 - 5000) |
		 (h_numper == 8 & fownu18 == 6 & hwsval<= 38999/2 - 6000) |
		 (h_numper == 8 & fownu18 == 7 & hwsval<= 38668/2 - 7000) |
		 (h_numper == 9 & fownu18 == 1 & hwsval<= 52747/2 - 1000) |
			(h_numper == 9 & fownu18 == 2 & hwsval<= 52046/2 - 2000) |
		 (h_numper == 9 & fownu18 == 3 & hwsval<= 51457/2 - 3000) |
		 (h_numper == 9 & fownu18 == 4 & hwsval<= 50490/2 - 4000) |
		 (h_numper == 9 & fownu18 == 5 & hwsval<= 49159/2 - 5000) |
		 (h_numper == 9 & fownu18 == 6 & hwsval<= 47956/2 - 6000) |
		 (h_numper == 9 & fownu18 == 7 & hwsval<= 47658/2 - 7000) |
		 (h_numper == 9 & fownu18 == 8 & hwsval<= 45822/2 - 8000)) &
		fownu6 > 0 & a_age < 6 
		#& povll < 2
		)


svytotal(
	~one ,
	ychildpov)

svytotal(
	~one ,
	ychildpov_wPayment)

svytotal(
	~one ,
	childpov)

svytotal(
	~one ,
	childpov_wPayment)

svytotal(
	~one ,
	childpov_XTR)

svytotal(
	~one ,
	ychildpov_XTR)

svytotal(
	~one ,
	childpov_XTR_wPayment)

svytotal(
	~one ,
	ychildpov_XTR_wPayment)


svytotal(
	~one ,
	childpov_wPayment_s)

svytotal(
	~one ,
	ychildpov_wPayment_s)

svytotal(
	~one ,
	childpov_XTR_wPayment_s)

svytotal(
	~one ,
	ychildpov_XTR_wPayment_s)


#fownu18 -Children under 18
#H_NUMPER -number of persons in houshol

poverty_thresh15 <-
	subset( 
		y ,
		a_age < 15 &
		povll < 4
	)
