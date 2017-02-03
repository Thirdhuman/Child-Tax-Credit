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
		 (h_numper == 9 & fownu18 == 8 & hwsval<= 45822/2)) & fownu18 >= 0 & a_age < 18  
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
		 (h_numper == 9 & fownu18 == 8 & hwsval<= 45822/2)) & fownu6 > 0 & a_age < 5 
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
		 (h_numper == 9 & fownu18 == 8 & hwsval<= 45822)) & fownu18 > 0 & a_age < 18  
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
		 (h_numper == 9 & fownu18 == 8 & hwsval<= 45822)) & fownu6 > 0 &	a_age < 5 
	#& povll < 4
	)


childpov_wPayment <-subset( 
		y , 
		((h_numper == 2 & fownu18 == 1 & hwsval<=	16337 - 4000/2) |
			(h_numper == 3 & fownu18 == 1 & hwsval<= 19078 - 4000/2) |
			(h_numper == 3 & fownu18 == 2 & hwsval<= 19096 - 8000/2) |
		 (h_numper == 4 & fownu18 == 1 & hwsval<= 24847 - 4000/2) |
			(h_numper == 4 & fownu18 == 2 & hwsval<= 24036 - 8000/2) |
		 (h_numper == 4 & fownu18 == 3 & hwsval<= 24120 -12000/2) |
		 (h_numper == 5 & fownu18 == 1 & hwsval<= 29911 - 4000/2) |
		 (h_numper == 5 & fownu18 == 2 & hwsval<= 28995 - 8000/2) |
		 (h_numper == 5 & fownu18 == 3 & hwsval<= 28286 -12000/2) |
			(h_numper == 5 & fownu18 == 4 & hwsval<= 27853 -16000/2) |
		 (h_numper == 6 & fownu18 == 1 & hwsval<=	34044 - 4000/2) |
			(h_numper == 6 & fownu18 == 2 & hwsval<=	33342 - 8000/2) |
		 (h_numper == 6 & fownu18 == 3 & hwsval<=	32670 -12000/2) |
		 (h_numper == 6 & fownu18 == 4 & hwsval<=	31670 -16000/2) |
		 (h_numper == 6 & fownu18 == 5 & hwsval<=	31078 -20000/2) |
		 (h_numper == 7 & fownu18 == 1 & hwsval<=	39260 - 4000/2) |
		 (h_numper == 7 & fownu18 == 2 & hwsval<= 38421 - 8000/2) |
			(h_numper == 7 & fownu18 == 3 & hwsval<= 37835 -12000/2) |
		 (h_numper == 7 & fownu18 == 4 & hwsval<= 36745 -16000/2) |
		 (h_numper == 7 & fownu18 == 5 & hwsval<= 35473 -20000/2) |
		 (h_numper == 7 & fownu18 == 6 & hwsval<= 34077 - 24000/2) |
		 (h_numper == 8 & fownu18 == 1 & hwsval<= 44023 - 4000/2) |
		 (h_numper == 8 & fownu18 == 2 & hwsval<= 43230 - 8000/2) |
		 (h_numper == 8 & fownu18 == 3 & hwsval<= 42536 -12000/2) |
		 (h_numper == 8 & fownu18 == 4 & hwsval<= 41551 -16000/2) |
		 (h_numper == 8 & fownu18 == 5 & hwsval<= 40300 -20000/2) |
		 (h_numper == 8 & fownu18 == 6 & hwsval<= 38999 - 24000/2) |
		 (h_numper == 8 & fownu18 == 7 & hwsval<= 38668 - 28000/2) |
		 (h_numper == 9 & fownu18 == 1 & hwsval<= 52747 - 4000/2) |
			(h_numper == 9 & fownu18 == 2 & hwsval<= 52046 - 8000/2) |
		 (h_numper == 9 & fownu18 == 3 & hwsval<= 51457 -12000/2) |
		 (h_numper == 9 & fownu18 == 4 & hwsval<= 50490 -16000/2) |
		 (h_numper == 9 & fownu18 == 5 & hwsval<= 49159 -20000/2) |
		 (h_numper == 9 & fownu18 == 6 & hwsval<= 47956 - 24000/2) |
		 (h_numper == 9 & fownu18 == 7 & hwsval<= 47658 - 28000/2) |
		 (h_numper == 9 & fownu18 == 8 & hwsval<= 45822 - 32000/2)) &
		fownu18 > 0 &
		a_age < 18  
		#& povll < 4
		)


ychildpov_wPayment <-subset( 
		y , 
		((h_numper == 2 & fownu18 == 1 & hwsval<=	16337 - 4000/2) |
			(h_numper == 3 & fownu18 == 1 & hwsval<= 19078 - 4000 ) |
			(h_numper == 3 & fownu18 == 2 & hwsval<= 19096 - 8000/2) |
		 (h_numper == 4 & fownu18 == 1 & hwsval<= 24847 - 4000/2) |
			(h_numper == 4 & fownu18 == 2 & hwsval<= 24036 - 8000/2) |
		 (h_numper == 4 & fownu18 == 3 & hwsval<= 24120 -12000/2) |
		 (h_numper == 5 & fownu18 == 1 & hwsval<= 29911 - 4000/2) |
		 (h_numper == 5 & fownu18 == 2 & hwsval<= 28995 - 8000/2) |
		 (h_numper == 5 & fownu18 == 3 & hwsval<= 28286 -12000/2) |
			(h_numper == 5 & fownu18 == 4 & hwsval<= 27853 -16000/2) |
		 (h_numper == 6 & fownu18 == 1 & hwsval<=	34044 - 4000/2) |
			(h_numper == 6 & fownu18 == 2 & hwsval<=	33342 - 8000/2) |
		 (h_numper == 6 & fownu18 == 3 & hwsval<=	32670 -12000/2) |
		 (h_numper == 6 & fownu18 == 4 & hwsval<=	31670 -16000/2) |
		 (h_numper == 6 & fownu18 == 5 & hwsval<=	31078 -20000/2) |
		 (h_numper == 7 & fownu18 == 1 & hwsval<=	39260 - 4000/2) |
		 (h_numper == 7 & fownu18 == 2 & hwsval<= 38421 - 8000/2) |
			(h_numper == 7 & fownu18 == 3 & hwsval<= 37835 -12000/2) |
		 (h_numper == 7 & fownu18 == 4 & hwsval<= 36745 -16000/2) |
		 (h_numper == 7 & fownu18 == 5 & hwsval<= 35473 -20000/2) |
		 (h_numper == 7 & fownu18 == 6 & hwsval<= 34077 - 24000/2) |
		 (h_numper == 8 & fownu18 == 1 & hwsval<= 44023 - 4000/2) |
		 (h_numper == 8 & fownu18 == 2 & hwsval<= 43230 - 8000/2) |
		 (h_numper == 8 & fownu18 == 3 & hwsval<= 42536 -12000/2) |
		 (h_numper == 8 & fownu18 == 4 & hwsval<= 41551 -16000/2) |
		 (h_numper == 8 & fownu18 == 5 & hwsval<= 40300 -20000/2) |
		 (h_numper == 8 & fownu18 == 6 & hwsval<= 38999 - 24000/2) |
		 (h_numper == 8 & fownu18 == 7 & hwsval<= 38668 - 28000/2) |
		 (h_numper == 9 & fownu18 == 1 & hwsval<= 52747 - 4000/2) |
			(h_numper == 9 & fownu18 == 2 & hwsval<= 52046 - 8000/2) |
		 (h_numper == 9 & fownu18 == 3 & hwsval<= 51457 -12000/2) |
		 (h_numper == 9 & fownu18 == 4 & hwsval<= 50490 -16000/2) |
		 (h_numper == 9 & fownu18 == 5 & hwsval<= 49159 -20000/2) |
		 (h_numper == 9 & fownu18 == 6 & hwsval<= 47956 - 24000/2) |
		 (h_numper == 9 & fownu18 == 7 & hwsval<= 47658 - 28000/2) |
		 (h_numper == 9 & fownu18 == 8 & hwsval<= 45822 - 32000/2)) &
		fownu6 > 0 &	a_age < 5 
		#&	povll < 4
		)

childpov_XTR_wPayment <-subset( 
		y , 
		((h_numper == 2 & fownu18 == 1 & hwsval<=	16337/2 - 4000/2) |
			(h_numper == 3 & fownu18 == 1 & hwsval<= 19078/2 - 4000/2) |
			(h_numper == 3 & fownu18 == 2 & hwsval<= 19096/2 - 8000/2) |
		 (h_numper == 4 & fownu18 == 1 & hwsval<= 24847/2 - 4000/2) |
			(h_numper == 4 & fownu18 == 2 & hwsval<= 24036/2 - 8000/2) |
		 (h_numper == 4 & fownu18 == 3 & hwsval<= 24120/2 -12000/2) |
		 (h_numper == 5 & fownu18 == 1 & hwsval<= 29911/2 - 4000/2) |
		 (h_numper == 5 & fownu18 == 2 & hwsval<= 28995/2 - 8000/2) |
		 (h_numper == 5 & fownu18 == 3 & hwsval<= 28286/2 -12000/2) |
			(h_numper == 5 & fownu18 == 4 & hwsval<= 27853/2 -16000/2) |
		 (h_numper == 6 & fownu18 == 1 & hwsval<=	34044/2 - 4000/2) |
			(h_numper == 6 & fownu18 == 2 & hwsval<=	33342/2 - 8000/2) |
		 (h_numper == 6 & fownu18 == 3 & hwsval<=	32670/2 -12000/2) |
		 (h_numper == 6 & fownu18 == 4 & hwsval<=	31670/2 -16000/2) |
		 (h_numper == 6 & fownu18 == 5 & hwsval<=	31078/2 -20000/2) |
		 (h_numper == 7 & fownu18 == 1 & hwsval<=	39260/2 - 4000/2) |
		 (h_numper == 7 & fownu18 == 2 & hwsval<= 38421/2 - 8000/2) |
			(h_numper == 7 & fownu18 == 3 & hwsval<= 37835/2 -12000/2) |
		 (h_numper == 7 & fownu18 == 4 & hwsval<= 36745/2 -16000/2) |
		 (h_numper == 7 & fownu18 == 5 & hwsval<= 35473/2 -20000/2) |
		 (h_numper == 7 & fownu18 == 6 & hwsval<= 34077/2 - 24000/2) |
		 (h_numper == 8 & fownu18 == 1 & hwsval<= 44023/2 - 4000/2) |
		 (h_numper == 8 & fownu18 == 2 & hwsval<= 43230/2 - 8000/2) |
		 (h_numper == 8 & fownu18 == 3 & hwsval<= 42536/2 -12000/2) |
		 (h_numper == 8 & fownu18 == 4 & hwsval<= 41551/2 -16000/2) |
		 (h_numper == 8 & fownu18 == 5 & hwsval<= 40300/2 -20000/2) |
		 (h_numper == 8 & fownu18 == 6 & hwsval<= 38999/2 - 24000/2) |
		 (h_numper == 8 & fownu18 == 7 & hwsval<= 38668/2 - 28000/2) |
		 (h_numper == 9 & fownu18 == 1 & hwsval<= 52747/2 - 4000/2) |
			(h_numper == 9 & fownu18 == 2 & hwsval<= 52046/2 - 8000/2) |
		 (h_numper == 9 & fownu18 == 3 & hwsval<= 51457/2 -12000/2) |
		 (h_numper == 9 & fownu18 == 4 & hwsval<= 50490/2 -16000/2) |
		 (h_numper == 9 & fownu18 == 5 & hwsval<= 49159/2 -20000/2) |
		 (h_numper == 9 & fownu18 == 6 & hwsval<= 47956/2 - 24000/2) |
		 (h_numper == 9 & fownu18 == 7 & hwsval<= 47658/2 - 28000/2) |
		 (h_numper == 9 & fownu18 == 8 & hwsval<= 45822/2 - 32000/2)) & fownu18 > 0 &	a_age < 18  
		#& povll < 2
		)

ychildpov_XTR_wPayment <-subset( 
		y , 
		((h_numper == 2 & fownu18 == 1 & hwsval<=	16337/2 - 4000/2) |
			(h_numper == 3 & fownu18 == 1 & hwsval<= 19078/2 - 4000 ) |
			(h_numper == 3 & fownu18 == 2 & hwsval<= 19096/2 - 8000/2) |
		 (h_numper == 4 & fownu18 == 1 & hwsval<= 24847/2 - 4000/2) |
			(h_numper == 4 & fownu18 == 2 & hwsval<= 24036/2 - 8000/2) |
		 (h_numper == 4 & fownu18 == 3 & hwsval<= 24120/2 - 12000/2) |
		 (h_numper == 5 & fownu18 == 1 & hwsval<= 29911/2 - 4000/2) |
		 (h_numper == 5 & fownu18 == 2 & hwsval<= 28995/2 - 8000/2) |
		 (h_numper == 5 & fownu18 == 3 & hwsval<= 28286/2 - 12000/2) |
			(h_numper == 5 & fownu18 == 4 & hwsval<= 27853/2 - 16000/2) |
		 (h_numper == 6 & fownu18 == 1 & hwsval<=	34044/2 - 4000/2) |
			(h_numper == 6 & fownu18 == 2 & hwsval<=	33342/2 - 8000/2) |
		 (h_numper == 6 & fownu18 == 3 & hwsval<=	32670/2 - 12000/2) |
		 (h_numper == 6 & fownu18 == 4 & hwsval<=	31670/2 - 16000/2) |
		 (h_numper == 6 & fownu18 == 5 & hwsval<=	31078/2 - 20000/2) |
		 (h_numper == 7 & fownu18 == 1 & hwsval<=	39260/2 - 4000/2) |
		 (h_numper == 7 & fownu18 == 2 & hwsval<= 38421/2 - 8000/2) |
			(h_numper == 7 & fownu18 == 3 & hwsval<= 37835/2 - 12000/2) |
		 (h_numper == 7 & fownu18 == 4 & hwsval<= 36745/2 - 16000/2) |
		 (h_numper == 7 & fownu18 == 5 & hwsval<= 35473/2 - 20000/2) |
		 (h_numper == 7 & fownu18 == 6 & hwsval<= 34077/2 - 24000/2) |
		 (h_numper == 8 & fownu18 == 1 & hwsval<= 44023/2 - 4000/2) |
		 (h_numper == 8 & fownu18 == 2 & hwsval<= 43230/2 - 8000/2) |
		 (h_numper == 8 & fownu18 == 3 & hwsval<= 42536/2 - 12000/2) |
		 (h_numper == 8 & fownu18 == 4 & hwsval<= 41551/2 - 16000/2) |
		 (h_numper == 8 & fownu18 == 5 & hwsval<= 40300/2 - 20000/2) |
		 (h_numper == 8 & fownu18 == 6 & hwsval<= 38999/2 - 24000/2) |
		 (h_numper == 8 & fownu18 == 7 & hwsval<= 38668/2 - 28000/2) |
		 (h_numper == 9 & fownu18 == 1 & hwsval<= 52747/2 - 4000/2) |
			(h_numper == 9 & fownu18 == 2 & hwsval<= 52046/2 - 8000/2) |
		 (h_numper == 9 & fownu18 == 3 & hwsval<= 51457/2 - 12000/2) |
		 (h_numper == 9 & fownu18 == 4 & hwsval<= 50490/2 - 16000/2) |
		 (h_numper == 9 & fownu18 == 5 & hwsval<= 49159/2 - 20000/2) |
		 (h_numper == 9 & fownu18 == 6 & hwsval<= 47956/2 - 24000/2) |
		 (h_numper == 9 & fownu18 == 7 & hwsval<= 47658/2 - 28000/2) |
		 (h_numper == 9 & fownu18 == 8 & hwsval<= 45822/2 - 32000/2)) & fownu6 > 0 &	a_age < 5 
			#& povll < 2
		)

childpov_wPayment_s <-subset( 
		y , 
		((h_numper == 2 & fownu18 == 1 & hwsval<=	16337 - 2000/2) |
			(h_numper == 3 & fownu18 == 1 & hwsval<= 19078 - 2000/2) |
			(h_numper == 3 & fownu18 == 2 & hwsval<= 19096 - 4000/2) |
		 (h_numper == 4 & fownu18 == 1 & hwsval<= 24847 - 2000/2) |
			(h_numper == 4 & fownu18 == 2 & hwsval<= 24036 - 4000/2) |
		 (h_numper == 4 & fownu18 == 3 & hwsval<= 24120 - 6000/2) |
		 (h_numper == 5 & fownu18 == 1 & hwsval<= 29911 - 2000/2) |
		 (h_numper == 5 & fownu18 == 2 & hwsval<= 28995 - 4000/2) |
		 (h_numper == 5 & fownu18 == 3 & hwsval<= 28286 - 6000/2) |
			(h_numper == 5 & fownu18 == 4 & hwsval<= 27853 - 8000/2) |
		 (h_numper == 6 & fownu18 == 1 & hwsval<=	34044 - 2000/2) |
			(h_numper == 6 & fownu18 == 2 & hwsval<=	33342 - 4000/2) |
		 (h_numper == 6 & fownu18 == 3 & hwsval<=	32670 - 6000/2) |
		 (h_numper == 6 & fownu18 == 4 & hwsval<=	31670 - 8000/2) |
		 (h_numper == 6 & fownu18 == 5 & hwsval<=	31078 - 10000/2) |
		 (h_numper == 7 & fownu18 == 1 & hwsval<=	39260 - 2000/2) |
		 (h_numper == 7 & fownu18 == 2 & hwsval<= 38421 - 4000/2) |
			(h_numper == 7 & fownu18 == 3 & hwsval<= 37835 - 6000/2) |
		 (h_numper == 7 & fownu18 == 4 & hwsval<= 36745 - 8000/2) |
		 (h_numper == 7 & fownu18 == 5 & hwsval<= 35473 - 10000/2) |
		 (h_numper == 7 & fownu18 == 6 & hwsval<= 34077 - 8000/2) |
		 (h_numper == 8 & fownu18 == 1 & hwsval<= 44023 - 2000/2) |
		 (h_numper == 8 & fownu18 == 2 & hwsval<= 43230 - 4000/2) |
		 (h_numper == 8 & fownu18 == 3 & hwsval<= 42536 - 6000/2) |
		 (h_numper == 8 & fownu18 == 4 & hwsval<= 41551 - 8000/2) |
		 (h_numper == 8 & fownu18 == 5 & hwsval<= 40300 - 10000/2) |
		 (h_numper == 8 & fownu18 == 6 & hwsval<= 38999 - 8000/2) |
		 (h_numper == 8 & fownu18 == 7 & hwsval<= 38668 - 14000/2) |
		 (h_numper == 9 & fownu18 == 1 & hwsval<= 52747 - 2000/2) |
			(h_numper == 9 & fownu18 == 2 & hwsval<= 52046 - 4000/2) |
		 (h_numper == 9 & fownu18 == 3 & hwsval<= 51457 - 6000/2) |
		 (h_numper == 9 & fownu18 == 4 & hwsval<= 50490 - 8000/2) |
		 (h_numper == 9 & fownu18 == 5 & hwsval<= 49159 - 10000/2) |
		 (h_numper == 9 & fownu18 == 6 & hwsval<= 47956 - 12000/2) |
		 (h_numper == 9 & fownu18 == 7 & hwsval<= 47658 - 14000/2) |
		 (h_numper == 9 & fownu18 == 8 & hwsval<= 45822 - 16000/2)) &
		fownu18 > 0 &	a_age < 18  
		#& povll < 4
		)

ychildpov_wPayment_s <-subset( 
		y , 
		((h_numper == 2 & fownu18 == 1 & hwsval<=	16337 - 2000/2) |
			(h_numper == 3 & fownu18 == 1 & hwsval<= 19078 - 2000/2) |
			(h_numper == 3 & fownu18 == 2 & hwsval<= 19096 - 4000/2) |
		 (h_numper == 4 & fownu18 == 1 & hwsval<= 24847 - 2000/2) |
			(h_numper == 4 & fownu18 == 2 & hwsval<= 24036 - 4000/2) |
		 (h_numper == 4 & fownu18 == 3 & hwsval<= 24120 - 6000/2) |
		 (h_numper == 5 & fownu18 == 1 & hwsval<= 29911 - 2000/2) |
		 (h_numper == 5 & fownu18 == 2 & hwsval<= 28995 - 4000/2) |
		 (h_numper == 5 & fownu18 == 3 & hwsval<= 28286 - 6000/2) |
			(h_numper == 5 & fownu18 == 4 & hwsval<= 27853 - 8000/2) |
		 (h_numper == 6 & fownu18 == 1 & hwsval<=	34044 - 2000/2) |
			(h_numper == 6 & fownu18 == 2 & hwsval<=	33342 - 4000/2) |
		 (h_numper == 6 & fownu18 == 3 & hwsval<=	32670 - 6000/2) |
		 (h_numper == 6 & fownu18 == 4 & hwsval<=	31670 - 8000/2) |
		 (h_numper == 6 & fownu18 == 5 & hwsval<=	31078 - 10000/2) |
		 (h_numper == 7 & fownu18 == 1 & hwsval<=	39260 - 2000/2) |
		 (h_numper == 7 & fownu18 == 2 & hwsval<= 38421 - 4000/2) |
			(h_numper == 7 & fownu18 == 3 & hwsval<= 37835 - 6000/2) |
		 (h_numper == 7 & fownu18 == 4 & hwsval<= 36745 - 8000/2) |
		 (h_numper == 7 & fownu18 == 5 & hwsval<= 35473 - 10000/2) |
		 (h_numper == 7 & fownu18 == 6 & hwsval<= 34077 - 12000/2) |
		 (h_numper == 8 & fownu18 == 1 & hwsval<= 44023 - 2000/2) |
		 (h_numper == 8 & fownu18 == 2 & hwsval<= 43230 - 4000/2) |
		 (h_numper == 8 & fownu18 == 3 & hwsval<= 42536 - 6000/2) |
		 (h_numper == 8 & fownu18 == 4 & hwsval<= 41551 - 8000/2) |
		 (h_numper == 8 & fownu18 == 5 & hwsval<= 40300 - 10000/2) |
		 (h_numper == 8 & fownu18 == 6 & hwsval<= 38999 - 12000/2) |
		 (h_numper == 8 & fownu18 == 7 & hwsval<= 38668 - 14000/2) |
		 (h_numper == 9 & fownu18 == 1 & hwsval<= 52747 - 2000/2) |
			(h_numper == 9 & fownu18 == 2 & hwsval<= 52046 - 4000/2) |
		 (h_numper == 9 & fownu18 == 3 & hwsval<= 51457 - 6000/2) |
		 (h_numper == 9 & fownu18 == 4 & hwsval<= 50490 - 8000/2) |
		 (h_numper == 9 & fownu18 == 5 & hwsval<= 49159 - 10000/2) |
		 (h_numper == 9 & fownu18 == 6 & hwsval<= 47956 - 12000/2) |
		 (h_numper == 9 & fownu18 == 7 & hwsval<= 47658 - 14000/2) |
		 (h_numper == 9 & fownu18 == 8 & hwsval<= 45822 - 16000/2)) &
		fownu6 > 0 & a_age < 5 
		#& povll < 4
		)

childpov_XTR_wPayment_s <-subset( 
		y , 
		((h_numper == 2 & fownu18 == 1 & hwsval<=	16337/2 - 2000/2) |
			(h_numper == 3 & fownu18 == 1 & hwsval<= 19078/2 - 2000/2) |
			(h_numper == 3 & fownu18 == 2 & hwsval<= 19096/2 - 4000/2) |
		 (h_numper == 4 & fownu18 == 1 & hwsval<= 24847/2 - 2000/2) |
			(h_numper == 4 & fownu18 == 2 & hwsval<= 24036/2 - 4000/2) |
		 (h_numper == 4 & fownu18 == 3 & hwsval<= 24120/2 - 6000/2) |
		 (h_numper == 5 & fownu18 == 1 & hwsval<= 29911/2 - 2000/2) |
		 (h_numper == 5 & fownu18 == 2 & hwsval<= 28995/2 - 4000/2) |
		 (h_numper == 5 & fownu18 == 3 & hwsval<= 28286/2 - 6000/2) |
			(h_numper == 5 & fownu18 == 4 & hwsval<= 27853/2 - 8000/2) |
		 (h_numper == 6 & fownu18 == 1 & hwsval<=	34044/2 - 2000/2) |
			(h_numper == 6 & fownu18 == 2 & hwsval<=	33342/2 - 4000/2) |
		 (h_numper == 6 & fownu18 == 3 & hwsval<=	32670/2 - 6000/2) |
		 (h_numper == 6 & fownu18 == 4 & hwsval<=	31670/2 - 8000/2) |
		 (h_numper == 6 & fownu18 == 5 & hwsval<=	31078/2 - 10000/2) |
		 (h_numper == 7 & fownu18 == 1 & hwsval<=	39260/2 - 2000/2) |
		 (h_numper == 7 & fownu18 == 2 & hwsval<= 38421/2 - 4000/2) |
			(h_numper == 7 & fownu18 == 3 & hwsval<= 37835/2 - 6000/2) |
		 (h_numper == 7 & fownu18 == 4 & hwsval<= 36745/2 - 8000/2) |
		 (h_numper == 7 & fownu18 == 5 & hwsval<= 35473/2 - 10000/2) |
		 (h_numper == 7 & fownu18 == 6 & hwsval<= 34077/2 - 12000/2) |
		 (h_numper == 8 & fownu18 == 1 & hwsval<= 44023/2 - 2000/2) |
		 (h_numper == 8 & fownu18 == 2 & hwsval<= 43230/2 - 4000/2) |
		 (h_numper == 8 & fownu18 == 3 & hwsval<= 42536/2 - 6000/2) |
		 (h_numper == 8 & fownu18 == 4 & hwsval<= 41551/2 - 8000/2) |
		 (h_numper == 8 & fownu18 == 5 & hwsval<= 40300/2 - 10000/2) |
		 (h_numper == 8 & fownu18 == 6 & hwsval<= 38999/2 - 12000/2) |
		 (h_numper == 8 & fownu18 == 7 & hwsval<= 38668/2 - 14000/2) |
		 (h_numper == 9 & fownu18 == 1 & hwsval<= 52747/2 - 2000/2) |
			(h_numper == 9 & fownu18 == 2 & hwsval<= 52046/2 - 4000/2) |
		 (h_numper == 9 & fownu18 == 3 & hwsval<= 51457/2 - 6000/2) |
		 (h_numper == 9 & fownu18 == 4 & hwsval<= 50490/2 - 8000/2) |
		 (h_numper == 9 & fownu18 == 5 & hwsval<= 49159/2 - 10000/2) |
		 (h_numper == 9 & fownu18 == 6 & hwsval<= 47956/2 - 12000/2) |
		 (h_numper == 9 & fownu18 == 7 & hwsval<= 47658/2 - 14000/2) |
		 (h_numper == 9 & fownu18 == 8 & hwsval<= 45822/2 - 16000/2)) &
		fownu18 > 0 &
		a_age < 18 
		#& povll < 2
	)


ychildpov_XTR_wPayment_s <-subset( 
		y , 
		((h_numper == 2 & fownu18 == 1 & hwsval<=	16337/2 - 2000/2) |
			(h_numper == 3 & fownu18 == 1 & hwsval<= 19078/2 - 2000/2) |
			(h_numper == 3 & fownu18 == 2 & hwsval<= 19096/2 - 4000/2) |
		 (h_numper == 4 & fownu18 == 1 & hwsval<= 24847/2 - 2000/2) |
			(h_numper == 4 & fownu18 == 2 & hwsval<= 24036/2 - 4000/2) |
		 (h_numper == 4 & fownu18 == 3 & hwsval<= 24120/2 - 6000/2) |
		 (h_numper == 5 & fownu18 == 1 & hwsval<= 29911/2 - 2000/2) |
		 (h_numper == 5 & fownu18 == 2 & hwsval<= 28995/2 - 4000/2) |
		 (h_numper == 5 & fownu18 == 3 & hwsval<= 28286/2 - 6000/2) |
			(h_numper == 5 & fownu18 == 4 & hwsval<= 27853/2 - 8000/2) |
		 (h_numper == 6 & fownu18 == 1 & hwsval<=	34044/2 - 2000/2) |
			(h_numper == 6 & fownu18 == 2 & hwsval<=	33342/2 - 4000/2) |
		 (h_numper == 6 & fownu18 == 3 & hwsval<=	32670/2 - 6000/2) |
		 (h_numper == 6 & fownu18 == 4 & hwsval<=	31670/2 - 8000/2) |
		 (h_numper == 6 & fownu18 == 5 & hwsval<=	31078/2 - 10000/2) |
		 (h_numper == 7 & fownu18 == 1 & hwsval<=	39260/2 - 2000/2) |
		 (h_numper == 7 & fownu18 == 2 & hwsval<= 38421/2 - 4000/2) |
			(h_numper == 7 & fownu18 == 3 & hwsval<= 37835/2 - 6000/2) |
		 (h_numper == 7 & fownu18 == 4 & hwsval<= 36745/2 - 8000/2) |
		 (h_numper == 7 & fownu18 == 5 & hwsval<= 35473/2 - 10000/2) |
		 (h_numper == 7 & fownu18 == 6 & hwsval<= 34077/2 - 12000/2) |
		 (h_numper == 8 & fownu18 == 1 & hwsval<= 44023/2 - 2000/2) |
		 (h_numper == 8 & fownu18 == 2 & hwsval<= 43230/2 - 4000/2) |
		 (h_numper == 8 & fownu18 == 3 & hwsval<= 42536/2 - 6000/2) |
		 (h_numper == 8 & fownu18 == 4 & hwsval<= 41551/2 - 8000/2) |
		 (h_numper == 8 & fownu18 == 5 & hwsval<= 40300/2 - 10000/2) |
		 (h_numper == 8 & fownu18 == 6 & hwsval<= 38999/2 - 12000/2) |
		 (h_numper == 8 & fownu18 == 7 & hwsval<= 38668/2 - 14000/2) |
		 (h_numper == 9 & fownu18 == 1 & hwsval<= 52747/2 - 2000/2) |
			(h_numper == 9 & fownu18 == 2 & hwsval<= 52046/2 - 4000/2) |
		 (h_numper == 9 & fownu18 == 3 & hwsval<= 51457/2 - 6000/2) |
		 (h_numper == 9 & fownu18 == 4 & hwsval<= 50490/2 - 8000/2) |
		 (h_numper == 9 & fownu18 == 5 & hwsval<= 49159/2 - 10000/2) |
		 (h_numper == 9 & fownu18 == 6 & hwsval<= 47956/2 - 12000/2) |
		 (h_numper == 9 & fownu18 == 7 & hwsval<= 47658/2 - 14000/2) |
		 (h_numper == 9 & fownu18 == 8 & hwsval<= 45822/2 - 16000/2)) &
		fownu6 > 0 & a_age < 5 
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
	~one,
	ychildpov_XTR_wPayment_s)


#fownu18 -Children under 18
#H_NUMPER -number of persons in houshol

poverty_thresh15 <-
	subset( 
		y ,
		a_age < 15 &
		povll < 4
	)
