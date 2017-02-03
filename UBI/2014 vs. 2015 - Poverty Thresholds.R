#Poverty Thresholds
#2014 = 2015
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


