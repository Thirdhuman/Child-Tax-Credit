Poverty_Adults <-subset( 
		y , 
		 ((h_numper == 1 & hwsval<=	12331) |
			(h_numper == 2 & fownu18 == 0 & hwsval<=	15871) |
			(h_numper == 2 & fownu18 == 1 & hwsval<=	16337) |
			(h_numper == 3 & fownu18 == 0 & hwsval<= 18540) |
			(h_numper == 3 & fownu18 == 1 & hwsval<= 19078) |
			(h_numper == 3 & fownu18 == 2 & hwsval<= 19096) |
			(h_numper == 4 & fownu18 == 0 & hwsval<= 24447) |
		 (h_numper == 4 & fownu18 == 1 & hwsval<= 24847) |
			(h_numper == 4 & fownu18 == 2 & hwsval<= 24036) |
		 (h_numper == 4 & fownu18 == 3 & hwsval<= 24120) |
		 (h_numper == 5 & fownu18 == 0 & hwsval<= 29482) |
			(h_numper == 5 & fownu18 == 1 & hwsval<= 29911) |
		 (h_numper == 5 & fownu18 == 2 & hwsval<= 28995) |
		 (h_numper == 5 & fownu18 == 3 & hwsval<= 28286) |
			(h_numper == 5 & fownu18 == 4 & hwsval<= 27853) |
		 (h_numper == 6 & fownu18 == 0 & hwsval<=	33909) |
			(h_numper == 6 & fownu18 == 1 & hwsval<=	34044) |
			(h_numper == 6 & fownu18 == 2 & hwsval<=	33342) |
		 (h_numper == 6 & fownu18 == 3 & hwsval<=	32670) |
		 (h_numper == 6 & fownu18 == 4 & hwsval<=	31670) |
		 (h_numper == 6 & fownu18 == 5 & hwsval<=	31078) |
		 (h_numper == 7 & fownu18 == 0 & hwsval<=	39017) |
			(h_numper == 7 & fownu18 == 1 & hwsval<=	39260) |
		 (h_numper == 7 & fownu18 == 2 & hwsval<= 38421) |
			(h_numper == 7 & fownu18 == 3 & hwsval<= 37835) |
		 (h_numper == 7 & fownu18 == 4 & hwsval<= 36745) |
		 (h_numper == 7 & fownu18 == 5 & hwsval<= 35473) |
		 (h_numper == 7 & fownu18 == 6 & hwsval<= 34077) |
		 (h_numper == 8 & fownu18 == 0 & hwsval<= 43637) |
			(h_numper == 8 & fownu18 == 1 & hwsval<= 44023) |
		 (h_numper == 8 & fownu18 == 2 & hwsval<= 43230) |
		 (h_numper == 8 & fownu18 == 3 & hwsval<= 42536) |
		 (h_numper == 8 & fownu18 == 4 & hwsval<= 41551) |
		 (h_numper == 8 & fownu18 == 5 & hwsval<= 40300) |
		 (h_numper == 8 & fownu18 == 6 & hwsval<= 38999) |
		 (h_numper == 8 & fownu18 == 7 & hwsval<= 38668) |
		 (h_numper == 9 & fownu18 == 0 & hwsval<= 52493) |
			(h_numper == 9 & fownu18 == 1 & hwsval<= 52747) |
			(h_numper == 9 & fownu18 == 2 & hwsval<= 52046) |
		 (h_numper == 9 & fownu18 == 3 & hwsval<= 51457) |
		 (h_numper == 9 & fownu18 == 4 & hwsval<= 50490) |
		 (h_numper == 9 & fownu18 == 5 & hwsval<= 49159) |
		 (h_numper == 9 & fownu18 == 6 & hwsval<= 47956) |
		 (h_numper == 9 & fownu18 == 7 & hwsval<= 47658) |
		 (h_numper == 9 & fownu18 == 8 & hwsval<= 45822)) &	a_age > 17 & a_age < 66)

Poverty_Adults_w1000 <-subset( 
			y , 
	  ((h_numper == 1 & hwsval<=	12331 -1000) |
			(h_numper == 2 & fownu18 == 0 & hwsval<=	15871 -1000) |
			(h_numper == 2 & fownu18 == 1 & hwsval<=	16337 -1000) |
			(h_numper == 3 & fownu18 == 0 & hwsval<= 18540 -1000) |
			(h_numper == 3 & fownu18 == 1 & hwsval<= 19078 -1000) |
			(h_numper == 3 & fownu18 == 2 & hwsval<= 19096 -1000) |
			(h_numper == 4 & fownu18 == 0 & hwsval<= 24447 -1000) |
		 (h_numper == 4 & fownu18 == 1 & hwsval<= 24847 -1000) |
			(h_numper == 4 & fownu18 == 2 & hwsval<= 24036 -1000) |
		 (h_numper == 4 & fownu18 == 3 & hwsval<= 24120 -1000) |
		 (h_numper == 5 & fownu18 == 0 & hwsval<= 29482 -1000) |
			(h_numper == 5 & fownu18 == 1 & hwsval<= 29911 -1000) |
		 (h_numper == 5 & fownu18 == 2 & hwsval<= 28995 -1000) |
		 (h_numper == 5 & fownu18 == 3 & hwsval<= 28286 -1000) |
			(h_numper == 5 & fownu18 == 4 & hwsval<= 27853 -1000) |
		 (h_numper == 6 & fownu18 == 0 & hwsval<=	33909 -1000) |
			(h_numper == 6 & fownu18 == 1 & hwsval<=	34044 -1000) |
			(h_numper == 6 & fownu18 == 2 & hwsval<=	33342 -1000) |
		 (h_numper == 6 & fownu18 == 3 & hwsval<=	32670 -1000) |
		 (h_numper == 6 & fownu18 == 4 & hwsval<=	31670 -1000) |
		 (h_numper == 6 & fownu18 == 5 & hwsval<=	31078 -1000) |
		 (h_numper == 7 & fownu18 == 0 & hwsval<=	39017 -1000) |
			(h_numper == 7 & fownu18 == 1 & hwsval<=	39260 -1000) |
		 (h_numper == 7 & fownu18 == 2 & hwsval<= 38421 -1000) |
			(h_numper == 7 & fownu18 == 3 & hwsval<= 37835 -1000) |
		 (h_numper == 7 & fownu18 == 4 & hwsval<= 36745 -1000) |
		 (h_numper == 7 & fownu18 == 5 & hwsval<= 35473 -1000) |
		 (h_numper == 7 & fownu18 == 6 & hwsval<= 34077 -1000) |
		 (h_numper == 8 & fownu18 == 0 & hwsval<= 43637 -1000) |
			(h_numper == 8 & fownu18 == 1 & hwsval<= 44023 -1000) |
		 (h_numper == 8 & fownu18 == 2 & hwsval<= 43230 -1000) |
		 (h_numper == 8 & fownu18 == 3 & hwsval<= 42536 -1000) |
		 (h_numper == 8 & fownu18 == 4 & hwsval<= 41551 -1000) |
		 (h_numper == 8 & fownu18 == 5 & hwsval<= 40300 -1000) |
		 (h_numper == 8 & fownu18 == 6 & hwsval<= 38999 -1000) |
		 (h_numper == 8 & fownu18 == 7 & hwsval<= 38668 -1000) |
		 (h_numper == 9 & fownu18 == 0 & hwsval<= 52493 -1000) |
			(h_numper == 9 & fownu18 == 1 & hwsval<= 52747 -1000) |
			(h_numper == 9 & fownu18 == 2 & hwsval<= 52046 -1000) |
		 (h_numper == 9 & fownu18 == 3 & hwsval<= 51457 -1000) |
		 (h_numper == 9 & fownu18 == 4 & hwsval<= 50490 -1000) |
		 (h_numper == 9 & fownu18 == 5 & hwsval<= 49159 -1000) |
		 (h_numper == 9 & fownu18 == 6 & hwsval<= 47956 -1000) |
		 (h_numper == 9 & fownu18 == 7 & hwsval<= 47658 -1000) |
		 (h_numper == 9 & fownu18 == 8 & hwsval<= 45822 -1000)) & a_age > 17 & a_age < 66)

Poverty_Adults_w5000 <-subset( 
		y , 
		 ((h_numper == 1 & hwsval<=	12331 - 5000) |
			(h_numper == 2 & fownu18 == 0 & hwsval<=	15871 - 5000) |
			(h_numper == 2 & fownu18 == 1 & hwsval<=	16337 - 5000) |
			(h_numper == 3 & fownu18 == 0 & hwsval<= 18540 - 5000) |
			(h_numper == 3 & fownu18 == 1 & hwsval<= 19078 - 5000) |
			(h_numper == 3 & fownu18 == 2 & hwsval<= 19096 - 5000) |
			(h_numper == 4 & fownu18 == 0 & hwsval<= 24447 - 5000) |
		 (h_numper == 4 & fownu18 == 1 & hwsval<= 24847 - 5000) |
			(h_numper == 4 & fownu18 == 2 & hwsval<= 24036 - 5000) |
		 (h_numper == 4 & fownu18 == 3 & hwsval<= 24120 - 5000) |
		 (h_numper == 5 & fownu18 == 0 & hwsval<= 29482 - 5000) |
			(h_numper == 5 & fownu18 == 1 & hwsval<= 29911 - 5000) |
		 (h_numper == 5 & fownu18 == 2 & hwsval<= 28995 - 5000) |
		 (h_numper == 5 & fownu18 == 3 & hwsval<= 28286 - 5000) |
			(h_numper == 5 & fownu18 == 4 & hwsval<= 27853 - 5000) |
		 (h_numper == 6 & fownu18 == 0 & hwsval<=	33909 - 5000) |
			(h_numper == 6 & fownu18 == 1 & hwsval<=	34044 - 5000) |
			(h_numper == 6 & fownu18 == 2 & hwsval<=	33342 - 5000) |
		 (h_numper == 6 & fownu18 == 3 & hwsval<=	32670 - 5000) |
		 (h_numper == 6 & fownu18 == 4 & hwsval<=	31670 - 5000) |
		 (h_numper == 6 & fownu18 == 5 & hwsval<=	31078 - 5000) |
		 (h_numper == 7 & fownu18 == 0 & hwsval<=	39017 - 5000) |
			(h_numper == 7 & fownu18 == 1 & hwsval<=	39260 - 5000) |
		 (h_numper == 7 & fownu18 == 2 & hwsval<= 38421 - 5000) |
			(h_numper == 7 & fownu18 == 3 & hwsval<= 37835 - 5000) |
		 (h_numper == 7 & fownu18 == 4 & hwsval<= 36745 - 5000) |
		 (h_numper == 7 & fownu18 == 5 & hwsval<= 35473 - 5000) |
		 (h_numper == 7 & fownu18 == 6 & hwsval<= 34077 - 5000) |
		 (h_numper == 8 & fownu18 == 0 & hwsval<= 43637 - 5000) |
			(h_numper == 8 & fownu18 == 1 & hwsval<= 44023 - 5000) |
		 (h_numper == 8 & fownu18 == 2 & hwsval<= 43230 - 5000) |
		 (h_numper == 8 & fownu18 == 3 & hwsval<= 42536 - 5000) |
		 (h_numper == 8 & fownu18 == 4 & hwsval<= 41551 - 5000) |
		 (h_numper == 8 & fownu18 == 5 & hwsval<= 40300 - 5000) |
		 (h_numper == 8 & fownu18 == 6 & hwsval<= 38999 - 5000) |
		 (h_numper == 8 & fownu18 == 7 & hwsval<= 38668 - 5000) |
		 (h_numper == 9 & fownu18 == 0 & hwsval<= 52493 - 5000) |
			(h_numper == 9 & fownu18 == 1 & hwsval<= 52747 - 5000) |
			(h_numper == 9 & fownu18 == 2 & hwsval<= 52046 - 5000) |
		 (h_numper == 9 & fownu18 == 3 & hwsval<= 51457 - 5000) |
		 (h_numper == 9 & fownu18 == 4 & hwsval<= 50490 - 5000) |
		 (h_numper == 9 & fownu18 == 5 & hwsval<= 49159 - 5000) |
		 (h_numper == 9 & fownu18 == 6 & hwsval<= 47956 - 5000) |
		 (h_numper == 9 & fownu18 == 7 & hwsval<= 47658 - 5000) |
		 (h_numper == 9 & fownu18 == 8 & hwsval<= 45822 - 5000)) &	a_age > 17 & a_age < 66)
		
Poverty_Adults_w10000 <-subset( 
			y , 
	  ((h_numper == 1 & hwsval<=	12331 -1000) |
			(h_numper == 2 & fownu18 == 0 & hwsval<=	15871-10000) |
			(h_numper == 2 & fownu18 == 1 & hwsval<=	16337-10000) |
			(h_numper == 3 & fownu18 == 0 & hwsval<= 18540-10000) |
			(h_numper == 3 & fownu18 == 1 & hwsval<= 19078-10000) |
			(h_numper == 3 & fownu18 == 2 & hwsval<= 19096-10000) |
			(h_numper == 4 & fownu18 == 0 & hwsval<= 24447-10000) |
		 (h_numper == 4 & fownu18 == 1 & hwsval<= 24847-10000) |
			(h_numper == 4 & fownu18 == 2 & hwsval<= 24036-10000) |
		 (h_numper == 4 & fownu18 == 3 & hwsval<= 24120-10000) |
		 (h_numper == 5 & fownu18 == 0 & hwsval<= 29482-10000) |
			(h_numper == 5 & fownu18 == 1 & hwsval<= 29911-10000) |
		 (h_numper == 5 & fownu18 == 2 & hwsval<= 28995-10000) |
		 (h_numper == 5 & fownu18 == 3 & hwsval<= 28286-10000) |
			(h_numper == 5 & fownu18 == 4 & hwsval<= 27853-10000) |
		 (h_numper == 6 & fownu18 == 0 & hwsval<=	33909-10000) |
			(h_numper == 6 & fownu18 == 1 & hwsval<=	34044-10000) |
			(h_numper == 6 & fownu18 == 2 & hwsval<=	33342-10000) |
		 (h_numper == 6 & fownu18 == 3 & hwsval<=	32670-10000) |
		 (h_numper == 6 & fownu18 == 4 & hwsval<=	31670-10000) |
		 (h_numper == 6 & fownu18 == 5 & hwsval<=	31078-10000) |
		 (h_numper == 7 & fownu18 == 0 & hwsval<=	39017-10000) |
			(h_numper == 7 & fownu18 == 1 & hwsval<=	39260-10000) |
		 (h_numper == 7 & fownu18 == 2 & hwsval<= 38421-10000) |
			(h_numper == 7 & fownu18 == 3 & hwsval<= 37835-10000) |
		 (h_numper == 7 & fownu18 == 4 & hwsval<= 36745-10000) |
		 (h_numper == 7 & fownu18 == 5 & hwsval<= 35473-10000) |
		 (h_numper == 7 & fownu18 == 6 & hwsval<= 34077-10000) |
		 (h_numper == 8 & fownu18 == 0 & hwsval<= 43637-10000) |
			(h_numper == 8 & fownu18 == 1 & hwsval<= 44023-10000) |
		 (h_numper == 8 & fownu18 == 2 & hwsval<= 43230-10000) |
		 (h_numper == 8 & fownu18 == 3 & hwsval<= 42536-10000) |
		 (h_numper == 8 & fownu18 == 4 & hwsval<= 41551-10000) |
		 (h_numper == 8 & fownu18 == 5 & hwsval<= 40300-10000) |
		 (h_numper == 8 & fownu18 == 6 & hwsval<= 38999-10000) |
		 (h_numper == 8 & fownu18 == 7 & hwsval<= 38668-10000) |
		 (h_numper == 9 & fownu18 == 0 & hwsval<= 52493-10000) |
			(h_numper == 9 & fownu18 == 1 & hwsval<= 52747-10000) |
			(h_numper == 9 & fownu18 == 2 & hwsval<= 52046-10000) |
		 (h_numper == 9 & fownu18 == 3 & hwsval<= 51457-10000) |
		 (h_numper == 9 & fownu18 == 4 & hwsval<= 50490-10000) |
		 (h_numper == 9 & fownu18 == 5 & hwsval<= 49159-10000) |
		 (h_numper == 9 & fownu18 == 6 & hwsval<= 47956-10000) |
		 (h_numper == 9 & fownu18 == 7 & hwsval<= 47658-10000) |
		 (h_numper == 9 & fownu18 == 8 & hwsval<= 45822-10000)) &	a_age > 17 & a_age < 66 )

Poverty_Adults_Deep <-subset( 
		y , 
		 ((h_numper == 1 & hwsval<=	12331/2) |
			(h_numper == 2 & fownu18 == 0 & hwsval<=	15871/2) |
			(h_numper == 2 & fownu18 == 1 & hwsval<=	16337/2) |
			(h_numper == 3 & fownu18 == 0 & hwsval<= 18540/2) |
			(h_numper == 3 & fownu18 == 1 & hwsval<= 19078/2) |
			(h_numper == 3 & fownu18 == 2 & hwsval<= 19096/2) |
			(h_numper == 4 & fownu18 == 0 & hwsval<= 24447/2) |
		 (h_numper == 4 & fownu18 == 1 & hwsval<= 24847/2) |
			(h_numper == 4 & fownu18 == 2 & hwsval<= 24036/2) |
		 (h_numper == 4 & fownu18 == 3 & hwsval<= 24120/2) |
		 (h_numper == 5 & fownu18 == 0 & hwsval<= 29482/2) |
			(h_numper == 5 & fownu18 == 1 & hwsval<= 29911/2) |
		 (h_numper == 5 & fownu18 == 2 & hwsval<= 28995/2) |
		 (h_numper == 5 & fownu18 == 3 & hwsval<= 28286/2) |
			(h_numper == 5 & fownu18 == 4 & hwsval<= 27853/2) |
		 (h_numper == 6 & fownu18 == 0 & hwsval<=	33909/2) |
			(h_numper == 6 & fownu18 == 1 & hwsval<=	34044/2) |
			(h_numper == 6 & fownu18 == 2 & hwsval<=	33342/2) |
		 (h_numper == 6 & fownu18 == 3 & hwsval<=	32670/2) |
		 (h_numper == 6 & fownu18 == 4 & hwsval<=	31670/2) |
		 (h_numper == 6 & fownu18 == 5 & hwsval<=	31078/2) |
		 (h_numper == 7 & fownu18 == 0 & hwsval<=	39017/2) |
			(h_numper == 7 & fownu18 == 1 & hwsval<=	39260/2) |
		 (h_numper == 7 & fownu18 == 2 & hwsval<= 38421/2) |
			(h_numper == 7 & fownu18 == 3 & hwsval<= 37835/2) |
		 (h_numper == 7 & fownu18 == 4 & hwsval<= 36745/2) |
		 (h_numper == 7 & fownu18 == 5 & hwsval<= 35473/2) |
		 (h_numper == 7 & fownu18 == 6 & hwsval<= 34077/2) |
		 (h_numper == 8 & fownu18 == 0 & hwsval<= 43637/2) |
			(h_numper == 8 & fownu18 == 1 & hwsval<= 44023/2) |
		 (h_numper == 8 & fownu18 == 2 & hwsval<= 43230/2) |
		 (h_numper == 8 & fownu18 == 3 & hwsval<= 42536/2) |
		 (h_numper == 8 & fownu18 == 4 & hwsval<= 41551/2) |
		 (h_numper == 8 & fownu18 == 5 & hwsval<= 40300/2) |
		 (h_numper == 8 & fownu18 == 6 & hwsval<= 38999/2) |
		 (h_numper == 8 & fownu18 == 7 & hwsval<= 38668/2) |
		 (h_numper == 9 & fownu18 == 0 & hwsval<= 52493/2) |
			(h_numper == 9 & fownu18 == 1 & hwsval<= 52747/2) |
			(h_numper == 9 & fownu18 == 2 & hwsval<= 52046/2) |
		 (h_numper == 9 & fownu18 == 3 & hwsval<= 51457/2) |
		 (h_numper == 9 & fownu18 == 4 & hwsval<= 50490/2) |
		 (h_numper == 9 & fownu18 == 5 & hwsval<= 49159/2) |
		 (h_numper == 9 & fownu18 == 6 & hwsval<= 47956/2) |
		 (h_numper == 9 & fownu18 == 7 & hwsval<= 47658/2) |
		 (h_numper == 9 & fownu18 == 8 & hwsval<= 45822/2)) & a_age > 17 & a_age < 66)

Poverty_Adults_w1000_Deep <-subset( 
			y , 
	  ((h_numper == 1 & hwsval<=	12331/2 -1000) |
			(h_numper == 2 & fownu18 == 0 & hwsval<=	15871/2 -1000) |
			(h_numper == 2 & fownu18 == 1 & hwsval<=	16337/2 -1000) |
			(h_numper == 3 & fownu18 == 0 & hwsval<= 18540/2 -1000) |
			(h_numper == 3 & fownu18 == 1 & hwsval<= 19078/2 -1000) |
			(h_numper == 3 & fownu18 == 2 & hwsval<= 19096/2 -1000) |
			(h_numper == 4 & fownu18 == 0 & hwsval<= 24447/2 -1000) |
		 (h_numper == 4 & fownu18 == 1 & hwsval<= 24847/2 -1000) |
			(h_numper == 4 & fownu18 == 2 & hwsval<= 24036/2 -1000) |
		 (h_numper == 4 & fownu18 == 3 & hwsval<= 24120/2 -1000) |
		 (h_numper == 5 & fownu18 == 0 & hwsval<= 29482/2 -1000) |
			(h_numper == 5 & fownu18 == 1 & hwsval<= 29911/2 -1000) |
		 (h_numper == 5 & fownu18 == 2 & hwsval<= 28995/2 -1000) |
		 (h_numper == 5 & fownu18 == 3 & hwsval<= 28286/2 -1000) |
			(h_numper == 5 & fownu18 == 4 & hwsval<= 27853/2 -1000) |
		 (h_numper == 6 & fownu18 == 0 & hwsval<=	33909/2 -1000) |
			(h_numper == 6 & fownu18 == 1 & hwsval<=	34044/2 -1000) |
			(h_numper == 6 & fownu18 == 2 & hwsval<=	33342/2 -1000) |
		 (h_numper == 6 & fownu18 == 3 & hwsval<=	32670/2 -1000) |
		 (h_numper == 6 & fownu18 == 4 & hwsval<=	31670/2 -1000) |
		 (h_numper == 6 & fownu18 == 5 & hwsval<=	31078/2 -1000) |
		 (h_numper == 7 & fownu18 == 0 & hwsval<=	39017/2 -1000) |
			(h_numper == 7 & fownu18 == 1 & hwsval<=	39260/2 -1000) |
		 (h_numper == 7 & fownu18 == 2 & hwsval<= 38421/2 -1000) |
			(h_numper == 7 & fownu18 == 3 & hwsval<= 37835/2 -1000) |
		 (h_numper == 7 & fownu18 == 4 & hwsval<= 36745/2 -1000) |
		 (h_numper == 7 & fownu18 == 5 & hwsval<= 35473/2 -1000) |
		 (h_numper == 7 & fownu18 == 6 & hwsval<= 34077/2 -1000) |
		 (h_numper == 8 & fownu18 == 0 & hwsval<= 43637/2 -1000) |
			(h_numper == 8 & fownu18 == 1 & hwsval<= 44023/2 -1000) |
		 (h_numper == 8 & fownu18 == 2 & hwsval<= 43230/2 -1000) |
		 (h_numper == 8 & fownu18 == 3 & hwsval<= 42536/2 -1000) |
		 (h_numper == 8 & fownu18 == 4 & hwsval<= 41551/2 -1000) |
		 (h_numper == 8 & fownu18 == 5 & hwsval<= 40300/2 -1000) |
		 (h_numper == 8 & fownu18 == 6 & hwsval<= 38999/2 -1000) |
		 (h_numper == 8 & fownu18 == 7 & hwsval<= 38668/2 -1000) |
		 (h_numper == 9 & fownu18 == 0 & hwsval<= 52493/2 -1000) |
			(h_numper == 9 & fownu18 == 1 & hwsval<= 52747/2 -1000) |
			(h_numper == 9 & fownu18 == 2 & hwsval<= 52046/2 -1000) |
		 (h_numper == 9 & fownu18 == 3 & hwsval<= 51457/2 -1000) |
		 (h_numper == 9 & fownu18 == 4 & hwsval<= 50490/2 -1000) |
		 (h_numper == 9 & fownu18 == 5 & hwsval<= 49159/2 -1000) |
		 (h_numper == 9 & fownu18 == 6 & hwsval<= 47956/2 -1000) |
		 (h_numper == 9 & fownu18 == 7 & hwsval<= 47658/2 -1000) |
		 (h_numper == 9 & fownu18 == 8 & hwsval<= 45822/2 -1000)) & a_age > 17 & a_age < 66)

Poverty_Adults_w5000_Deep <-subset( 
		y , 
		 ((h_numper == 1 & hwsval<=	12331/2 - 5000) |
			(h_numper == 2 & fownu18 == 0 & hwsval<=	15871/2 - 5000) |
			(h_numper == 2 & fownu18 == 1 & hwsval<=	16337/2 - 5000) |
			(h_numper == 3 & fownu18 == 0 & hwsval<= 18540/2 - 5000) |
			(h_numper == 3 & fownu18 == 1 & hwsval<= 19078/2 - 5000) |
			(h_numper == 3 & fownu18 == 2 & hwsval<= 19096/2 - 5000) |
			(h_numper == 4 & fownu18 == 0 & hwsval<= 24447/2 - 5000) |
		 (h_numper == 4 & fownu18 == 1 & hwsval<= 24847/2 - 5000) |
			(h_numper == 4 & fownu18 == 2 & hwsval<= 24036/2 - 5000) |
		 (h_numper == 4 & fownu18 == 3 & hwsval<= 24120/2 - 5000) |
		 (h_numper == 5 & fownu18 == 0 & hwsval<= 29482/2 - 5000) |
			(h_numper == 5 & fownu18 == 1 & hwsval<= 29911/2 - 5000) |
		 (h_numper == 5 & fownu18 == 2 & hwsval<= 28995/2 - 5000) |
		 (h_numper == 5 & fownu18 == 3 & hwsval<= 28286/2 - 5000) |
			(h_numper == 5 & fownu18 == 4 & hwsval<= 27853/2 - 5000) |
		 (h_numper == 6 & fownu18 == 0 & hwsval<=	33909/2 - 5000) |
			(h_numper == 6 & fownu18 == 1 & hwsval<=	34044/2 - 5000) |
			(h_numper == 6 & fownu18 == 2 & hwsval<=	33342/2 - 5000) |
		 (h_numper == 6 & fownu18 == 3 & hwsval<=	32670/2 - 5000) |
		 (h_numper == 6 & fownu18 == 4 & hwsval<=	31670/2 - 5000) |
		 (h_numper == 6 & fownu18 == 5 & hwsval<=	31078/2 - 5000) |
		 (h_numper == 7 & fownu18 == 0 & hwsval<=	39017/2 - 5000) |
			(h_numper == 7 & fownu18 == 1 & hwsval<=	39260/2 - 5000) |
		 (h_numper == 7 & fownu18 == 2 & hwsval<= 38421/2 - 5000) |
			(h_numper == 7 & fownu18 == 3 & hwsval<= 37835/2 - 5000) |
		 (h_numper == 7 & fownu18 == 4 & hwsval<= 36745/2 - 5000) |
		 (h_numper == 7 & fownu18 == 5 & hwsval<= 35473/2 - 5000) |
		 (h_numper == 7 & fownu18 == 6 & hwsval<= 34077/2 - 5000) |
		 (h_numper == 8 & fownu18 == 0 & hwsval<= 43637/2 - 5000) |
			(h_numper == 8 & fownu18 == 1 & hwsval<= 44023/2 - 5000) |
		 (h_numper == 8 & fownu18 == 2 & hwsval<= 43230/2 - 5000) |
		 (h_numper == 8 & fownu18 == 3 & hwsval<= 42536/2 - 5000) |
		 (h_numper == 8 & fownu18 == 4 & hwsval<= 41551/2 - 5000) |
		 (h_numper == 8 & fownu18 == 5 & hwsval<= 40300/2 - 5000) |
		 (h_numper == 8 & fownu18 == 6 & hwsval<= 38999/2 - 5000) |
		 (h_numper == 8 & fownu18 == 7 & hwsval<= 38668/2 - 5000) |
		 (h_numper == 9 & fownu18 == 0 & hwsval<= 52493/2 - 5000) |
			(h_numper == 9 & fownu18 == 1 & hwsval<= 52747/2 - 5000) |
			(h_numper == 9 & fownu18 == 2 & hwsval<= 52046/2 - 5000) |
		 (h_numper == 9 & fownu18 == 3 & hwsval<= 51457/2 - 5000) |
		 (h_numper == 9 & fownu18 == 4 & hwsval<= 50490/2 - 5000) |
		 (h_numper == 9 & fownu18 == 5 & hwsval<= 49159/2 - 5000) |
		 (h_numper == 9 & fownu18 == 6 & hwsval<= 47956/2 - 5000) |
		 (h_numper == 9 & fownu18 == 7 & hwsval<= 47658/2 - 5000) |
		 (h_numper == 9 & fownu18 == 8 & hwsval<= 45822/2 - 5000)) & a_age > 17 & a_age < 66)

		
Poverty_Adults_w10000_Deep <-subset( 
			y, 
	  ((h_numper == 1 & hwsval<=	12331/2 -1000) |
			(h_numper == 2 & fownu18 == 0 & hwsval<=	15871/2 -10000) |
			(h_numper == 2 & fownu18 == 1 & hwsval<=	16337/2 -10000) |
			(h_numper == 3 & fownu18 == 0 & hwsval<= 18540/2 -10000) |
			(h_numper == 3 & fownu18 == 1 & hwsval<= 19078/2 -10000) |
			(h_numper == 3 & fownu18 == 2 & hwsval<= 19096/2 -10000) |
			(h_numper == 4 & fownu18 == 0 & hwsval<= 24447/2 -10000) |
		 (h_numper == 4 & fownu18 == 1 & hwsval<= 24847/2 -10000) |
			(h_numper == 4 & fownu18 == 2 & hwsval<= 24036/2 -10000) |
		 (h_numper == 4 & fownu18 == 3 & hwsval<= 24120/2 -10000) |
		 (h_numper == 5 & fownu18 == 0 & hwsval<= 29482/2 -10000) |
			(h_numper == 5 & fownu18 == 1 & hwsval<= 29911/2 -10000) |
		 (h_numper == 5 & fownu18 == 2 & hwsval<= 28995/2 -10000) |
		 (h_numper == 5 & fownu18 == 3 & hwsval<= 28286/2 -10000) |
			(h_numper == 5 & fownu18 == 4 & hwsval<= 27853/2 -10000) |
		 (h_numper == 6 & fownu18 == 0 & hwsval<=	33909/2 -10000) |
			(h_numper == 6 & fownu18 == 1 & hwsval<=	34044/2 -10000) |
			(h_numper == 6 & fownu18 == 2 & hwsval<=	33342/2 -10000) |
		 (h_numper == 6 & fownu18 == 3 & hwsval<=	32670/2 -10000) |
		 (h_numper == 6 & fownu18 == 4 & hwsval<=	31670/2 -10000) |
		 (h_numper == 6 & fownu18 == 5 & hwsval<=	31078/2 -10000) |
		 (h_numper == 7 & fownu18 == 0 & hwsval<=	39017/2 -10000) |
			(h_numper == 7 & fownu18 == 1 & hwsval<=	39260/2 -10000) |
		 (h_numper == 7 & fownu18 == 2 & hwsval<= 38421/2 -10000) |
			(h_numper == 7 & fownu18 == 3 & hwsval<= 37835/2 -10000) |
		 (h_numper == 7 & fownu18 == 4 & hwsval<= 36745/2 -10000) |
		 (h_numper == 7 & fownu18 == 5 & hwsval<= 35473/2 -10000) |
		 (h_numper == 7 & fownu18 == 6 & hwsval<= 34077/2 -10000) |
		 (h_numper == 8 & fownu18 == 0 & hwsval<= 43637/2 -10000) |
			(h_numper == 8 & fownu18 == 1 & hwsval<= 44023/2 -10000) |
		 (h_numper == 8 & fownu18 == 2 & hwsval<= 43230/2 -10000) |
		 (h_numper == 8 & fownu18 == 3 & hwsval<= 42536/2 -10000) |
		 (h_numper == 8 & fownu18 == 4 & hwsval<= 41551/2 -10000) |
		 (h_numper == 8 & fownu18 == 5 & hwsval<= 40300/2 -10000) |
		 (h_numper == 8 & fownu18 == 6 & hwsval<= 38999/2 -10000) |
		 (h_numper == 8 & fownu18 == 7 & hwsval<= 38668/2 -10000) |
		 (h_numper == 9 & fownu18 == 0 & hwsval<= 52493/2 -10000) |
			(h_numper == 9 & fownu18 == 1 & hwsval<= 52747/2 -10000) |
			(h_numper == 9 & fownu18 == 2 & hwsval<= 52046/2 -10000) |
		 (h_numper == 9 & fownu18 == 3 & hwsval<= 51457/2 -10000) |
		 (h_numper == 9 & fownu18 == 4 & hwsval<= 50490/2 -10000) |
		 (h_numper == 9 & fownu18 == 5 & hwsval<= 49159/2 -10000) |
		 (h_numper == 9 & fownu18 == 6 & hwsval<= 47956/2 -10000) |
		 (h_numper == 9 & fownu18 == 7 & hwsval<= 47658/2 -10000) |
		 (h_numper == 9 & fownu18 == 8 & hwsval<= 45822/2 -10000)) & a_age > 17 & a_age < 66)

svytotal(
	~one ,
	Poverty_Adults)
		
svytotal(
	~one ,
	Poverty_Adults_w1000)

svytotal(
	~one ,
	Poverty_Adults_w5000)

svytotal(
	~one ,
	Poverty_Adults_w10000)

svytotal(
	~one ,
	Poverty_Adults_Deep)
		
svytotal(
	~one ,
	Poverty_Adults_w1000_Deep)

svytotal(
	~one ,
	Poverty_Adults_w5000_Deep)

svytotal(
	~one ,
	Poverty_Adults_w10000_Deep)
