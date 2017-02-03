# 2014 Poverty

Poverty_Adults <-subset( 
		y , 
		 ((h_numper == 1 & hwsval<=	12316) |
			(h_numper == 2 & fownu18 == 0 & hwsval<=	15853) |
			(h_numper == 2 & fownu18 == 1 & hwsval<=	16317) |
			(h_numper == 3 & fownu18 == 0 & hwsval<= 18518) |
			(h_numper == 3 & fownu18 == 1 & hwsval<= 19055) |
			(h_numper == 3 & fownu18 == 2 & hwsval<= 19073) |
			(h_numper == 4 & fownu18 == 0 & hwsval<= 24418) |
		 (h_numper == 4 & fownu18 == 1 & hwsval<= 24817) |
			(h_numper == 4 & fownu18 == 2 & hwsval<= 24008) |
		 (h_numper == 4 & fownu18 == 3 & hwsval<= 24091) |
		 (h_numper == 5 & fownu18 == 0 & hwsval<= 29447) |
			(h_numper == 5 & fownu18 == 1 & hwsval<= 29875) |
		 (h_numper == 5 & fownu18 == 2 & hwsval<= 28960) |
		 (h_numper == 5 & fownu18 == 3 & hwsval<= 28252) |
			(h_numper == 5 & fownu18 == 4 & hwsval<= 27820) |
		 (h_numper == 6 & fownu18 == 0 & hwsval<=	33869) |
			(h_numper == 6 & fownu18 == 1 & hwsval<=	34004) |
			(h_numper == 6 & fownu18 == 2 & hwsval<=	33303) |
		 (h_numper == 6 & fownu18 == 3 & hwsval<=	32631) |
		 (h_numper == 6 & fownu18 == 4 & hwsval<=	31633) |
		 (h_numper == 6 & fownu18 == 5 & hwsval<=	31041) |
		 (h_numper == 7 & fownu18 == 0 & hwsval<=	38971) |
			(h_numper == 7 & fownu18 == 1 & hwsval<=	39214) |
		 (h_numper == 7 & fownu18 == 2 & hwsval<= 38375) |
			(h_numper == 7 & fownu18 == 3 & hwsval<= 37791) |
		 (h_numper == 7 & fownu18 == 4 & hwsval<= 36701) |
		 (h_numper == 7 & fownu18 == 5 & hwsval<= 35431) |
		 (h_numper == 7 & fownu18 == 6 & hwsval<= 34036) |
		 (h_numper == 8 & fownu18 == 0 & hwsval<= 43586) |
			(h_numper == 8 & fownu18 == 1 & hwsval<= 43970) |
		 (h_numper == 8 & fownu18 == 2 & hwsval<= 43179) |
		 (h_numper == 8 & fownu18 == 3 & hwsval<= 42485) |
		 (h_numper == 8 & fownu18 == 4 & hwsval<= 41501) |
		 (h_numper == 8 & fownu18 == 5 & hwsval<= 40252) |
		 (h_numper == 8 & fownu18 == 6 & hwsval<= 38953) |
		 (h_numper == 8 & fownu18 == 7 & hwsval<= 38622) |
		 (h_numper == 9 & fownu18 == 0 & hwsval<= 52430) |
			(h_numper == 9 & fownu18 == 1 & hwsval<= 52685) |
			(h_numper == 9 & fownu18 == 2 & hwsval<= 51984) |
		 (h_numper == 9 & fownu18 == 3 & hwsval<= 51396) |
		 (h_numper == 9 & fownu18 == 4 & hwsval<= 50430) |
		 (h_numper == 9 & fownu18 == 5 & hwsval<= 49101) |
		 (h_numper == 9 & fownu18 == 6 & hwsval<= 47899) |
		 (h_numper == 9 & fownu18 == 7 & hwsval<= 47601) |
		 (h_numper == 9 & fownu18 == 8 & hwsval<= 45768)) &	a_age > 17 & a_age < 66)

Poverty_Adults_w1000 <-subset( 
		y , 
		 ((h_numper == 1 & hwsval<=	12316-1000) |
			(h_numper == 2 & fownu18 == 0 & hwsval<=	15853-1000) |
			(h_numper == 2 & fownu18 == 1 & hwsval<=	16317-1000) |
			(h_numper == 3 & fownu18 == 0 & hwsval<= 18518-1000) |
			(h_numper == 3 & fownu18 == 1 & hwsval<= 19055-1000) |
			(h_numper == 3 & fownu18 == 2 & hwsval<= 19073-1000) |
			(h_numper == 4 & fownu18 == 0 & hwsval<= 24418-1000) |
		 (h_numper == 4 & fownu18 == 1 & hwsval<= 24817-1000) |
			(h_numper == 4 & fownu18 == 2 & hwsval<= 24008-1000) |
		 (h_numper == 4 & fownu18 == 3 & hwsval<= 24091-1000) |
		 (h_numper == 5 & fownu18 == 0 & hwsval<= 29447-1000) |
			(h_numper == 5 & fownu18 == 1 & hwsval<= 29875-1000) |
		 (h_numper == 5 & fownu18 == 2 & hwsval<= 28960-1000) |
		 (h_numper == 5 & fownu18 == 3 & hwsval<= 28252-1000) |
			(h_numper == 5 & fownu18 == 4 & hwsval<= 27820-1000) |
		 (h_numper == 6 & fownu18 == 0 & hwsval<=	33869-1000) |
			(h_numper == 6 & fownu18 == 1 & hwsval<=	34004-1000) |
			(h_numper == 6 & fownu18 == 2 & hwsval<=	33303-1000) |
		 (h_numper == 6 & fownu18 == 3 & hwsval<=	32631-1000) |
		 (h_numper == 6 & fownu18 == 4 & hwsval<=	31633-1000) |
		 (h_numper == 6 & fownu18 == 5 & hwsval<=	31041-1000) |
		 (h_numper == 7 & fownu18 == 0 & hwsval<=	38971-1000) |
			(h_numper == 7 & fownu18 == 1 & hwsval<=	39214-1000) |
		 (h_numper == 7 & fownu18 == 2 & hwsval<= 38375-1000) |
			(h_numper == 7 & fownu18 == 3 & hwsval<= 37791-1000) |
		 (h_numper == 7 & fownu18 == 4 & hwsval<= 36701-1000) |
		 (h_numper == 7 & fownu18 == 5 & hwsval<= 35431-1000) |
		 (h_numper == 7 & fownu18 == 6 & hwsval<= 34036-1000) |
		 (h_numper == 8 & fownu18 == 0 & hwsval<= 43586-1000) |
			(h_numper == 8 & fownu18 == 1 & hwsval<= 43970-1000) |
		 (h_numper == 8 & fownu18 == 2 & hwsval<= 43179-1000) |
		 (h_numper == 8 & fownu18 == 3 & hwsval<= 42485-1000) |
		 (h_numper == 8 & fownu18 == 4 & hwsval<= 41501-1000) |
		 (h_numper == 8 & fownu18 == 5 & hwsval<= 40252-1000) |
		 (h_numper == 8 & fownu18 == 6 & hwsval<= 38953-1000) |
		 (h_numper == 8 & fownu18 == 7 & hwsval<= 38622-1000) |
		 (h_numper == 9 & fownu18 == 0 & hwsval<= 52430-1000) |
			(h_numper == 9 & fownu18 == 1 & hwsval<= 52685-1000) |
			(h_numper == 9 & fownu18 == 2 & hwsval<= 51984-1000) |
		 (h_numper == 9 & fownu18 == 3 & hwsval<= 51396-1000) |
		 (h_numper == 9 & fownu18 == 4 & hwsval<= 50430-1000) |
		 (h_numper == 9 & fownu18 == 5 & hwsval<= 49101-1000) |
		 (h_numper == 9 & fownu18 == 6 & hwsval<= 47899-1000) |
		 (h_numper == 9 & fownu18 == 7 & hwsval<= 47601-1000) |
		 (h_numper == 9 & fownu18 == 8 & hwsval<= 45768-1000)) &	a_age > 17 & a_age < 66)


Poverty_Adults_w5000 <-subset( 
		y , 
		 ((h_numper == 1 & hwsval<=	12316-5000 ) |
			(h_numper == 2 & fownu18 == 0 & hwsval<=	15853-5000 ) |
			(h_numper == 2 & fownu18 == 1 & hwsval<=	16317-5000 ) |
			(h_numper == 3 & fownu18 == 0 & hwsval<= 18518-5000 ) |
			(h_numper == 3 & fownu18 == 1 & hwsval<= 19055-5000 ) |
			(h_numper == 3 & fownu18 == 2 & hwsval<= 19073-5000 ) |
			(h_numper == 4 & fownu18 == 0 & hwsval<= 24418-5000 ) |
		 (h_numper == 4 & fownu18 == 1 & hwsval<= 24817-5000 ) |
			(h_numper == 4 & fownu18 == 2 & hwsval<= 24008-5000 ) |
		 (h_numper == 4 & fownu18 == 3 & hwsval<= 24091-5000 ) |
		 (h_numper == 5 & fownu18 == 0 & hwsval<= 29447-5000 ) |
			(h_numper == 5 & fownu18 == 1 & hwsval<= 29875-5000 ) |
		 (h_numper == 5 & fownu18 == 2 & hwsval<= 28960-5000 ) |
		 (h_numper == 5 & fownu18 == 3 & hwsval<= 28252-5000 ) |
			(h_numper == 5 & fownu18 == 4 & hwsval<= 27820-5000 ) |
		 (h_numper == 6 & fownu18 == 0 & hwsval<=	33869-5000 ) |
			(h_numper == 6 & fownu18 == 1 & hwsval<=	34004-5000 ) |
			(h_numper == 6 & fownu18 == 2 & hwsval<=	33303-5000 ) |
		 (h_numper == 6 & fownu18 == 3 & hwsval<=	32631-5000 ) |
		 (h_numper == 6 & fownu18 == 4 & hwsval<=	31633-5000 ) |
		 (h_numper == 6 & fownu18 == 5 & hwsval<=	31041-5000 ) |
		 (h_numper == 7 & fownu18 == 0 & hwsval<=	38971-5000 ) |
			(h_numper == 7 & fownu18 == 1 & hwsval<=	39214-5000 ) |
		 (h_numper == 7 & fownu18 == 2 & hwsval<= 38375-5000 ) |
			(h_numper == 7 & fownu18 == 3 & hwsval<= 37791-5000 ) |
		 (h_numper == 7 & fownu18 == 4 & hwsval<= 36701-5000 ) |
		 (h_numper == 7 & fownu18 == 5 & hwsval<= 35431-5000 ) |
		 (h_numper == 7 & fownu18 == 6 & hwsval<= 34036-5000 ) |
		 (h_numper == 8 & fownu18 == 0 & hwsval<= 43586-5000 ) |
			(h_numper == 8 & fownu18 == 1 & hwsval<= 43970-5000 ) |
		 (h_numper == 8 & fownu18 == 2 & hwsval<= 43179-5000 ) |
		 (h_numper == 8 & fownu18 == 3 & hwsval<= 42485-5000 ) |
		 (h_numper == 8 & fownu18 == 4 & hwsval<= 41501-5000 ) |
		 (h_numper == 8 & fownu18 == 5 & hwsval<= 40252-5000 ) |
		 (h_numper == 8 & fownu18 == 6 & hwsval<= 38953-5000 ) |
		 (h_numper == 8 & fownu18 == 7 & hwsval<= 38622-5000 ) |
		 (h_numper == 9 & fownu18 == 0 & hwsval<= 52430-5000 ) |
			(h_numper == 9 & fownu18 == 1 & hwsval<= 52685-5000 ) |
			(h_numper == 9 & fownu18 == 2 & hwsval<= 51984-5000 ) |
		 (h_numper == 9 & fownu18 == 3 & hwsval<= 51396-5000 ) |
		 (h_numper == 9 & fownu18 == 4 & hwsval<= 50430-5000 ) |
		 (h_numper == 9 & fownu18 == 5 & hwsval<= 49101-5000 ) |
		 (h_numper == 9 & fownu18 == 6 & hwsval<= 47899-5000 ) |
		 (h_numper == 9 & fownu18 == 7 & hwsval<= 47601-5000 ) |
		 (h_numper == 9 & fownu18 == 8 & hwsval<= 45768-5000 )) &	a_age > 17 & a_age < 66)


Poverty_Adults_w10000 <-subset( 
		y , 
		 ((h_numper == 1 & hwsval<=	12316-10000) |
			(h_numper == 2 & fownu18 == 0 & hwsval<=	15853-10000) |
			(h_numper == 2 & fownu18 == 1 & hwsval<=	16317-10000) |
			(h_numper == 3 & fownu18 == 0 & hwsval<= 18518-10000) |
			(h_numper == 3 & fownu18 == 1 & hwsval<= 19055-10000) |
			(h_numper == 3 & fownu18 == 2 & hwsval<= 19073-10000) |
			(h_numper == 4 & fownu18 == 0 & hwsval<= 24418-10000) |
		 (h_numper == 4 & fownu18 == 1 & hwsval<= 24817-10000) |
			(h_numper == 4 & fownu18 == 2 & hwsval<= 24008-10000) |
		 (h_numper == 4 & fownu18 == 3 & hwsval<= 24091-10000) |
		 (h_numper == 5 & fownu18 == 0 & hwsval<= 29447-10000) |
			(h_numper == 5 & fownu18 == 1 & hwsval<= 29875-10000) |
		 (h_numper == 5 & fownu18 == 2 & hwsval<= 28960-10000) |
		 (h_numper == 5 & fownu18 == 3 & hwsval<= 28252-10000) |
			(h_numper == 5 & fownu18 == 4 & hwsval<= 27820-10000) |
		 (h_numper == 6 & fownu18 == 0 & hwsval<=	33869-10000) |
			(h_numper == 6 & fownu18 == 1 & hwsval<=	34004-10000) |
			(h_numper == 6 & fownu18 == 2 & hwsval<=	33303-10000) |
		 (h_numper == 6 & fownu18 == 3 & hwsval<=	32631-10000) |
		 (h_numper == 6 & fownu18 == 4 & hwsval<=	31633-10000) |
		 (h_numper == 6 & fownu18 == 5 & hwsval<=	31041-10000) |
		 (h_numper == 7 & fownu18 == 0 & hwsval<=	38971-10000) |
			(h_numper == 7 & fownu18 == 1 & hwsval<=	39214-10000) |
		 (h_numper == 7 & fownu18 == 2 & hwsval<= 38375-10000) |
			(h_numper == 7 & fownu18 == 3 & hwsval<= 37791-10000) |
		 (h_numper == 7 & fownu18 == 4 & hwsval<= 36701-10000) |
		 (h_numper == 7 & fownu18 == 5 & hwsval<= 35431-10000) |
		 (h_numper == 7 & fownu18 == 6 & hwsval<= 34036-10000) |
		 (h_numper == 8 & fownu18 == 0 & hwsval<= 43586-10000) |
			(h_numper == 8 & fownu18 == 1 & hwsval<= 43970-10000) |
		 (h_numper == 8 & fownu18 == 2 & hwsval<= 43179-10000) |
		 (h_numper == 8 & fownu18 == 3 & hwsval<= 42485-10000) |
		 (h_numper == 8 & fownu18 == 4 & hwsval<= 41501-10000) |
		 (h_numper == 8 & fownu18 == 5 & hwsval<= 40252-10000) |
		 (h_numper == 8 & fownu18 == 6 & hwsval<= 38953-10000) |
		 (h_numper == 8 & fownu18 == 7 & hwsval<= 38622-10000) |
		 (h_numper == 9 & fownu18 == 0 & hwsval<= 52430-10000) |
			(h_numper == 9 & fownu18 == 1 & hwsval<= 52685-10000) |
			(h_numper == 9 & fownu18 == 2 & hwsval<= 51984-10000) |
		 (h_numper == 9 & fownu18 == 3 & hwsval<= 51396-10000) |
		 (h_numper == 9 & fownu18 == 4 & hwsval<= 50430-10000) |
		 (h_numper == 9 & fownu18 == 5 & hwsval<= 49101-10000) |
		 (h_numper == 9 & fownu18 == 6 & hwsval<= 47899-10000) |
		 (h_numper == 9 & fownu18 == 7 & hwsval<= 47601-10000) |
		 (h_numper == 9 & fownu18 == 8 & hwsval<= 45768-10000)) &	a_age > 17 & a_age < 66)

Poverty_Adults_Deep <-subset( 
		y , 
		 ((h_numper == 1 & hwsval<=	12316/2 ) |
			(h_numper == 2 & fownu18 == 0 & hwsval<=	15853/2 ) |
			(h_numper == 2 & fownu18 == 1 & hwsval<=	16317/2 ) |
			(h_numper == 3 & fownu18 == 0 & hwsval<= 18518/2 ) |
			(h_numper == 3 & fownu18 == 1 & hwsval<= 19055/2 ) |
			(h_numper == 3 & fownu18 == 2 & hwsval<= 19073/2 ) |
			(h_numper == 4 & fownu18 == 0 & hwsval<= 24418/2 ) |
		 (h_numper == 4 & fownu18 == 1 & hwsval<= 24817/2 ) |
			(h_numper == 4 & fownu18 == 2 & hwsval<= 24008/2 ) |
		 (h_numper == 4 & fownu18 == 3 & hwsval<= 24091/2 ) |
		 (h_numper == 5 & fownu18 == 0 & hwsval<= 29447/2 ) |
			(h_numper == 5 & fownu18 == 1 & hwsval<= 29875/2 ) |
		 (h_numper == 5 & fownu18 == 2 & hwsval<= 28960/2 ) |
		 (h_numper == 5 & fownu18 == 3 & hwsval<= 28252/2 ) |
			(h_numper == 5 & fownu18 == 4 & hwsval<= 27820/2 ) |
		 (h_numper == 6 & fownu18 == 0 & hwsval<=	33869/2 ) |
			(h_numper == 6 & fownu18 == 1 & hwsval<=	34004/2 ) |
			(h_numper == 6 & fownu18 == 2 & hwsval<=	33303/2 ) |
		 (h_numper == 6 & fownu18 == 3 & hwsval<=	32631/2 ) |
		 (h_numper == 6 & fownu18 == 4 & hwsval<=	31633/2 ) |
		 (h_numper == 6 & fownu18 == 5 & hwsval<=	31041/2 ) |
		 (h_numper == 7 & fownu18 == 0 & hwsval<=	38971/2 ) |
			(h_numper == 7 & fownu18 == 1 & hwsval<=	39214/2 ) |
		 (h_numper == 7 & fownu18 == 2 & hwsval<= 38375/2 ) |
			(h_numper == 7 & fownu18 == 3 & hwsval<= 37791/2 ) |
		 (h_numper == 7 & fownu18 == 4 & hwsval<= 36701/2 ) |
		 (h_numper == 7 & fownu18 == 5 & hwsval<= 35431/2 ) |
		 (h_numper == 7 & fownu18 == 6 & hwsval<= 34036/2 ) |
		 (h_numper == 8 & fownu18 == 0 & hwsval<= 43586/2 ) |
			(h_numper == 8 & fownu18 == 1 & hwsval<= 43970/2 ) |
		 (h_numper == 8 & fownu18 == 2 & hwsval<= 43179/2 ) |
		 (h_numper == 8 & fownu18 == 3 & hwsval<= 42485/2 ) |
		 (h_numper == 8 & fownu18 == 4 & hwsval<= 41501/2 ) |
		 (h_numper == 8 & fownu18 == 5 & hwsval<= 40252/2 ) |
		 (h_numper == 8 & fownu18 == 6 & hwsval<= 38953/2 ) |
		 (h_numper == 8 & fownu18 == 7 & hwsval<= 38622/2 ) |
		 (h_numper == 9 & fownu18 == 0 & hwsval<= 52430/2 ) |
			(h_numper == 9 & fownu18 == 1 & hwsval<= 52685/2 ) |
			(h_numper == 9 & fownu18 == 2 & hwsval<= 51984/2 ) |
		 (h_numper == 9 & fownu18 == 3 & hwsval<= 51396/2 ) |
		 (h_numper == 9 & fownu18 == 4 & hwsval<= 50430/2 ) |
		 (h_numper == 9 & fownu18 == 5 & hwsval<= 49101/2 ) |
		 (h_numper == 9 & fownu18 == 6 & hwsval<= 47899/2 ) |
		 (h_numper == 9 & fownu18 == 7 & hwsval<= 47601/2 ) |
		 (h_numper == 9 & fownu18 == 8 & hwsval<= 45768/2 )) &	a_age > 17 & a_age < 66)

Poverty_Adults_w1000 <- subset( 
		y , 
		 ((h_numper == 1 & hwsval<=	12316/2 - 1000) |
			(h_numper == 2 & fownu18 == 0 & hwsval<=	15853/2 - 1000) |
			(h_numper == 2 & fownu18 == 1 & hwsval<=	16317/2 - 1000) |
			(h_numper == 3 & fownu18 == 0 & hwsval<= 18518/2 - 1000) |
			(h_numper == 3 & fownu18 == 1 & hwsval<= 19055/2 - 1000) |
			(h_numper == 3 & fownu18 == 2 & hwsval<= 19073/2 - 1000) |
			(h_numper == 4 & fownu18 == 0 & hwsval<= 24418/2 - 1000) |
		 (h_numper == 4 & fownu18 == 1 & hwsval<= 24817/2 - 1000) |
			(h_numper == 4 & fownu18 == 2 & hwsval<= 24008/2 - 1000) |
		 (h_numper == 4 & fownu18 == 3 & hwsval<= 24091/2 - 1000) |
		 (h_numper == 5 & fownu18 == 0 & hwsval<= 29447/2 - 1000) |
			(h_numper == 5 & fownu18 == 1 & hwsval<= 29875/2 - 1000) |
		 (h_numper == 5 & fownu18 == 2 & hwsval<= 28960/2 - 1000) |
		 (h_numper == 5 & fownu18 == 3 & hwsval<= 28252/2 - 1000) |
			(h_numper == 5 & fownu18 == 4 & hwsval<= 27820/2 - 1000) |
		 (h_numper == 6 & fownu18 == 0 & hwsval<=	33869/2 - 1000) |
			(h_numper == 6 & fownu18 == 1 & hwsval<=	34004/2 - 1000) |
			(h_numper == 6 & fownu18 == 2 & hwsval<=	33303/2 - 1000) |
		 (h_numper == 6 & fownu18 == 3 & hwsval<=	32631/2 - 1000) |
		 (h_numper == 6 & fownu18 == 4 & hwsval<=	31633/2 - 1000) |
		 (h_numper == 6 & fownu18 == 5 & hwsval<=	31041/2 - 1000) |
		 (h_numper == 7 & fownu18 == 0 & hwsval<=	38971/2 - 1000) |
			(h_numper == 7 & fownu18 == 1 & hwsval<=	39214/2 - 1000) |
		 (h_numper == 7 & fownu18 == 2 & hwsval<= 38375/2 - 1000) |
			(h_numper == 7 & fownu18 == 3 & hwsval<= 37791/2 - 1000) |
		 (h_numper == 7 & fownu18 == 4 & hwsval<= 36701/2 - 1000) |
		 (h_numper == 7 & fownu18 == 5 & hwsval<= 35431/2 - 1000) |
		 (h_numper == 7 & fownu18 == 6 & hwsval<= 34036/2 - 1000) |
		 (h_numper == 8 & fownu18 == 0 & hwsval<= 43586/2 - 1000) |
			(h_numper == 8 & fownu18 == 1 & hwsval<= 43970/2 - 1000) |
		 (h_numper == 8 & fownu18 == 2 & hwsval<= 43179/2 - 1000) |
		 (h_numper == 8 & fownu18 == 3 & hwsval<= 42485/2 - 1000) |
		 (h_numper == 8 & fownu18 == 4 & hwsval<= 41501/2 - 1000) |
		 (h_numper == 8 & fownu18 == 5 & hwsval<= 40252/2 - 1000) |
		 (h_numper == 8 & fownu18 == 6 & hwsval<= 38953/2 - 1000) |
		 (h_numper == 8 & fownu18 == 7 & hwsval<= 38622/2 - 1000) |
		 (h_numper == 9 & fownu18 == 0 & hwsval<= 52430/2 - 1000) |
			(h_numper == 9 & fownu18 == 1 & hwsval<= 52685/2 - 1000) |
			(h_numper == 9 & fownu18 == 2 & hwsval<= 51984/2 - 1000) |
		 (h_numper == 9 & fownu18 == 3 & hwsval<= 51396/2 - 1000) |
		 (h_numper == 9 & fownu18 == 4 & hwsval<= 50430/2 - 1000) |
		 (h_numper == 9 & fownu18 == 5 & hwsval<= 49101/2 - 1000) |
		 (h_numper == 9 & fownu18 == 6 & hwsval<= 47899/2 - 1000) |
		 (h_numper == 9 & fownu18 == 7 & hwsval<= 47601/2 - 1000) |
		 (h_numper == 9 & fownu18 == 8 & hwsval<= 45768/2 - 1000)) &	a_age > 17 & a_age < 66)


Poverty_Adults_w5000 <- subset( 
		y , 
		 ((h_numper == 1 & hwsval<=	12316/2 - 5000 ) |
			(h_numper == 2 & fownu18 == 0 & hwsval<=	15853/2 - 5000 ) |
			(h_numper == 2 & fownu18 == 1 & hwsval<=	16317/2 - 5000 ) |
			(h_numper == 3 & fownu18 == 0 & hwsval<= 18518/2 - 5000 ) |
			(h_numper == 3 & fownu18 == 1 & hwsval<= 19055/2 - 5000 ) |
			(h_numper == 3 & fownu18 == 2 & hwsval<= 19073/2 - 5000 ) |
			(h_numper == 4 & fownu18 == 0 & hwsval<= 24418/2 - 5000 ) |
		 (h_numper == 4 & fownu18 == 1 & hwsval<= 24817/2 - 5000 ) |
			(h_numper == 4 & fownu18 == 2 & hwsval<= 24008/2 - 5000 ) |
		 (h_numper == 4 & fownu18 == 3 & hwsval<= 24091/2 - 5000 ) |
		 (h_numper == 5 & fownu18 == 0 & hwsval<= 29447/2 - 5000 ) |
			(h_numper == 5 & fownu18 == 1 & hwsval<= 29875/2 - 5000 ) |
		 (h_numper == 5 & fownu18 == 2 & hwsval<= 28960/2 - 5000 ) |
		 (h_numper == 5 & fownu18 == 3 & hwsval<= 28252/2 - 5000 ) |
			(h_numper == 5 & fownu18 == 4 & hwsval<= 27820/2 - 5000 ) |
		 (h_numper == 6 & fownu18 == 0 & hwsval<=	33869/2 - 5000 ) |
			(h_numper == 6 & fownu18 == 1 & hwsval<=	34004/2 - 5000 ) |
			(h_numper == 6 & fownu18 == 2 & hwsval<=	33303/2 - 5000 ) |
		 (h_numper == 6 & fownu18 == 3 & hwsval<=	32631/2 - 5000 ) |
		 (h_numper == 6 & fownu18 == 4 & hwsval<=	31633/2 - 5000 ) |
		 (h_numper == 6 & fownu18 == 5 & hwsval<=	31041/2 - 5000 ) |
		 (h_numper == 7 & fownu18 == 0 & hwsval<=	38971/2 - 5000 ) |
			(h_numper == 7 & fownu18 == 1 & hwsval<=	39214/2 - 5000 ) |
		 (h_numper == 7 & fownu18 == 2 & hwsval<= 38375/2 - 5000 ) |
			(h_numper == 7 & fownu18 == 3 & hwsval<= 37791/2 - 5000 ) |
		 (h_numper == 7 & fownu18 == 4 & hwsval<= 36701/2 - 5000 ) |
		 (h_numper == 7 & fownu18 == 5 & hwsval<= 35431/2 - 5000 ) |
		 (h_numper == 7 & fownu18 == 6 & hwsval<= 34036/2 - 5000 ) |
		 (h_numper == 8 & fownu18 == 0 & hwsval<= 43586/2 - 5000 ) |
			(h_numper == 8 & fownu18 == 1 & hwsval<= 43970/2 - 5000 ) |
		 (h_numper == 8 & fownu18 == 2 & hwsval<= 43179/2 - 5000 ) |
		 (h_numper == 8 & fownu18 == 3 & hwsval<= 42485/2 - 5000 ) |
		 (h_numper == 8 & fownu18 == 4 & hwsval<= 41501/2 - 5000 ) |
		 (h_numper == 8 & fownu18 == 5 & hwsval<= 40252/2 - 5000 ) |
		 (h_numper == 8 & fownu18 == 6 & hwsval<= 38953/2 - 5000 ) |
		 (h_numper == 8 & fownu18 == 7 & hwsval<= 38622/2 - 5000 ) |
		 (h_numper == 9 & fownu18 == 0 & hwsval<= 52430/2 - 5000 ) |
			(h_numper == 9 & fownu18 == 1 & hwsval<= 52685/2 - 5000 ) |
			(h_numper == 9 & fownu18 == 2 & hwsval<= 51984/2 - 5000 ) |
		 (h_numper == 9 & fownu18 == 3 & hwsval<= 51396/2 - 5000 ) |
		 (h_numper == 9 & fownu18 == 4 & hwsval<= 50430/2 - 5000 ) |
		 (h_numper == 9 & fownu18 == 5 & hwsval<= 49101/2 - 5000 ) |
		 (h_numper == 9 & fownu18 == 6 & hwsval<= 47899/2 - 5000 ) |
		 (h_numper == 9 & fownu18 == 7 & hwsval<= 47601/2 - 5000 ) |
		 (h_numper == 9 & fownu18 == 8 & hwsval<= 45768/2 - 5000 )) &	a_age > 17 & a_age < 66)


Poverty_Adults_w10000_deep <- subset( 
		y , 
		 ((h_numper == 1 & hwsval<=	12316/2 - 10000) |
			(h_numper == 2 & fownu18 == 0 & hwsval<=	15853/2 - 10000) |
			(h_numper == 2 & fownu18 == 1 & hwsval<=	16317/2 - 10000) |
			(h_numper == 3 & fownu18 == 0 & hwsval<= 18518/2 - 10000) |
			(h_numper == 3 & fownu18 == 1 & hwsval<= 19055/2 - 10000) |
			(h_numper == 3 & fownu18 == 2 & hwsval<= 19073/2 - 10000) |
			(h_numper == 4 & fownu18 == 0 & hwsval<= 24418/2 - 10000) |
		 (h_numper == 4 & fownu18 == 1 & hwsval<= 24817/2 - 10000) |
			(h_numper == 4 & fownu18 == 2 & hwsval<= 24008/2 - 10000) |
		 (h_numper == 4 & fownu18 == 3 & hwsval<= 24091/2 - 10000) |
		 (h_numper == 5 & fownu18 == 0 & hwsval<= 29447/2 - 10000) |
			(h_numper == 5 & fownu18 == 1 & hwsval<= 29875/2 - 10000) |
		 (h_numper == 5 & fownu18 == 2 & hwsval<= 28960/2 - 10000) |
		 (h_numper == 5 & fownu18 == 3 & hwsval<= 28252/2 - 10000) |
			(h_numper == 5 & fownu18 == 4 & hwsval<= 27820/2 - 10000) |
		 (h_numper == 6 & fownu18 == 0 & hwsval<=	33869/2 - 10000) |
			(h_numper == 6 & fownu18 == 1 & hwsval<=	34004/2 - 10000) |
			(h_numper == 6 & fownu18 == 2 & hwsval<=	33303/2 - 10000) |
		 (h_numper == 6 & fownu18 == 3 & hwsval<=	32631/2 - 10000) |
		 (h_numper == 6 & fownu18 == 4 & hwsval<=	31633/2 - 10000) |
		 (h_numper == 6 & fownu18 == 5 & hwsval<=	31041/2 - 10000) |
		 (h_numper == 7 & fownu18 == 0 & hwsval<=	38971/2 - 10000) |
			(h_numper == 7 & fownu18 == 1 & hwsval<=	39214/2 - 10000) |
		 (h_numper == 7 & fownu18 == 2 & hwsval<= 38375/2 - 10000) |
			(h_numper == 7 & fownu18 == 3 & hwsval<= 37791/2 - 10000) |
		 (h_numper == 7 & fownu18 == 4 & hwsval<= 36701/2 - 10000) |
		 (h_numper == 7 & fownu18 == 5 & hwsval<= 35431/2 - 10000) |
		 (h_numper == 7 & fownu18 == 6 & hwsval<= 34036/2 - 10000) |
		 (h_numper == 8 & fownu18 == 0 & hwsval<= 43586/2 - 10000) |
			(h_numper == 8 & fownu18 == 1 & hwsval<= 43970/2 - 10000) |
		 (h_numper == 8 & fownu18 == 2 & hwsval<= 43179/2 - 10000) |
		 (h_numper == 8 & fownu18 == 3 & hwsval<= 42485/2 - 10000) |
		 (h_numper == 8 & fownu18 == 4 & hwsval<= 41501/2 - 10000) |
		 (h_numper == 8 & fownu18 == 5 & hwsval<= 40252/2 - 10000) |
		 (h_numper == 8 & fownu18 == 6 & hwsval<= 38953/2 - 10000) |
		 (h_numper == 8 & fownu18 == 7 & hwsval<= 38622/2 - 10000) |
		 (h_numper == 9 & fownu18 == 0 & hwsval<= 52430/2 - 10000) |
			(h_numper == 9 & fownu18 == 1 & hwsval<= 52685/2 - 10000) |
			(h_numper == 9 & fownu18 == 2 & hwsval<= 51984/2 - 10000) |
		 (h_numper == 9 & fownu18 == 3 & hwsval<= 51396/2 - 10000) |
		 (h_numper == 9 & fownu18 == 4 & hwsval<= 50430/2 - 10000) |
		 (h_numper == 9 & fownu18 == 5 & hwsval<= 49101/2 - 10000) |
		 (h_numper == 9 & fownu18 == 6 & hwsval<= 47899/2 - 10000) |
		 (h_numper == 9 & fownu18 == 7 & hwsval<= 47601/2 - 10000) |
		 (h_numper == 9 & fownu18 == 8 & hwsval<= 45768/2 - 10000)) &	a_age > 17 & a_age < 66)

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
