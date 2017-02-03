# CPS Yearly Analyes Script

##### 2005 #####
kids_free5 <- subset(kids5,
			(fpersons == 1 & gestfips != 2 & gestfips != 15 & ftotval <= (9570*1.30)) | 
			(fpersons == 2 & gestfips != 2 & gestfips != 15 & ftotval <= (12830*1.30)) | 
			(fpersons == 3 & gestfips != 2 & gestfips != 15 & ftotval <= (16090	*1.30)) |
		 (fpersons == 4 & gestfips != 2 & gestfips != 15 & ftotval <= (19350	*1.30)) |
		 (fpersons == 5 & gestfips != 2 & gestfips != 15 & ftotval <= (22610*1.30)) |
		 (fpersons == 6 & gestfips != 2 & gestfips != 15 & ftotval <= (25870	*1.30)) |
		 (fpersons == 7 & gestfips != 2 & gestfips != 15 & ftotval <= (29130	*1.30)) |
		 (fpersons == 8 & gestfips != 2 & gestfips != 15 & ftotval <= (32390	*1.30)) |
		 (fpersons >= 9 & gestfips != 2 & gestfips != 15 & ftotval <= ((32390+(fpersons - 8)* 3260	)*1.30)) |
	#Hawaii			
							(fpersons == 1 & gestfips != 2 & gestfips == 15 & ftotval <= (11010*1.30)) | 
			(fpersons == 2 & gestfips != 2 & gestfips == 15 & ftotval <= (14760*1.30)) | 
			(fpersons == 3 & gestfips != 2 & gestfips == 15 & ftotval <= (18510*1.30)) |
		 (fpersons == 4 & gestfips != 2 & gestfips == 15 & ftotval <= (22260*1.30)) |
		 (fpersons == 5 & gestfips != 2 & gestfips == 15 & ftotval <= (26010 *1.30)) |
		 (fpersons == 6 & gestfips != 2 & gestfips == 15 & ftotval <= (29760*1.30)) |
		 (fpersons == 7 & gestfips != 2 & gestfips == 15 & ftotval <= (33510*1.30)) |
		 (fpersons == 8 & gestfips != 2 & gestfips == 15 & ftotval <= (37260*1.30)) |
		 (fpersons >= 9 & gestfips != 2 & gestfips == 15 & ftotval <= ((37260+(fpersons - 8)*3750)*1.30)) |
		#Alaska
			(fpersons == 1 & gestfips == 2 & gestfips != 15 & ftotval <= (11950*1.30)) | 
			(fpersons == 2 & gestfips == 2 & gestfips != 15 & ftotval <= (16030	*1.30)) | 
			(fpersons == 3 & gestfips == 2 & gestfips != 15 & ftotval <= (20110	*1.30)) |
		 (fpersons == 4 & gestfips == 2 & gestfips != 15 & ftotval <= (24190	*1.30)) |
		 (fpersons == 5 & gestfips == 2 & gestfips != 15 & ftotval <= (28270	 *1.30)) |
		 (fpersons == 6 & gestfips == 2 & gestfips != 15 & ftotval <= (32350	*1.30)) |
		 (fpersons == 7 & gestfips == 2 & gestfips != 15 & ftotval <= (36430	*1.30)) |
		 (fpersons == 8 & gestfips == 2 & gestfips != 15 & ftotval <= (40510	*1.30)) |
		 (fpersons >= 9 & gestfips == 2 & gestfips != 15 & ftotval <= ((40510+(fpersons - 8)* 4080	)*1.30)) |
				(paw_typ == 1 | paw_typ == 3 | hfoodsp == 1 | povll < 5))

kids_reduced5 <- subset(kids5,
			((fpersons == 1 & gestfips != 2 & gestfips != 15 & ftotval > (9570 * 1.30) & ftotval < 9570*1.85) | 
			(fpersons == 2 & gestfips != 2 & gestfips != 15 & ftotval > (12830 * 1.30) & ftotval < 12830*1.85) | 
			(fpersons == 3 & gestfips != 2 & gestfips != 15 & ftotval > (16090 * 1.30) & ftotval < 16090 * 1.85) | 
		 (fpersons == 4 & gestfips != 2 & gestfips != 15 & ftotval > (19350 * 1.30) & ftotval < 19350 * 1.85) | 
		 (fpersons == 5 & gestfips != 2 & gestfips != 15 & ftotval > (22610	 * 1.30) & ftotval < 22610	 *1.85) | 
		 (fpersons == 6 & gestfips != 2 & gestfips != 15 & ftotval > (25870	 * 1.30) & ftotval < 25870	 *1.85) | 
		 (fpersons == 7 & gestfips != 2 & gestfips != 15 & ftotval > (29130 * 1.30) & ftotval < 29130 *1.85) | 
		 (fpersons == 8 & gestfips != 2 & gestfips != 15 & ftotval > (32390 * 1.30) & ftotval < 32390 *1.85) | 
		 (fpersons >= 9 & gestfips != 2 & gestfips != 15 & ftotval > (32390+(fpersons - 8)*3260)* 1.30 & ftotval < (32390+(fpersons - 8)*3260)*1.85) | 
	#Hawaii			 
					(fpersons == 1 & gestfips != 2 & gestfips == 15 & ftotval > (11010 * 1.30) & ftotval < (11010 *1.85)) | 
			(fpersons == 2 & gestfips != 2 & gestfips == 15 & ftotval > (14760 * 1.30) & ftotval < (14760 *1.85)) | 
			(fpersons == 3 & gestfips != 2 & gestfips == 15 & ftotval > (18510 * 1.30) & ftotval < (18510 *1.85)) | 
		 (fpersons == 4 & gestfips != 2 & gestfips == 15 & ftotval > (22260 * 1.30) & ftotval < (22260 *1.85)) | 
			(fpersons == 5 & gestfips != 2 & gestfips == 15 & ftotval > (26010 * 1.30) & ftotval < (26010 *1.85)) | 
		 (fpersons == 6 & gestfips != 2 & gestfips == 15 & ftotval > (29760 * 1.30) & ftotval < (29760 *1.85)) | 
		 (fpersons == 7 & gestfips != 2 & gestfips == 15 & ftotval > (33510 * 1.30) & ftotval < (33510 *1.85)) | 
		 (fpersons == 8 & gestfips != 2 & gestfips == 15 & ftotval > (37260 * 1.30) & ftotval < (37260 *1.85)) | 
		 (fpersons >= 9 & gestfips != 2 & gestfips == 15 & ftotval > (37260+(fpersons - 8)*3750) * 1.30 & ftotval < (37260+(fpersons - 8)*3750)*1.85) | 
					(fpersons == 1 & gestfips == 2 & gestfips != 15 & ftotval > (11950 * 1.30) & ftotval < (11950 *1.85)) | 
			(fpersons == 2 & gestfips == 2 & gestfips != 15 & ftotval > (16030 * 1.30) & ftotval < (16030 *1.85)) | 
			(fpersons == 3 & gestfips == 2 & gestfips != 15 & ftotval > (20110 * 1.30) & ftotval < (20110 *1.85)) | 
		 (fpersons == 4 & gestfips == 2 & gestfips != 15 & ftotval > (24190 * 1.30) & ftotval < (24190 *1.85)) | 
			(fpersons == 5 & gestfips == 2 & gestfips != 15 & ftotval > (28270 * 1.30) & ftotval < (28270 *1.85)) | 
		 (fpersons == 6 & gestfips == 2 & gestfips != 15 & ftotval > (32350 * 1.30) & ftotval < (32350 *1.85)) | 
		 (fpersons == 7 & gestfips == 2 & gestfips != 15 & ftotval > (36430 * 1.30) & ftotval < (36430 *1.85)) | 
		 (fpersons == 8 & gestfips == 2 & gestfips != 15 & ftotval > (40510 * 1.30) & ftotval < (40510 *1.85)) | 
		 (fpersons >= 9 & gestfips == 2 & gestfips != 15 & ftotval > (40510+(fpersons - 8)*4080) * 1.30 & ftotval < (40510+(fpersons - 8)*4080) *1.85)) 
			& ((paw_typ == 0 | paw_typ == 3 ) & (hfoodsp == 2 | 0) & povll >= 5 & povll < 9 ))

kids_full5 <- subset(kids5,
			((fpersons == 1 & gestfips != 2 & gestfips != 15 & ftotval > (9570 * 1.30) & ftotval >  9570*1.85) | 
			(fpersons == 2 & gestfips != 2 & gestfips != 15 & ftotval > (12830 * 1.30) & ftotval >  12830*1.85) | 
			(fpersons == 3 & gestfips != 2 & gestfips != 15 & ftotval > (16090 * 1.30) & ftotval >  16090 * 1.85) | 
		 (fpersons == 4 & gestfips != 2 & gestfips != 15 & ftotval > (19350 * 1.30) & ftotval >  19350 * 1.85) | 
		 (fpersons == 5 & gestfips != 2 & gestfips != 15 & ftotval > (22610	 * 1.30) & ftotval >  22610	 *1.85) | 
		 (fpersons == 6 & gestfips != 2 & gestfips != 15 & ftotval > (25870	 * 1.30) & ftotval >  25870	 *1.85) | 
		 (fpersons == 7 & gestfips != 2 & gestfips != 15 & ftotval > (29130 * 1.30) & ftotval >  29130 *1.85) | 
		 (fpersons == 8 & gestfips != 2 & gestfips != 15 & ftotval > (32390 * 1.30) & ftotval >  32390 *1.85) | 
		 (fpersons >= 9 & gestfips != 2 & gestfips != 15 & ftotval > (32390+(fpersons - 8)*3260)* 1.30 & ftotval >  (32390+(fpersons - 8)*3260)*1.85) | 
	#Hawaii			 
					(fpersons == 1 & gestfips != 2 & gestfips == 15 & ftotval > (11010 * 1.30) & ftotval >  (11010 *1.85)) | 
			(fpersons == 2 & gestfips != 2 & gestfips == 15 & ftotval > (14760 * 1.30) & ftotval >  (14760 *1.85)) | 
			(fpersons == 3 & gestfips != 2 & gestfips == 15 & ftotval > (18510 * 1.30) & ftotval >  (18510 *1.85)) | 
		 (fpersons == 4 & gestfips != 2 & gestfips == 15 & ftotval > (22260 * 1.30) & ftotval >  (22260 *1.85)) | 
			(fpersons == 5 & gestfips != 2 & gestfips == 15 & ftotval > (26010 * 1.30) & ftotval >  (26010 *1.85)) | 
		 (fpersons == 6 & gestfips != 2 & gestfips == 15 & ftotval > (29760 * 1.30) & ftotval >  (29760 *1.85)) | 
		 (fpersons == 7 & gestfips != 2 & gestfips == 15 & ftotval > (33510 * 1.30) & ftotval >  (33510 *1.85)) | 
		 (fpersons == 8 & gestfips != 2 & gestfips == 15 & ftotval > (37260 * 1.30) & ftotval >  (37260 *1.85)) | 
		 (fpersons >= 9 & gestfips != 2 & gestfips == 15 & ftotval > (37260+(fpersons - 8)*3750) * 1.30 & ftotval >  (37260+(fpersons - 8)*3750)*1.85) | 
					(fpersons == 1 & gestfips == 2 & gestfips != 15 & ftotval > (11950 * 1.30) & ftotval >  (11950 *1.85)) | 
			(fpersons == 2 & gestfips == 2 & gestfips != 15 & ftotval > (16030 * 1.30) & ftotval >  (16030 *1.85)) | 
			(fpersons == 3 & gestfips == 2 & gestfips != 15 & ftotval > (20110 * 1.30) & ftotval >  (20110 *1.85)) | 
		 (fpersons == 4 & gestfips == 2 & gestfips != 15 & ftotval > (24190 * 1.30) & ftotval >  (24190 *1.85)) | 
			(fpersons == 5 & gestfips == 2 & gestfips != 15 & ftotval > (28270 * 1.30) & ftotval >  (28270 *1.85)) | 
		 (fpersons == 6 & gestfips == 2 & gestfips != 15 & ftotval > (32350 * 1.30) & ftotval >  (32350 *1.85)) | 
		 (fpersons == 7 & gestfips == 2 & gestfips != 15 & ftotval > (36430 * 1.30) & ftotval >  (36430 *1.85)) | 
		 (fpersons == 8 & gestfips == 2 & gestfips != 15 & ftotval > (40510 * 1.30) & ftotval >  (40510 *1.85)) | 
		 (fpersons >= 9 & gestfips == 2 & gestfips != 15 & ftotval > (40510+(fpersons - 8)*4080) * 1.30 & ftotval >  (40510+(fpersons - 8)*4080) *1.85)) 
			& ((paw_typ == 0 | paw_typ == 3 ) & (hfoodsp == 2 | 0) & povll >= 7))

##### 2006 #####
kids_free6 <- subset(kids6,
			(fpersons == 1 & gestfips != 2 & gestfips != 15 & ftotval <= (9800*1.30)) | 
			(fpersons == 2 & gestfips != 2 & gestfips != 15 & ftotval <= (13200*1.30)) | 
			(fpersons == 3 & gestfips != 2 & gestfips != 15 & ftotval <= (16600	*1.30)) |
		 (fpersons == 4 & gestfips != 2 & gestfips != 15 & ftotval <= (20000	*1.30)) |
		 (fpersons == 5 & gestfips != 2 & gestfips != 15 & ftotval <= (23400*1.30)) |
		 (fpersons == 6 & gestfips != 2 & gestfips != 15 & ftotval <= (26800	*1.30)) |
		 (fpersons == 7 & gestfips != 2 & gestfips != 15 & ftotval <= (30200	*1.30)) |
		 (fpersons == 8 & gestfips != 2 & gestfips != 15 & ftotval <= (33600	*1.30)) |
		 (fpersons >= 9 & gestfips != 2 & gestfips != 15 & ftotval <= ((33600+(fpersons - 8)* 3400	)*1.30)) |
	#Hawaii			
							(fpersons == 1 & gestfips != 2 & gestfips == 15 & ftotval <= (11270*1.30)) | 
			(fpersons == 2 & gestfips != 2 & gestfips == 15 & ftotval <= (15180*1.30)) | 
			(fpersons == 3 & gestfips != 2 & gestfips == 15 & ftotval <= (19090*1.30)) |
		 (fpersons == 4 & gestfips != 2 & gestfips == 15 & ftotval <= (23000*1.30)) |
		 (fpersons == 5 & gestfips != 2 & gestfips == 15 & ftotval <= (26910 *1.30)) |
		 (fpersons == 6 & gestfips != 2 & gestfips == 15 & ftotval <= (30820*1.30)) |
		 (fpersons == 7 & gestfips != 2 & gestfips == 15 & ftotval <= (34730*1.30)) |
		 (fpersons == 8 & gestfips != 2 & gestfips == 15 & ftotval <= (38640*1.30)) |
		 (fpersons >= 9 & gestfips != 2 & gestfips == 15 & ftotval <= ((38640+(fpersons - 8)*3910)*1.30)) |
#Alaska
			(fpersons == 1 & gestfips == 2 & gestfips != 15 & ftotval <= (12250*1.30)) | 
			(fpersons == 2 & gestfips == 2 & gestfips != 15 & ftotval <= (16500	*1.30)) | 
			(fpersons == 3 & gestfips == 2 & gestfips != 15 & ftotval <= (20750	*1.30)) |
		 (fpersons == 4 & gestfips == 2 & gestfips != 15 & ftotval <= (25000	*1.30)) |
		 (fpersons == 5 & gestfips == 2 & gestfips != 15 & ftotval <= (29250	 *1.30)) |
		 (fpersons == 6 & gestfips == 2 & gestfips != 15 & ftotval <= (33500	*1.30)) |
		 (fpersons == 7 & gestfips == 2 & gestfips != 15 & ftotval <= (37750	*1.30)) |
		 (fpersons == 8 & gestfips == 2 & gestfips != 15 & ftotval <= (42000	*1.30)) |
		 (fpersons >= 9 & gestfips == 2 & gestfips != 15 & ftotval <= ((42000+(fpersons - 8)* 4250	)*1.30)) |
				(paw_typ == 1 | paw_typ == 3 | hfoodsp == 1 | povll < 5))

kids_reduced6 <- subset(kids6,
			((fpersons == 1 & gestfips != 2 & gestfips != 15 & ftotval > (9800 * 1.30) & ftotval < 9800*1.85) | 
			(fpersons == 2 & gestfips != 2 & gestfips != 15 & ftotval > (13200 * 1.30) & ftotval < 13200*1.85) | 
			(fpersons == 3 & gestfips != 2 & gestfips != 15 & ftotval > (16600 * 1.30) & ftotval < 16600 * 1.85) | 
		 (fpersons == 4 & gestfips != 2 & gestfips != 15 & ftotval > (20000 * 1.30) & ftotval < 20000 * 1.85) | 
		 (fpersons == 5 & gestfips != 2 & gestfips != 15 & ftotval > (23400	* 1.30) & ftotval < 23400	 *1.85) | 
		 (fpersons == 6 & gestfips != 2 & gestfips != 15 & ftotval > (26800	* 1.30) & ftotval < 26800	 *1.85) | 
		 (fpersons == 7 & gestfips != 2 & gestfips != 15 & ftotval > (30200 * 1.30) & ftotval < 30200 *1.85) | 
		 (fpersons == 8 & gestfips != 2 & gestfips != 15 & ftotval > (33600 * 1.30) & ftotval < 33600 *1.85) | 
		 (fpersons >= 9 & gestfips != 2 & gestfips != 15 & ftotval > (33600+(fpersons - 8)*3400)* 1.30 & ftotval < (33600+(fpersons - 8)*3400)*1.85) | 
					(fpersons == 1 & gestfips != 2 & gestfips == 15 & ftotval > (11270 * 1.30) & ftotval < (11270 *1.85)) | 
			(fpersons == 2 & gestfips != 2 & gestfips == 15 & ftotval > (15180 * 1.30) & ftotval < (15180 *1.85)) | 
			(fpersons == 3 & gestfips != 2 & gestfips == 15 & ftotval > (19090 * 1.30) & ftotval < (19090 *1.85)) | 
		 (fpersons == 4 & gestfips != 2 & gestfips == 15 & ftotval > (23000 * 1.30) & ftotval < (23000 *1.85)) | 
			(fpersons == 5 & gestfips != 2 & gestfips == 15 & ftotval > (26910 * 1.30) & ftotval < (26910 *1.85)) | 
		 (fpersons == 6 & gestfips != 2 & gestfips == 15 & ftotval > (30820 * 1.30) & ftotval < (30820 *1.85)) | 
		 (fpersons == 7 & gestfips != 2 & gestfips == 15 & ftotval > (34730 * 1.30) & ftotval < (34730 *1.85)) | 
		 (fpersons == 8 & gestfips != 2 & gestfips == 15 & ftotval > (38640 * 1.30) & ftotval < (38640 *1.85)) | 
		 (fpersons >= 9 & gestfips != 2 & gestfips == 15 & ftotval > (38640+(fpersons - 8)*3910) * 1.30 & ftotval < (38640+(fpersons - 8)*3910)*1.85) | 
					(fpersons == 1 & gestfips == 2 & gestfips != 15 & ftotval > (12250 * 1.30) & ftotval < (12250 *1.85)) | 
			(fpersons == 2 & gestfips == 2 & gestfips != 15 & ftotval > (16500 * 1.30) & ftotval < (16500 *1.85)) | 
			(fpersons == 3 & gestfips == 2 & gestfips != 15 & ftotval > (20750 * 1.30) & ftotval < (20750 *1.85)) | 
		 (fpersons == 4 & gestfips == 2 & gestfips != 15 & ftotval > (25000 * 1.30) & ftotval < (25000 *1.85)) | 
			(fpersons == 5 & gestfips == 2 & gestfips != 15 & ftotval > (29250 * 1.30) & ftotval < (29250 *1.85)) | 
		 (fpersons == 6 & gestfips == 2 & gestfips != 15 & ftotval > (33500 * 1.30) & ftotval < (33500 *1.85)) | 
		 (fpersons == 7 & gestfips == 2 & gestfips != 15 & ftotval > (37750 * 1.30) & ftotval < (37750 *1.85)) | 
		 (fpersons == 8 & gestfips == 2 & gestfips != 15 & ftotval > (42000 * 1.30) & ftotval < (42000 *1.85)) | 
		 (fpersons >= 9 & gestfips == 2 & gestfips != 15 & ftotval > (42000+(fpersons - 8)*4250) * 1.30 & ftotval < (42000+(fpersons - 8)*4250) *1.85)) 
			& ((paw_typ == 0 | paw_typ == 3 ) & (hfoodsp == 2 | 0) & povll >= 5 & povll < 9 ))

kids_full6 <- subset(kids6,
			((fpersons == 1 & gestfips != 2 & gestfips != 15 & ftotval > (9800 * 1.30) & ftotval >  9800*1.85) | 
			(fpersons == 2 & gestfips != 2 & gestfips != 15 & ftotval > (13200 * 1.30) & ftotval >  13200*1.85) | 
			(fpersons == 3 & gestfips != 2 & gestfips != 15 & ftotval > (16600 * 1.30) & ftotval >  16600 * 1.85) | 
		 (fpersons == 4 & gestfips != 2 & gestfips != 15 & ftotval > (20000 * 1.30) & ftotval >  20000 * 1.85) | 
		 (fpersons == 5 & gestfips != 2 & gestfips != 15 & ftotval > (23400	* 1.30) & ftotval >  23400	 *1.85) | 
		 (fpersons == 6 & gestfips != 2 & gestfips != 15 & ftotval > (26800	* 1.30) & ftotval >  26800	 *1.85) | 
		 (fpersons == 7 & gestfips != 2 & gestfips != 15 & ftotval > (30200 * 1.30) & ftotval >  30200 *1.85) | 
		 (fpersons == 8 & gestfips != 2 & gestfips != 15 & ftotval > (33600 * 1.30) & ftotval >  33600 *1.85) | 
		 (fpersons >= 9 & gestfips != 2 & gestfips != 15 & ftotval > (33600+(fpersons - 8)*3400)* 1.30 & ftotval >  (33600+(fpersons - 8)*3400)*1.85) | 
	#Hawaii			 
					(fpersons == 1 & gestfips != 2 & gestfips == 15 & ftotval > (11270 * 1.30) & ftotval >  (11270 *1.85)) | 
			(fpersons == 2 & gestfips != 2 & gestfips == 15 & ftotval > (15180 * 1.30) & ftotval >  (15180 *1.85)) | 
			(fpersons == 3 & gestfips != 2 & gestfips == 15 & ftotval > (19090 * 1.30) & ftotval >  (19090 *1.85)) | 
		 (fpersons == 4 & gestfips != 2 & gestfips == 15 & ftotval > (23000 * 1.30) & ftotval >  (23000 *1.85)) | 
			(fpersons == 5 & gestfips != 2 & gestfips == 15 & ftotval > (26910 * 1.30) & ftotval >  (26910 *1.85)) | 
		 (fpersons == 6 & gestfips != 2 & gestfips == 15 & ftotval > (30820 * 1.30) & ftotval >  (30820 *1.85)) | 
		 (fpersons == 7 & gestfips != 2 & gestfips == 15 & ftotval > (34730 * 1.30) & ftotval >  (34730 *1.85)) | 
		 (fpersons == 8 & gestfips != 2 & gestfips == 15 & ftotval > (38640 * 1.30) & ftotval >  (38640 *1.85)) | 
		 (fpersons >= 9 & gestfips != 2 & gestfips == 15 & ftotval > (38640+(fpersons - 8)*3910) * 1.30 & ftotval >  (38640+(fpersons - 8)*3910)*1.85) | 
					(fpersons == 1 & gestfips == 2 & gestfips != 15 & ftotval > (12250 * 1.30) & ftotval >  (12250 *1.85)) | 
			(fpersons == 2 & gestfips == 2 & gestfips != 15 & ftotval > (16500 * 1.30) & ftotval >  (16500 *1.85)) | 
			(fpersons == 3 & gestfips == 2 & gestfips != 15 & ftotval > (20750 * 1.30) & ftotval >  (20750 *1.85)) | 
		 (fpersons == 4 & gestfips == 2 & gestfips != 15 & ftotval > (25000 * 1.30) & ftotval >  (25000 *1.85)) | 
			(fpersons == 5 & gestfips == 2 & gestfips != 15 & ftotval > (29250 * 1.30) & ftotval >  (29250 *1.85)) | 
		 (fpersons == 6 & gestfips == 2 & gestfips != 15 & ftotval > (33500 * 1.30) & ftotval >  (33500 *1.85)) | 
		 (fpersons == 7 & gestfips == 2 & gestfips != 15 & ftotval > (37750 * 1.30) & ftotval >  (37750 *1.85)) | 
		 (fpersons == 8 & gestfips == 2 & gestfips != 15 & ftotval > (42000 * 1.30) & ftotval >  (42000 *1.85)) | 
		 (fpersons >= 9 & gestfips == 2 & gestfips != 15 & ftotval > (42000+(fpersons - 8)*4250) * 1.30 & ftotval >  (42000+(fpersons - 8)*4250) *1.85)) 
			& ((paw_typ == 0 | paw_typ == 3 ) & (hfoodsp == 2 | 0) & povll >= 7))

##### 2007 #####
kids_free7 <- subset(kids7,
			(fpersons == 1 & gestfips != 2 & gestfips != 15 & ftotval <= (10210*1.30)) | 
			(fpersons == 2 & gestfips != 2 & gestfips != 15 & ftotval <= (13690*1.30)) | 
			(fpersons == 3 & gestfips != 2 & gestfips != 15 & ftotval <= (17170	*1.30)) |
		 (fpersons == 4 & gestfips != 2 & gestfips != 15 & ftotval <= (20650	*1.30)) |
		 (fpersons == 5 & gestfips != 2 & gestfips != 15 & ftotval <= (24130*1.30)) |
		 (fpersons == 6 & gestfips != 2 & gestfips != 15 & ftotval <= (27610	*1.30)) |
		 (fpersons == 7 & gestfips != 2 & gestfips != 15 & ftotval <= (31090	*1.30)) |
		 (fpersons == 8 & gestfips != 2 & gestfips != 15 & ftotval <= (34570	*1.30)) |
		 (fpersons >= 9 & gestfips != 2 & gestfips != 15 & ftotval <= ((34570+(fpersons - 8)* 3480	)*1.30)) |
	#Hawaii			
							(fpersons == 1 & gestfips != 2 & gestfips == 15 & ftotval <= (11750*1.30)) | 
			(fpersons == 2 & gestfips != 2 & gestfips == 15 & ftotval <= (15750*1.30)) | 
			(fpersons == 3 & gestfips != 2 & gestfips == 15 & ftotval <= (19750*1.30)) |
		 (fpersons == 4 & gestfips != 2 & gestfips == 15 & ftotval <= (23750*1.30)) |
		 (fpersons == 5 & gestfips != 2 & gestfips == 15 & ftotval <= (27750 *1.30)) |
		 (fpersons == 6 & gestfips != 2 & gestfips == 15 & ftotval <= (31750*1.30)) |
		 (fpersons == 7 & gestfips != 2 & gestfips == 15 & ftotval <= (35750*1.30)) |
		 (fpersons == 8 & gestfips != 2 & gestfips == 15 & ftotval <= (39750*1.30)) |
		 (fpersons >= 9 & gestfips != 2 & gestfips == 15 & ftotval <= ((39750+(fpersons - 8)*4000)*1.30)) |
#Alaska
			(fpersons == 1 & gestfips == 2 & gestfips != 15 & ftotval <= (12770*1.30)) | 
			(fpersons == 2 & gestfips == 2 & gestfips != 15 & ftotval <= (17120	*1.30)) | 
			(fpersons == 3 & gestfips == 2 & gestfips != 15 & ftotval <= (21470	*1.30)) |
		 (fpersons == 4 & gestfips == 2 & gestfips != 15 & ftotval <= (25820	*1.30)) |
		 (fpersons == 5 & gestfips == 2 & gestfips != 15 & ftotval <= (30170	 *1.30)) |
		 (fpersons == 6 & gestfips == 2 & gestfips != 15 & ftotval <= (34520	*1.30)) |
		 (fpersons == 7 & gestfips == 2 & gestfips != 15 & ftotval <= (38870	*1.30)) |
		 (fpersons == 8 & gestfips == 2 & gestfips != 15 & ftotval <= (43220	*1.30)) |
		 (fpersons >= 9 & gestfips == 2 & gestfips != 15 & ftotval <= ((43220+(fpersons - 8)* 4350	)*1.30)) |
				(paw_typ == 1 | paw_typ == 3 | hfoodsp == 1 | povll < 5))

kids_reduced7 <- subset(kids7,
			((fpersons == 1 & gestfips != 2 & gestfips != 15 & ftotval > (10210 * 1.30) & ftotval < 10210*1.85) | 
			(fpersons == 2 & gestfips != 2 & gestfips != 15 & ftotval > (13690 * 1.30) & ftotval < 13690*1.85) | 
			(fpersons == 3 & gestfips != 2 & gestfips != 15 & ftotval > (17170 * 1.30) & ftotval < 17170 * 1.85) | 
		 (fpersons == 4 & gestfips != 2 & gestfips != 15 & ftotval > (20650 * 1.30) & ftotval < 20650 * 1.85) | 
		 (fpersons == 5 & gestfips != 2 & gestfips != 15 & ftotval > (24130	* 1.30) & ftotval < 24130	* 1.85) | 
		 (fpersons == 6 & gestfips != 2 & gestfips != 15 & ftotval > (27610	* 1.30) & ftotval < 27610	* 1.85) | 
		 (fpersons == 7 & gestfips != 2 & gestfips != 15 & ftotval > (31090 * 1.30) & ftotval < 31090 * 1.85) | 
		 (fpersons == 8 & gestfips != 2 & gestfips != 15 & ftotval > (34570 * 1.30) & ftotval < 34570 * 1.85) | 
		 (fpersons >= 9 & gestfips != 2 & gestfips != 15 & ftotval > (34570+(fpersons - 8)*3480)* 1.30 & ftotval < (34570+(fpersons - 8)*3480)*1.85) | 
	#Hawaii			 
					(fpersons == 1 & gestfips != 2 & gestfips == 15 & ftotval > (11750 * 1.30) & ftotval < (11750 *1.85)) | 
			(fpersons == 2 & gestfips != 2 & gestfips == 15 & ftotval > (15750 * 1.30) & ftotval < (15750 *1.85)) | 
			(fpersons == 3 & gestfips != 2 & gestfips == 15 & ftotval > (19750 * 1.30) & ftotval < (19750 *1.85)) | 
		 (fpersons == 4 & gestfips != 2 & gestfips == 15 & ftotval > (23750 * 1.30) & ftotval < (23750 *1.85)) | 
			(fpersons == 5 & gestfips != 2 & gestfips == 15 & ftotval > (27750 * 1.30) & ftotval < (27750 *1.85)) | 
		 (fpersons == 6 & gestfips != 2 & gestfips == 15 & ftotval > (31750 * 1.30) & ftotval < (31750 *1.85)) | 
		 (fpersons == 7 & gestfips != 2 & gestfips == 15 & ftotval > (35750 * 1.30) & ftotval < (35750 *1.85)) | 
		 (fpersons == 8 & gestfips != 2 & gestfips == 15 & ftotval > (39750 * 1.30) & ftotval < (39750 *1.85)) | 
		 (fpersons >= 9 & gestfips != 2 & gestfips == 15 & ftotval > (39750+(fpersons - 8)*4000) * 1.30 & ftotval < (39750+(fpersons - 8)*4000)*1.85) | 
					(fpersons == 1 & gestfips == 2 & gestfips != 15 & ftotval > (12770 * 1.30) & ftotval < (12770 *1.85)) | 
			(fpersons == 2 & gestfips == 2 & gestfips != 15 & ftotval > (17120 * 1.30) & ftotval < (17120 *1.85)) | 
			(fpersons == 3 & gestfips == 2 & gestfips != 15 & ftotval > (21470 * 1.30) & ftotval < (21470 *1.85)) | 
		 (fpersons == 4 & gestfips == 2 & gestfips != 15 & ftotval > (25820 * 1.30) & ftotval < (25820 *1.85)) | 
			(fpersons == 5 & gestfips == 2 & gestfips != 15 & ftotval > (30170 * 1.30) & ftotval < (30170 *1.85)) | 
		 (fpersons == 6 & gestfips == 2 & gestfips != 15 & ftotval > (34520 * 1.30) & ftotval < (34520 *1.85)) | 
		 (fpersons == 7 & gestfips == 2 & gestfips != 15 & ftotval > (38870 * 1.30) & ftotval < (38870 *1.85)) | 
		 (fpersons == 8 & gestfips == 2 & gestfips != 15 & ftotval > (43220 * 1.30) & ftotval < (43220 *1.85)) | 
		 (fpersons >= 9 & gestfips == 2 & gestfips != 15 & ftotval > (43220+(fpersons - 8)*4350) * 1.30 & ftotval < (43220+(fpersons - 8)*4350) *1.85)) 
			& ((paw_typ == 0 | paw_typ == 3 ) & (hfoodsp == 2 | 0) & povll >= 5 & povll < 9 ))

kids_full7 <- subset(kids7,
			((fpersons == 1 & gestfips != 2 & gestfips != 15 & ftotval > (10210 * 1.30) & ftotval > 10210*1.85) | 
			(fpersons == 2 & gestfips != 2 & gestfips != 15 & ftotval > (13690 * 1.30) & ftotval >  13690*1.85) | 
			(fpersons == 3 & gestfips != 2 & gestfips != 15 & ftotval > (17170 * 1.30) & ftotval >  17170 * 1.85) | 
		 (fpersons == 4 & gestfips != 2 & gestfips != 15 & ftotval > (20650 * 1.30) & ftotval >  20650 * 1.85) | 
		 (fpersons == 5 & gestfips != 2 & gestfips != 15 & ftotval > (24130	* 1.30) & ftotval >  24130	* 1.85) | 
		 (fpersons == 6 & gestfips != 2 & gestfips != 15 & ftotval > (27610	* 1.30) & ftotval >  27610	* 1.85) | 
		 (fpersons == 7 & gestfips != 2 & gestfips != 15 & ftotval > (31090 * 1.30) & ftotval >  31090 * 1.85) | 
		 (fpersons == 8 & gestfips != 2 & gestfips != 15 & ftotval > (34570 * 1.30) & ftotval >  34570 * 1.85) | 
		 (fpersons >= 9 & gestfips != 2 & gestfips != 15 & ftotval > (34570+(fpersons - 8)*3480)* 1.30 & ftotval > (34570+(fpersons - 8)*3480)*1.85) | 
					(fpersons == 1 & gestfips != 2 & gestfips == 15 & ftotval > (11750 * 1.30) & ftotval >  (11750 *1.85)) | 
			(fpersons == 2 & gestfips != 2 & gestfips == 15 & ftotval > (15750 * 1.30) & ftotval >  (15750 *1.85)) | 
			(fpersons == 3 & gestfips != 2 & gestfips == 15 & ftotval > (19750 * 1.30) & ftotval >  (19750 *1.85)) | 
		 (fpersons == 4 & gestfips != 2 & gestfips == 15 & ftotval > (23750 * 1.30) & ftotval >  (23750 *1.85)) | 
			(fpersons == 5 & gestfips != 2 & gestfips == 15 & ftotval > (27750 * 1.30) & ftotval >  (27750 *1.85)) | 
		 (fpersons == 6 & gestfips != 2 & gestfips == 15 & ftotval > (31750 * 1.30) & ftotval >  (31750 *1.85)) | 
		 (fpersons == 7 & gestfips != 2 & gestfips == 15 & ftotval > (35750 * 1.30) & ftotval >  (35750 *1.85)) | 
		 (fpersons == 8 & gestfips != 2 & gestfips == 15 & ftotval > (39750 * 1.30) & ftotval >  (39750 *1.85)) | 
		 (fpersons >= 9 & gestfips != 2 & gestfips == 15 & ftotval > (39750+(fpersons - 8)*4000) * 1.30 & ftotval > (39750+(fpersons - 8)*4000)*1.85) | 
					(fpersons == 1 & gestfips == 2 & gestfips != 15 & ftotval > (12770 * 1.30) & ftotval >  (12770 *1.85)) | 
			(fpersons == 2 & gestfips == 2 & gestfips != 15 & ftotval > (17120 * 1.30) & ftotval >  (17120 *1.85)) | 
			(fpersons == 3 & gestfips == 2 & gestfips != 15 & ftotval > (21470 * 1.30) & ftotval >  (21470 *1.85)) | 
		 (fpersons == 4 & gestfips == 2 & gestfips != 15 & ftotval > (25820 * 1.30) & ftotval >  (25820 *1.85)) | 
			(fpersons == 5 & gestfips == 2 & gestfips != 15 & ftotval > (30170 * 1.30) & ftotval >  (30170 *1.85)) | 
		 (fpersons == 6 & gestfips == 2 & gestfips != 15 & ftotval > (34520 * 1.30) & ftotval >  (34520 *1.85)) | 
		 (fpersons == 7 & gestfips == 2 & gestfips != 15 & ftotval > (38870 * 1.30) & ftotval >  (38870 *1.85)) | 
		 (fpersons == 8 & gestfips == 2 & gestfips != 15 & ftotval > (43220 * 1.30) & ftotval >  (43220 *1.85)) | 
		 (fpersons >= 9 & gestfips == 2 & gestfips != 15 & ftotval > (43220+(fpersons - 8)*4350) * 1.30 & ftotval > (43220+(fpersons - 8)*4350) *1.85)) 
			& ((paw_typ == 0 | paw_typ == 3 ) & (hfoodsp == 2 | 0) & povll >= 7))

##### 2008 #####
kids_free8 <- subset(kids8,
			(fpersons == 1 & gestfips != 2 & gestfips != 15 & ftotval <= (10400*1.30)) | 
			(fpersons == 2 & gestfips != 2 & gestfips != 15 & ftotval <= (14000*1.30)) | 
			(fpersons == 3 & gestfips != 2 & gestfips != 15 & ftotval <= (17600	*1.30)) |
		 (fpersons == 4 & gestfips != 2 & gestfips != 15 & ftotval <= (21200	*1.30)) |
		 (fpersons == 5 & gestfips != 2 & gestfips != 15 & ftotval <= (24800*1.30)) |
		 (fpersons == 6 & gestfips != 2 & gestfips != 15 & ftotval <= (28400	*1.30)) |
		 (fpersons == 7 & gestfips != 2 & gestfips != 15 & ftotval <= (32000	*1.30)) |
		 (fpersons == 8 & gestfips != 2 & gestfips != 15 & ftotval <= (35600	*1.30)) |
		 (fpersons >= 9 & gestfips != 2 & gestfips != 15 & ftotval <= ((35600+(fpersons - 8)* 3600	)*1.30)) |
							(fpersons == 1 & gestfips != 2 & gestfips == 15 & ftotval <= (11960*1.30)) | 
			(fpersons == 2 & gestfips != 2 & gestfips == 15 & ftotval <= (16100*1.30)) | 
			(fpersons == 3 & gestfips != 2 & gestfips == 15 & ftotval <= (20240*1.30)) |
		 (fpersons == 4 & gestfips != 2 & gestfips == 15 & ftotval <= (24380*1.30)) |
		 (fpersons == 5 & gestfips != 2 & gestfips == 15 & ftotval <= (28520 *1.30)) |
		 (fpersons == 6 & gestfips != 2 & gestfips == 15 & ftotval <= (32660*1.30)) |
		 (fpersons == 7 & gestfips != 2 & gestfips == 15 & ftotval <= (36800*1.30)) |
		 (fpersons == 8 & gestfips != 2 & gestfips == 15 & ftotval <= (40940*1.30)) |
		 (fpersons >= 9 & gestfips != 2 & gestfips == 15 & ftotval <= ((40940+(fpersons - 8)*4140)*1.30)) |
						(fpersons == 1 & gestfips == 2 & gestfips != 15 & ftotval <= (13000*1.30)) | 
			(fpersons == 2 & gestfips == 2 & gestfips != 15 & ftotval <= (17500	*1.30)) | 
			(fpersons == 3 & gestfips == 2 & gestfips != 15 & ftotval <= (22000	*1.30)) |
		 (fpersons == 4 & gestfips == 2 & gestfips != 15 & ftotval <= (26500	*1.30)) |
		 (fpersons == 5 & gestfips == 2 & gestfips != 15 & ftotval <= (31000	 *1.30)) |
		 (fpersons == 6 & gestfips == 2 & gestfips != 15 & ftotval <= (35500	*1.30)) |
		 (fpersons == 7 & gestfips == 2 & gestfips != 15 & ftotval <= (40000	*1.30)) |
		 (fpersons == 8 & gestfips == 2 & gestfips != 15 & ftotval <= (44500	*1.30)) |
		 (fpersons >= 9 & gestfips == 2 & gestfips != 15 & ftotval <= ((44500+(fpersons - 8)* 4500	)*1.30)) |
				(paw_typ == 1 | paw_typ == 3 | hfoodsp == 1 | povll < 5))

kids_reduced8 <- subset(kids8,
			((fpersons == 1 & gestfips != 2 & gestfips != 15 & ftotval > (10400 * 1.30) & ftotval < 10400*1.85) | 
			(fpersons == 2 & gestfips != 2 & gestfips != 15 & ftotval > (14000 * 1.30) & ftotval < 14000*1.85) | 
			(fpersons == 3 & gestfips != 2 & gestfips != 15 & ftotval > (17600 * 1.30) & ftotval < 17600 * 1.85) | 
		 (fpersons == 4 & gestfips != 2 & gestfips != 15 & ftotval > (21200 * 1.30) & ftotval < 21200 * 1.85) | 
		 (fpersons == 5 & gestfips != 2 & gestfips != 15 & ftotval > (24800	* 1.30) & ftotval < 24800	* 1.85) | 
		 (fpersons == 6 & gestfips != 2 & gestfips != 15 & ftotval > (28400	* 1.30) & ftotval < 28400	* 1.85) | 
		 (fpersons == 7 & gestfips != 2 & gestfips != 15 & ftotval > (32000 * 1.30) & ftotval < 32000 * 1.85) | 
		 (fpersons == 8 & gestfips != 2 & gestfips != 15 & ftotval > (35600 * 1.30) & ftotval < 35600 * 1.85) | 
		 (fpersons >= 9 & gestfips != 2 & gestfips != 15 & ftotval > (35600+(fpersons - 8)*3600)* 1.30 & ftotval < (35600+(fpersons - 8)*3600)*1.85) | 
	#Hawaii			 
					(fpersons == 1 & gestfips != 2 & gestfips == 15 & ftotval > (11960 * 1.30) & ftotval < (11960 *1.85)) | 
			(fpersons == 2 & gestfips != 2 & gestfips == 15 & ftotval > (16100 * 1.30) & ftotval < (16100 *1.85)) | 
			(fpersons == 3 & gestfips != 2 & gestfips == 15 & ftotval > (20240 * 1.30) & ftotval < (20240 *1.85)) | 
		 (fpersons == 4 & gestfips != 2 & gestfips == 15 & ftotval > (24380 * 1.30) & ftotval < (24380 *1.85)) | 
			(fpersons == 5 & gestfips != 2 & gestfips == 15 & ftotval > (28520 * 1.30) & ftotval < (28520 *1.85)) | 
		 (fpersons == 6 & gestfips != 2 & gestfips == 15 & ftotval > (32660 * 1.30) & ftotval < (32660 *1.85)) | 
		 (fpersons == 7 & gestfips != 2 & gestfips == 15 & ftotval > (36800 * 1.30) & ftotval < (36800 *1.85)) | 
		 (fpersons == 8 & gestfips != 2 & gestfips == 15 & ftotval > (40940 * 1.30) & ftotval < (40940 *1.85)) | 
		 (fpersons >= 9 & gestfips != 2 & gestfips == 15 & ftotval > (40940+(fpersons - 8)*4140) * 1.30 & ftotval < (40940+(fpersons - 8)*4140)*1.85) | 
					(fpersons == 1 & gestfips == 2 & gestfips != 15 & ftotval > (13000 * 1.30) & ftotval < (13000 *1.85)) | 
			(fpersons == 2 & gestfips == 2 & gestfips != 15 & ftotval > (17500 * 1.30) & ftotval < (17500 *1.85)) | 
			(fpersons == 3 & gestfips == 2 & gestfips != 15 & ftotval > (22000 * 1.30) & ftotval < (22000 *1.85)) | 
		 (fpersons == 4 & gestfips == 2 & gestfips != 15 & ftotval > (26500 * 1.30) & ftotval < (26500 *1.85)) | 
			(fpersons == 5 & gestfips == 2 & gestfips != 15 & ftotval > (31000 * 1.30) & ftotval < (31000 *1.85)) | 
		 (fpersons == 6 & gestfips == 2 & gestfips != 15 & ftotval > (35500 * 1.30) & ftotval < (35500 *1.85)) | 
		 (fpersons == 7 & gestfips == 2 & gestfips != 15 & ftotval > (40000 * 1.30) & ftotval < (40000 *1.85)) | 
		 (fpersons == 8 & gestfips == 2 & gestfips != 15 & ftotval > (44500 * 1.30) & ftotval < (44500 *1.85)) | 
		 (fpersons >= 9 & gestfips == 2 & gestfips != 15 & ftotval > (44500+(fpersons - 8)*4500) * 1.30 & ftotval < (44500+(fpersons - 8)*4500) *1.85)) 
			& ((paw_typ == 0 | paw_typ == 3 ) & (hfoodsp == 2 | 0) & povll >= 5 & povll < 9 ))

kids_full8 <- subset(kids8,
			((fpersons == 1 & gestfips != 2 & gestfips != 15 & ftotval > (10400 * 1.30) & ftotval > 10400*1.85) | 
			(fpersons == 2 & gestfips != 2 & gestfips != 15 & ftotval > (14000 * 1.30) & ftotval >  14000*1.85) | 
			(fpersons == 3 & gestfips != 2 & gestfips != 15 & ftotval > (17600 * 1.30) & ftotval >  17600 * 1.85) | 
		 (fpersons == 4 & gestfips != 2 & gestfips != 15 & ftotval > (21200 * 1.30) & ftotval >  21200 * 1.85) | 
		 (fpersons == 5 & gestfips != 2 & gestfips != 15 & ftotval > (24800	* 1.30) & ftotval >  24800	* 1.85) | 
		 (fpersons == 6 & gestfips != 2 & gestfips != 15 & ftotval > (28400	* 1.30) & ftotval >  28400	* 1.85) | 
		 (fpersons == 7 & gestfips != 2 & gestfips != 15 & ftotval > (32000 * 1.30) & ftotval >  32000 * 1.85) | 
		 (fpersons == 8 & gestfips != 2 & gestfips != 15 & ftotval > (35600 * 1.30) & ftotval >  35600 * 1.85) | 
		 (fpersons >= 9 & gestfips != 2 & gestfips != 15 & ftotval > (35600+(fpersons - 8)*3600)* 1.30 & ftotval >  (35600+(fpersons - 8)*3600)*1.85) | 
					(fpersons == 1 & gestfips != 2 & gestfips == 15 & ftotval > (11960 * 1.30) & ftotval >  (11960 *1.85)) | 
			(fpersons == 2 & gestfips != 2 & gestfips == 15 & ftotval > (16100 * 1.30) & ftotval >  (16100 *1.85)) | 
			(fpersons == 3 & gestfips != 2 & gestfips == 15 & ftotval > (20240 * 1.30) & ftotval >  (20240 *1.85)) | 
		 (fpersons == 4 & gestfips != 2 & gestfips == 15 & ftotval > (24380 * 1.30) & ftotval >  (24380 *1.85)) | 
			(fpersons == 5 & gestfips != 2 & gestfips == 15 & ftotval > (28520 * 1.30) & ftotval >  (28520 *1.85)) | 
		 (fpersons == 6 & gestfips != 2 & gestfips == 15 & ftotval > (32660 * 1.30) & ftotval >  (32660 *1.85)) | 
		 (fpersons == 7 & gestfips != 2 & gestfips == 15 & ftotval > (36800 * 1.30) & ftotval >  (36800 *1.85)) | 
		 (fpersons == 8 & gestfips != 2 & gestfips == 15 & ftotval > (40940 * 1.30) & ftotval >  (40940 *1.85)) | 
		 (fpersons >= 9 & gestfips != 2 & gestfips == 15 & ftotval > (40940+(fpersons - 8)*4140) * 1.30 & ftotval >  (40940+(fpersons - 8)*4140)*1.85) | 
					(fpersons == 1 & gestfips == 2 & gestfips != 15 & ftotval > (13000 * 1.30) & ftotval >  (13000 *1.85)) | 
			(fpersons == 2 & gestfips == 2 & gestfips != 15 & ftotval > (17500 * 1.30) & ftotval >  (17500 *1.85)) | 
			(fpersons == 3 & gestfips == 2 & gestfips != 15 & ftotval > (22000 * 1.30) & ftotval >  (22000 *1.85)) | 
		 (fpersons == 4 & gestfips == 2 & gestfips != 15 & ftotval > (26500 * 1.30) & ftotval >  (26500 *1.85)) | 
			(fpersons == 5 & gestfips == 2 & gestfips != 15 & ftotval > (31000 * 1.30) & ftotval >  (31000 *1.85)) | 
		 (fpersons == 6 & gestfips == 2 & gestfips != 15 & ftotval > (35500 * 1.30) & ftotval >  (35500 *1.85)) | 
		 (fpersons == 7 & gestfips == 2 & gestfips != 15 & ftotval > (40000 * 1.30) & ftotval >  (40000 *1.85)) | 
		 (fpersons == 8 & gestfips == 2 & gestfips != 15 & ftotval > (44500 * 1.30) & ftotval >  (44500 *1.85)) | 
		 (fpersons >= 9 & gestfips == 2 & gestfips != 15 & ftotval > (44500+(fpersons - 8)*4500) * 1.30 & ftotval >  (44500+(fpersons - 8)*4500)*1.85)) 
			& ((paw_typ == 0 | paw_typ == 3 ) & (hfoodsp == 2 | 0) & povll >= 7))

##### 2009 #####

kids_free9 <- subset(kids9,
			(fpersons == 1 & gestfips != 2 & gestfips != 15 & ftotval <= (10830*1.30)) | 
			(fpersons == 2 & gestfips != 2 & gestfips != 15 & ftotval <= (14570*1.30)) | 
			(fpersons == 3 & gestfips != 2 & gestfips != 15 & ftotval <= (18310*1.30)) |
		 (fpersons == 4 & gestfips != 2 & gestfips != 15 & ftotval <= (22050*1.30)) |
		 (fpersons == 5 & gestfips != 2 & gestfips != 15 & ftotval <= (25790*1.30)) |
		 (fpersons == 6 & gestfips != 2 & gestfips != 15 & ftotval <= (29530*1.30)) |
		 (fpersons == 7 & gestfips != 2 & gestfips != 15 & ftotval <= (33270*1.30)) |
		 (fpersons == 8 & gestfips != 2 & gestfips != 15 & ftotval <= (37010*1.30)) |
		 (fpersons >= 9 & gestfips != 2 & gestfips != 15 & ftotval <= ((37010+(fpersons - 8)*3740)*1.30)) |
	#Hawaii			
							(fpersons == 1 & gestfips != 2 & gestfips == 15 & ftotval <= (12460*1.30)) | 
			(fpersons == 2 & gestfips != 2 & gestfips == 15 & ftotval <= (16760*1.30)) | 
			(fpersons == 3 & gestfips != 2 & gestfips == 15 & ftotval <= (21060*1.30)) |
		 (fpersons == 4 & gestfips != 2 & gestfips == 15 & ftotval <= (25360*1.30)) |
		 (fpersons == 5 & gestfips != 2 & gestfips == 15 & ftotval <= (29660 *1.30)) |
		 (fpersons == 6 & gestfips != 2 & gestfips == 15 & ftotval <= (33960*1.30)) |
		 (fpersons == 7 & gestfips != 2 & gestfips == 15 & ftotval <= (38260*1.30)) |
		 (fpersons == 8 & gestfips != 2 & gestfips == 15 & ftotval <= (42560*1.30)) |
		 (fpersons >= 9 & gestfips != 2 & gestfips == 15 & ftotval <= ((42560+(fpersons - 8)*4300)*1.30)) |
#Alaska
			(fpersons == 1 & gestfips == 2 & gestfips != 15 & ftotval <= (13530*1.30)) | 
			(fpersons == 2 & gestfips == 2 & gestfips != 15 & ftotval <= (18210*1.30)) | 
			(fpersons == 3 & gestfips == 2 & gestfips != 15 & ftotval <= (22890*1.30)) |
		 (fpersons == 4 & gestfips == 2 & gestfips != 15 & ftotval <= (27570*1.30)) |
		 (fpersons == 5 & gestfips == 2 & gestfips != 15 & ftotval <= (32250 *1.30)) |
		 (fpersons == 6 & gestfips == 2 & gestfips != 15 & ftotval <= (36930*1.30)) |
		 (fpersons == 7 & gestfips == 2 & gestfips != 15 & ftotval <= (41610*1.30)) |
		 (fpersons == 8 & gestfips == 2 & gestfips != 15 & ftotval <= (46290*1.30)) |
		 (fpersons >= 9 & gestfips == 2 & gestfips != 15 & ftotval <= ((46290+(fpersons - 8)*4680)*1.30)) |
				(paw_typ == 1 | paw_typ == 3 | hfoodsp == 1 | povll < 5))

kids_reduced9 <- subset(kids9,
			((fpersons == 1 & gestfips != 2 & gestfips != 15 & ftotval > (10830 * 1.30) & ftotval < 10830*1.85) | 
			(fpersons == 2 & gestfips != 2 & gestfips != 15 & ftotval > (14570 * 1.30) & ftotval < 14570*1.85) | 
			(fpersons == 3 & gestfips != 2 & gestfips != 15 & ftotval > (18310 * 1.30) & ftotval < 18310 * 1.85) | 
		 (fpersons == 4 & gestfips != 2 & gestfips != 15 & ftotval > (22050 * 1.30) & ftotval < 22050 * 1.85) | 
		 (fpersons == 5 & gestfips != 2 & gestfips != 15 & ftotval > (25790 * 1.30) & ftotval < 25790 *1.85) | 
		 (fpersons == 6 & gestfips != 2 & gestfips != 15 & ftotval > (29530 * 1.30) & ftotval < 29530 *1.85) | 
		 (fpersons == 7 & gestfips != 2 & gestfips != 15 & ftotval > (33270 * 1.30) & ftotval < 33270 *1.85) | 
		 (fpersons == 8 & gestfips != 2 & gestfips != 15 & ftotval > (37010 * 1.30) & ftotval < 37010 *1.85) | 
		 (fpersons >= 9 & gestfips != 2 & gestfips != 15 & ftotval > (37010+(fpersons - 8)*3740)* 1.30 & ftotval < (37010+(fpersons - 8)*3740)*1.85) | 
	#Hawaii			 
					(fpersons == 1 & gestfips != 2 & gestfips == 15 & ftotval > (12460 * 1.30) & ftotval < (12460 *1.85)) | 
			(fpersons == 2 & gestfips != 2 & gestfips == 15 & ftotval > (16760 * 1.30) & ftotval < (16760 *1.85)) | 
			(fpersons == 3 & gestfips != 2 & gestfips == 15 & ftotval > (21060 * 1.30) & ftotval < (21060 *1.85)) | 
		 (fpersons == 4 & gestfips != 2 & gestfips == 15 & ftotval > (25360 * 1.30) & ftotval < (25360 *1.85)) | 
			(fpersons == 5 & gestfips != 2 & gestfips == 15 & ftotval > (29660 * 1.30) & ftotval < (29660 *1.85)) | 
		 (fpersons == 6 & gestfips != 2 & gestfips == 15 & ftotval > (33960 * 1.30) & ftotval < (33960 *1.85)) | 
		 (fpersons == 7 & gestfips != 2 & gestfips == 15 & ftotval > (38260 * 1.30) & ftotval < (38260 *1.85)) | 
		 (fpersons == 8 & gestfips != 2 & gestfips == 15 & ftotval > (42560 * 1.30) & ftotval < (42560 *1.85)) | 
		 (fpersons >= 9 & gestfips != 2 & gestfips == 15 & ftotval > (42560+(fpersons - 8)*4300) * 1.30 & ftotval < (42560+(fpersons - 8)*4300)*1.85) | 
					(fpersons == 1 & gestfips == 2 & gestfips != 15 & ftotval > (13530 * 1.30) & ftotval < (13530 *1.85)) | 
			(fpersons == 2 & gestfips == 2 & gestfips != 15 & ftotval > (18210 * 1.30) & ftotval < (18210 *1.85)) | 
			(fpersons == 3 & gestfips == 2 & gestfips != 15 & ftotval > (22890 * 1.30) & ftotval < (22890 *1.85)) | 
		 (fpersons == 4 & gestfips == 2 & gestfips != 15 & ftotval > (27570 * 1.30) & ftotval < (27570 *1.85)) | 
			(fpersons == 5 & gestfips == 2 & gestfips != 15 & ftotval > (32250 * 1.30) & ftotval < (32250 *1.85)) | 
		 (fpersons == 6 & gestfips == 2 & gestfips != 15 & ftotval > (36930 * 1.30) & ftotval < (36930 *1.85)) | 
		 (fpersons == 7 & gestfips == 2 & gestfips != 15 & ftotval > (41610 * 1.30) & ftotval < (41610 *1.85)) | 
		 (fpersons == 8 & gestfips == 2 & gestfips != 15 & ftotval > (46290 * 1.30) & ftotval < (46290 *1.85)) | 
		 (fpersons >= 9 & gestfips == 2 & gestfips != 15 & ftotval > (46290+(fpersons - 8)*4680) * 1.30 & ftotval < (46290+(fpersons - 8)*4680) *1.85)) 
			& ((paw_typ == 0 | paw_typ == 3 ) & (hfoodsp == 2 | 0) & povll >= 5 & povll < 9 ))

kids_full9 <- subset(kids9,
			((fpersons == 1 & gestfips != 2 & gestfips != 15 & ftotval > (10830 * 1.30) & ftotval >= (10830*1.85)) | 
			(fpersons == 2 & gestfips != 2 & gestfips != 15 & ftotval > (14570 * 1.30) & ftotval >= (14570*1.85)) | 
			(fpersons == 3 & gestfips != 2 & gestfips != 15 & ftotval > (18310 * 1.30) & ftotval >= (18310 * 1.85)) | 
		 (fpersons == 4 & gestfips != 2 & gestfips != 15 & ftotval > (22050 * 1.30) & ftotval >= (22050 * 1.85)) | 
		 (fpersons == 5 & gestfips != 2 & gestfips != 15 & ftotval > (25790 * 1.30) & ftotval >= (25790 *1.85)) | 
		 (fpersons == 6 & gestfips != 2 & gestfips != 15 & ftotval > (29530 * 1.30) & ftotval >= (29530 *1.85)) | 
		 (fpersons == 7 & gestfips != 2 & gestfips != 15 & ftotval > (33270 * 1.30) & ftotval >= (33270 *1.85)) | 
		 (fpersons == 8 & gestfips != 2 & gestfips != 15 & ftotval > (37010 * 1.30) & ftotval >= (37010 *1.85)) | 
		 (fpersons >= 9 & gestfips != 2 & gestfips != 15 & ftotval > (37010+(fpersons - 8)*3740)* 1.30 & ftotval > (37010+(fpersons - 8)*3740)*1.85) | 
	#Hawaii			 
					(fpersons == 1 & gestfips != 2 & gestfips == 15 & ftotval > (12460 * 1.30) & ftotval >= (12460 *1.85)) | 
			(fpersons == 2 & gestfips != 2 & gestfips == 15 & ftotval > (16760 * 1.30) & ftotval >= (16760 *1.85)) | 
			(fpersons == 3 & gestfips != 2 & gestfips == 15 & ftotval > (21060 * 1.30) & ftotval >= (21060 *1.85)) | 
		 (fpersons == 4 & gestfips != 2 & gestfips == 15 & ftotval > (25360 * 1.30) & ftotval >= (25360 *1.85)) | 
			(fpersons == 5 & gestfips != 2 & gestfips == 15 & ftotval > (29660 * 1.30) & ftotval >= (29660 *1.85)) | 
		 (fpersons == 6 & gestfips != 2 & gestfips == 15 & ftotval > (33960 * 1.30) & ftotval >= (33960 *1.85)) | 
		 (fpersons == 7 & gestfips != 2 & gestfips == 15 & ftotval > (38260 * 1.30) & ftotval >= (38260 *1.85)) | 
		 (fpersons == 8 & gestfips != 2 & gestfips == 15 & ftotval > (42560 * 1.30) & ftotval >= (42560 *1.85)) | 
		 (fpersons >= 9 & gestfips != 2 & gestfips == 15 & ftotval > (42560+(fpersons - 8)*4300) * 1.30 & ftotval > (42560+(fpersons - 8)*4300)*1.85) | 
					(fpersons == 1 & gestfips == 2 & gestfips != 15 & ftotval > (13530 * 1.30) & ftotval >= (13530 *1.85)) | 
			(fpersons == 2 & gestfips == 2 & gestfips != 15 & ftotval > (18210 * 1.30) & ftotval >= (18210 *1.85)) | 
			(fpersons == 3 & gestfips == 2 & gestfips != 15 & ftotval > (22890 * 1.30) & ftotval >= (22890 *1.85)) | 
		 (fpersons == 4 & gestfips == 2 & gestfips != 15 & ftotval > (27570 * 1.30) & ftotval >= (27570 *1.85)) | 
			(fpersons == 5 & gestfips == 2 & gestfips != 15 & ftotval > (32250 * 1.30) & ftotval >= (32250 *1.85)) | 
		 (fpersons == 6 & gestfips == 2 & gestfips != 15 & ftotval > (36930 * 1.30) & ftotval >= (36930 *1.85)) | 
		 (fpersons == 7 & gestfips == 2 & gestfips != 15 & ftotval > (41610 * 1.30) & ftotval >= (41610 *1.85)) | 
		 (fpersons == 8 & gestfips == 2 & gestfips != 15 & ftotval > (46290 * 1.30) & ftotval >= (46290 *1.85)) | 
		 (fpersons >= 9 & gestfips == 2 & gestfips != 15 & ftotval > (46290+(fpersons - 8)*4680) * 1.30 & ftotval > (46290+(fpersons - 8)*4680) *1.85)) 
			& ((paw_typ == 0 | paw_typ == 3 ) & (hfoodsp == 2 | 0) & povll >= 7))

##### 2010 #####

kids_free10 <- subset(kids10,
						#Continental + DC (July, August, September)
			(fpersons == 1 & gestfips != 2 & gestfips != 15 & ftotval <= (10830*1.30)) | 
			(fpersons == 2 & gestfips != 2 & gestfips != 15 & ftotval <= (14570*1.30)) | 
			(fpersons == 3 & gestfips != 2 & gestfips != 15 & ftotval <= (18310*1.30)) |
		 (fpersons == 4 & gestfips != 2 & gestfips != 15 & ftotval <= (22050*1.30)) |
		 (fpersons == 5 & gestfips != 2 & gestfips != 15 & ftotval <= (25790 *1.30)) |
		 (fpersons == 6 & gestfips != 2 & gestfips != 15 & ftotval <= (29530*1.30)) |
		 (fpersons == 7 & gestfips != 2 & gestfips != 15 & ftotval <= (33270*1.30)) |
		 (fpersons == 8 & gestfips != 2 & gestfips != 15 & ftotval <= (37010*1.30)) |
		 (fpersons >= 9 & gestfips != 2 & gestfips != 15 & ftotval <= ((37010+(fpersons - 8)*3740)*1.30)) |
					(fpersons == 1 & gestfips != 2 & gestfips == 15 & ftotval <= (12460*1.30)) | 
			(fpersons == 2 & gestfips != 2 & gestfips == 15 & ftotval <= (16760*1.30)) | 
			(fpersons == 3 & gestfips != 2 & gestfips == 15 & ftotval <= (21060*1.30)) |
		 (fpersons == 4 & gestfips != 2 & gestfips == 15 & ftotval <= (25360*1.30)) |
			(fpersons == 5 & gestfips != 2 & gestfips == 15 & ftotval <= (29660 *1.30)) |
		 (fpersons == 6 & gestfips != 2 & gestfips == 15 & ftotval <= (33960*1.30)) |
		 (fpersons == 7 & gestfips != 2 & gestfips == 15 & ftotval <= (38260*1.30)) |
		 (fpersons == 8 & gestfips != 2 & gestfips == 15 & ftotval <= (42560*1.30)) |
		 (fpersons >= 9 & gestfips != 2 & gestfips == 15 & ftotval <= ((42560+(fpersons - 8)*4300)*1.30)) | 
#Alaska
			(fpersons == 1 & gestfips == 2 & gestfips != 15 & ftotval <= (13530*1.30)) | 
			(fpersons == 2 & gestfips == 2 & gestfips != 15 & ftotval <= (18210*1.30)) | 
			(fpersons == 3 & gestfips == 2 & gestfips != 15 & ftotval <= (22890*1.30)) |
		 (fpersons == 4 & gestfips == 2 & gestfips != 15 & ftotval <= (27570*1.30)) |
		 (fpersons == 5 & gestfips == 2 & gestfips != 15 & ftotval <= (32250 *1.30)) |
		 (fpersons == 6 & gestfips == 2 & gestfips != 15 & ftotval <= (36930*1.30)) |
		 (fpersons == 7 & gestfips == 2 & gestfips != 15 & ftotval <= (41610*1.30)) |
		 (fpersons == 8 & gestfips == 2 & gestfips != 15 & ftotval <= (46290*1.30)) |
		 (fpersons >= 9 & gestfips == 2 & gestfips != 15 & ftotval <= ((46290+(fpersons - 8)*4680)*1.30)) |
				  (paw_typ == 1 | paw_typ == 3 | hfoodsp == 1 | povll < 5))

kids_reduced10 <- subset(kids10,
			((fpersons == 1 & gestfips != 2 & gestfips != 15 & ftotval > (10830 * 1.30) & ftotval < (10830*1.85)) | 
			(fpersons == 2 & gestfips != 2 & gestfips != 15 & ftotval > (14570 * 1.30) & ftotval < (14570*1.85)) | 
			(fpersons == 3 & gestfips != 2 & gestfips != 15 & ftotval > (18310 * 1.30) & ftotval < (18310 * 1.85)) | 
		 (fpersons == 4 & gestfips != 2 & gestfips != 15 & ftotval > (22050 * 1.30) & ftotval < (22050 * 1.85)) | 
		 (fpersons == 5 & gestfips != 2 & gestfips != 15 & ftotval > (25790 * 1.30) & ftotval < (25790 *1.85)) | 
		 (fpersons == 6 & gestfips != 2 & gestfips != 15 & ftotval > (29530 * 1.30) & ftotval < (29530 *1.85)) | 
		 (fpersons == 7 & gestfips != 2 & gestfips != 15 & ftotval > (33270 * 1.30) & ftotval < (33270 *1.85)) | 
		 (fpersons == 8 & gestfips != 2 & gestfips != 15 & ftotval > (37010 * 1.30) & ftotval < (37010 *1.85)) | 
		 (fpersons >= 9 & gestfips != 2 & gestfips != 15 & ftotval > ((37010+(fpersons - 8)*3740)* 1.30) & ftotval < (37010+(fpersons - 8)*3740)*1.85) | 
					(fpersons == 1 & gestfips != 2 & gestfips == 15 & ftotval > (12460 * 1.30) & ftotval < (12460 *1.85)) | 
			(fpersons == 2 & gestfips != 2 & gestfips == 15 & ftotval > (16760 * 1.30) & ftotval < (16760 *1.85)) | 
			(fpersons == 3 & gestfips != 2 & gestfips == 15 & ftotval > (21060 * 1.30) & ftotval < (21060 *1.85)) | 
		 (fpersons == 4 & gestfips != 2 & gestfips == 15 & ftotval > (25360 * 1.30) & ftotval < (25360 *1.85)) | 
			(fpersons == 5 & gestfips != 2 & gestfips == 15 & ftotval > (29660 * 1.30) & ftotval < (29660 *1.85)) | 
		 (fpersons == 6 & gestfips != 2 & gestfips == 15 & ftotval > (33960 * 1.30) & ftotval < (33960 *1.85)) | 
		 (fpersons == 7 & gestfips != 2 & gestfips == 15 & ftotval > (38260 * 1.30) & ftotval < (38260 *1.85)) | 
		 (fpersons == 8 & gestfips != 2 & gestfips == 15 & ftotval > (42560 * 1.30) & ftotval < (42560 *1.85)) | 
		 (fpersons >= 9 & gestfips != 2 & gestfips == 15 & ftotval > ((42560+(fpersons - 8)*4300) * 1.30) & ftotval < (42560+(fpersons - 8)*4300)*1.85) | 
#Alaska
					(fpersons == 1 & gestfips == 2 & gestfips != 15 & ftotval > (13530 * 1.30) & ftotval < (13530 *1.85)) | 
			(fpersons == 2 & gestfips == 2 & gestfips != 15 & ftotval > (18210 * 1.30) & ftotval < (18210 *1.85)) | 
			(fpersons == 3 & gestfips == 2 & gestfips != 15 & ftotval > (22890 * 1.30) & ftotval < (22890 *1.85)) | 
		 (fpersons == 4 & gestfips == 2 & gestfips != 15 & ftotval > (27570 * 1.30) & ftotval < (27570 *1.85)) | 
			(fpersons == 5 & gestfips == 2 & gestfips != 15 & ftotval > (32250 * 1.30) & ftotval < (32250 *1.85)) | 
		 (fpersons == 6 & gestfips == 2 & gestfips != 15 & ftotval > (36930 * 1.30) & ftotval < (36930 *1.85)) | 
		 (fpersons == 7 & gestfips == 2 & gestfips != 15 & ftotval > (41610 * 1.30) & ftotval < (41610 *1.85)) | 
		 (fpersons == 8 & gestfips == 2 & gestfips != 15 & ftotval > (46290 * 1.30) & ftotval < (46290 *1.85)) | 
		 (fpersons >= 9 & gestfips == 2 & gestfips != 15 & ftotval > ((46290+(fpersons - 8)*4680) * 1.30) & ftotval < (46290+(fpersons - 8)*4680) *1.85)) 
			& ((paw_typ == 0 | paw_typ == 3 ) & (hfoodsp == 2 | 0) & povll >= 5 & povll < 9 ))

kids_full10 <- subset(kids10,
			((fpersons == 1 & gestfips != 2 & gestfips != 15 & ftotval > (10830 * 1.30) & ftotval >= (10830*1.85)) | 
			(fpersons == 2 & gestfips != 2 & gestfips != 15 & ftotval > (14570 * 1.30) & ftotval >= (14570*1.85)) | 
			(fpersons == 3 & gestfips != 2 & gestfips != 15 & ftotval > (18310 * 1.30) & ftotval >= (18310 * 1.85)) | 
		 (fpersons == 4 & gestfips != 2 & gestfips != 15 & ftotval > (22050 * 1.30) & ftotval >= (22050 * 1.85)) | 
		 (fpersons == 5 & gestfips != 2 & gestfips != 15 & ftotval > (25790 * 1.30) & ftotval >= (25790 *1.85)) | 
		 (fpersons == 6 & gestfips != 2 & gestfips != 15 & ftotval > (29530 * 1.30) & ftotval >= (29530 *1.85)) | 
		 (fpersons == 7 & gestfips != 2 & gestfips != 15 & ftotval > (33270 * 1.30) & ftotval >= (33270 *1.85)) | 
		 (fpersons == 8 & gestfips != 2 & gestfips != 15 & ftotval > (37010 * 1.30) & ftotval >= (37010 *1.85)) | 
		 (fpersons >= 9 & gestfips != 2 & gestfips != 15 & ftotval > ((37010+(fpersons - 8)*3740)* 1.30) & ftotval >= (37010+(fpersons - 8)*3740)*1.85) | 
					(fpersons == 1 & gestfips != 2 & gestfips == 15 & ftotval > (12460 * 1.30) & ftotval >= (12460 *1.85)) | 
			(fpersons == 2 & gestfips != 2 & gestfips == 15 & ftotval > (16760 * 1.30) & ftotval >= (16760 *1.85)) | 
			(fpersons == 3 & gestfips != 2 & gestfips == 15 & ftotval > (21060 * 1.30) & ftotval >= (21060 *1.85)) | 
		 (fpersons == 4 & gestfips != 2 & gestfips == 15 & ftotval > (25360 * 1.30) & ftotval >= (25360 *1.85)) | 
			(fpersons == 5 & gestfips != 2 & gestfips == 15 & ftotval > (29660 * 1.30) & ftotval >= (29660 *1.85)) | 
		 (fpersons == 6 & gestfips != 2 & gestfips == 15 & ftotval > (33960 * 1.30) & ftotval >= (33960 *1.85)) | 
		 (fpersons == 7 & gestfips != 2 & gestfips == 15 & ftotval > (38260 * 1.30) & ftotval >= (38260 *1.85)) | 
		 (fpersons == 8 & gestfips != 2 & gestfips == 15 & ftotval > (42560 * 1.30) & ftotval >= (42560 *1.85)) | 
		 (fpersons >= 9 & gestfips != 2 & gestfips == 15 & ftotval > ((42560+(fpersons - 8)*4300) * 1.30) & ftotval >= (42560+(fpersons - 8)*4300)*1.85) | 
#Alaska
					(fpersons == 1 & gestfips == 2 & gestfips != 15 & ftotval > (13530 * 1.30) & ftotval >= (13530 *1.85)) | 
			(fpersons == 2 & gestfips == 2 & gestfips != 15 & ftotval > (18210 * 1.30) & ftotval >= (18210 *1.85)) | 
			(fpersons == 3 & gestfips == 2 & gestfips != 15 & ftotval > (22890 * 1.30) & ftotval >= (22890 *1.85)) | 
		 (fpersons == 4 & gestfips == 2 & gestfips != 15 & ftotval > (27570 * 1.30) & ftotval >= (27570 *1.85)) | 
			(fpersons == 5 & gestfips == 2 & gestfips != 15 & ftotval > (32250 * 1.30) & ftotval >= (32250 *1.85)) | 
		 (fpersons == 6 & gestfips == 2 & gestfips != 15 & ftotval > (36930 * 1.30) & ftotval >= (36930 *1.85)) | 
		 (fpersons == 7 & gestfips == 2 & gestfips != 15 & ftotval > (41610 * 1.30) & ftotval >= (41610 *1.85)) | 
		 (fpersons == 8 & gestfips == 2 & gestfips != 15 & ftotval > (46290 * 1.30) & ftotval >= (46290 *1.85)) | 
		 (fpersons >= 9 & gestfips == 2 & gestfips != 15 & ftotval > ((46290+(fpersons - 8)*4680) * 1.30) & ftotval >= (46290+(fpersons - 8)*4680) *1.85)) 
			& ((paw_typ == 0 | paw_typ == 3 ) & (hfoodsp == 2 | 0) & povll >= 7))

##### 2011 #####
kids_free11 <- subset(kids11,
					(fpersons == 1 & gestfips != 2 & gestfips != 15 & ftotval <= (10890*1.30)) | 
			(fpersons == 2 & gestfips != 2 & gestfips != 15 & ftotval <= (14710*1.30)) | 
			(fpersons == 3 & gestfips != 2 & gestfips != 15 & ftotval <= (22350*1.30)) |
		 (fpersons == 4 & gestfips != 2 & gestfips != 15 & ftotval <= (22490*1.30)) |
			(fpersons == 5 & gestfips != 2 & gestfips != 15 & ftotval <= (26170*1.30)) |
		 (fpersons == 6 & gestfips != 2 & gestfips != 15 & ftotval <= (29990*1.30)) |
		 (fpersons == 7 & gestfips != 2 & gestfips != 15 & ftotval <= (33810*1.30)) |
		 (fpersons == 8 & gestfips != 2 & gestfips != 15 & ftotval <= (37630*1.30)) |
		 (fpersons >= 9 & gestfips != 2 & gestfips != 15 & ftotval <= ((37630+(fpersons - 8)*3820)*1.30)) | 
	#Hawaii			
							(fpersons == 1 & gestfips != 2 & gestfips == 15 & ftotval <= (12540*1.30)) | 
			(fpersons == 2 & gestfips != 2 & gestfips == 15 & ftotval <= (16930*1.30)) | 
			(fpersons == 3 & gestfips != 2 & gestfips == 15 & ftotval <= (21320*1.30)) |
		 (fpersons == 4 & gestfips != 2 & gestfips == 15 & ftotval <= (25710*1.30)) |
		 (fpersons == 5 & gestfips != 2 & gestfips == 15 & ftotval <= (30100 *1.30)) |
		 (fpersons == 6 & gestfips != 2 & gestfips == 15 & ftotval <= (34490*1.30)) |
		 (fpersons == 7 & gestfips != 2 & gestfips == 15 & ftotval <= (38880*1.30)) |
		 (fpersons == 8 & gestfips != 2 & gestfips == 15 & ftotval <= (43270*1.30)) |
		 (fpersons >= 9 & gestfips != 2 & gestfips == 15 & ftotval <= ((43270+(fpersons - 8)*4390)*1.30)) |
					(fpersons == 1 & gestfips == 2 & gestfips != 15 & ftotval <= (13600*1.30)) | 
			(fpersons == 2 & gestfips == 2 & gestfips != 15 & ftotval <= (18380*1.30)) | 
			(fpersons == 3 & gestfips == 2 & gestfips != 15 & ftotval <= (23160*1.30)) |
		 (fpersons == 4 & gestfips == 2 & gestfips != 15 & ftotval <= (27940*1.30)) |
			(fpersons == 5 & gestfips == 2 & gestfips != 15 & ftotval <= (32720	*1.30)) |
		 (fpersons == 6 & gestfips == 2 & gestfips != 15 & ftotval <= (37500*1.30)) |
		 (fpersons == 7 & gestfips == 2 & gestfips != 15 & ftotval <= (42280*1.30)) |
		 (fpersons == 8 & gestfips == 2 & gestfips != 15 & ftotval <= (47060*1.30)) |
		 (fpersons >= 9 & gestfips == 2 & gestfips != 15 & ftotval <= ((47060+(fpersons - 8)*4780)*1.30)) | 
				 (paw_typ == 1 | paw_typ == 3 | hfoodsp == 1 | povll < 5))

kids_reduced11 <- subset(kids11,
						#Continental + DC (July, August, September)
			((fpersons == 1 & gestfips != 2 & gestfips != 15 & ftotval > (10890 * 1.30) & ftotval < (10890*1.85)) | 
			(fpersons == 2 & gestfips != 2 & gestfips != 15 & ftotval > (14710 * 1.30) & ftotval < (14710*1.85)) | 
			(fpersons == 3 & gestfips != 2 & gestfips != 15 & ftotval > (18530 * 1.30) & ftotval < (18530 * 1.85)) | 
		 (fpersons == 4 & gestfips != 2 & gestfips != 15 & ftotval > (22350 * 1.30) & ftotval < (22350 * 1.85)) | 
		 (fpersons == 5 & gestfips != 2 & gestfips != 15 & ftotval > (26170 * 1.30) & ftotval < (26170 *1.85)) | 
		 (fpersons == 6 & gestfips != 2 & gestfips != 15 & ftotval > (29990 * 1.30) & ftotval < (29990 *1.85)) | 
		 (fpersons == 7 & gestfips != 2 & gestfips != 15 & ftotval > (33810 * 1.30) & ftotval < (33810 *1.85)) | 
		 (fpersons == 8 & gestfips != 2 & gestfips != 15 & ftotval > (37630 * 1.30) & ftotval < (37630 *1.85)) | 
		 (fpersons >= 9 & gestfips != 2 & gestfips != 15 & ftotval > (37630+(fpersons - 8)*3820)* 1.30 & ftotval < ((37630+(fpersons - 8)*3820)*1.85)) | 
					(fpersons == 1 & gestfips != 2 & gestfips == 15 & ftotval > (12540 * 1.30) & ftotval < (12540 *1.85)) | 
			(fpersons == 2 & gestfips != 2 & gestfips == 15 & ftotval > (16930 * 1.30) & ftotval < (16930 *1.85)) | 
			(fpersons == 3 & gestfips != 2 & gestfips == 15 & ftotval > (21320 * 1.30) & ftotval < (21320 *1.85)) | 
		 (fpersons == 4 & gestfips != 2 & gestfips == 15 & ftotval > (25710 * 1.30) & ftotval < (25710 *1.85)) | 
			(fpersons == 5 & gestfips != 2 & gestfips == 15 & ftotval > (30100 * 1.30) & ftotval < (30100 *1.85)) | 
		 (fpersons == 6 & gestfips != 2 & gestfips == 15 & ftotval > (34490 * 1.30) & ftotval < (34490 *1.85)) | 
		 (fpersons == 7 & gestfips != 2 & gestfips == 15 & ftotval > (38880 * 1.30) & ftotval < (38880 *1.85)) | 
		 (fpersons == 8 & gestfips != 2 & gestfips == 15 & ftotval > (43270 * 1.30) & ftotval < (43270 *1.85)) | 
		 (fpersons >= 9 & gestfips != 2 & gestfips == 15 & ftotval > ((43270+(fpersons - 8)*4390) * 1.30) & ftotval < ((43270+(fpersons - 8)*4390)*1.85)) | 
					(fpersons == 1 & gestfips == 2 & gestfips != 15 & ftotval > (13600 * 1.30) & ftotval < (13600 *1.85)) | 
			(fpersons == 2 & gestfips == 2 & gestfips != 15 & ftotval > (18380 * 1.30) & ftotval < (18380 *1.85)) | 
			(fpersons == 3 & gestfips == 2 & gestfips != 15 & ftotval > (23160 * 1.30) & ftotval < (23160 *1.85)) | 
		 (fpersons == 4 & gestfips == 2 & gestfips != 15 & ftotval > (27940 * 1.30) & ftotval < (27940 *1.85)) | 
			(fpersons == 5 & gestfips == 2 & gestfips != 15 & ftotval > (32720 * 1.30) & ftotval < (32720 *1.85)) | 
		 (fpersons == 6 & gestfips == 2 & gestfips != 15 & ftotval > (37500 * 1.30) & ftotval < (37500 *1.85)) | 
		 (fpersons == 7 & gestfips == 2 & gestfips != 15 & ftotval > (42280 * 1.30) & ftotval < (42280 *1.85)) | 
		 (fpersons == 8 & gestfips == 2 & gestfips != 15 & ftotval > (47060 * 1.30) & ftotval < (47060 *1.85)) | 
		 (fpersons >= 9 & gestfips == 2 & gestfips != 15 & ftotval > ((47060+(fpersons - 8)*4780) * 1.30) & ftotval < (47060+(fpersons - 8)*4780) *1.85)) 
& ((paw_typ == 0 | paw_typ == 3 ) & (hfoodsp == 2 | 0) & povll >= 5 & povll < 9 ))

kids_full11 <- subset(kids11,
						#Continental + DC (July, August, September)
			((fpersons == 1 & gestfips != 2 & gestfips != 15 & ftotval > (10890 * 1.30) & ftotval >= (10890*1.85)) | 
			(fpersons == 2 & gestfips != 2 & gestfips != 15 & ftotval > (14710 * 1.30) & ftotval >= (14710*1.85)) | 
			(fpersons == 3 & gestfips != 2 & gestfips != 15 & ftotval > (18530 * 1.30) & ftotval >= (18530 * 1.85)) | 
		 (fpersons == 4 & gestfips != 2 & gestfips != 15 & ftotval > (22350 * 1.30) & ftotval >= (22350 * 1.85)) | 
		 (fpersons == 5 & gestfips != 2 & gestfips != 15 & ftotval > (26170 * 1.30) & ftotval >= (26170 *1.85)) | 
		 (fpersons == 6 & gestfips != 2 & gestfips != 15 & ftotval > (29990 * 1.30) & ftotval >= (29990 *1.85)) | 
		 (fpersons == 7 & gestfips != 2 & gestfips != 15 & ftotval > (33810 * 1.30) & ftotval >= (33810 *1.85)) | 
		 (fpersons == 8 & gestfips != 2 & gestfips != 15 & ftotval > (37630 * 1.30) & ftotval >= (37630 *1.85)) | 
		 (fpersons >= 9 & gestfips != 2 & gestfips != 15 & ftotval > (37630+(fpersons - 8)*3820)* 1.30 & ftotval > ((37630+(fpersons - 8)*3820)*1.85)) | 
					(fpersons == 1 & gestfips != 2 & gestfips == 15 & ftotval > (12540 * 1.30) & ftotval >= (12540 *1.85)) | 
			(fpersons == 2 & gestfips != 2 & gestfips == 15 & ftotval > (16930 * 1.30) & ftotval >= (16930 *1.85)) | 
			(fpersons == 3 & gestfips != 2 & gestfips == 15 & ftotval > (21320 * 1.30) & ftotval >= (21320 *1.85)) | 
		 (fpersons == 4 & gestfips != 2 & gestfips == 15 & ftotval > (25710 * 1.30) & ftotval >= (25710 *1.85)) | 
			(fpersons == 5 & gestfips != 2 & gestfips == 15 & ftotval > (30100 * 1.30) & ftotval >= (30100 *1.85)) | 
		 (fpersons == 6 & gestfips != 2 & gestfips == 15 & ftotval > (34490 * 1.30) & ftotval >= (34490 *1.85)) | 
		 (fpersons == 7 & gestfips != 2 & gestfips == 15 & ftotval > (38880 * 1.30) & ftotval >= (38880 *1.85)) | 
		 (fpersons == 8 & gestfips != 2 & gestfips == 15 & ftotval > (43270 * 1.30) & ftotval >= (43270 *1.85)) | 
		 (fpersons >= 9 & gestfips != 2 & gestfips == 15 & ftotval > ((43270+(fpersons - 8)*4390) * 1.30) & ftotval >= ((43270+(fpersons - 8)*4390)*1.85)) | 
					(fpersons == 1 & gestfips == 2 & gestfips != 15 & ftotval > (13600 * 1.30) & ftotval >= (13600 *1.85)) | 
			(fpersons == 2 & gestfips == 2 & gestfips != 15 & ftotval > (18380 * 1.30) & ftotval >= (18380 *1.85)) | 
			(fpersons == 3 & gestfips == 2 & gestfips != 15 & ftotval > (23160 * 1.30) & ftotval >= (23160 *1.85)) | 
		 (fpersons == 4 & gestfips == 2 & gestfips != 15 & ftotval > (27940 * 1.30) & ftotval >= (27940 *1.85)) | 
			(fpersons == 5 & gestfips == 2 & gestfips != 15 & ftotval > (32720 * 1.30) & ftotval >= (32720 *1.85)) | 
		 (fpersons == 6 & gestfips == 2 & gestfips != 15 & ftotval > (37500 * 1.30) & ftotval >= (37500 *1.85)) | 
		 (fpersons == 7 & gestfips == 2 & gestfips != 15 & ftotval > (42280 * 1.30) & ftotval >= (42280 *1.85)) | 
		 (fpersons == 8 & gestfips == 2 & gestfips != 15 & ftotval > (47060 * 1.30) & ftotval >= (47060 *1.85)) | 
		 (fpersons >= 9 & gestfips == 2 & gestfips != 15 & ftotval > ((47060+(fpersons - 8)*4780) * 1.30) & ftotval >= (47060+(fpersons - 8)*4780) *1.85)) 
& ((paw_typ == 0 | paw_typ == 3 ) & (hfoodsp == 2 | 0) & povll >= 7))

##### 2012 #####
kids_free12 <- subset(kids12,
						#Continental + DC (July, August, September)
			(fpersons == 1 & gestfips != 2 & gestfips != 15 & ftotval <= (11170*1.30)) | 
			(fpersons == 2 & gestfips != 2 & gestfips != 15 & ftotval <= (15130*1.30)) | 
			(fpersons == 3 & gestfips != 2 & gestfips != 15 & ftotval <= (19090*1.30)) |
		 (fpersons == 4 & gestfips != 2 & gestfips != 15 & ftotval <= (23050*1.30)) |
		 (fpersons == 5 & gestfips != 2 & gestfips != 15 & ftotval <= (27010 *1.30)) |
		 (fpersons == 6 & gestfips != 2 & gestfips != 15 & ftotval <= (30970*1.30)) |
		 (fpersons == 7 & gestfips != 2 & gestfips != 15 & ftotval <= (34930*1.30)) |
		 (fpersons == 8 & gestfips != 2 & gestfips != 15 & ftotval <= (38890*1.30)) |
		 (fpersons >= 9 & gestfips != 2 & gestfips != 15 & ftotval <= ((38890+(fpersons - 8)*3960)*1.30)) |
					(fpersons == 1 & gestfips != 2 & gestfips == 15 & ftotval <= (12860*1.30)) | 
			(fpersons == 2 & gestfips != 2 & gestfips == 15 & ftotval <= (17410*1.30)) | 
			(fpersons == 3 & gestfips != 2 & gestfips == 15 & ftotval <= (21960*1.30)) |
		 (fpersons == 4 & gestfips != 2 & gestfips == 15 & ftotval <= (26510*1.30)) |
			(fpersons == 5 & gestfips != 2 & gestfips == 15 & ftotval <= (31060 *1.30)) |
		 (fpersons == 6 & gestfips != 2 & gestfips == 15 & ftotval <= (35610*1.30)) |
		 (fpersons == 7 & gestfips != 2 & gestfips == 15 & ftotval <= (40160*1.30)) |
		 (fpersons == 8 & gestfips != 2 & gestfips == 15 & ftotval <= (44710*1.30)) |
		 (fpersons >= 9 & gestfips != 2 & gestfips == 15 & ftotval <= ((44710+(fpersons - 8)*4550)*1.30)) | 
#Alaska
					(fpersons == 1 & gestfips == 2 & gestfips != 15 & ftotval <= (13970*1.30)) | 
			(fpersons == 2 & gestfips == 2 & gestfips != 15 & ftotval <= (18920*1.30)) | 
			(fpersons == 3 & gestfips == 2 & gestfips != 15 & ftotval <= (23870*1.30)) |
		 (fpersons == 4 & gestfips == 2 & gestfips != 15 & ftotval <= (28820*1.30)) |
			(fpersons == 5 & gestfips == 2 & gestfips != 15 & ftotval <= (33770*1.30)) |
		 (fpersons == 6 & gestfips == 2 & gestfips != 15 & ftotval <= (38720*1.30)) |
		 (fpersons == 7 & gestfips == 2 & gestfips != 15 & ftotval <= (42280*1.30)) |
		 (fpersons == 8 & gestfips == 2 & gestfips != 15 & ftotval <= (48620*1.30)) |
		 (fpersons >= 9 & gestfips == 2 & gestfips != 15 & ftotval <= ((48620+(fpersons - 8)*4950)*1.30)) 
| (paw_typ == 1 | paw_typ == 3 | hfoodsp == 1 | povll < 5))


kids_reduced12 <- subset(kids12,
						#Continental + DC (July, August, September)
			((fpersons == 1 & gestfips != 2 & gestfips != 15 & ftotval > (11170 * 1.30) & ftotval < (11170*1.85)) | 
			(fpersons == 2 & gestfips != 2 & gestfips != 15 & ftotval > (15130 * 1.30) & ftotval < (15130*1.85)) | 
			(fpersons == 3 & gestfips != 2 & gestfips != 15 & ftotval > (19090 * 1.30) & ftotval < (19090 *1.85)) | 
		 (fpersons == 4 & gestfips != 2 & gestfips != 15 & ftotval > (23050 * 1.30) & ftotval < (23050 *1.85)) | 
		 (fpersons == 5 & gestfips != 2 & gestfips != 15 & ftotval > (27010 * 1.30) & ftotval < (27010 *1.85)) | 
		 (fpersons == 6 & gestfips != 2 & gestfips != 15 & ftotval > (30970 * 1.30) & ftotval < (30970 *1.85)) | 
		 (fpersons == 7 & gestfips != 2 & gestfips != 15 & ftotval > (34930 * 1.30) & ftotval < (34930 *1.85)) | 
		 (fpersons == 8 & gestfips != 2 & gestfips != 15 & ftotval > (38890 * 1.30) & ftotval < (38890 *1.85)) | 
		 (fpersons >= 9 & gestfips != 2 & gestfips != 15 & ftotval > (38890+(fpersons - 8)*3960)*1.30 & ftotval < (38890+(fpersons - 8)*3960)*1.85) | 
					(fpersons == 1 & gestfips != 2 & gestfips == 15 & ftotval > (12860 * 1.30) & ftotval < (12860 *1.85)) | 
			(fpersons == 2 & gestfips != 2 & gestfips == 15 & ftotval > (17410 * 1.30) & ftotval < (17410 *1.85)) | 
			(fpersons == 3 & gestfips != 2 & gestfips == 15 & ftotval > (21960 * 1.30) & ftotval < (21960 *1.85)) | 
		 (fpersons == 4 & gestfips != 2 & gestfips == 15 & ftotval > (26510 * 1.30) & ftotval < (26510 *1.85)) | 
			(fpersons == 5 & gestfips != 2 & gestfips == 15 & ftotval > (31060 * 1.30) & ftotval < (31060 *1.85)) | 
		 (fpersons == 6 & gestfips != 2 & gestfips == 15 & ftotval > (35610 * 1.30) & ftotval < (35610 *1.85)) | 
		 (fpersons == 7 & gestfips != 2 & gestfips == 15 & ftotval > (40160 * 1.30) & ftotval < (40160 *1.85)) | 
		 (fpersons == 8 & gestfips != 2 & gestfips == 15 & ftotval > (44710 * 1.30) & ftotval < (44710 *1.85)) | 
		 (fpersons >= 9 & gestfips != 2 & gestfips == 15 & ftotval > (44710+(fpersons - 8)*4550) * 1.30 & ftotval < (44710+(fpersons - 8)*4550)*1.85) | 
					(fpersons == 1 & gestfips == 2 & gestfips != 15 & ftotval > (13970 * 1.30) & ftotval < (13970 *1.85)) | 
			(fpersons == 2 & gestfips == 2 & gestfips != 15 & ftotval > (18920 * 1.30) & ftotval < (18920 *1.85)) | 
			(fpersons == 3 & gestfips == 2 & gestfips != 15 & ftotval > (23870 * 1.30) & ftotval < (23870 *1.85)) | 
		 (fpersons == 4 & gestfips == 2 & gestfips != 15 & ftotval > (28820 * 1.30) & ftotval < (28820 *1.85)) | 
			(fpersons == 5 & gestfips == 2 & gestfips != 15 & ftotval > (33770 * 1.30) & ftotval < (33770 *1.85)) | 
		 (fpersons == 6 & gestfips == 2 & gestfips != 15 & ftotval > (38720 * 1.30) & ftotval < (38720 *1.85)) | 
		 (fpersons == 7 & gestfips == 2 & gestfips != 15 & ftotval > (43670 * 1.30) & ftotval < (43670 *1.85)) | 
		 (fpersons == 8 & gestfips == 2 & gestfips != 15 & ftotval > (48620 * 1.30) & ftotval < (48620 *1.85)) | 
		 (fpersons >= 9 & gestfips == 2 & gestfips != 15 & ftotval > ((48620+(fpersons - 8)*4950) * 1.30) & ftotval < (48620+(fpersons - 8)*4950) *1.85)) 
			& ((paw_typ == 0 | paw_typ == 3 ) & (hfoodsp == 2 | 0) & povll >= 5 & povll < 9 ))

kids_full12 <- subset(kids12,
						#Continental + DC (July, August, September)
			((fpersons == 1 & gestfips != 2 & gestfips != 15 & ftotval > (11170 * 1.30) & ftotval >= (11170*1.85)) | 
			(fpersons == 2 & gestfips != 2 & gestfips != 15 & ftotval > (15130 * 1.30) & ftotval >= (15130*1.85)) | 
			(fpersons == 3 & gestfips != 2 & gestfips != 15 & ftotval > (19090 * 1.30) & ftotval >= (19090 * 1.85)) | 
		 (fpersons == 4 & gestfips != 2 & gestfips != 15 & ftotval > (23050 * 1.30) & ftotval >= (23050 * 1.85)) | 
		 (fpersons == 5 & gestfips != 2 & gestfips != 15 & ftotval > (27010 * 1.30) & ftotval >= (27010 *1.85)) | 
		 (fpersons == 6 & gestfips != 2 & gestfips != 15 & ftotval > (30970 * 1.30) & ftotval >= (30970 *1.85)) | 
		 (fpersons == 7 & gestfips != 2 & gestfips != 15 & ftotval > (34930 * 1.30) & ftotval >= (34930 *1.85)) | 
		 (fpersons == 8 & gestfips != 2 & gestfips != 15 & ftotval > (38890 * 1.30) & ftotval >= (38890 *1.85)) | 
		 (fpersons >= 9 & gestfips != 2 & gestfips != 15 & ftotval > ((38890+(fpersons - 8)*3960)* 1.30) & ftotval >= (38890+(fpersons - 8)*3960)*1.85) | 
					(fpersons == 1 & gestfips != 2 & gestfips == 15 & ftotval > (12860 * 1.30) & ftotval >= (12860 *1.85)) | 
			(fpersons == 2 & gestfips != 2 & gestfips == 15 & ftotval > (17410 * 1.30) & ftotval >= (17410 *1.85)) | 
			(fpersons == 3 & gestfips != 2 & gestfips == 15 & ftotval > (21960 * 1.30) & ftotval >= (21960 *1.85)) | 
		 (fpersons == 4 & gestfips != 2 & gestfips == 15 & ftotval > (26510 * 1.30) & ftotval >= (26510 *1.85)) | 
			(fpersons == 5 & gestfips != 2 & gestfips == 15 & ftotval > (31060 * 1.30) & ftotval >= (31060 *1.85)) | 
		 (fpersons == 6 & gestfips != 2 & gestfips == 15 & ftotval > (35610 * 1.30) & ftotval >= (35610 *1.85)) | 
		 (fpersons == 7 & gestfips != 2 & gestfips == 15 & ftotval > (40160 * 1.30) & ftotval >= (40160 *1.85)) | 
		 (fpersons == 8 & gestfips != 2 & gestfips == 15 & ftotval > (44710 * 1.30) & ftotval >= (44710 *1.85)) | 
		 (fpersons >= 9 & gestfips != 2 & gestfips == 15 & ftotval > ((44710+(fpersons - 8)*4550) * 1.30) & ftotval >= (44710+(fpersons - 8)*4550)*1.85) | 
#Alaska
					(fpersons == 1 & gestfips == 2 & gestfips != 15 & ftotval > (13970 * 1.30) & ftotval >= (13970 *1.85)) | 
			(fpersons == 2 & gestfips == 2 & gestfips != 15 & ftotval > (18920 * 1.30) & ftotval >= (18920 *1.85)) | 
			(fpersons == 3 & gestfips == 2 & gestfips != 15 & ftotval > (23870 * 1.30) & ftotval >= (23870 *1.85)) | 
		 (fpersons == 4 & gestfips == 2 & gestfips != 15 & ftotval > (28820 * 1.30) & ftotval >= (28820 *1.85)) | 
			(fpersons == 5 & gestfips == 2 & gestfips != 15 & ftotval > (33770 * 1.30) & ftotval >= (33770 *1.85)) | 
		 (fpersons == 6 & gestfips == 2 & gestfips != 15 & ftotval > (38720 * 1.30) & ftotval >= (38720 *1.85)) | 
		 (fpersons == 7 & gestfips == 2 & gestfips != 15 & ftotval > (43670 * 1.30) & ftotval >= (43670 *1.85)) | 
		 (fpersons == 8 & gestfips == 2 & gestfips != 15 & ftotval > (48620 * 1.30) & ftotval >= (48620 *1.85)) | 
		 (fpersons >= 9 & gestfips == 2 & gestfips != 15 & ftotval > ((48620+(fpersons - 8)*4950) * 1.30) & ftotval >= (48620+(fpersons - 8)*4950) *1.85)) 
			& ((paw_typ == 0 | paw_typ == 3 ) & (hfoodsp == 2 | 0) & povll >= 7))

##### 2013 #####
kids_free13 <- subset(kids13,
			((fpersons == 1 & gestfips != 2 & gestfips != 15 & ftotval <= (11490*1.30)) | 
			(fpersons == 2 & gestfips != 2 & gestfips != 15 & ftotval <= (15510*1.30)) | 
			(fpersons == 3 & gestfips != 2 & gestfips != 15 & ftotval <= (19530*1.30)) |
		 (fpersons == 4 & gestfips != 2 & gestfips != 15 & ftotval <= (23550*1.30)) |
		 (fpersons == 5 & gestfips != 2 & gestfips != 15 & ftotval <= (27570 *1.30)) |
		 (fpersons == 6 & gestfips != 2 & gestfips != 15 & ftotval <= (31590*1.30)) |
		 (fpersons == 7 & gestfips != 2 & gestfips != 15 & ftotval <= (35610*1.30)) |
		 (fpersons == 8 & gestfips != 2 & gestfips != 15 & ftotval <= (39630*1.30)) |
		 (fpersons >= 9 & gestfips != 2 & gestfips != 15 & ftotval <= ((39630+(fpersons - 8)*4020)*1.30)) |
					(fpersons == 1 & gestfips != 2 & gestfips == 15 & ftotval <= (13230*1.30)) | 
			(fpersons == 2 & gestfips != 2 & gestfips == 15 & ftotval <= (17850*1.30)) | 
			(fpersons == 3 & gestfips != 2 & gestfips == 15 & ftotval <= (22470*1.30)) |
		 (fpersons == 4 & gestfips != 2 & gestfips == 15 & ftotval <= (27090*1.30)) |
			(fpersons == 5 & gestfips != 2 & gestfips == 15 & ftotval <= (31710 *1.30)) |
		 (fpersons == 6 & gestfips != 2 & gestfips == 15 & ftotval <= (36330*1.30)) |
		 (fpersons == 7 & gestfips != 2 & gestfips == 15 & ftotval <= (40950*1.30)) |
		 (fpersons == 8 & gestfips != 2 & gestfips == 15 & ftotval <= (45570*1.30)) |
		 (fpersons >= 9 & gestfips != 2 & gestfips == 15 & ftotval <= ((45570+(fpersons - 8)*4620)*1.30)) | 
#Alaska
					(fpersons == 1 & gestfips == 2 & gestfips != 15 & ftotval <= (14350*1.30)) | 
			(fpersons == 2 & gestfips == 2 & gestfips != 15 & ftotval <= (19380*1.30)) | 
			(fpersons == 3 & gestfips == 2 & gestfips != 15 & ftotval <= (24410*1.30)) |
		 (fpersons == 4 & gestfips == 2 & gestfips != 15 & ftotval <= (29440*1.30)) |
			(fpersons == 5 & gestfips == 2 & gestfips != 15 & ftotval <= (34470*1.30)) |
		 (fpersons == 6 & gestfips == 2 & gestfips != 15 & ftotval <= (39500*1.30)) |
		 (fpersons == 7 & gestfips == 2 & gestfips != 15 & ftotval <= (42280*1.30)) |
		 (fpersons == 8 & gestfips == 2 & gestfips != 15 & ftotval <= (49560*1.30)) |
		 (fpersons >= 9 & gestfips == 2 & gestfips != 15 & ftotval <= ((49560+(fpersons - 8)*5030)*1.30)))
| (paw_typ == 1 | paw_typ == 3 | hfoodsp == 1 | povll < 5))

kids_reduced13 <- subset(kids13,
			((fpersons == 1 & gestfips != 2 & gestfips != 15 & ftotval > (11490 * 1.30) & ftotval < (11490*1.85)) | 
			(fpersons == 2 & gestfips != 2 & gestfips != 15 & ftotval > (15510 * 1.30) & ftotval < (15510*1.85)) | 
			(fpersons == 3 & gestfips != 2 & gestfips != 15 & ftotval > (19530 * 1.30) & ftotval < (19530 * 1.85)) | 
		 (fpersons == 4 & gestfips != 2 & gestfips != 15 & ftotval > (23550 * 1.30) & ftotval < (23550 * 1.85)) | 
		 (fpersons == 5 & gestfips != 2 & gestfips != 15 & ftotval > (27570 * 1.30) & ftotval < (27570 *1.85)) | 
		 (fpersons == 6 & gestfips != 2 & gestfips != 15 & ftotval > (31590 * 1.30) & ftotval < (31590 *1.85)) | 
		 (fpersons == 7 & gestfips != 2 & gestfips != 15 & ftotval > (35610 * 1.30) & ftotval < (35610 *1.85)) | 
		 (fpersons == 8 & gestfips != 2 & gestfips != 15 & ftotval > (39630 * 1.30) & ftotval < (39630 *1.85)) | 
		 (fpersons >= 9 & gestfips != 2 & gestfips != 15 & ftotval > ((39630+(fpersons - 8)*4020)* 1.30) & ftotval < (39630+(fpersons - 8)*4020)*1.85) | 
					(fpersons == 1 & gestfips != 2 & gestfips == 15 & ftotval > (13230 * 1.30) & ftotval < (13230 *1.85)) | 
			(fpersons == 2 & gestfips != 2 & gestfips == 15 & ftotval > (17850 * 1.30) & ftotval < (17850 *1.85)) | 
			(fpersons == 3 & gestfips != 2 & gestfips == 15 & ftotval > (22470 * 1.30) & ftotval < (22470 *1.85)) | 
		 (fpersons == 4 & gestfips != 2 & gestfips == 15 & ftotval > (27090 * 1.30) & ftotval < (27090 *1.85)) | 
			(fpersons == 5 & gestfips != 2 & gestfips == 15 & ftotval > (31710 * 1.30) & ftotval < (31710 *1.85)) | 
		 (fpersons == 6 & gestfips != 2 & gestfips == 15 & ftotval > (36330 * 1.30) & ftotval < (36330 *1.85)) | 
		 (fpersons == 7 & gestfips != 2 & gestfips == 15 & ftotval > (40950 * 1.30) & ftotval < (40950 *1.85)) | 
		 (fpersons == 8 & gestfips != 2 & gestfips == 15 & ftotval > (45570 * 1.30) & ftotval < (45570 *1.85)) | 
		 (fpersons >= 9 & gestfips != 2 & gestfips == 15 & ftotval > ((45570+(fpersons - 8)*4620) * 1.30) & ftotval < (45570+(fpersons - 8)*4620)*1.85) | 
#Alaska
					(fpersons == 1 & gestfips == 2 & gestfips != 15 & ftotval > (14350 * 1.30) & ftotval < (14350 *1.85)) | 
			(fpersons == 2 & gestfips == 2 & gestfips != 15 & ftotval > (19380 * 1.30) & ftotval < (19380 *1.85)) | 
			(fpersons == 3 & gestfips == 2 & gestfips != 15 & ftotval > (24410 * 1.30) & ftotval < (24410 *1.85)) | 
		 (fpersons == 4 & gestfips == 2 & gestfips != 15 & ftotval > (29440 * 1.30) & ftotval < (29440 *1.85)) | 
			(fpersons == 5 & gestfips == 2 & gestfips != 15 & ftotval > (34470 * 1.30) & ftotval < (34470 *1.85)) | 
		 (fpersons == 6 & gestfips == 2 & gestfips != 15 & ftotval > (39500 * 1.30) & ftotval < (39500 *1.85)) | 
		 (fpersons == 7 & gestfips == 2 & gestfips != 15 & ftotval > (44530 * 1.30) & ftotval < (44530 *1.85)) | 
		 (fpersons == 8 & gestfips == 2 & gestfips != 15 & ftotval > (49560 * 1.30) & ftotval < (49560 *1.85)) | 
		 (fpersons >= 9 & gestfips == 2 & gestfips != 15 & ftotval > ((49560+(fpersons - 8)*5030) *1.30) & ftotval < (49560+(fpersons - 8)*5030) *1.85))
			& ((paw_typ == 0 | paw_typ == 3 ) & (hfoodsp == 2 | 0) & povll >= 5 & povll < 9 ))


kids_full13 <- subset(kids13,
			((fpersons == 1 & gestfips != 2 & gestfips != 15 & ftotval > (11490 * 1.30) & ftotval >= (11490*1.85)) | 
			(fpersons == 2 & gestfips != 2 & gestfips != 15 & ftotval > (15510 * 1.30) & ftotval >= (15510*1.85)) | 
			(fpersons == 3 & gestfips != 2 & gestfips != 15 & ftotval > (19530 * 1.30) & ftotval >= (19530 * 1.85)) | 
		 (fpersons == 4 & gestfips != 2 & gestfips != 15 & ftotval > (23550 * 1.30) & ftotval >= (23550 * 1.85)) | 
		 (fpersons == 5 & gestfips != 2 & gestfips != 15 & ftotval > (27570 * 1.30) & ftotval >= (27570 *1.85)) | 
		 (fpersons == 6 & gestfips != 2 & gestfips != 15 & ftotval > (31590 * 1.30) & ftotval >= (31590 *1.85)) | 
		 (fpersons == 7 & gestfips != 2 & gestfips != 15 & ftotval > (35610 * 1.30) & ftotval >= (35610 *1.85)) | 
		 (fpersons == 8 & gestfips != 2 & gestfips != 15 & ftotval > (39630 * 1.30) & ftotval >= (39630 *1.85)) | 
		 (fpersons >= 9 & gestfips != 2 & gestfips != 15 & ftotval > ((39630+(fpersons - 8)*4020)* 1.30) & ftotval >= (39630+(fpersons - 8)*4020)*1.85) | 
					(fpersons == 1 & gestfips != 2 & gestfips == 15 & ftotval > (13230 * 1.30) & ftotval >= (13230 *1.85)) | 
			(fpersons == 2 & gestfips != 2 & gestfips == 15 & ftotval > (17850 * 1.30) & ftotval >= (17850 *1.85)) | 
			(fpersons == 3 & gestfips != 2 & gestfips == 15 & ftotval > (22470 * 1.30) & ftotval >= (22470 *1.85)) | 
		 (fpersons == 4 & gestfips != 2 & gestfips == 15 & ftotval > (27090 * 1.30) & ftotval >= (27090 *1.85)) | 
			(fpersons == 5 & gestfips != 2 & gestfips == 15 & ftotval > (31710 * 1.30) & ftotval >= (31710 *1.85)) | 
		 (fpersons == 6 & gestfips != 2 & gestfips == 15 & ftotval > (36330 * 1.30) & ftotval >= (36330 *1.85)) | 
		 (fpersons == 7 & gestfips != 2 & gestfips == 15 & ftotval > (40950 * 1.30) & ftotval >= (40950 *1.85)) | 
		 (fpersons == 8 & gestfips != 2 & gestfips == 15 & ftotval > (45570 * 1.30) & ftotval >= (45570 *1.85)) | 
		 (fpersons >= 9 & gestfips != 2 & gestfips == 15 & ftotval > ((45570+(fpersons - 8)*4620) * 1.30) & ftotval >= (45570+(fpersons - 8)*4620)*1.85) | 
#Alaska
					(fpersons == 1 & gestfips == 2 & gestfips != 15 & ftotval > (14350 * 1.30) & ftotval >= (14350 *1.85)) | 
			(fpersons == 2 & gestfips == 2 & gestfips != 15 & ftotval > (19380 * 1.30) & ftotval >= (19380 *1.85)) | 
			(fpersons == 3 & gestfips == 2 & gestfips != 15 & ftotval > (24410 * 1.30) & ftotval >= (24410 *1.85)) | 
		 (fpersons == 4 & gestfips == 2 & gestfips != 15 & ftotval > (29440 * 1.30) & ftotval >= (29440 *1.85)) | 
			(fpersons == 5 & gestfips == 2 & gestfips != 15 & ftotval > (34470 * 1.30) & ftotval >= (34470 *1.85)) | 
		 (fpersons == 6 & gestfips == 2 & gestfips != 15 & ftotval > (39500 * 1.30) & ftotval >= (39500 *1.85)) | 
		 (fpersons == 7 & gestfips == 2 & gestfips != 15 & ftotval > (44530 * 1.30) & ftotval >= (44530 *1.85)) | 
		 (fpersons == 8 & gestfips == 2 & gestfips != 15 & ftotval > (49560 * 1.30) & ftotval >= (49560 *1.85)) | 
		 (fpersons >= 9 & gestfips == 2 & gestfips != 15 & ftotval > ((49560+(fpersons - 8)*5030) *1.30) & ftotval >= (49560+(fpersons - 8)*5030) *1.85))
			& ((paw_typ == 0 | paw_typ == 3 ) & (hfoodsp == 2 | 0)& povll >= 7))

##### 2014 #####
kids_free14 <- subset(kids14,
			((fpersons == 1 & gestfips != 2 & gestfips != 15 & ftotval <= (11670*1.30)) | 
			(fpersons == 2 & gestfips != 2 & gestfips != 15 & ftotval <= (15730*1.30)) | 
			(fpersons == 3 & gestfips != 2 & gestfips != 15 & ftotval <= (19790*1.30)) |
		 (fpersons == 4 & gestfips != 2 & gestfips != 15 & ftotval <= (23850*1.30)) |
		 (fpersons == 5 & gestfips != 2 & gestfips != 15 & ftotval <= (27910 *1.30)) |
		 (fpersons == 6 & gestfips != 2 & gestfips != 15 & ftotval <= (31970*1.30)) |
		 (fpersons == 7 & gestfips != 2 & gestfips != 15 & ftotval <= (36030*1.30)) |
		 (fpersons == 8 & gestfips != 2 & gestfips != 15 & ftotval <= (40090*1.30)) |
		 (fpersons >= 9 & gestfips != 2 & gestfips != 15 & ftotval <= ((40090+(fpersons - 8)*4060)*1.30)) |
					(fpersons == 1 & gestfips != 2 & gestfips == 15 & ftotval <= (13420*1.30)) | 
			(fpersons == 2 & gestfips != 2 & gestfips == 15 & ftotval <= (18090*1.30)) | 
			(fpersons == 3 & gestfips != 2 & gestfips == 15 & ftotval <= (22760*1.30)) |
		 (fpersons == 4 & gestfips != 2 & gestfips == 15 & ftotval <= (27430*1.30)) |
			(fpersons == 5 & gestfips != 2 & gestfips == 15 & ftotval <= (32100 *1.30)) |
		 (fpersons == 6 & gestfips != 2 & gestfips == 15 & ftotval <= (36770*1.30)) |
		 (fpersons == 7 & gestfips != 2 & gestfips == 15 & ftotval <= (41440*1.30)) |
		 (fpersons == 8 & gestfips != 2 & gestfips == 15 & ftotval <= (46110*1.30)) |
		 (fpersons >= 9 & gestfips != 2 & gestfips == 15 & ftotval <= ((46110+(fpersons - 8)*4670)*1.30)) | 
#Alaska
					(fpersons == 1 & gestfips == 2 & gestfips != 15 & ftotval <= (14580*1.30)) | 
			(fpersons == 2 & gestfips == 2 & gestfips != 15 & ftotval <= (19660*1.30)) | 
			(fpersons == 3 & gestfips == 2 & gestfips != 15 & ftotval <= (24740*1.30)) |
		 (fpersons == 4 & gestfips == 2 & gestfips != 15 & ftotval <= (29820*1.30)) |
			(fpersons == 5 & gestfips == 2 & gestfips != 15 & ftotval <= (34900*1.30)) |
		 (fpersons == 6 & gestfips == 2 & gestfips != 15 & ftotval <= (39980*1.30)) |
		 (fpersons == 7 & gestfips == 2 & gestfips != 15 & ftotval <= (45060*1.30)) |
		 (fpersons == 8 & gestfips == 2 & gestfips != 15 & ftotval <= (50140*1.30)) |
		 (fpersons >= 9 & gestfips == 2 & gestfips != 15 & ftotval <= ((50140+(fpersons - 8)*5080)*1.30))) | 
				 (paw_typ == 1 | paw_typ == 3 | hfoodsp == 1 | povll < 5))

kids_reduced14 <- subset(kids14,
			((fpersons == 1 & gestfips != 2 & gestfips != 15 & ftotval > (11670 * 1.30) & ftotval < (11670*1.85)) | 
			(fpersons == 2 & gestfips != 2 & gestfips != 15 & ftotval > (15730 * 1.30) & ftotval < (15730*1.85)) | 
			(fpersons == 3 & gestfips != 2 & gestfips != 15 & ftotval > (19790 * 1.30) & ftotval < (19790 * 1.85)) | 
		 (fpersons == 4 & gestfips != 2 & gestfips != 15 & ftotval > (23850 * 1.30) & ftotval < (23850 * 1.85)) | 
		 (fpersons == 5 & gestfips != 2 & gestfips != 15 & ftotval > (27910 * 1.30) & ftotval < (27910 *1.85)) | 
		 (fpersons == 6 & gestfips != 2 & gestfips != 15 & ftotval > (31970 * 1.30) & ftotval < (31970 *1.85)) | 
		 (fpersons == 7 & gestfips != 2 & gestfips != 15 & ftotval > (36030 * 1.30) & ftotval < (36030 *1.85)) | 
		 (fpersons == 8 & gestfips != 2 & gestfips != 15 & ftotval > (40090 * 1.30) & ftotval < (40090 *1.85)) | 
		 (fpersons >= 9 & gestfips != 2 & gestfips != 15 & ftotval > ((40090+(fpersons - 8)*4060)* 1.30) & ftotval < ((40090+(fpersons - 8)*4060)*1.85)) | 
					(fpersons == 1 & gestfips != 2 & gestfips == 15 & ftotval > (13420 * 1.30) & ftotval < (13420 *1.85)) | 
			(fpersons == 2 & gestfips != 2 & gestfips == 15 & ftotval > (18090 * 1.30) & ftotval < (18090 *1.85)) | 
			(fpersons == 3 & gestfips != 2 & gestfips == 15 & ftotval > (22760 * 1.30) & ftotval < (22760 *1.85)) | 
		 (fpersons == 4 & gestfips != 2 & gestfips == 15 & ftotval > (27430 * 1.30) & ftotval < (27430 *1.85)) | 
			(fpersons == 5 & gestfips != 2 & gestfips == 15 & ftotval > (32100 * 1.30) & ftotval < (32100 *1.85)) | 
		 (fpersons == 6 & gestfips != 2 & gestfips == 15 & ftotval > (36770 * 1.30) & ftotval < (36770 *1.85)) | 
		 (fpersons == 7 & gestfips != 2 & gestfips == 15 & ftotval > (41440 * 1.30) & ftotval < (41440 *1.85)) | 
		 (fpersons == 8 & gestfips != 2 & gestfips == 15 & ftotval > (46110 * 1.30) & ftotval < (46110 *1.85)) | 
		 (fpersons >= 9 & gestfips != 2 & gestfips == 15 & ftotval > ((46110+(fpersons - 8)*4670) * 1.30) & ftotval < ((46110+(fpersons - 8)*4670)*1.85)) | 
#Alaska
					(fpersons == 1 & gestfips == 2 & gestfips != 15 & ftotval > (14580 * 1.30) & ftotval < (14580 *1.85)) | 
			(fpersons == 2 & gestfips == 2 & gestfips != 15 & ftotval > (19660 * 1.30) & ftotval < (19660 *1.85)) | 
			(fpersons == 3 & gestfips == 2 & gestfips != 15 & ftotval > (24740 * 1.30) & ftotval < (24740 *1.85)) | 
		 (fpersons == 4 & gestfips == 2 & gestfips != 15 & ftotval > (29820 * 1.30) & ftotval < (29820 *1.85)) | 
			(fpersons == 5 & gestfips == 2 & gestfips != 15 & ftotval > (34900 * 1.30) & ftotval < (34900 *1.85)) | 
		 (fpersons == 6 & gestfips == 2 & gestfips != 15 & ftotval > (39980 * 1.30) & ftotval < (39980 *1.85)) | 
		 (fpersons == 7 & gestfips == 2 & gestfips != 15 & ftotval > (45060 * 1.30) & ftotval < (45060 *1.85)) | 
		 (fpersons == 8 & gestfips == 2 & gestfips != 15 & ftotval > (50140 * 1.30) & ftotval < (50140 *1.85)) | 
		 (fpersons >= 9 & gestfips == 2 & gestfips != 15 & ftotval > ((50140+(fpersons - 8)*5080) *1.30) & ftotval < ((50140+(fpersons - 8)*5080) *1.85))) 
			& ((paw_typ == 0 | paw_typ == 3 ) & (hfoodsp == 2 | 0)  & povll >= 5 & povll < 9 ))

kids_full14 <- subset(kids14,
			((fpersons == 1 & gestfips != 2 & gestfips != 15 & ftotval > (11670 * 1.30) & ftotval >= (11670*1.85)) | 
			(fpersons == 2 & gestfips != 2 & gestfips != 15 & ftotval > (15730 * 1.30) & ftotval >= (15730*1.85)) | 
			(fpersons == 3 & gestfips != 2 & gestfips != 15 & ftotval > (19790 * 1.30) & ftotval >= (19790 * 1.85)) | 
		 (fpersons == 4 & gestfips != 2 & gestfips != 15 & ftotval > (23850 * 1.30) & ftotval >= (23850 * 1.85)) | 
		 (fpersons == 5 & gestfips != 2 & gestfips != 15 & ftotval > (27910 * 1.30) & ftotval >= (27910 *1.85)) | 
		 (fpersons == 6 & gestfips != 2 & gestfips != 15 & ftotval > (31970 * 1.30) & ftotval >= (31970 *1.85)) | 
		 (fpersons == 7 & gestfips != 2 & gestfips != 15 & ftotval > (36030 * 1.30) & ftotval >= (36030 *1.85)) | 
		 (fpersons == 8 & gestfips != 2 & gestfips != 15 & ftotval > (40090 * 1.30) & ftotval >= (40090 *1.85)) | 
		 (fpersons >= 9 & gestfips != 2 & gestfips != 15 & ftotval > ((40090+(fpersons - 8)*4060)* 1.30) & ftotval >= ((40090+(fpersons - 8)*4060)*1.85)) | 
					(fpersons == 1 & gestfips != 2 & gestfips == 15 & ftotval > (13420 * 1.30) & ftotval >= (13420 *1.85)) | 
			(fpersons == 2 & gestfips != 2 & gestfips == 15 & ftotval > (18090 * 1.30) & ftotval >= (18090 *1.85)) | 
			(fpersons == 3 & gestfips != 2 & gestfips == 15 & ftotval > (22760 * 1.30) & ftotval >= (22760 *1.85)) | 
		 (fpersons == 4 & gestfips != 2 & gestfips == 15 & ftotval > (27430 * 1.30) & ftotval >= (27430 *1.85)) | 
			(fpersons == 5 & gestfips != 2 & gestfips == 15 & ftotval > (32100 * 1.30) & ftotval >= (32100 *1.85)) | 
		 (fpersons == 6 & gestfips != 2 & gestfips == 15 & ftotval > (36770 * 1.30) & ftotval >= (36770 *1.85)) | 
		 (fpersons == 7 & gestfips != 2 & gestfips == 15 & ftotval > (41440 * 1.30) & ftotval >= (41440 *1.85)) | 
		 (fpersons == 8 & gestfips != 2 & gestfips == 15 & ftotval > (46110 * 1.30) & ftotval >= (46110 *1.85)) | 
		 (fpersons >= 9 & gestfips != 2 & gestfips == 15 & ftotval > ((46110+(fpersons - 8)*4670) * 1.30) & ftotval >= ((46110+(fpersons - 8)*4670)*1.85)) | 
#Alaska
					(fpersons == 1 & gestfips == 2 & gestfips != 15 & ftotval > (14580 * 1.30) & ftotval >= (14580 *1.85)) | 
			(fpersons == 2 & gestfips == 2 & gestfips != 15 & ftotval > (19660 * 1.30) & ftotval >= (19660 *1.85)) | 
			(fpersons == 3 & gestfips == 2 & gestfips != 15 & ftotval > (24740 * 1.30) & ftotval >= (24740 *1.85)) | 
		 (fpersons == 4 & gestfips == 2 & gestfips != 15 & ftotval > (29820 * 1.30) & ftotval >= (29820 *1.85)) | 
			(fpersons == 5 & gestfips == 2 & gestfips != 15 & ftotval > (34900 * 1.30) & ftotval >= (34900 *1.85)) | 
		 (fpersons == 6 & gestfips == 2 & gestfips != 15 & ftotval > (39980 * 1.30) & ftotval >= (39980 *1.85)) | 
		 (fpersons == 7 & gestfips == 2 & gestfips != 15 & ftotval > (45060 * 1.30) & ftotval >= (45060 *1.85)) | 
		 (fpersons == 8 & gestfips == 2 & gestfips != 15 & ftotval > (50140 * 1.30) & ftotval >= (50140 *1.85)) | 
		 (fpersons >= 9 & gestfips == 2 & gestfips != 15 & ftotval > ((50140+(fpersons - 8)*5080) *1.30) & ftotval >= ((50140+(fpersons - 8)*5080) *1.85))) 
			& ((paw_typ == 0 | paw_typ == 3 ) & (hfoodsp == 2 | 0) & povll >= 7))

##### 2015 #####
kids_free15 <- subset(kids15,
			((fpersons == 1 & gestfips != 2 & gestfips != 15 & ftotval <= (11770*1.30)) | 
			(fpersons == 2 & gestfips != 2 & gestfips != 15 & ftotval <= (15930*1.30)) | 
			(fpersons == 3 & gestfips != 2 & gestfips != 15 & ftotval <= (20090*1.30)) |
		 (fpersons == 4 & gestfips != 2 & gestfips != 15 & ftotval <= (24250*1.30)) |
		 (fpersons == 5 & gestfips != 2 & gestfips != 15 & ftotval <= (28410 *1.30)) |
		 (fpersons == 6 & gestfips != 2 & gestfips != 15 & ftotval <= (32570*1.30)) |
		 (fpersons == 7 & gestfips != 2 & gestfips != 15 & ftotval <= (36730*1.30)) |
		 (fpersons == 8 & gestfips != 2 & gestfips != 15 & ftotval <= (40890*1.30)) |
		 (fpersons >= 9 & gestfips != 2 & gestfips != 15 & ftotval <= (40890+(fpersons - 8)*4160)*1.30) |
					(fpersons == 1 & gestfips != 2 & gestfips == 15 & ftotval <= (13550*1.30)) | 
			(fpersons == 2 & gestfips != 2 & gestfips == 15 & ftotval <= (18330*1.30)) | 
			(fpersons == 3 & gestfips != 2 & gestfips == 15 & ftotval <= (23110*1.30)) |
		 (fpersons == 4 & gestfips != 2 & gestfips == 15 & ftotval <= (27890*1.30)) |
			(fpersons == 5 & gestfips != 2 & gestfips == 15 & ftotval <= (32670 *1.30)) |
		 (fpersons == 6 & gestfips != 2 & gestfips == 15 & ftotval <= (37450*1.30)) |
		 (fpersons == 7 & gestfips != 2 & gestfips == 15 & ftotval <= (42230*1.30)) |
		 (fpersons == 8 & gestfips != 2 & gestfips == 15 & ftotval <= (47010*1.30)) |
		 (fpersons >= 9 & gestfips != 2 & gestfips == 15 & ftotval <= ((47010+(fpersons - 8)*4780)*1.30)) | 
#Alaska
					(fpersons == 1 & gestfips == 2 & gestfips != 15 & ftotval <= (14720*1.30)) | 
			(fpersons == 2 & gestfips == 2 & gestfips != 15 & ftotval <= (19920*1.30)) | 
			(fpersons == 3 & gestfips == 2 & gestfips != 15 & ftotval <= (25120*1.30)) |
		 (fpersons == 4 & gestfips == 2 & gestfips != 15 & ftotval <= (30320*1.30)) |
			(fpersons == 5 & gestfips == 2 & gestfips != 15 & ftotval <= (35520*1.30)) |
		 (fpersons == 6 & gestfips == 2 & gestfips != 15 & ftotval <= (40720*1.30)) |
		 (fpersons == 7 & gestfips == 2 & gestfips != 15 & ftotval <= (45920*1.30)) |
		 (fpersons == 8 & gestfips == 2 & gestfips != 15 & ftotval <= (51120*1.30)) |
		 (fpersons >= 9 & gestfips == 2 & gestfips != 15 & ftotval <= (51120+(fpersons - 8)*5200)*1.30))  
				 | (paw_typ == 1 | paw_typ == 3 | hfoodsp == 1 | povll < 5))

kids_reduced15 <- subset(kids15,
			((fpersons == 1 & gestfips != 2 & gestfips != 15 & ftotval > (11770 * 1.30) & ftotval < (11770*1.85)) | 
			(fpersons == 2 & gestfips != 2 & gestfips != 15 & ftotval > (15930 * 1.30) & ftotval < (15930*1.85)) | 
			(fpersons == 3 & gestfips != 2 & gestfips != 15 & ftotval > (20090 * 1.30) & ftotval < (20090 * 1.85)) | 
		 (fpersons == 4 & gestfips != 2 & gestfips != 15 & ftotval > (24250 * 1.30) & ftotval < (24250 * 1.85)) | 
		 (fpersons == 5 & gestfips != 2 & gestfips != 15 & ftotval > (28410 * 1.30) & ftotval < (28410 *1.85)) | 
		 (fpersons == 6 & gestfips != 2 & gestfips != 15 & ftotval > (32570 * 1.30) & ftotval < (32570 *1.85)) | 
		 (fpersons == 7 & gestfips != 2 & gestfips != 15 & ftotval > (36730 * 1.30) & ftotval < (36730 *1.85)) | 
		 (fpersons == 8 & gestfips != 2 & gestfips != 15 & ftotval > (40890 * 1.30) & ftotval < (40890 *1.85)) | 
		 (fpersons >= 9 & gestfips != 2 & gestfips != 15 & ftotval > ((40890+(fpersons - 8)*4160)* 1.30) & ftotval < ((40890+(fpersons - 8)*4160)*1.85)) | 
					(fpersons == 1 & gestfips != 2 & gestfips == 15 & ftotval > (13550 * 1.30) & ftotval < (13550 *1.85)) | 
			(fpersons == 2 & gestfips != 2 & gestfips == 15 & ftotval > (18330 * 1.30) & ftotval < (18330 *1.85)) | 
			(fpersons == 3 & gestfips != 2 & gestfips == 15 & ftotval > (23110 * 1.30) & ftotval < (23110 *1.85)) | 
		 (fpersons == 4 & gestfips != 2 & gestfips == 15 & ftotval > (27890 * 1.30) & ftotval < (27890 *1.85)) | 
			(fpersons == 5 & gestfips != 2 & gestfips == 15 & ftotval > (32670 * 1.30) & ftotval < (32670 *1.85)) | 
		 (fpersons == 6 & gestfips != 2 & gestfips == 15 & ftotval > (37450 * 1.30) & ftotval < (37450 *1.85)) | 
		 (fpersons == 7 & gestfips != 2 & gestfips == 15 & ftotval > (42230 * 1.30) & ftotval < (42230 *1.85)) | 
		 (fpersons == 8 & gestfips != 2 & gestfips == 15 & ftotval > (47010 * 1.30) & ftotval < (47010 *1.85)) | 
		 (fpersons >= 9 & gestfips != 2 & gestfips == 15 & ftotval > ((47010+(fpersons - 8)*4780) * 1.30) & ftotval < ((47010+(fpersons - 8)*4780)*1.85)) | 
#Alaska
					(fpersons == 1 & gestfips == 2 & gestfips != 15 & ftotval > (14720 * 1.30) & ftotval < (14720 *1.85)) | 
			(fpersons == 2 & gestfips == 2 & gestfips != 15 & ftotval > (19920 * 1.30) & ftotval < (19920 *1.85)) | 
			(fpersons == 3 & gestfips == 2 & gestfips != 15 & ftotval > (25120 * 1.30) & ftotval < (25120 *1.85)) | 
		 (fpersons == 4 & gestfips == 2 & gestfips != 15 & ftotval > (30320 * 1.30) & ftotval < (30320 *1.85)) | 
			(fpersons == 5 & gestfips == 2 & gestfips != 15 & ftotval > (35520 * 1.30) & ftotval < (35520 *1.85)) | 
		 (fpersons == 6 & gestfips == 2 & gestfips != 15 & ftotval > (40720 * 1.30) & ftotval < (40720 *1.85)) | 
		 (fpersons == 7 & gestfips == 2 & gestfips != 15 & ftotval > (45920 * 1.30) & ftotval < (45920 *1.85)) | 
		 (fpersons == 8 & gestfips == 2 & gestfips != 15 & ftotval > (51120 * 1.30) & ftotval < (51120 *1.85)) | 
		 (fpersons >= 9 & gestfips == 2 & gestfips != 15 & ftotval > ((51120+(fpersons - 8)*5200) *1.30) & ftotval < ((51120+(fpersons - 8)*5200) *1.85))) 
			& ((paw_typ == 0 | paw_typ == 3 ) & (hfoodsp == 2 | 0) & povll >= 5 & povll < 9 ))

kids_full15 <- subset(kids15,
			((fpersons == 1 & gestfips != 2 & gestfips != 15 & ftotval > (11770 * 1.30) & ftotval >= (11770*1.85)) | 
			(fpersons == 2 & gestfips != 2 & gestfips != 15 & ftotval > (15930 * 1.30) & ftotval >= (15930*1.85)) | 
			(fpersons == 3 & gestfips != 2 & gestfips != 15 & ftotval > (20090 * 1.30) & ftotval >= (20090 * 1.85)) | 
		 (fpersons == 4 & gestfips != 2 & gestfips != 15 & ftotval > (24250 * 1.30) & ftotval >= (24250 * 1.85)) | 
		 (fpersons == 5 & gestfips != 2 & gestfips != 15 & ftotval > (28410 * 1.30) & ftotval >= (28410 *1.85)) | 
		 (fpersons == 6 & gestfips != 2 & gestfips != 15 & ftotval > (32570 * 1.30) & ftotval >= (32570 *1.85)) | 
		 (fpersons == 7 & gestfips != 2 & gestfips != 15 & ftotval > (36730 * 1.30) & ftotval >= (36730 *1.85)) | 
		 (fpersons == 8 & gestfips != 2 & gestfips != 15 & ftotval > (40890 * 1.30) & ftotval >= (40890 *1.85)) | 
		 (fpersons >= 9 & gestfips != 2 & gestfips != 15 & ftotval > ((40890+(fpersons - 8)*4160)* 1.30) & ftotval >= ((40890+(fpersons - 8)*4160)*1.85)) | 
					(fpersons == 1 & gestfips != 2 & gestfips == 15 & ftotval > (13550 * 1.30) & ftotval >= (13550 *1.85)) | 
			(fpersons == 2 & gestfips != 2 & gestfips == 15 & ftotval > (18330 * 1.30) & ftotval >= (18330 *1.85)) | 
			(fpersons == 3 & gestfips != 2 & gestfips == 15 & ftotval > (23110 * 1.30) & ftotval >= (23110 *1.85)) | 
		 (fpersons == 4 & gestfips != 2 & gestfips == 15 & ftotval > (27890 * 1.30) & ftotval >= (27890 *1.85)) | 
			(fpersons == 5 & gestfips != 2 & gestfips == 15 & ftotval > (32670 * 1.30) & ftotval >= (32670 *1.85)) | 
		 (fpersons == 6 & gestfips != 2 & gestfips == 15 & ftotval > (37450 * 1.30) & ftotval >= (37450 *1.85)) | 
		 (fpersons == 7 & gestfips != 2 & gestfips == 15 & ftotval > (42230 * 1.30) & ftotval >= (42230 *1.85)) | 
		 (fpersons == 8 & gestfips != 2 & gestfips == 15 & ftotval > (47010 * 1.30) & ftotval >= (47010 *1.85)) | 
		 (fpersons >= 9 & gestfips != 2 & gestfips == 15 & ftotval > ((47010+(fpersons - 8)*4780) * 1.30) & ftotval >= ((47010+(fpersons - 8)*4780)*1.85)) | 
#Alaska
					(fpersons == 1 & gestfips == 2 & gestfips != 15 & ftotval > (14720 * 1.30) & ftotval >= (14720 *1.85)) | 
			(fpersons == 2 & gestfips == 2 & gestfips != 15 & ftotval > (19920 * 1.30) & ftotval >= (19920 *1.85)) | 
			(fpersons == 3 & gestfips == 2 & gestfips != 15 & ftotval > (25120 * 1.30) & ftotval >= (25120 *1.85)) | 
		 (fpersons == 4 & gestfips == 2 & gestfips != 15 & ftotval > (30320 * 1.30) & ftotval >= (30320 *1.85)) | 
			(fpersons == 5 & gestfips == 2 & gestfips != 15 & ftotval > (35520 * 1.30) & ftotval >= (35520 *1.85)) | 
		 (fpersons == 6 & gestfips == 2 & gestfips != 15 & ftotval > (40720 * 1.30) & ftotval >= (40720 *1.85)) | 
		 (fpersons == 7 & gestfips == 2 & gestfips != 15 & ftotval > (45920 * 1.30) & ftotval >= (45920 *1.85)) | 
		 (fpersons == 8 & gestfips == 2 & gestfips != 15 & ftotval > (51120 * 1.30) & ftotval >= (51120 *1.85)) | 
		 (fpersons >= 9 & gestfips == 2 & gestfips != 15 & ftotval > ((51120+(fpersons - 8)*5200) *1.30) & ftotval >= ((51120+(fpersons - 8)*5200) *1.85))) 
			& ((paw_typ == 0 | paw_typ == 3 ) & (hfoodsp == 2 | 0) & povll >= 7))



