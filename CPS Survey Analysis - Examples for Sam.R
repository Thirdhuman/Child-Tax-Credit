
### Attach Packages
library(survey)				# load survey package (analyzes complex design surveys)
library(MonetDBLite) # Data base package
library(DBI)			# load the DBI package (implements the R-database coding)

# Set working directory
setwd("~/Desktop/Welfare Economics/Current Population Survey - Master/R")

### Creates path to Monetdb ###
dbfolder <- paste0( getwd() , "/MonetDB" )

### This is necessary to get results that mirror Stata ###
options( survey.replicates.mse = TRUE )

### Use R complex survey designs function.   ###
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

### Females in with Children in poverty - Example #1
females.above15.w_child <-
	subset( 
		y ,
		a_age > 15 &		# age 16+
		a_sex %in% 2 &		# females
	 hunder18 > 0 & # children in house
		h_type %in% 4  # single female household
	)

# Change poverty to categorical variable to derive percentages
females.above15.w_child <-
	update(
		povll = factor( povll ) , 
		females.above15.w_child
	)

# Derive percentage of females 16+ that are living in poverty
svymean(
	~povll , # Poverty Thresholds
	design = females.above15.w_child
)

### Quantiles of income for sample
svyquantile(
	~hwsval, #Household Income
	design = females.above15.w_child ,
	c( 0 , .05 , .10, .15, .20, .25 , .30, .35, .40, .45, .5 , .55, .60, .65, .70, .75, .80, .85, .9 , .95 , 1 )
)

### Child Poverty (some population subsets) - Example #2

#h_numper = Individuals in a houshold
#fownu18 = number of individuals in household under 18
#hwsval = earned income

#Children 18 and younger 
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

# Children under 6
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

#Derive totals from the subsetted populations

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



