# name the database files in the "MonetDB" folder of the current working directory
dbfolder <- paste0( getwd() , "/MonetDB" )


#######################################
# survey design for replicate weights #
#######################################

# create survey design object with CPS design information
# using existing data frame of CPS data
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
#pwsswgt
#marsupwt

# workaround for a bug in survey::svrepdesign.character
y$mse <- TRUE

females.above15.w_child <-
	subset( 
		y ,
		a_age > 15 &		# age 16+
		a_sex %in% 2 &		# females
	 hunder18 > 0 & # children in house
		a_pfrel %in% 5  # single 
	)

females.above15.w_child2 <-
	subset( 
		y ,
		a_age > 15 &		# age 16+
		a_sex %in% 2 &		# females
	 hunder18 > 0 & # children in house
		h_type %in% 4  # single female household
	)


females.above15.w_child <-
	update(
		povll = factor( povll ) ,
		females.above15.w_child
	)

females.above15.w_child2 <-
	update(
		povll = factor( povll ) ,
		females.above15.w_child2
	)

svymean(
	~povll ,
	design = females.above15.w_child
)

svymean(
	~povll ,
	design = females.above15.w_child2
)

svyquantile(
	~hwsval,
	design = females.above15.w_child2 ,
	c( 0 , .05 , .10, .15, .20, .25 , .30, .35, .40, .45, .5 , .55, .60, .65, .70, .75, .80, .85, .9 , .95 , 1 )
)
	