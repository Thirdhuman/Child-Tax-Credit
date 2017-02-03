#Illegal Welfare 2

#### Personal ####
# supplemental security income, child received - ssikidyn
# child covered by medicare/medicaid - ch_mc
# child covered by state’s chip - pchip

#### Personal ####

# covered by medicare - care
# covered by medicaid - caid
# wic benefits received - wicyn
# supplemental security income received - ssi_yn
# public assistance - person - paw_yn
# educational assistance - ed_yn
# survivor's benefits other than social security or veterans benefits - sur_yn
# unemployment compensation - uc_yn

bad_hombres <- function(x){
	k <- subset(x, care == 1 | caid == 1 | wicyn == 1 | ssi_yn == 1 | paw_yn == 1 | ed_yn == 1 | sur_yn == 1 | uc_yn == 1 
													| pchip == 1 | ch_mc == 1 | ssikidyn == 1)
	svyby( ~one, by = ~ i_stat, design =  k, 	FUN = svytotal)}
 
bad_hombres(z)

#### Household ####

# food stamps recipients - hfoodsp
# public housing project - hpublic
# educational assistance benefits - hed_yn
# energy assistance benefits - hengast 
# supplemental security benefits - hssi_yn
# children receiving free or reduced price lunches - hflunch

household_bad_hombres <- function(x){
	k <- subset(x,  care == 1 | caid == 1 | wicyn == 1 | ssi_yn == 1 | paw_yn == 1 | ed_yn == 1 | sur_yn == 1 | uc_yn == 1 |
													 pchip == 1 | ch_mc == 1 | ssikidyn == 1	| 
															hfoodsp == 1 | hpaw_yn == 1 | hpublic == 1 | hed_yn == 1 | hflunch == 1 | hengast == 1 | uc_yn == 1)
	svyby( ~one, by = ~ i_stat, design =  k, 	FUN = svytotal)}

household_bad_hombres <- function(x){
	k <- subset(x, 	hfoodsp == 1 | hpaw_yn == 1 | hpublic == 1 | hed_yn == 1 | hflunch == 1 | hengast == 1 | uc_yn == 1)
	svyby( ~one, by = ~ i_stat, design =  k, 	FUN = svytotal)}
 
household_bad_hombres(z)

# public assistance - household - hpaw_yn
# supplemental unemployment benefits received - subuc
# medicaid, anyone in hhld covered by hmcaid
# medicare, anyone in hhld covered by hmcare
# social security income - hssval
# wic program benefits, anyone - hrwicyn

