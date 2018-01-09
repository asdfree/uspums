if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

library(lodown)
lodown( "uspums" , output_dir = file.path( getwd() ) )
library(lodown)
# examine all available USPUMS microdata files
uspums_cat <-
	get_catalog( "uspums" ,
		output_dir = file.path( getwd() ) )

# 2000 1% sample only
uspums_cat <- subset( uspums_cat , year == 2000 & percent == 1 )
# download the microdata to your local computer


library(DBI)
library(RSQLite)
library(survey)

options( survey.lonely.psu = "adjust" )

uspums_design <- readRDS( file.path( getwd() , "pums_2000_1_m.rds" ) )

uspums_design <- open( uspums_design , driver = SQLite() )
uspums_design <-
	update(
		
		uspums_design ,
		
		age_categories = factor( 1 + findInterval( age , c( 18 , 35 , 65 ) ) , labels = c( "under 18" , "18-34" , "35-64" , "65+" ) ) ,
		
		married = as.numeric( marstat == 1 ) ,
		
		poverty_status = ifelse( poverty == 0 , NA , poverty ) ,
		
		unemployed = as.numeric( esr %in% 3 ) ,
		
		labor_force = as.numeric( esr %in% 1:5 ) ,
		
		employment_status = 
			factor( 
				esr , 
				levels = 0:6 , 
				labels = 
					c( 
						"NIU" ,
						"Employed, at work" , 
						"Employed, with a job but not at work" ,
						"Unemployed" ,
						"Armed Forces, at work" ,
						"Armed Forces, with a job but not at work" ,
						"Not in labor force"
					)
			) ,
			
		
		state_name =
		
			factor(
			
				state ,
				
				levels = 
					c(1, 2, 4, 5, 6, 8, 9, 10, 11, 12, 13, 15, 16, 17, 18, 19, 20, 
					21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 
					37, 38, 39, 40, 41, 42, 44, 45, 46, 47, 48, 49, 50, 51, 53, 54, 
					55, 56, 66, 72, 78) ,
					
				labels = 
					c("ALABAMA", "ALASKA", "ARIZONA", "ARKANSAS", "CALIFORNIA", 
					"COLORADO", "CONNECTICUT", "DELAWARE", "DISTRICT OF COLUMBIA", 
					"FLORIDA", "GEORGIA", "HAWAII", "IDAHO", "ILLINOIS", "INDIANA",
					"IOWA", "KANSAS", "KENTUCKY", "LOUISIANA", "MAINE", "MARYLAND",
					"MASSACHUSETTS", "MICHIGAN", "MINNESOTA", "MISSISSIPPI", 
					"MISSOURI", "MONTANA", "NEBRASKA", "NEVADA", "NEW HAMPSHIRE",
					"NEW JERSEY", "NEW MEXICO", "NEW YORK", "NORTH CAROLINA", 
					"NORTH DAKOTA", "OHIO", "OKLAHOMA", "OREGON", "PENNSYLVANIA",
					"RHODE ISLAND", "SOUTH CAROLINA", "SOUTH DAKOTA", "TENNESSEE",
					"TEXAS", "UTAH", "VERMONT", "VIRGINIA", "WASHINGTON",
					"WEST VIRGINIA", "WISCONSIN", "WYOMING", "GUAM", "PUERTO RICO",
					"U.S. VIRGIN ISLANDS")
					
			) 
	)
sum( weights( uspums_design , "sampling" ) != 0 )

svyby( ~ one , ~ state_name , uspums_design , unwtd.count )
svytotal( ~ one , uspums_design )

svyby( ~ one , ~ state_name , uspums_design , svytotal )
svymean( ~ poverty_status , uspums_design , na.rm = TRUE )

svyby( ~ poverty_status , ~ state_name , uspums_design , svymean , na.rm = TRUE )
svymean( ~ employment_status , uspums_design )

svyby( ~ employment_status , ~ state_name , uspums_design , svymean )
svytotal( ~ poverty_status , uspums_design , na.rm = TRUE )

svyby( ~ poverty_status , ~ state_name , uspums_design , svytotal , na.rm = TRUE )
svytotal( ~ employment_status , uspums_design )

svyby( ~ employment_status , ~ state_name , uspums_design , svytotal )
svyquantile( ~ poverty_status , uspums_design , 0.5 , na.rm = TRUE )

svyby( 
	~ poverty_status , 
	~ state_name , 
	uspums_design , 
	svyquantile , 
	0.5 ,
	ci = TRUE ,
	keep.var = TRUE ,
	na.rm = TRUE
)
svyratio( 
	numerator = ~ unemployed , 
	denominator = ~ labor_force , 
	uspums_design ,
	na.rm = TRUE
)
sub_uspums_design <- subset( uspums_design , sex == 2 )
svymean( ~ poverty_status , sub_uspums_design , na.rm = TRUE )
this_result <- svymean( ~ poverty_status , uspums_design , na.rm = TRUE )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
	svyby( 
		~ poverty_status , 
		~ state_name , 
		uspums_design , 
		svymean ,
		na.rm = TRUE 
	)
	
coef( grouped_result )
SE( grouped_result )
confint( grouped_result )
cv( grouped_result )
degf( uspums_design )
svyvar( ~ poverty_status , uspums_design , na.rm = TRUE )
# SRS without replacement
svymean( ~ poverty_status , uspums_design , na.rm = TRUE , deff = TRUE )

# SRS with replacement
svymean( ~ poverty_status , uspums_design , na.rm = TRUE , deff = "replace" )
svyciprop( ~ married , uspums_design ,
	method = "likelihood" , na.rm = TRUE )
svyttest( poverty_status ~ married , uspums_design )
svychisq( 
	~ married + employment_status , 
	uspums_design 
)
glm_result <- 
	svyglm( 
		poverty_status ~ married + employment_status , 
		uspums_design 
	)

summary( glm_result )
library(convey)
uspums_design <- convey_prep( uspums_design )

svygini( ~ hinc , uspums_design , na.rm = TRUE )

