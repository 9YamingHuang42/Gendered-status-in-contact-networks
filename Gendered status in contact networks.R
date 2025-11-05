library( dagitty )
library( igraph )
library( ergm )
library( tidytable )
library( dplyr )
library( plyr )
library( stringr )
library( ggplot2 )
library( sf )
library( elevatr )
library( raster )
library( ggrepel )
library( gt )
library( gto )

#1 Dag ----
Dag <-  
  dagitty( 'dag {
bb="-0.5,-0.5,0.5,0.5"
Alter.age [pos="0.375,-0.168"]
Alter.gender [exposure,pos="0.316,-0.349"]
Alter.kinship [pos="0.341,0.188"]
Alter.market [pos="0.141,-0.416"]
Alter.rep [pos="0.243,0.269"]
Alter.residence [pos="0.388,0.067"]
Ego.age [pos="-0.423,-0.152"]
Ego.gender [pos="-0.368,-0.333"]
Ego.kinship [pos="-0.356,0.202"]
Ego.market [pos="-0.174,-0.436"]
Ego.rep [pos="-0.235,0.292"]
Ego.residence [pos="-0.404,0.061"]
Nomination [outcome,pos="0.000,0.320"]
Same.age [pos="0.133,-0.110"]
Same.gender [pos="0.045,-0.292"]
Same.market [pos="-0.005,-0.444"]
Same.rep [pos="-0.140,-0.070"]
Alter.age -> Alter.market
Alter.age -> Alter.rep
Alter.age -> Alter.residence
Alter.age -> Nomination
Alter.age -> Same.age
Alter.gender -> Alter.market
Alter.gender -> Alter.rep
Alter.gender -> Nomination
Alter.gender -> Same.gender
Alter.kinship -> Nomination
Alter.market -> Alter.rep
Alter.market -> Nomination
Alter.market -> Same.market
Alter.rep -> Nomination
Alter.rep -> Same.rep
Alter.residence -> Alter.kinship
Alter.residence -> Alter.market
Alter.residence -> Alter.rep
Alter.residence -> Nomination
Ego.age -> Ego.market
Ego.age -> Ego.rep
Ego.age -> Ego.residence
Ego.age -> Nomination
Ego.age -> Same.age
Ego.gender -> Ego.market
Ego.gender -> Ego.rep
Ego.gender -> Nomination
Ego.gender -> Same.gender
Ego.kinship -> Nomination
Ego.market -> Ego.rep
Ego.market -> Nomination
Ego.market -> Same.market
Ego.rep -> Nomination
Ego.rep -> Same.rep
Ego.residence -> Ego.kinship
Ego.residence -> Ego.market
Ego.residence -> Ego.rep
Ego.residence -> Nomination
Same.age -> Nomination
Same.gender -> Nomination
Same.market -> Nomination
Same.rep -> Nomination
}
')

#2 Data preparation ----
load( "Gendered status in contact networks.RData" )

Network <- igraph::graph_from_data_frame(
  d = Ties ,
  vertices = Nodes ,
  directed = T ) %>% 
  intergraph::asNetwork()

#3 Model ----
##3.1 Age ----
adjustmentSets( Dag , 
                exposure = "Alter.age" , 
                outcome = "Nomination" ,
                effect = "total" )

Fit.ReceiverAge <- ergm( Network ~ edges + 
                           nodeifactor( "Gender" ) * nodeifactor( "Age.c" ) + 
                           nodematch( "VID" ) +
                           mutual( ) , 
                         control = control.ergm( seed = 123 , 
                                                 MCMC.samplesize = 1e5 ,
                                                 MCMC.interval = 1e4 ,
                                                 MCMC.burnin = 2e5 ) ,
                         verbose = FALSE )
summary( Fit.ReceiverAge )

##3.2 Residence ----
adjustmentSets( Dag , 
                exposure = "Alter.residence" , 
                outcome = "Nomination" ,
                effect = "total" )

Fit.ReceiverResi <- ergm( Network ~ edges + 
                            nodeifactor( "Age.c" ) + 
                            nodeifactor( "Gender" ) * nodeifactor( "Residence" ) + 
                            nodematch( "VID" ) +
                            mutual( ) , 
                          control = control.ergm( seed = 123 , 
                                                  MCMC.samplesize = 1e5 ,
                                                  MCMC.interval = 1e4 ,
                                                  MCMC.burnin = 2e5 ) ,
                          verbose = FALSE )
summary( Fit.ReceiverResi )

##3.3 Kinship ----
adjustmentSets( Dag , 
                exposure = "Alter.kinship" , 
                outcome = "Nomination" ,
                effect = "total" )

Fit.ReceiverKin <- ergm( Network ~ edges + 
                           nodeifactor( "Residence" ) + 
                           nodeifactor( "Gender" ) * nodeifactor( "Kinship" ) + 
                           nodematch( "VID" ) +
                           mutual( ) , 
                         control = control.ergm( seed = 123 , 
                                                 MCMC.samplesize = 1e5 ,
                                                 MCMC.interval = 1e4 ,
                                                 MCMC.burnin = 2e5 ) ,
                         verbose = FALSE )
summary( Fit.ReceiverKin )

##3.4 Market ----
adjustmentSets( Dag , 
                exposure = "Alter.market" , 
                outcome = "Nomination" ,
                effect = "total" )

Fit.ReceiverMar <- ergm( Network ~ edges + 
                           nodeifactor( "Age.c" ) + 
                           nodeifactor( "Residence" ) + 
                           nodeifactor( "Gender" ) * nodeicov( "Market" ) + 
                           nodematch( "VID" ) +
                           mutual( ) , 
                         control = control.ergm( seed = 123 , 
                                                 MCMC.samplesize = 1e5 ,
                                                 MCMC.interval = 1e4 ,
                                                 MCMC.burnin = 2e5 ) ,
                         verbose = FALSE )
summary( Fit.ReceiverMar )

##3.5 Reputation ----
adjustmentSets( Dag , 
                exposure = "Alter.rep" , 
                outcome = "Nomination" ,
                effect = "total" )

Fit.ReceiverRep <- ergm( Network ~ edges + 
                           nodeifactor( "Age.c" ) + 
                           nodeicov( "Market" ) + nodeifactor( "Residence" ) + 
                           nodeifactor( "Gender" ) * nodeicov( "Reputation" ) + 
                           nodematch( "VID" ) +
                           mutual( ) , 
                         control = control.ergm( seed = 123 , 
                                                 MCMC.samplesize = 1e5 ,
                                                 MCMC.interval = 1e4 ,
                                                 MCMC.burnin = 2e5 ) ,
                         verbose = FALSE )
summary( Fit.ReceiverRep )

#4 Output -----
##4.1 Age ----
# Model results 
ReceiverAge.output <- Fit.ReceiverAge %>% 
  broom::tidy( conf.int = TRUE ) %>% 
  select( term , estimate , std.error , conf.low , conf.high , p.value ) %>%
  dplyr::rename( Estimate = estimate ,
                 se = std.error ,
                 CI_Lower = conf.low,
                 CI_Upper = conf.high ) %>%
  mutate( OR = exp( Estimate ) , 
          CI5 = exp( CI_Lower ) ,  
          CI95 = exp( CI_Upper ) ,  
          Variable = c( "Edges" , "Male" , ">=65" , "35-49" , "50-64" , 
                        "Male: >=65" , "Male: 35-49" , "Male: 50-64" , 
                        "Same village" , "Reciprocity" ) ,
          Variable = factor( Variable , 
                             levels = c( "Edges" , ">=65" , "35-49" , "50-64" , 
                                         "Male" , "Male: >=65" , "Male: 35-49" , "Male: 50-64" , 
                                         "Same village" , "Reciprocity" ) ,
                             labels = c( "Edges" , ">=65" , "35-49" , "50-64" , 
                                         "Male" , "Male: >=65" , "Male: 35-49" , "Male: 50-64" , 
                                         "Same village" , "Reciprocity" ) ) ,
          Model = "Receiver's age across gender" ,
          p_signif = c( "***" , "***" , "" , "***" , "***" , "***" , "***" , "**" , "***" , "***" ) ) %>%
  mutate( "OR [CI]" = paste( format( round( OR , 2 ), nsmall = 2 ) ,
                             " [" ,
                             format( round( CI5 , 2 ), nsmall = 2 ) ,
                             ", " ,
                             format( round( CI95 , 2 ), nsmall = 2 ) ,
                             "]",
                             sep = " " , 
                             p_signif ) ) %>% 
  select( Variable , Model , Estimate , se , p.value , p_signif , OR , CI5 , CI95 , `OR [CI]` )

ReceiverAge.output %>% 
  mutate_at( c( "Estimate" , "se" ) , ~ round( . , 2 ) ) %>% 
  dplyr::rename( Coefficient = Estimate , 
                 `Standard error` = se ) %>% 
  mutate( "OR [CI]" = paste( format( round( OR , 2 ), nsmall = 2 ) ,
                             " [" ,
                             format( round( CI5 , 2 ), nsmall = 2 ) ,
                             ", " ,
                             format( round( CI95 , 2 ), nsmall = 2 ) ,
                             "]" ) ) %>% 
  mutate_at( c( "p.value" ) , ~ ifelse( . < 0.001 , "<0.001", round( . , 2 ) ) ) %>% 
  mutate( p_value = paste( p.value ,
                           sep = " " , 
                           p_signif ) ) %>% 
  select( Variable , Coefficient , `Standard error` , `OR [CI]` , p_value ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "center",
              columns = c( "Variable" , "Coefficient" , "Standard error" , "OR [CI]" , "p_value") ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 3 ) ,
               column_labels.border.bottom.color = "black" ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 3 ) ) %>% 
  cols_width( "Variable" ~ px( 170 ) , 
              everything() ~ px( 160 ) )

# Compute gender difference across age
Gdiff.Age.output <- tibble( Age = c( "<35" , "35-49" , "50-64" , ">=65" ) ) %>%
  mutate( Age = factor( Age , 
                        levels = c( "<35" , "35-49" , "50-64" , ">=65" ) , 
                        labels = c( "<35" , "35-49" , "50-64" , ">=65" ) ), 
          Est = case_when( Age == "<35" ~ ReceiverAge.output$Estimate[2] , 
                           Age == "35-49" ~ ReceiverAge.output$Estimate[2] + ReceiverAge.output$Estimate[7] , 
                           Age == "50-64" ~ ReceiverAge.output$Estimate[2] + ReceiverAge.output$Estimate[8] , 
                           TRUE ~ ReceiverAge.output$Estimate[2] + ReceiverAge.output$Estimate[6] ) ,
          se = case_when( Age == "<35" ~ ReceiverAge.output$se[2] , 
                          Age == "35-49" ~ sqrt( vcov(Fit.ReceiverAge)[2,2] + vcov(Fit.ReceiverAge)[7,7] + 2*vcov(Fit.ReceiverAge)[2,7] ) , 
                          Age == "50-64" ~ sqrt( vcov(Fit.ReceiverAge)[2,2] + vcov(Fit.ReceiverAge)[8,8] + 2*vcov(Fit.ReceiverAge)[2,8] ) , 
                          TRUE ~ sqrt( vcov(Fit.ReceiverAge)[2,2] + vcov(Fit.ReceiverAge)[6,6] + 2*vcov(Fit.ReceiverAge)[2,6] ) ) , 
          OR = exp( Est ) ,
          CI5 = exp( Est - 1.96*se ) , 
          CI95 = exp( Est + 1.96*se ) , 
          p_value = 2 * pnorm( -abs( Est/se ) ) ) %>% 
  mutate_at( c( "Est" , "se" , "OR" , "CI5" , "CI95" ) , ~ round( . , 2 ) ) %>% 
  mutate( p_signif = case_when( p_value < 0.001 ~ "***" , 
                                p_value < 0.01 ~ "**" , 
                                p_value < 0.05 ~ "*" , 
                                TRUE ~ "" ) ) %>% 
  mutate_at( c( "p_value" ) , ~ ifelse( . < 0.001 , "<0.001", round( . , 4 ) ) )

Gdiff.Age.output %>% 
  dplyr::rename( Coefficient = Est , 
                 `Standard error` = se ) %>% 
  mutate( "OR [CI]" = paste( format( round( OR , 2 ), nsmall = 2 ) ,
                             " [" ,
                             format( round( CI5 , 2 ), nsmall = 2 ) ,
                             ", " ,
                             format( round( CI95 , 2 ), nsmall = 2 ) ,
                             "]" ) ) %>% 
  mutate( p_value = paste( p_value ,
                           sep = " " , 
                           p_signif ) ) %>% 
  select( Age , Coefficient , `Standard error` , `OR [CI]` , p_value ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "center",
              columns = c( "Age" , "Coefficient" , "Standard error" , "OR [CI]" , "p_value") ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 3 ) ,
               column_labels.border.bottom.color = "black" ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 3 ) ) %>% 
  cols_width( everything() ~ px( 150 ) )

##4.2 Residence ----
# Model results 
ReceiverResi.output <- Fit.ReceiverResi %>% 
  broom::tidy( conf.int = TRUE ) %>% 
  select( term , estimate , std.error , conf.low , conf.high , p.value ) %>%
  dplyr::rename( Estimate = estimate ,
                 se = std.error ,
                 CI_Lower = conf.low,
                 CI_Upper = conf.high ) %>%
  mutate( OR = exp( Estimate ) , 
          CI5 = exp( CI_Lower ) ,  
          CI95 = exp( CI_Upper ) ,  
          Variable = c( "Edges" , "Receiver's age: >=65" , "Receiver's age: 35-49" , "Receiver's age: 50-64" ,
                        "Male" , "Bilocal" , "Matrilocal" , 
                        "Bilocal male" , "Matrilocal male" , 
                        "Same village" , "Reciprocity" ) ,
          Variable = factor( Variable , 
                             levels = c( "Edges" , "Receiver's age: >=65" , "Receiver's age: 35-49" , "Receiver's age: 50-64" ,
                                         "Male" , "Bilocal" , "Matrilocal" , 
                                         "Bilocal male" , "Matrilocal male" , 
                                         "Same village" , "Reciprocity" ) ,
                             labels = c( "Edges" , "Receiver's age: >=65" , "Receiver's age: 35-49" , "Receiver's age: 50-64" ,
                                         "Male" , "Bilocal" , "Matrilocal" , 
                                         "Bilocal male" , "Matrilocal male" , 
                                         "Same village" , "Reciprocity" ) ) ,
          Model = "Residence" ,
          p_signif = c( "***" , "" , "***" , "***" , "**" , "." , "*" , "" , "." , "***" , "***" ) ) %>%
  mutate( "OR [CI]" = paste( format( round( OR , 2 ), nsmall = 2 ) ,
                             " [" ,
                             format( round( CI5 , 2 ), nsmall = 2 ) ,
                             ", " ,
                             format( round( CI95 , 2 ), nsmall = 2 ) ,
                             "]",
                             sep = " " , 
                             p_signif ) ) %>% 
  select( Variable , Model , Estimate , se , p.value , p_signif , OR , CI5 , CI95 , `OR [CI]` )

ReceiverResi.output %>% 
  mutate_at( c( "Estimate" , "se" ) , ~ round( . , 2 ) ) %>% 
  dplyr::rename( Coefficient = Estimate , 
                 `Standard error` = se ) %>% 
  mutate( "OR [CI]" = paste( format( round( OR , 2 ), nsmall = 2 ) ,
                             " [" ,
                             format( round( CI5 , 2 ), nsmall = 2 ) ,
                             ", " ,
                             format( round( CI95 , 2 ), nsmall = 2 ) ,
                             "]" ) ) %>% 
  mutate_at( c( "p.value" ) , ~ ifelse( . < 0.001 , "<0.001", round( . , 2 ) ) ) %>% 
  mutate( p_value = paste( p.value ,
                           sep = " " , 
                           p_signif ) ) %>% 
  select( Variable , Coefficient , `Standard error` , `OR [CI]` , p_value ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "center",
              columns = c( "Variable" , "Coefficient" , "Standard error" , "OR [CI]" , "p_value") ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 3 ) ,
               column_labels.border.bottom.color = "black" ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 3 ) ) %>% 
  cols_width( "Variable" ~ px( 170 ) , 
              everything() ~ px( 160 ) )

# Compute gender difference across residence
Gdiff.Resi.output <- tibble( Residence = c( "Patrilocal" , "Bilocal", "Matrilocal" ) ) %>%
  mutate( Residence = factor( Residence , 
                              levels = c( "Patrilocal" , "Bilocal", "Matrilocal" ) , 
                              labels = c( "Patrilocal" , "Bilocal", "Matrilocal" ) ), 
          Est = case_when( Residence == "Patrilocal" ~ ReceiverResi.output$Estimate[5] , 
                           Residence == "Bilocal" ~ ReceiverResi.output$Estimate[5] + ReceiverResi.output$Estimate[8] , 
                           TRUE ~ ReceiverResi.output$Estimate[5] + ReceiverResi.output$Estimate[9] ) ,
          se = case_when( Residence == "Patrilocal" ~ ReceiverResi.output$se[5] , 
                          Residence == "Bilocal" ~ sqrt( vcov(Fit.ReceiverResi)[5,5] + vcov(Fit.ReceiverResi)[8,8] + 2*vcov(Fit.ReceiverResi)[5,8] ) , 
                          TRUE ~ sqrt( vcov(Fit.ReceiverResi)[5,5] + vcov(Fit.ReceiverResi)[9,9] + 2*vcov(Fit.ReceiverResi)[5,9] ) ) , 
          OR = exp( Est ) ,
          CI5 = exp( Est - 1.96*se ) , 
          CI95 = exp( Est + 1.96*se ) , 
          p_value = 2 * pnorm( -abs( Est/se ) ) ) %>% 
  mutate_at( c( "Est" , "se" , "OR" , "CI5" , "CI95" ) , ~ round( . , 2 ) ) %>% 
  mutate( p_signif = case_when( p_value < 0.001 ~ "***" , 
                                p_value < 0.01 ~ "**" , 
                                p_value < 0.05 ~ "*" , 
                                TRUE ~ "" ) ) %>% 
  mutate_at( c( "p_value" ) , ~ ifelse( . < 0.001 , "<0.001", round( . , 4 ) ) )

Gdiff.Resi.output %>% 
  dplyr::rename( Coefficient = Est , 
                 `Standard error` = se ) %>% 
  mutate( "OR [CI]" = paste( format( round( OR , 2 ), nsmall = 2 ) ,
                             " [" ,
                             format( round( CI5 , 2 ), nsmall = 2 ) ,
                             ", " ,
                             format( round( CI95 , 2 ), nsmall = 2 ) ,
                             "]" ) ) %>% 
  mutate( p_value = paste( p_value ,
                           sep = " " , 
                           p_signif ) ) %>% 
  select( Residence , Coefficient , `Standard error` , `OR [CI]` , p_value ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "center",
              columns = c( "Residence" , "Coefficient" , "Standard error" , "OR [CI]" , "p_value") ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 3 ) ,
               column_labels.border.bottom.color = "black" ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 3 ) ) %>% 
  cols_width( everything() ~ px( 150 ) )

##4.3 Kinship ----
# Model results 
ReceiverKin.output <- Fit.ReceiverKin %>% 
  broom::tidy( conf.int = TRUE ) %>% 
  select( term , estimate , std.error , conf.low , conf.high , p.value ) %>%
  dplyr::rename( Estimate = estimate ,
                 se = std.error ,
                 CI_Lower = conf.low,
                 CI_Upper = conf.high ) %>%
  mutate( OR = exp( Estimate ) , 
          CI5 = exp( CI_Lower ) ,  
          CI95 = exp( CI_Upper ) ,  
          Variable = c( "Edges" , "Bilocal" , "Matrilocal" , 
                        "Male" , "Non-unilineal" , "Patrilineal" , 
                        "Non-unilineal male" , "Patrilineal male" , 
                        "Same village" , "Reciprocity" ) ,
          Variable = factor( Variable , 
                             levels = c( "Edges" , "Bilocal" , "Matrilocal" , 
                                         "Male" , "Non-unilineal" , "Patrilineal" , 
                                         "Non-unilineal male" , "Patrilineal male" , 
                                         "Same village" , "Reciprocity" ) ,
                             labels = c( "Edges" , "Bilocal" , "Matrilocal" , 
                                         "Male" , "Non-unilineal" , "Patrilineal" , 
                                         "Non-unilineal male" , "Patrilineal male" , 
                                         "Same village" , "Reciprocity" ) ) ,
          Model = "Kinship system" ,
          p_signif = c( "***" , "***" , "" , "***" , "***" , "" , "" , "" , "***" , "***" ) ) %>%
  mutate( "OR [CI]" = paste( format( round( OR , 2 ), nsmall = 2 ) ,
                             " [" ,
                             format( round( CI5 , 2 ), nsmall = 2 ) ,
                             ", " ,
                             format( round( CI95 , 2 ), nsmall = 2 ) ,
                             "]",
                             sep = " " , 
                             p_signif ) ) %>% 
  select( Variable , Model , Estimate , se , p.value , p_signif , OR , CI5 , CI95 , `OR [CI]` )

ReceiverKin.output %>% 
  mutate_at( c( "Estimate" , "se" ) , ~ round( . , 2 ) ) %>% 
  dplyr::rename( Coefficient = Estimate , 
                 `Standard error` = se ) %>% 
  mutate( "OR [CI]" = paste( format( round( OR , 2 ), nsmall = 2 ) ,
                             " [" ,
                             format( round( CI5 , 2 ), nsmall = 2 ) ,
                             ", " ,
                             format( round( CI95 , 2 ), nsmall = 2 ) ,
                             "]" ) ) %>% 
  mutate_at( c( "p.value" ) , ~ ifelse( . < 0.001 , "<0.001", round( . , 2 ) ) ) %>% 
  mutate( p_value = paste( p.value ,
                           sep = " " , 
                           p_signif ) ) %>% 
  select( Variable , Coefficient , `Standard error` , `OR [CI]` , p_value ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "center",
              columns = c( "Variable" , "Coefficient" , "Standard error" , "OR [CI]" , "p_value") ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 3 ) ,
               column_labels.border.bottom.color = "black" ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 3 ) ) %>% 
  cols_width( "Variable" ~ px( 170 ) , 
              everything() ~ px( 160 ) )

# Compute gender difference across kinship system
Gdiff.Kin.output <- tibble( Kinship = c( "Matrilineal", "Non-unilineal" , "Patrilineal" ) ) %>%
  mutate( Kinship = factor( Kinship , 
                            levels = c( "Matrilineal", "Non-unilineal" , "Patrilineal" ) , 
                            labels = c( "Matrilineal", "Non-unilineal" , "Patrilineal" ) ), 
          Est = case_when( Kinship == "Matrilineal" ~ ReceiverKin.output$Estimate[4] , 
                           Kinship == "Non-unilineal" ~ ReceiverKin.output$Estimate[4] + ReceiverKin.output$Estimate[7] , 
                           TRUE ~ ReceiverKin.output$Estimate[4] + ReceiverKin.output$Estimate[8] ) ,
          se = case_when( Kinship == "Matrilineal" ~ ReceiverKin.output$se[4] , 
                          Kinship == "Non-unilineal" ~ sqrt( vcov(Fit.ReceiverKin)[4,4] + vcov(Fit.ReceiverKin)[7,7] + 2*vcov(Fit.ReceiverKin)[4,7] ) , 
                          TRUE ~ sqrt( vcov(Fit.ReceiverKin)[4,4] + vcov(Fit.ReceiverKin)[8,8] + 2*vcov(Fit.ReceiverKin)[4,8] ) ) , 
          OR = exp( Est ) ,
          CI5 = exp( Est - 1.96*se ) , 
          CI95 = exp( Est + 1.96*se ) , 
          p_value = 2 * pnorm( -abs( Est/se ) ) ) %>% 
  mutate_at( c( "Est" , "se" , "OR" , "CI5" , "CI95" ) , ~ round( . , 2 ) ) %>% 
  mutate( p_signif = case_when( p_value < 0.001 ~ "***" , 
                                p_value < 0.01 ~ "**" , 
                                p_value < 0.05 ~ "*" , 
                                TRUE ~ "" ) ) %>% 
  mutate_at( c( "p_value" ) , ~ ifelse( . < 0.001 , "<0.001", round( . , 4 ) ) )

Gdiff.Kin.output %>% 
  dplyr::rename( Coefficient = Est , 
                 `Standard error` = se ) %>% 
  mutate( "OR [CI]" = paste( format( round( OR , 2 ), nsmall = 2 ) ,
                             " [" ,
                             format( round( CI5 , 2 ), nsmall = 2 ) ,
                             ", " ,
                             format( round( CI95 , 2 ), nsmall = 2 ) ,
                             "]" ) ) %>% 
  mutate( p_value = paste( p_value ,
                           sep = " " , 
                           p_signif ) ) %>% 
  select( Kinship , Coefficient , `Standard error` , `OR [CI]` , p_value ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "center",
              columns = c( "Kinship" , "Coefficient" , "Standard error" , "OR [CI]" , "p_value") ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 3 ) ,
               column_labels.border.bottom.color = "black" ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 3 ) ) %>% 
  cols_width( everything() ~ px( 150 ) )

##4.4 Market ----
# Model results 
ReceiverMar.output <- Fit.ReceiverMar %>% 
  broom::tidy( conf.int = TRUE ) %>% 
  select( term , estimate , std.error , conf.low , conf.high , p.value ) %>%
  dplyr::rename( Estimate = estimate ,
                 se = std.error ,
                 CI_Lower = conf.low,
                 CI_Upper = conf.high ) %>%
  mutate( OR = exp( Estimate ) , 
          CI5 = exp( CI_Lower ) ,  
          CI95 = exp( CI_Upper ) ,  
          Variable = c( "Edges" , "Receiver's age: >=65" , "Receiver's age: 35-49" , "Receiver's age: 50-64" ,
                        "Bilocal" , "Matrilocal" , 
                        "Male" , "Market" , "Market:male" , 
                        "Same village" , "Reciprocity" ) ,
          Variable = factor( Variable , 
                             levels = c( "Edges" , "Receiver's age: >=65" , "Receiver's age: 35-49" , "Receiver's age: 50-64" ,
                                         "Bilocal" , "Matrilocal" , 
                                         "Male" , "Market" , "Market:male" , 
                                         "Same village" , "Reciprocity" ) ,
                             labels = c( "Edges" , "Receiver's age: >=65" , "Receiver's age: 35-49" , "Receiver's age: 50-64" ,
                                         "Bilocal" , "Matrilocal" , 
                                         "Male" , "Market" , "Market:male" , 
                                         "Same village" , "Reciprocity" ) ) ,
          Model = "Market participation" ,
          p_signif = c( "***" , "**" , "***" , "***" , "**" , "" , "***" , "***" , "" , "***" , "***" ) ) %>%
  mutate( "OR [CI]" = paste( format( round( OR , 2 ), nsmall = 2 ) ,
                             " [" ,
                             format( round( CI5 , 2 ), nsmall = 2 ) ,
                             ", " ,
                             format( round( CI95 , 2 ), nsmall = 2 ) ,
                             "]",
                             sep = " " , 
                             p_signif ) ) %>% 
  select( Variable , Model , Estimate , se , p.value , p_signif , OR , CI5 , CI95 , `OR [CI]` )

ReceiverMar.output %>% 
  mutate_at( c( "Estimate" , "se" ) , ~ round( . , 2 ) ) %>% 
  dplyr::rename( Coefficient = Estimate , 
                 `Standard error` = se ) %>% 
  mutate( "OR [CI]" = paste( format( round( OR , 2 ), nsmall = 2 ) ,
                             " [" ,
                             format( round( CI5 , 2 ), nsmall = 2 ) ,
                             ", " ,
                             format( round( CI95 , 2 ), nsmall = 2 ) ,
                             "]" ) ) %>% 
  mutate_at( c( "p.value" ) , ~ ifelse( . < 0.001 , "<0.001", round( . , 2 ) ) ) %>% 
  mutate( p_value = paste( p.value ,
                           sep = " " , 
                           p_signif ) ) %>% 
  select( Variable , Coefficient , `Standard error` , `OR [CI]` , p_value ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "center",
              columns = c( "Variable" , "Coefficient" , "Standard error" , "OR [CI]" , "p_value") ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 3 ) ,
               column_labels.border.bottom.color = "black" ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 3 ) ) %>% 
  cols_width( "Variable" ~ px( 170 ) , 
              everything() ~ px( 160 ) )

# Compute the gender-specific effects of market participation
Market.Gender.output <- tibble( Gender = c( "Female" , "Male" ) ) %>%
  mutate( Est = case_when( Gender == "Female" ~ ReceiverMar.output$Estimate[8] , 
                           TRUE ~ ReceiverMar.output$Estimate[8] + ReceiverMar.output$Estimate[9] )  ,
          se = case_when( Gender == "Female" ~ ReceiverMar.output$se[8] , 
                          TRUE ~ sqrt( vcov(Fit.ReceiverMar)[8,8] + vcov(Fit.ReceiverMar)[9,9] + 2*vcov(Fit.ReceiverMar)[8,9] ) ) ,
          OR = exp( Est ) ,
          CI5 = exp( Est - 1.96*se ) , 
          CI95 = exp( Est + 1.96*se ) , 
          p_value = 2 * pnorm( -abs( Est/se ) ) ) %>% 
  mutate_at( c( "Est" , "se" , "OR" , "CI5" , "CI95" ) , ~ round( . , 2 ) ) %>% 
  mutate( p_signif = case_when( p_value < 0.001 ~ "***" , 
                                p_value < 0.01 ~ "**" , 
                                p_value < 0.05 ~ "*" , 
                                TRUE ~ "" ) ) %>% 
  mutate_at( c( "p_value" ) , ~ ifelse( . < 0.001 , "<0.001", round( . , 4 ) ) )

Market.Gender.output %>% 
  dplyr::rename( Coefficient = Est , 
                 `Standard error` = se ) %>% 
  mutate( "OR [CI]" = paste( format( round( OR , 2 ), nsmall = 2 ) ,
                             " [" ,
                             format( round( CI5 , 2 ), nsmall = 2 ) ,
                             ", " ,
                             format( round( CI95 , 2 ), nsmall = 2 ) ,
                             "]" ) ) %>% 
  mutate( p_value = paste( p_value ,
                           sep = " " , 
                           p_signif ) ) %>% 
  select( Gender , Coefficient , `Standard error` , `OR [CI]` , p_value ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "center",
              columns = c( "Gender" , "Coefficient" , "Standard error" , "OR [CI]" , "p_value") ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 3 ) ,
               column_labels.border.bottom.color = "black" ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 3 ) ) %>% 
  cols_width( everything() ~ px( 150 ) )

##4.5 Reputation ----
# Model results 
ReceiverRep.output <- Fit.ReceiverRep %>% 
  broom::tidy( conf.int = TRUE ) %>% 
  select( term , estimate , std.error , conf.low , conf.high , p.value ) %>%
  dplyr::rename( Estimate = estimate ,
                 se = std.error ,
                 CI_Lower = conf.low,
                 CI_Upper = conf.high ) %>%
  mutate( OR = exp( Estimate ) , 
          CI5 = exp( CI_Lower ) ,  
          CI95 = exp( CI_Upper ) ,  
          Variable = c( "Edges" , "Receiver's age: >=65" , "Receiver's age: 35-49" , "Receiver's age: 50-64" ,
                        "Market" , "Bilocal" , "Matrilocal" , 
                        "Male" , "Reputation" , "Reputation:male" , 
                        "Same village" , "Reciprocity" ) ,
          Variable = factor( Variable , 
                             levels = c( "Edges" , "Receiver's age: >=65" , "Receiver's age: 35-49" , "Receiver's age: 50-64" ,
                                         "Market" , "Bilocal" , "Matrilocal" , 
                                         "Male" , "Reputation" , "Reputation:male" , 
                                         "Same village" , "Reciprocity" ) ,
                             labels = c( "Edges" , "Receiver's age: >=65" , "Receiver's age: 35-49" , "Receiver's age: 50-64" ,
                                         "Market" , "Bilocal" , "Matrilocal" , 
                                         "Male" , "Reputation" , "Reputation:male" , 
                                         "Same village" , "Reciprocity" ) ) ,
          Model = "Reputation" ,
          p_signif = c( "***" , "" , "***" , "***" , "***" , "" , "" , "***" , "***" , "***" , "***" , "***" ) ) %>%
  mutate( "OR [CI]" = paste( format( round( OR , 2 ), nsmall = 2 ) ,
                             " [" ,
                             format( round( CI5 , 2 ), nsmall = 2 ) ,
                             ", " ,
                             format( round( CI95 , 2 ), nsmall = 2 ) ,
                             "]",
                             sep = " " , 
                             p_signif ) ) %>% 
  select( Variable , Model , Estimate , se , p.value , p_signif , OR , CI5 , CI95 , `OR [CI]` )

ReceiverRep.output %>% 
  mutate_at( c( "Estimate" , "se" ) , ~ round( . , 2 ) ) %>% 
  dplyr::rename( Coefficient = Estimate , 
                 `Standard error` = se ) %>% 
  mutate( "OR [CI]" = paste( format( round( OR , 2 ), nsmall = 2 ) ,
                             " [" ,
                             format( round( CI5 , 2 ), nsmall = 2 ) ,
                             ", " ,
                             format( round( CI95 , 2 ), nsmall = 2 ) ,
                             "]" ) ) %>% 
  mutate_at( c( "p.value" ) , ~ ifelse( . < 0.001 , "<0.001", round( . , 2 ) ) ) %>% 
  mutate( p_value = paste( p.value ,
                           sep = " " , 
                           p_signif ) ) %>% 
  select( Variable , Coefficient , `Standard error` , `OR [CI]` , p_value ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "center",
              columns = c( "Variable" , "Coefficient" , "Standard error" , "OR [CI]" , "p_value") ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 3 ) ,
               column_labels.border.bottom.color = "black" ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 3 ) ) %>% 
  cols_width( "Variable" ~ px( 170 ) , 
              everything() ~ px( 160 ) )

# Compute the gender-specific effects of reputation
Rep.Gender.output <- tibble( Gender = c( "Female" , "Male" ) ) %>%
  mutate( Est = case_when( Gender == "Female" ~ ReceiverRep.output$Estimate[9] , 
                           TRUE ~ ReceiverRep.output$Estimate[9] + ReceiverRep.output$Estimate[10] )  ,
          se = case_when( Gender == "Female" ~ ReceiverRep.output$se[9] , 
                          TRUE ~ sqrt( vcov(Fit.ReceiverRep)[9,9] + vcov(Fit.ReceiverRep)[10,10] + 2*vcov(Fit.ReceiverRep)[9,10] ) ) ,
          OR = exp( Est ) ,
          CI5 = exp( Est - 1.96*se ) , 
          CI95 = exp( Est + 1.96*se ) , 
          p_value = 2 * pnorm( -abs( Est/se ) ) ) %>% 
  mutate_at( c( "Est" , "se" , "OR" , "CI5" , "CI95" ) , ~ round( . , 2 ) ) %>% 
  mutate( p_signif = case_when( p_value < 0.001 ~ "***" , 
                                p_value < 0.01 ~ "**" , 
                                p_value < 0.05 ~ "*" , 
                                TRUE ~ "" ) ) %>% 
  mutate_at( c( "p_value" ) , ~ ifelse( . < 0.001 , "<0.001", round( . , 4 ) ) )

Rep.Gender.output %>% 
  dplyr::rename( Coefficient = Est , 
                 `Standard error` = se ) %>% 
  mutate( "OR [CI]" = paste( format( round( OR , 2 ), nsmall = 2 ) ,
                             " [" ,
                             format( round( CI5 , 2 ), nsmall = 2 ) ,
                             ", " ,
                             format( round( CI95 , 2 ), nsmall = 2 ) ,
                             "]" ) ) %>% 
  mutate( p_value = paste( p_value ,
                           sep = " " , 
                           p_signif ) ) %>% 
  select( Gender , Coefficient , `Standard error` , `OR [CI]` , p_value ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "center",
              columns = c( "Gender" , "Coefficient" , "Standard error" , "OR [CI]" , "p_value") ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 3 ) ,
               column_labels.border.bottom.color = "black" ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 3 ) ) %>% 
  cols_width( everything() ~ px( 150 ) )

#5 Figures ----
##5.1 OR (gender-specific effects from models) ----
OR.genderandother.f <- 
  tibble( Variable = c( "<35: male\n(ref: female)" , 
                        "35-49: male\n(ref: female)" , 
                        "50-64: male\n(ref: female)" , 
                        ">=65: male\n(ref: female)" , 
                        "Patrilocal: male\n(ref: female)" , 
                        "Bilocal: male\n(ref: female)" , 
                        "Matrilocal: male\n(ref: female)" ,
                        "Matrilineal: male\n(ref: female)" , 
                        "Non-unilineal: male\n(ref: female)" , 
                        "Patrilineal: male\n(ref: female)" , 
                        "Market: female" , 
                        "Market: male" ,
                        "Reputation: female" , 
                        "Reputation: male" ) , 
          Model = c( "Model 1: gender * age cohort" , "Model 1: gender * age cohort" , 
                     "Model 1: gender * age cohort" , "Model 1: gender * age cohort" , 
                     "Model 2: gender * residence" , 
                     "Model 2: gender * residence" , 
                     "Model 2: gender * residence" ,
                     "Model 3: gender * kinship system" , 
                     "Model 3: gender * kinship system" , 
                     "Model 3: gender * kinship system" , 
                     "Model 4: gender * market participation" , 
                     "Model 4: gender * market participation" ,
                     "Model 5: gender * reputation" , 
                     "Model 5: gender * reputation" ) , 
          OR = c( Gdiff.Age.output$OR , 
                  Gdiff.Resi.output$OR , 
                  Gdiff.Kin.output$OR , 
                  Market.Gender.output$OR ,
                  Rep.Gender.output$OR ) , 
          CI5 = c( Gdiff.Age.output$CI5 , 
                   Gdiff.Resi.output$CI5 , 
                   Gdiff.Kin.output$CI5 , 
                   Market.Gender.output$CI5 , 
                   Rep.Gender.output$CI5 ) , 
          CI95 = c( Gdiff.Age.output$CI95 , 
                    Gdiff.Resi.output$CI95 ,
                    Gdiff.Kin.output$CI95 , 
                    Market.Gender.output$CI95 , 
                    Rep.Gender.output$CI95 ) , 
          p_value = c( Gdiff.Age.output$p_value ,
                       Gdiff.Resi.output$p_value , 
                       Gdiff.Kin.output$p_value , 
                       Market.Gender.output$p_value , 
                       Rep.Gender.output$p_value ) , 
          p_signif = c( Gdiff.Age.output$p_signif , 
                        Gdiff.Resi.output$p_signif , 
                        Gdiff.Kin.output$p_signif , 
                        Market.Gender.output$p_signif , 
                        Rep.Gender.output$p_signif ) ) %>% 
  mutate( Variable = factor( Variable ,
                             levels = c( "Reputation: male" , 
                                         "Reputation: female" , 
                                         "Market: male" , 
                                         "Market: female" , 
                                         "Matrilineal: male\n(ref: female)" , 
                                         "Non-unilineal: male\n(ref: female)" , 
                                         "Patrilineal: male\n(ref: female)" , 
                                         "Matrilocal: male\n(ref: female)" , 
                                         "Bilocal: male\n(ref: female)" , 
                                         "Patrilocal: male\n(ref: female)" , 
                                         ">=65: male\n(ref: female)" ,
                                         "50-64: male\n(ref: female)" , 
                                         "35-49: male\n(ref: female)" , 
                                         "<35: male\n(ref: female)" ) , 
                             labels = c( "Reputation: male" , 
                                         "Reputation: female" , 
                                         "Market: male" , 
                                         "Market: female" , 
                                         "Matrilineal: male\n(ref: female)" , 
                                         "Non-unilineal: male\n(ref: female)" , 
                                         "Patrilineal: male\n(ref: female)" , 
                                         "Matrilocal: male\n(ref: female)" , 
                                         "Bilocal: male\n(ref: female)" , 
                                         "Patrilocal: male\n(ref: female)" , 
                                         ">=65: male\n(ref: female)" ,
                                         "50-64: male\n(ref: female)" , 
                                         "35-49: male\n(ref: female)" , 
                                         "<35: male\n(ref: female)" ) ) , 
          Model = factor( Model ,
                          levels = c( "Model 1: gender * age cohort" , 
                                      "Model 2: gender * residence" , 
                                      "Model 3: gender * kinship system" ,
                                      "Model 4: gender * market participation" , 
                                      "Model 5: gender * reputation" ) , 
                          labels = c( "Model 1: gender * age cohort" , 
                                      "Model 2: gender * residence" , 
                                      "Model 3: gender * kinship system" , 
                                      "Model 4: gender * market participation" , 
                                      "Model 5: gender * reputation" ) ) ) %>% 
  ggplot( aes( x = OR , 
               y = Variable , 
               group = Model ,
               color = Model ,
               fill = Model ) ) +
  geom_point( position = position_dodge( width = 0.2 ) , 
              size = 2 ) +
  geom_errorbar( aes( xmin = CI5 , 
                      xmax = CI95 ) , 
                 position = position_dodge( width = 0.2 ) , 
                 linewidth = 1.0, 
                 width = 0.15 ) +
  geom_vline( xintercept = 1 , 
              linetype = "dashed" , 
              color = "black" , 
              linewidth = 0.6 ) + 
  labs( x = "Odds ratio" , 
        y = "" ,
        group = "Models" , 
        color = "Models" , 
        fill = "Models" ) + 
  scale_x_continuous( limits = c( 0 , 2 ) , breaks = seq( 0 , 2 , 0.5 ) ) +
  scale_color_manual( values = c( "#d45621" , "#dbb428" , "#84ba42" , "#682487" , "#4485c7" ) ) + 
  scale_fill_manual(  values = c( "#d45621" , "#dbb428" , "#84ba42" , "#682487" , "#4485c7" ) ) + 
  theme( strip.background = element_rect( color = "black" , fill = "white" ) ,
         strip.text.x = element_text( size = 16 , face = "bold" ) ,
         axis.line = element_line( colour = "black" ) ,
         panel.grid.major = element_line(colour = "Gainsboro") ,
         panel.grid.minor = element_blank() ,
         panel.border = element_rect(colour="black",fill=NA) ,
         panel.background = element_blank() ,
         plot.title = element_text( size=16 , face = "bold" , hjust = 0.5 ) ,
         legend.position = c(0.99,0.99) ,
         legend.justification = c(0.99,0.99) ,
         axis.title.x = element_text( size = 16 , face = "bold" ,
                                      margin = margin( t = 0.3 , r = 0 , b = 0 , l = 0 , unit = "cm" ) ) ,
         axis.text.x = element_text( colour = "black" , size = 14 , face = "bold" ,
                                     margin = margin( t = 0.1 , r = 0 , b = 0 , l = 0 , unit = "cm" ) ) ,
         axis.title.y = element_text( size = 16 , face = "bold" ,
                                      margin = margin( t = 0, r = 0.5, b = 0, l = 0 , unit = "cm" ) ) ,
         axis.text.y = element_text( colour = "black" , size=14 , face = "bold" ,
                                     margin = margin( t = 0 , r = 0.2 , b = 0 , l = 0 , unit = "cm" ) ) ,
         legend.title = element_text( size=12 , face = "bold" ) ,
         legend.text = element_text( size=10 , face = "bold" ) )

OR.genderandother.f

##5.2 Original data: gender differences across age cohorts ----
Sum_gender_age <- Ego.data.use %>% 
  ddply( c( "Gender" , "Age.c" ) , summarise ,
         N = length( ID ) ,
         mean = mean( No.being.nominated ) , 
         se = sd( No.being.nominated ) / sqrt( N ) , 
         Clow = mean - se , 
         Chigh = mean + se )

No.beingN.Age.G <- 
  ggplot( Sum_gender_age , 
          aes( x = Age.c , 
               y = mean , 
               group = Gender ,
               colour = Gender ) ) +
  geom_errorbar( aes( ymin = Clow , 
                      ymax = Chigh ) ,
                 linewidth = 1.0 , 
                 width = 0.10 , 
                 position = position_dodge( 0.1 ) ) +
  geom_point( size = 3 , position = position_dodge( 0.1 ) ) +
  geom_line( linewidth = 0.8 , position = position_dodge( 0.1 ) ) +
  labs( x = "Age cohorts" , 
        y = "Counts of being nominated" ,
        group = "Gender" ,
        colour = "Gender" ) +
  scale_x_discrete( breaks = c( "<35" , "35-49" , "50-64" , ">=65" ) ,
                    labels = c( "<35" , "35-49" , "50-64" , ">=65" ) ) +
  scale_y_continuous( limits = c( 0 , 5 ) , breaks = seq( 0 , 5 , 1 ) ) +
  scale_colour_manual( values = c( "#B8474D" , "#4E6691" ) ) +
  theme(strip.background = element_rect(color = "black", fill = "white"),
        strip.text.x = element_text(size = 16,face = "bold"),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(colour = "Gainsboro"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour="black",fill=NA),
        panel.background = element_blank(),
        legend.position = c(0.99,0.99),
        legend.justification = c(0.99,0.99),
        plot.title = element_text(size=18,face = "bold"),
        axis.title.x = element_text(size = 18,face = "bold",
                                    margin = margin(t = 0.3, r = 0, b = 0, l = 0,unit = "cm")),
        axis.text.x = element_text(colour="black",size=16,face = "bold",
                                   margin = margin(t = 0.1, r = 0, b = 0, l = 0,unit = "cm")),
        axis.title.y = element_text(size = 18,face = "bold",
                                    margin = margin(t = 0, r = 0.5, b = 0, l = 0,unit = "cm")),
        axis.text.y = element_text(colour="black",size=16,face = "bold",
                                   margin = margin(t = 0, r = 0.1, b = 0, l = 0,unit = "cm")),
        legend.title=element_text(size=14,face = "bold"),
        legend.text=element_text(size=12,face = "bold"))

No.beingN.Age.G

##5.3 Original data: gender differences across residence ----
Sum_gender_residence <- Ego.data.use %>% 
  ddply( c( "Gender" , "Residence" ) , summarise ,
         N = length( ID ) ,
         mean = mean( No.being.nominated ) , 
         se = sd( No.being.nominated ) / sqrt( N ) , 
         Clow = mean - se , 
         Chigh = mean + se )

No.beingN.Resi.G <- 
  ggplot( Sum_gender_residence , 
          aes( x = Residence , 
               y = mean , 
               group = Gender ,
               colour = Gender ) ) +
  geom_errorbar( aes( ymin = Clow , 
                      ymax = Chigh ) ,
                 linewidth = 1.0 , 
                 width = 0.10 , 
                 position = position_dodge( 0.1 ) ) +
  geom_point( size = 3 , position = position_dodge( 0.1 ) ) +
  geom_line( linewidth = 0.8 , position = position_dodge( 0.1 ) ) +
  labs( x = "Post-marital residence patterns" , 
        y = "Counts of being nominated" ,
        group = "Gender" ,
        colour = "Gender" ) +
  scale_x_discrete( breaks = c( "1Patrilocal" , "2Bilocal" , "3Matrilocal" ) ,
                    labels = c( "Patrilocal" , "Bilocal" , "Matrilocal" ) ) +
  scale_y_continuous( limits = c( 0 , 5 ) , breaks = seq( 0 , 5 , 1 ) ) +
  scale_colour_manual( values = c( "#B8474D" , "#4E6691" ) ) +
  theme(strip.background = element_rect(color = "black", fill = "white"),
        strip.text.x = element_text(size = 16,face = "bold"),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(colour = "Gainsboro"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour="black",fill=NA),
        panel.background = element_blank(),
        legend.position = c(0.99,0.99),
        legend.justification = c(0.99,0.99),
        plot.title = element_text(size=18,face = "bold"),
        axis.title.x = element_text(size = 18,face = "bold",
                                    margin = margin(t = 0.3, r = 0, b = 0, l = 0,unit = "cm")),
        axis.text.x = element_text(colour="black",size=16,face = "bold",
                                   margin = margin(t = 0.1, r = 0, b = 0, l = 0,unit = "cm")),
        axis.title.y = element_text(size = 18,face = "bold",
                                    margin = margin(t = 0, r = 0.5, b = 0, l = 0,unit = "cm")),
        axis.text.y = element_text(colour="black",size=16,face = "bold",
                                   margin = margin(t = 0, r = 0.1, b = 0, l = 0,unit = "cm")),
        legend.title=element_text(size=14,face = "bold"),
        legend.text=element_text(size=12,face = "bold"))

No.beingN.Resi.G

##5.4 Original data: gender differences across kinship ----
Sum_gender_kinship <- Ego.data.use %>% 
  ddply( c( "Gender" , "Kinship" ) , summarise ,
         N = length( ID ) ,
         mean = mean( No.being.nominated ) , 
         se = sd( No.being.nominated ) / sqrt( N ) , 
         Clow = mean - se , 
         Chigh = mean + se )

No.beingN.Kin.G <- 
  ggplot( Sum_gender_kinship , 
          aes( x = Kinship , 
               y = mean , 
               group = Gender ,
               colour = Gender ) ) +
  geom_errorbar( aes( ymin = Clow , 
                      ymax = Chigh ) ,
                 linewidth = 1.0 , 
                 width = 0.10 , 
                 position = position_dodge( 0.1 ) ) +
  geom_point( size = 3 , position = position_dodge( 0.1 ) ) +
  geom_line( linewidth = 0.8 , position = position_dodge( 0.1 ) ) +
  labs( x = "Kinship systems" , 
        y = "Counts of being nominated" ,
        group = "Gender" ,
        colour = "Gender" ) +
  scale_x_discrete( breaks = c( "Matrilineal" , "Non-unilineal" , "Patrilineal" ) ,
                    labels = c( "Matrilineal" , "Non-unilineal" , "Patrilineal" ) ) +
  scale_y_continuous( limits = c( 0 , 5 ) , breaks = seq( 0 , 5 , 1 ) ) +
  scale_colour_manual( values = c( "#B8474D" , "#4E6691" ) ) +
  theme(strip.background = element_rect(color = "black", fill = "white"),
        strip.text.x = element_text(size = 16,face = "bold"),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(colour = "Gainsboro"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour="black",fill=NA),
        panel.background = element_blank(),
        legend.position = c(0.99,0.99),
        legend.justification = c(0.99,0.99),
        plot.title = element_text(size=18,face = "bold"),
        axis.title.x = element_text(size = 18,face = "bold",
                                    margin = margin(t = 0.3, r = 0, b = 0, l = 0,unit = "cm")),
        axis.text.x = element_text(colour="black",size=16,face = "bold",
                                   margin = margin(t = 0.1, r = 0, b = 0, l = 0,unit = "cm")),
        axis.title.y = element_text(size = 18,face = "bold",
                                    margin = margin(t = 0, r = 0.5, b = 0, l = 0,unit = "cm")),
        axis.text.y = element_text(colour="black",size=16,face = "bold",
                                   margin = margin(t = 0, r = 0.1, b = 0, l = 0,unit = "cm")),
        legend.title=element_text(size=14,face = "bold"),
        legend.text=element_text(size=12,face = "bold"))

No.beingN.Kin.G

##5.5 Prediction: market participation and gender ----
# avg_offset = intercept + age_avg_effect + residence_avg_effect + Same_village_avg_effect + Reciprocity_avg_effect
Predicted_mar_gender <- tibble(
  Market = rep( seq( -2.4 , 2.0 , by = 0.1 ) , 2 ) ,
  Gender = rep( c( "Female" , "Male" ) , each = 45 ) ) %>%
  mutate(
    avg_offset = ReceiverMar.output$Estimate[1] + 
      mean( c( 0 , ReceiverMar.output$Estimate[2:4] ) ) + 
      mean( c( 0 , ReceiverMar.output$Estimate[5:6] ) ) + 
      mean( c( 0 , ReceiverMar.output$Estimate[10] ) ) + 
      mean( c( 0 , ReceiverMar.output$Estimate[11] ) ) ,
    Market_Effect = ifelse( Gender == "Female" ,
                            ReceiverMar.output$Estimate[8] ,
                            ReceiverMar.output$Estimate[8] + ReceiverMar.output$Estimate[9] ) ,
    Gender_Effect = ifelse( Gender == "Female" , 0 , ReceiverMar.output$Estimate[7] ) ,
    
    Linear_Predictor = avg_offset + Gender_Effect + Market_Effect * Market ,
    Probability = plogis( Linear_Predictor ) ) %>% 
  mutate( Gender = factor( Gender , 
                           levels = c( "Female" , "Male" ) , 
                           labels = c("Female" , "Male" ) ) )

Figure_predicted_mar_gender <- 
  ggplot( Predicted_mar_gender , 
          aes( x = Market , 
               y = Probability , 
               group = Gender ,
               colour = Gender ) ) +
  geom_point( size = 3 ) +
  geom_line( linewidth = 1.2 ) +
  labs( x = "Market participation (std)" , 
        y = "Predicted probability of being nominated" ,
        group = "Gender" ,
        colour = "Gender" ) +
  scale_x_continuous( limits = c( -2.4 , 2.0 ) , breaks = c( -2 , -1 , 0 , 1 , 2 ) ) +
  scale_y_continuous( limits = c( 0.01 , 0.03 ) , breaks = seq( 0.01 , 0.03 , 0.005 ) ) +
  scale_colour_manual( values = c( "#B8474D" , "#4E6691" ) ) +
  theme(strip.background = element_rect(color = "black", fill = "white"),
        strip.text.x = element_text(size = 16,face = "bold"),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(colour = "Gainsboro"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour="black",fill=NA),
        panel.background = element_blank(),
        legend.position = c(0.01,0.99),
        legend.justification = c(0.01,0.99),
        plot.title = element_text(size=20,face = "bold"),
        axis.title.x = element_text(size = 20,face = "bold",
                                    margin = margin(t = 0.3, r = 0, b = 0, l = 0,unit = "cm")),
        axis.text.x = element_text(colour="black",size=18,face = "bold",
                                   margin = margin(t = 0.1, r = 0, b = 0, l = 0,unit = "cm")),
        axis.title.y = element_text(size = 20,face = "bold",
                                    margin = margin(t = 0, r = 0.5, b = 0, l = 0,unit = "cm")),
        axis.text.y = element_text(colour="black",size=18,face = "bold",
                                   margin = margin(t = 0, r = 0.1, b = 0, l = 0,unit = "cm")),
        legend.title=element_text(size=16,face = "bold"),
        legend.text=element_text(size=14,face = "bold"))

Figure_predicted_mar_gender

##5.6 Prediction: reputation and gender ----
# avg_offset = intercept + age_avg_effect + market_avg_effect + residence_avg_effect + Same_village_avg_effect + Reciprocity_avg_effect
# market participation is standardised; mean assumed to be 0, effect = 0
Predicted_rep_gender <- tibble(
  Reputation = rep( seq( -1.7 , 2.2 , by = 0.1 ) , 2 ) ,
  Gender = rep( c( "Female" , "Male" ) , each = 40 ) ) %>%
  mutate(
    avg_offset = ReceiverRep.output$Estimate[1] + 
      mean( c( 0 , ReceiverRep.output$Estimate[2:4] ) ) + 
      mean( c( 0 , ReceiverRep.output$Estimate[6:7] ) ) + 
      mean( c( 0 , ReceiverRep.output$Estimate[11] ) ) + 
      mean( c( 0 , ReceiverRep.output$Estimate[12] ) ) ,
    Reputation_Effect = ifelse( Gender == "Female" ,
                                ReceiverRep.output$Estimate[9] ,
                                ReceiverRep.output$Estimate[9] + ReceiverRep.output$Estimate[10] ) ,
    Gender_Effect = ifelse( Gender == "Female" , 0 , ReceiverRep.output$Estimate[8] ) ,
    
    Linear_Predictor = avg_offset + Gender_Effect + Reputation_Effect * Reputation ,
    Probability = plogis( Linear_Predictor ) ) %>% 
  mutate( Gender = factor( Gender , 
                           levels = c( "Female" , "Male" ) , 
                           labels = c("Female" , "Male" ) ) )

Figure_predicted_rep_gender <- 
  ggplot( Predicted_rep_gender , 
          aes( x = Reputation , 
               y = Probability , 
               group = Gender ,
               colour = Gender ) ) +
  geom_point( size = 3 ) +
  geom_line( linewidth = 1.2 ) +
  labs( x = "Reputation (std)" , 
        y = "Predicted probability of being nominated" ,
        group = "Gender" ,
        colour = "Gender" ) +
  scale_x_continuous( limits = c( -1.7 , 2.2 ) , breaks = c( -1 , 0 , 1 , 2 ) ) +
  scale_y_continuous( limits = c( 0 , 0.055 ) , breaks = seq( 0 , 0.055 , 0.005 ) ) +
  scale_colour_manual( values = c( "#B8474D" , "#4E6691" ) ) +
  theme(strip.background = element_rect(color = "black", fill = "white"),
        strip.text.x = element_text(size = 16,face = "bold"),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(colour = "Gainsboro"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour="black",fill=NA),
        panel.background = element_blank(),
        legend.position = c(0.01,0.99),
        legend.justification = c(0.01,0.99),
        plot.title = element_text(size=20,face = "bold"),
        axis.title.x = element_text(size = 20,face = "bold",
                                    margin = margin(t = 0.3, r = 0, b = 0, l = 0,unit = "cm")),
        axis.text.x = element_text(colour="black",size=18,face = "bold",
                                   margin = margin(t = 0.1, r = 0, b = 0, l = 0,unit = "cm")),
        axis.title.y = element_text(size = 20,face = "bold",
                                    margin = margin(t = 0, r = 0.5, b = 0, l = 0,unit = "cm")),
        axis.text.y = element_text(colour="black",size=18,face = "bold",
                                   margin = margin(t = 0, r = 0.1, b = 0, l = 0,unit = "cm")),
        legend.title=element_text(size=16,face = "bold"),
        legend.text=element_text(size=14,face = "bold"))

Figure_predicted_rep_gender

##5.7 Gender-based variance in number of times nominated across ages ----
Variance_gender_age <- 
  Ego.data.use %>% 
  ggplot() +
  geom_jitter( aes( x = Age.c,
                    y = No.being.nominated.jitter ,
                    colour = Gender ) ,
               position = position_jitterdodge( jitter.width = 0.55  , 
                                                dodge.width = 0.6 ) , 
               alpha = 0.4 ) +
  labs( x = "Age" ,
        y = "Counts of being nominated" , 
        color = "" ) +
  scale_y_continuous( limits = c( -0.5 , 35 ) , breaks = c( 0 , 10 , 20 , 30 ) ) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(colour = "Gainsboro"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour="black",fill=NA),
        panel.background = element_blank(),
        plot.title = element_text(size=18,face = "bold"),
        legend.margin = margin(b = -5),
        legend.position = "top" ,
        legend.justification = "center" ,
        legend.box = "horizontal" ,
        axis.title.x = element_text(size = 16,face = "bold",
                                    margin = margin(t = 0.3, r = 0, b = 0, l = 0,unit = "cm")),
        axis.text.x = element_text(colour="black",size=12,face = "bold",
                                   margin = margin(t = 0.1, r = 0, b = 0, l = 0,unit = "cm")),
        axis.title.y = element_text(size = 16,face = "bold",
                                    margin = margin(t = 0, r = 0.5, b = 0, l = 0,unit = "cm")),
        axis.text.y = element_text(colour="black",size=12,face = "bold",
                                   margin = margin(t = 0, r = 0.1, b = 0, l = 0,unit = "cm")),
        legend.title=element_text(size=12,face = "bold"),
        legend.text=element_text(size=12,face = "bold") )

Variance_gender_age

##5.8 Gender-based variance in number of times nominated across residence ----
Variance_gender_residence <- 
  Ego.data.use %>% 
  ggplot() +
  geom_jitter( data = Ego.data.use ,
               aes( x = Residence,
                    y = No.being.nominated.jitter ,
                    colour = Gender ) ,
               position = position_jitterdodge( jitter.width = 0.55  , 
                                                dodge.width = 0.6 ) , 
               alpha = 0.4 ) +
  labs( x = "Post-marital residence pattern" ,
        y = "Counts of being nominated" , 
        color = "" ) +
  scale_x_discrete( breaks = c( "1Patrilocal" , "2Bilocal" , "3Matrilocal" ) ,
                    labels = c( "Patrilocal" , "Bilocal" , "Matrilocal" ) ) +
  scale_y_continuous( limits = c( -0.65 , 35 ) , breaks = c( 0 , 10 , 20 , 30 ) ) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(colour = "Gainsboro"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour="black",fill=NA),
        panel.background = element_blank(),
        plot.title = element_text(size=18,face = "bold"),
        legend.margin = margin(b = -5),
        legend.position = "top" ,
        legend.justification = "center" ,
        legend.box = "horizontal" ,
        axis.title.x = element_text(size = 16,face = "bold",
                                    margin = margin(t = 0.3, r = 0, b = 0, l = 0,unit = "cm")),
        axis.text.x = element_text(colour="black",size=12,face = "bold",
                                   margin = margin(t = 0.1, r = 0, b = 0, l = 0,unit = "cm")),
        axis.title.y = element_text(size = 16,face = "bold",
                                    margin = margin(t = 0, r = 0.5, b = 0, l = 0,unit = "cm")),
        axis.text.y = element_text(colour="black",size=12,face = "bold",
                                   margin = margin(t = 0, r = 0.1, b = 0, l = 0,unit = "cm")),
        legend.title=element_text(size=12,face = "bold"),
        legend.text=element_text(size=12,face = "bold") )

Variance_gender_residence

##5.9 Gender-based variance in number of times nominated across kinships ----
Variance_gender_kinship <- 
  Ego.data.use %>% 
  ggplot() +
  geom_jitter( data = Ego.data.use ,
               aes( x = Kinship,
                    y = No.being.nominated.jitter ,
                    colour = Gender ) ,
               position = position_jitterdodge( jitter.width = 0.55  , 
                                                dodge.width = 0.6 ) , 
               alpha = 0.4 ) +
  labs( x = "Kinship system" ,
        y = "Counts of being nominated" , 
        color = "" ) +
  scale_x_discrete( breaks = c( "Patrilineal" , "Non-unilineal" , "Matrilineal" ) ,
                    labels = c( "Patrilineal" , "Non-unilineal" , "Matrilineal" ) ) +
  scale_y_continuous( limits = c( -0.90 , 35 ) , breaks = c( 0 , 10 , 20 , 30 ) ) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(colour = "Gainsboro"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour="black",fill=NA),
        panel.background = element_blank(),
        plot.title = element_text(size=18,face = "bold"),
        legend.margin = margin(b = -5),
        legend.position = "top" ,
        legend.justification = "center" ,
        legend.box = "horizontal" ,
        axis.title.x = element_text(size = 16,face = "bold",
                                    margin = margin(t = 0.3, r = 0, b = 0, l = 0,unit = "cm")),
        axis.text.x = element_text(colour="black",size=12,face = "bold",
                                   margin = margin(t = 0.1, r = 0, b = 0, l = 0,unit = "cm")),
        axis.title.y = element_text(size = 16,face = "bold",
                                    margin = margin(t = 0, r = 0.5, b = 0, l = 0,unit = "cm")),
        axis.text.y = element_text(colour="black",size=12,face = "bold",
                                   margin = margin(t = 0, r = 0.1, b = 0, l = 0,unit = "cm")),
        legend.title=element_text(size=12,face = "bold"),
        legend.text=element_text(size=12,face = "bold") )

Variance_gender_kinship

##5.10 Social networks ----
Igraph <- igraph::graph_from_data_frame(
  d = Ties,
  vertices = Nodes,
  directed = T) 

Igraph.colors <- c( "#A6CEE3" , "#3385BB" , "#84BF96" , "#6DBD57" , "#7F9D55" , 
                    "#F57C7C" , "#E42622" , "#FBB268"  , "#FE8D19" , "#DE9E83" , 
                    "#C6C3E1" , "#977899" ,"#F3E587" , "#B15928" )

Igraph.node.colors14 <- 
  ifelse( V(Igraph)$VID == "V1" , "#A6CEE3" ,
          ifelse( V(Igraph)$VID == "V2" , "#3385BB" ,
                  ifelse( V(Igraph)$VID == "V3" , "#84BF96" , 
                          ifelse( V(Igraph)$VID == "V4" , "#6DBD57" ,
                                  ifelse( V(Igraph)$VID == "V5" , "#7F9D55" , 
                                          ifelse( V(Igraph)$VID == "V6" , "#F57C7C" , 
                                                  ifelse( V(Igraph)$VID == "V7" , "#E42622" , 
                                                          ifelse( V(Igraph)$VID == "V8" , "#FBB268" , 
                                                                  ifelse( V(Igraph)$VID == "V9" , "#FE8D19" , 
                                                                          ifelse( V(Igraph)$VID == "V10" , "#DE9E83" , 
                                                                                  ifelse( V(Igraph)$VID == "V11" , "#C6C3E1" , 
                                                                                          ifelse( V(Igraph)$VID == "V12" , "#977899" , 
                                                                                                  ifelse( V(Igraph)$VID == "V13" , "#F3E587" , "#B15928" ) ) ) ) ) ) ) ) ) ) ) ) ) %>% 
  adjustcolor( alpha = 0.85 )

Igraph.edge.colors14 <- 
  ifelse( E(Igraph)$Alter.VID == "V1" , "#A6CEE3" ,
          ifelse( E(Igraph)$Alter.VID == "V2" , "#3385BB" ,
                  ifelse( E(Igraph)$Alter.VID == "V3" , "#84BF96" ,  
                          ifelse( E(Igraph)$Alter.VID == "V4" , "#6DBD57" ,
                                  ifelse( E(Igraph)$Alter.VID == "V5" , "#7F9D55" , 
                                          ifelse( E(Igraph)$Alter.VID == "V6" , "#F57C7C" , 
                                                  ifelse( E(Igraph)$Alter.VID == "V7" , "#E42622" , 
                                                          ifelse( E(Igraph)$Alter.VID == "V8" , "#FBB268"  , 
                                                                  ifelse( E(Igraph)$Alter.VID == "V9" , "#FE8D19" ,
                                                                          ifelse( E(Igraph)$Alter.VID == "V10" , "#DE9E83" , 
                                                                                  ifelse( E(Igraph)$Alter.VID == "V11" , "#C6C3E1" , 
                                                                                          ifelse( E(Igraph)$Alter.VID == "V12" , "#977899" ,
                                                                                                  ifelse( E(Igraph)$Alter.VID == "V13" , "#F3E587" , "#B15928" ) ) ) ) ) ) ) ) ) ) ) ) )

Igraph.shape <- ifelse( V( Igraph )$Gender == "Female" , "circle" , "square" )
Igraph.deg <- igraph::degree( Igraph , mode = "in" )
Igraph.re.deg <- datawizard::rescale( Igraph.deg , to = c( 1.5 , 5 ) )

Igraph.layout <- layout_with_fr( Igraph , 
                                 niter = 50000 , 
                                 repulserad = vcount( Igraph )^12 , 
                                 grid = "nogrid" )

par( oma = c( 0 , 0 , 0 , 0 ) , mar = c( 0, 0, 0, 0 ) )
set.seed( 123 )
plot.igraph( Igraph ,
             vertex.color = Igraph.node.colors14 ,
             vertex.frame.color = adjustcolor( "black" , alpha = 0.6 ) ,
             vertex.frame.width = 0.05 ,
             vertex.size = Igraph.re.deg ,
             vertex.shape = Igraph.shape, 
             edge.arrow.size = 0.15 ,
             edge.width = 0.6 ,
             edge.color = Igraph.edge.colors14 ,
             vertex.label = NA ,
             layout = Igraph.layout ,
             edge.curved = T , 
             vertex.frame.color = NA )
dev.off()

##5.11 Sdudy site ----
Locations <- data.frame( Village = c( "V1" , "V2" , "V3" , "V4" , "V5" ,
                                      "V6" , "V7" , "V8" , "V9" , "V10" ,
                                      "V11" , "V12" , "V13" , "V14" ) ,
                         Lat = c( 27.543056 , 27.557222 , 27.548889 , 27.533333 , 27.527500 , 
                                  27.521944 , 27.508611 , 27.499444 , 27.490556 , 27.480000 , 
                                  27.584722 , 27.531944 , 27.511944 , 27.501944 ) ,
                         Lon = c( 99.815278 , 99.803056 , 99.808611 , 99.829167 , 99.821111 , 
                                  99.844444 , 99.853333 , 99.863333 , 99.868056 , 99.857500 , 
                                  99.848611 , 99.876111 , 99.886667 , 99.889167 ) , 
                         colors = c( "#A6CEE3" , "#3385BB" , "#84BF96" , "#6DBD57" , "#7F9D55" , 
                                     "#F57C7C" , "#E42622" , "#FBB268"  , "#FE8D19" , "#DE9E83" , 
                                     "#C6C3E1" , "#977899" ,"#F3E587" , "#B15928" ) ) %>% 
  mutate( Village = factor( Village ,
                            levels = c( "V1" , "V2" , "V3" , "V4" , "V5" ,
                                        "V6" , "V7" , "V8" , "V9" , "V10" ,
                                        "V11" , "V12" , "V13" , "V14" ) ,
                            labels = c( "V1" , "V2" , "V3" , "V4" , "V5" ,
                                        "V6" , "V7" , "V8" , "V9" , "V10" ,
                                        "V11" , "V12" , "V13" , "V14" ) ) )

Locations.coordinates <- st_as_sf( Locations , coords = c( "Lon" ,"Lat" ) , crs = 4326 )
Locations.elev.raster <- get_elev_raster( locations = Locations.coordinates , z = 10 )
Locations.elev.df <- as.data.frame( rasterToPoints( Locations.elev.raster ) )
colnames( Locations.elev.df ) <- c( "Lon" , "Lat" ,"Elevation" )

Locations.x <- range( Locations$Lon ) + c(-0.005, 0.005)
Locations.y <- range( Locations$Lat ) + c(-0.005, 0.005)

Locations.bbox <- c( left = Locations.x[1] , bottom = Locations.y[1] ,
                     right = Locations.x[2] , top = Locations.y[2] )

Study.site <- 
  ggplot( ) +
  geom_contour( data = Locations.elev.df , 
                aes( x = Lon , y = Lat , z = Elevation ) ,
                color = "black" , alpha = 0.3 ) +  
  geom_point( data = Locations , 
              aes( x = Lon , y = Lat , color = Village ) , size = 4 ) + 
  scale_color_manual( values = Locations$colors ) +  
  geom_text_repel( data = Locations ,
                   aes( x = Lon , y = Lat , label = Village ) ,
                   color = Locations$colors ,
                   size = 5,
                   nudge_x = -0.003 , nudge_y = -0.003 , 
                   force = 10 ,
                   max.overlaps = Inf ,
                   segment.size = 0.5 ) + 
  geom_point( aes( x = 99.8114604 , y = 27.5593280 ) ,
              shape = 8 , color = "red" , size = 1.5 , stroke = 1 ) +
  geom_text_repel( data = data.frame( Lon = 99.8114604 , Lat = 27.5593280 , Label = "Markets" ) ,
                   aes( x = Lon , y = Lat , label = Label ) ,
                   color = "red" , size = 4 , fontface = "bold" ,
                   nudge_x = 0.003, segment.size = 0.5 ) + 
  coord_fixed( xlim = c( 99.78 , 99.91 ) , ylim = c( 27.45 , 27.60 ) , expand = FALSE ) +
  labs(x = "Longitude (°N)" , y = "Latitude (°E)" , color = "Villages") +
  scale_x_continuous( breaks = c( 99.80 , 99.85 , 99.90 ) , 
                      expand = c(0,0) ) +
  scale_y_continuous( breaks = c( 27.45 , 27.50 , 27.55 , 27.60 ) , 
                      expand = c(0,0) ) +
  theme( plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "pt") , 
         axis.line = element_line(colour = "black"),
         panel.grid.major = element_blank() ,
         panel.grid.minor = element_blank() ,
         panel.background = element_blank(),
         legend.position = "left" ,
         axis.title.x = element_text(size = 16,face = "bold",
                                     margin = margin(t = 0.2, r = 0, b = 0, l = 0,unit = "cm")),
         axis.text.x = element_text(colour = "black",size = 12,face = "bold",
                                    margin = margin(t = 0.1, r = 0, b = 0, l = 0,unit = "cm")),
         axis.title.y = element_text(size = 16,face = "bold",
                                     margin = margin(t = 0, r = 0.2, b = 0, l = 0,unit = "cm")),
         axis.text.y = element_text(colour = "black",size = 12,face = "bold",
                                    margin = margin(t = 0, r = 0.1, b = 0, l = 0,unit = "cm")),
         legend.title = element_text(size = 12,face = "bold"),
         legend.text = element_text(size = 12,face = "bold")) 

Study.site

