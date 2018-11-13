rm(list=ls())
functions <- list()
project_location <- paste0("/AnalysisProjects/DraftKings/")
working_directory <- paste0(file.path(Sys.getenv("USERPROFILE"),"Documents"),project_location)
setwd(working_directory)
print(getwd())

source('./RScripts/1 - DataWrangling/player_functions.r')

dk.14 <- functions$dk_df(2014)
player_df.14 <- functions$player_df(dk.14,'10-26',2014)

player_pos_14 <- functions$player_position_split(player_df.14)
player_qb_14 <- player_pos_14$QB
player_rb_14 <- player_pos_14$RB
player_wr_14 <- player_pos_14$WR
player_te_14 <- player_pos_14$TE
player_def_14 <- player_pos_14$Def


