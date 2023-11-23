library(natverse)
library(tidyverse)
library(fafbseg)
library(googlesheets4)
library(googledrive)
library(gargle)
library(dplyr)
library(glue)

##
choose_segmentation("flywire")


# some functions -----------------------------------------------------------

# function that takes segment(s) as input and returns flywire URL
flywire_url_func <- function(segment_id){
  fw_url=with_segmentation('flywire', getOption('fafbseg.sampleurl'))
  ngl_segments(fw_url)
  fw_sc=ngl_decode_scene(fw_url)
  fw_sc$layers[[2]]$segments= segment_id
  ngl_encode_url(fw_sc)
}

# function for connectivity tables with named partners 
flywire_partner_named <- function(segment_id, partners = "output", summary = FALSE){
  flywire_LUT = read_sheet("1z0qGbSRsQWjw3teznyHn2zQBYX8amum79V1_vva_iSE")
  segment_id = flywire_latestid(segment_id)
  con_table <- flywire_partner_summary(segment_id, partners = partners)
  con_table$fraction = round(con_table$weight/sum(con_table$weight), 3)
  con_table$flywire_URL = ""
  # for (i in seq_along(con_table$weight)){
  #   con_table$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(segment_id, as.character(con_table[i,1])))}","klickable_link")')
  # } 
  
  if (partners == "output"){
    con_table =  con_table %>% 
      left_join(flywire_LUT, by = c("post_id" = "seg_id" ))
  }
  
  if (partners == "input"){
    con_table = con_table %>% 
      left_join(flywire_LUT, by = c("pre_id" = "seg_id" ))
  }
  
  # con_table = con_table %>%
  #   select(contains("id"), weight, fraction, type, hemisphere, lab, author, name, flywire_URL.x) %>%
  #   rename(flywire_URL = flywire_URL.x)
  
  con_table = con_table %>%
    select(contains("id"), weight, fraction, type, hemisphere, lab, author, name)
  
  if(summary == FALSE){
    return(con_table)
  }
  
  if(summary == TRUE){
    message("collapsing connectivity table by 'type'")
    con_table_sum = con_table %>% 
      group_by(type) %>% 
      summarise(n = n (), weight_sum = sum(weight)) %>% 
      arrange(desc(weight_sum))
    con_table_sum$fraction = round(con_table_sum$weight_sum/sum(con_table_sum$weight_sum), 3)
    return(con_table_sum)
  }
  
}

# function to subset the flywire_neuron_name collection and optinally open a flywire link
flywire_neuron_type <- function(id, hsphere = c("L","R"), open = FALSE){
  flywire_LUT = read_sheet("1z0qGbSRsQWjw3teznyHn2zQBYX8amum79V1_vva_iSE")
  table = flywire_LUT %>% 
    filter(type == id, hemisphere == hsphere[1] | hemisphere == hsphere[2])
  if (open == TRUE){
    table = table %>% select(seg_id)
    message("opening flywire in browser")
    browseURL(flywire_url_func(as.character(table$seg_id)))
  }
  if (open == FALSE){
    return(table)
  }
}


# flywire neuron names ----------------------------------------------------
# reading the google sheet "flywire_neruon_names". adding / updating for new 
# entries: voxels in nm, name, the latest segment ID & flywire URL. removing 
# duplicates and arranging by type. Then pushing back to google sheets. 

#### RETIRED  flywire_neuron_names-table update code 

# flywire_neuron_name = read_sheet("1z0qGbSRsQWjw3teznyHn2zQBYX8amum79V1_vva_iSE")
# 
# start_time <- Sys.time()
# for (i in 1:length(flywire_neuron_name$type)){
#   flywire_neuron_name$voxel_nm_x[i] = flywire_neuron_name$voxel_raw_x[i]*4
#   flywire_neuron_name$voxel_nm_y[i] = flywire_neuron_name$voxel_raw_y[i]*4
#   flywire_neuron_name$voxel_nm_z[i] = flywire_neuron_name$voxel_raw_z[i]*40
#   flywire_neuron_name$name[i] = glue('Putative_{flywire_neuron_name$type[i]}_{flywire_neuron_name$voxel_raw_x[i]}_{flywire_neuron_name$voxel_raw_y[i]}_{flywire_neuron_name$voxel_raw_z[i]}')
#   flywire_neuron_name$seg_id[i] = flywire_xyz2id(matrix(c(flywire_neuron_name$voxel_nm_x[i],
#                                                           flywire_neuron_name$voxel_nm_y[i],
#                                                           flywire_neuron_name$voxel_nm_z[i]),
#                                                         ncol = 3,
#                                                         byrow = FALSE,
#                                                         dimnames = list(NULL, c("X","Y","Z"))))
#   flywire_neuron_name$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(flywire_neuron_name$seg_id[i])}","klickable_link")')
# }
# end_time <- Sys.time()
# 
# run_time <- end_time - start_time
# 
# flywire_neuron_name = flywire_neuron_name %>% 
#   distinct(seg_id, .keep_all = TRUE) %>% 
#   arrange(type,hemisphere,name)
# 
# sheet_write(flywire_neuron_name, ss="1z0qGbSRsQWjw3teznyHn2zQBYX8amum79V1_vva_iSE", sheet = "Sheet1")

#### NEW flywire_neuron_names-table update code

flywire_neuron_name = read_sheet("1z0qGbSRsQWjw3teznyHn2zQBYX8amum79V1_vva_iSE")

start_time <- Sys.time()
flywire_neuron_name = flywire_neuron_name %>% separate(XYZ, c("voxel_raw_x","voxel_raw_y","voxel_raw_z"), sep = ",",remove = F)
flywire_neuron_name$name = glue('Putative_{flywire_neuron_name$type}_{flywire_neuron_name$voxel_raw_x}_{flywire_neuron_name$voxel_raw_y}_{flywire_neuron_name$voxel_raw_z}')
flywire_neuron_name$seg_id = flywire_xyz2id(matrix(c(as.numeric(flywire_neuron_name$voxel_raw_x),
                                                     as.numeric(flywire_neuron_name$voxel_raw_y),
                                                     as.numeric(flywire_neuron_name$voxel_raw_z)),
                                                   ncol = 3,
                                                   byrow = FALSE,
                                                   dimnames = list(NULL, c("X","Y","Z"))),
                                            rawcoords = TRUE,
                                            fast_root = TRUE)
end_time <- Sys.time()
run_time <- end_time - start_time
run_time

flywire_neuron_name = flywire_neuron_name %>% 
  distinct(seg_id, .keep_all = TRUE) %>% 
  arrange(type,hemisphere,name)

sheet_write(flywire_neuron_name, ss="1z0qGbSRsQWjw3teznyHn2zQBYX8amum79V1_vva_iSE", sheet = "Sheet1")




#1st most important targets:


# LC14a input


# ------------------------------------------------------------------------
# LC9 Samra 1 --------------------------------- Last run 30/01/2023
# Putative_LC9_176787_62996_3601

# First LC9 : https://docs.google.com/spreadsheets/d/14FS2ayAzV5gNq3sDQ6IuYfU_VrA3OltY-moant0EX-8/edit?usp=sharing
Putative_LC9_176787_62996_3601_id = "720575940624621389"

# getting the newest ID if changes happened
Putative_LC9_176787_62996_3601_id = flywire_latestid(Putative_LC9_176787_62996_3601_id)

# read mesh from flywire
Putative_LC9_176787_62996_3601_mesh = read_cloudvolume_meshes(Putative_LC9_176787_62996_3601_id) 


## DOWNSTREAM PARTNERS based on buhmann synapses 
Putative_LC9_176787_62996_3601_output = flywire_partner_summary(Putative_LC9_176787_62996_3601_id,partners = "output", cleft.threshold = 100)
Putative_LC9_176787_62996_3601_output$pre_id = Putative_LC9_176787_62996_3601_id

# check for the latest ID for each partner and add a flywire_URL
Putative_LC9_176787_62996_3601_output$partner_type = ""
Putative_LC9_176787_62996_3601_output$partner_name = ""
Putative_LC9_176787_62996_3601_output$flywire_URL = ""
for (i in 1:length(Putative_LC9_176787_62996_3601_output$post_id)){
  Putative_LC9_176787_62996_3601_output$post_id[i] = flywire_latestid(Putative_LC9_176787_62996_3601_output$post_id[i])
  Putative_LC9_176787_62996_3601_output$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_LC9_176787_62996_3601_output$pre_id[i],Putative_LC9_176787_62996_3601_output$post_id[i]))}","klickable_link")')
} 

# get names and types from flywire_neuron_names table
Putative_LC9_176787_62996_3601_output$partner_name = flywire_neuron_name$name[match(Putative_LC9_176787_62996_3601_output$post_id,flywire_neuron_name$seg_id)]
Putative_LC9_176787_62996_3601_output$partner_type = flywire_neuron_name$type[match(Putative_LC9_176787_62996_3601_output$post_id,flywire_neuron_name$seg_id)]



## UPSTREAM PARTNERS based on buhmann synapses 
Putative_LC9_176787_62996_3601_input = flywire_partner_summary(Putative_LC9_176787_62996_3601_id,partners = "input", cleft.threshold = 100)
Putative_LC9_176787_62996_3601_input$post_id = Putative_LC9_176787_62996_3601_id

# check for the latest ID for each partner and add a flywire_URL
Putative_LC9_176787_62996_3601_input$partner_type = ""
Putative_LC9_176787_62996_3601_input$partner_name = ""
Putative_LC9_176787_62996_3601_input$flywire_URL = ""
for (i in 1:length(Putative_LC9_176787_62996_3601_input$pre_id)){
  Putative_LC9_176787_62996_3601_input$pre_id[i] = flywire_latestid(Putative_LC9_176787_62996_3601_input$pre_id[i])
  Putative_LC9_176787_62996_3601_input$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_LC9_176787_62996_3601_input$post_id[i],Putative_LC9_176787_62996_3601_input$pre_id[i]))}","klickable_link")')
} 

# get names 
Putative_LC9_176787_62996_3601_input$partner_name = flywire_neuron_name$name[match(Putative_LC9_176787_62996_3601_input$pre_id,flywire_neuron_name$seg_id)]
Putative_LC9_176787_62996_3601_input$partner_type = flywire_neuron_name$type[match(Putative_LC9_176787_62996_3601_input$pre_id,flywire_neuron_name$seg_id)]



# writing the connectivity tables back to google sheets
sheet_write(Putative_LC9_176787_62996_3601_output, ss="14FS2ayAzV5gNq3sDQ6IuYfU_VrA3OltY-moant0EX-8", sheet = "output")
sheet_write(Putative_LC9_176787_62996_3601_input, ss="14FS2ayAzV5gNq3sDQ6IuYfU_VrA3OltY-moant0EX-8", sheet = "input")
# sheet_write(Putative_LC9_176787_62996_3601_exclude, ss= "14FS2ayAzV5gNq3sDQ6IuYfU_VrA3OltY-moant0EX-8", sheet = "exclude")




# ------------------------------------------------------------------------
# LC9 Samra 2 --------------------------------- Last run 30/01/2023
# Putative_LC9_181296_62202_3552

# Second LC9 : https://docs.google.com/spreadsheets/d/1C2sc5_E4sTeyVoHIq11SHTJM1gdpvDhW6nzwZPRJyyY/edit?usp=sharing
Putative_LC9_181296_62202_3552_id = "720575940619434432"

# getting the newest ID if changes happened
Putative_LC9_181296_62202_3552_id = flywire_latestid(Putative_LC9_181296_62202_3552_id)

# read mesh from flywire
Putative_LC9_181296_62202_3552_mesh = read_cloudvolume_meshes(Putative_LC9_181296_62202_3552_id) 


## DOWNSTREAM PARTNERS based on buhmann synapses 
Putative_LC9_181296_62202_3552_output = flywire_partner_summary(Putative_LC9_181296_62202_3552_id,partners = "output", cleft.threshold = 100)
Putative_LC9_181296_62202_3552_output$pre_id = Putative_LC9_181296_62202_3552_id

# check for the latest ID for each partner and add a flywire_URL
Putative_LC9_181296_62202_3552_output$partner_type = ""
Putative_LC9_181296_62202_3552_output$partner_name = ""
Putative_LC9_181296_62202_3552_output$flywire_URL = ""
for (i in 1:length(Putative_LC9_181296_62202_3552_output$post_id)){
  Putative_LC9_181296_62202_3552_output$post_id[i] = flywire_latestid(Putative_LC9_181296_62202_3552_output$post_id[i])
  Putative_LC9_181296_62202_3552_output$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_LC9_181296_62202_3552_output$pre_id[i],Putative_LC9_181296_62202_3552_output$post_id[i]))}","klickable_link")')
} 

# get names and types from flywire_neuron_names table
Putative_LC9_181296_62202_3552_output$partner_name = flywire_neuron_name$name[match(Putative_LC9_181296_62202_3552_output$post_id,flywire_neuron_name$seg_id)]
Putative_LC9_181296_62202_3552_output$partner_type = flywire_neuron_name$type[match(Putative_LC9_181296_62202_3552_output$post_id,flywire_neuron_name$seg_id)]



## UPSTREAM PARTNERS based on buhmann synapses 
Putative_LC9_181296_62202_3552_input = flywire_partner_summary(Putative_LC9_181296_62202_3552_id,partners = "input", cleft.threshold = 100)
Putative_LC9_181296_62202_3552_input$post_id = Putative_LC9_181296_62202_3552_id

# check for the latest ID for each partner and add a flywire_URL
Putative_LC9_181296_62202_3552_input$partner_type = ""
Putative_LC9_181296_62202_3552_input$partner_name = ""
Putative_LC9_181296_62202_3552_input$flywire_URL = ""
for (i in 1:length(Putative_LC9_181296_62202_3552_input$pre_id)){
  Putative_LC9_181296_62202_3552_input$pre_id[i] = flywire_latestid(Putative_LC9_181296_62202_3552_input$pre_id[i])
  Putative_LC9_181296_62202_3552_input$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_LC9_181296_62202_3552_input$post_id[i],Putative_LC9_181296_62202_3552_input$pre_id[i]))}","klickable_link")')
} 

# get names 
Putative_LC9_181296_62202_3552_input$partner_name = flywire_neuron_name$name[match(Putative_LC9_181296_62202_3552_input$pre_id,flywire_neuron_name$seg_id)]
Putative_LC9_181296_62202_3552_input$partner_type = flywire_neuron_name$type[match(Putative_LC9_181296_62202_3552_input$pre_id,flywire_neuron_name$seg_id)]



# writing the connectivity tables back to google sheets
sheet_write(Putative_LC9_181296_62202_3552_output, ss="1C2sc5_E4sTeyVoHIq11SHTJM1gdpvDhW6nzwZPRJyyY", sheet = "output")
sheet_write(Putative_LC9_181296_62202_3552_input, ss="1C2sc5_E4sTeyVoHIq11SHTJM1gdpvDhW6nzwZPRJyyY", sheet = "input")
# sheet_write(Putative_LC9_181296_62202_3552_exclude, ss= "1C2sc5_E4sTeyVoHIq11SHTJM1gdpvDhW6nzwZPRJyyY", sheet = "exclude")


# ------------------------------------------------------------------------

# LC9 Samra 3 --------------------------------- Last run 30/01/2023
# Putative_LC9_83899_63927_4405

# 3 LC9 : https://docs.google.com/spreadsheets/d/16EJN1c4EgW25OPwkBg9pTDP9WO3ddt8_18W1Z3zRHdk/edit?usp=sharing
Putative_LC9_83899_63927_4405_id = "720575940628261873"

# getting the newest ID if changes happened
Putative_LC9_83899_63927_4405_id = flywire_latestid(Putative_LC9_83899_63927_4405_id)

# read mesh from flywire
Putative_LC9_83899_63927_4405_mesh = read_cloudvolume_meshes(Putative_LC9_83899_63927_4405_id) 


## DOWNSTREAM PARTNERS based on buhmann synapses 
Putative_LC9_83899_63927_4405_output = flywire_partner_summary(Putative_LC9_83899_63927_4405_id,partners = "output", cleft.threshold = 100)
Putative_LC9_83899_63927_4405_output$pre_id = Putative_LC9_83899_63927_4405_id

# check for the latest ID for each partner and add a flywire_URL
Putative_LC9_83899_63927_4405_output$partner_type = ""
Putative_LC9_83899_63927_4405_output$partner_name = ""
Putative_LC9_83899_63927_4405_output$flywire_URL = ""
for (i in 1:length(Putative_LC9_83899_63927_4405_output$post_id)){
  Putative_LC9_83899_63927_4405_output$post_id[i] = flywire_latestid(Putative_LC9_83899_63927_4405_output$post_id[i])
  Putative_LC9_83899_63927_4405_output$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_LC9_83899_63927_4405_output$pre_id[i],Putative_LC9_83899_63927_4405_output$post_id[i]))}","klickable_link")')
} 

# get names and types from flywire_neuron_names table
Putative_LC9_83899_63927_4405_output$partner_name = flywire_neuron_name$name[match(Putative_LC9_83899_63927_4405_output$post_id,flywire_neuron_name$seg_id)]
Putative_LC9_83899_63927_4405_output$partner_type = flywire_neuron_name$type[match(Putative_LC9_83899_63927_4405_output$post_id,flywire_neuron_name$seg_id)]



## UPSTREAM PARTNERS based on buhmann synapses 
Putative_LC9_83899_63927_4405_input = flywire_partner_summary(Putative_LC9_83899_63927_4405_id,partners = "input", cleft.threshold = 100)
Putative_LC9_83899_63927_4405_input$post_id = Putative_LC9_83899_63927_4405_id

# check for the latest ID for each partner and add a flywire_URL
Putative_LC9_83899_63927_4405_input$partner_type = ""
Putative_LC9_83899_63927_4405_input$partner_name = ""
Putative_LC9_83899_63927_4405_input$flywire_URL = ""
for (i in 1:length(Putative_LC9_83899_63927_4405_input$pre_id)){
  Putative_LC9_83899_63927_4405_input$pre_id[i] = flywire_latestid(Putative_LC9_83899_63927_4405_input$pre_id[i])
  Putative_LC9_83899_63927_4405_input$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_LC9_83899_63927_4405_input$post_id[i],Putative_LC9_83899_63927_4405_input$pre_id[i]))}","klickable_link")')
} 

# get names 
Putative_LC9_83899_63927_4405_input$partner_name = flywire_neuron_name$name[match(Putative_LC9_83899_63927_4405_input$pre_id,flywire_neuron_name$seg_id)]
Putative_LC9_83899_63927_4405_input$partner_type = flywire_neuron_name$type[match(Putative_LC9_83899_63927_4405_input$pre_id,flywire_neuron_name$seg_id)]



# writing the connectivity tables back to google sheets
sheet_write(Putative_LC9_83899_63927_4405_output, ss="16EJN1c4EgW25OPwkBg9pTDP9WO3ddt8_18W1Z3zRHdk", sheet = "output")
sheet_write(Putative_LC9_83899_63927_4405_input, ss="16EJN1c4EgW25OPwkBg9pTDP9WO3ddt8_18W1Z3zRHdk", sheet = "input")
# sheet_write(Putative_LC9_83899_63927_4405_exclude, ss= "16EJN1c4EgW25OPwkBg9pTDP9WO3ddt8_18W1Z3zRHdk", sheet = "exclude")


# LC9 middle as a check what is going on in the absence of LC14a.... 



# ------------------------------------------------------------------------
# LC9 Samra 4 --------------------------------- Last run 30/01/2022
# Putative_LC9_161016_ 46645_ 2160

# 4 LC9 : https://docs.google.com/spreadsheets/d/1O1q48IxN90ZBENaFefw-deEhJasS7XDdSPHkq_ZG_x4/edit?usp=sharing
Putative_LC9_161016_46645_2160_id = "720575940635703262"

# getting the newest ID if changes happened
Putative_LC9_161016_46645_2160_id = flywire_latestid(Putative_LC9_161016_46645_2160_id)

# read mesh from flywire
Putative_LC9_161016_46645_2160_mesh = read_cloudvolume_meshes(Putative_LC9_161016_46645_2160_id) 


## DOWNSTREAM PARTNERS based on buhmann synapses 
Putative_LC9_161016_46645_2160_output = flywire_partner_summary(Putative_LC9_161016_46645_2160_id,partners = "output", cleft.threshold = 100)
Putative_LC9_161016_46645_2160_output$pre_id = Putative_LC9_161016_46645_2160_id

# check for the latest ID for each partner and add a flywire_URL
Putative_LC9_161016_46645_2160_output$partner_type = ""
Putative_LC9_161016_46645_2160_output$partner_name = ""
Putative_LC9_161016_46645_2160_output$flywire_URL = ""
for (i in 1:length(Putative_LC9_161016_46645_2160_output$post_id)){
  Putative_LC9_161016_46645_2160_output$post_id[i] = flywire_latestid(Putative_LC9_161016_46645_2160_output$post_id[i])
  Putative_LC9_161016_46645_2160_output$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_LC9_161016_46645_2160_output$pre_id[i],Putative_LC9_161016_46645_2160_output$post_id[i]))}","klickable_link")')
} 

# get names and types from flywire_neuron_names table
Putative_LC9_161016_46645_2160_output$partner_name = flywire_neuron_name$name[match(Putative_LC9_161016_46645_2160_output$post_id,flywire_neuron_name$seg_id)]
Putative_LC9_161016_46645_2160_output$partner_type = flywire_neuron_name$type[match(Putative_LC9_161016_46645_2160_output$post_id,flywire_neuron_name$seg_id)]



## UPSTREAM PARTNERS based on buhmann synapses 
Putative_LC9_161016_46645_2160_input = flywire_partner_summary(Putative_LC9_161016_46645_2160_id,partners = "input", cleft.threshold = 100)
Putative_LC9_161016_46645_2160_input$post_id = Putative_LC9_161016_46645_2160_id

# check for the latest ID for each partner and add a flywire_URL
Putative_LC9_161016_46645_2160_input$partner_type = ""
Putative_LC9_161016_46645_2160_input$partner_name = ""
Putative_LC9_161016_46645_2160_input$flywire_URL = ""
for (i in 1:length(Putative_LC9_161016_46645_2160_input$pre_id)){
  Putative_LC9_161016_46645_2160_input$pre_id[i] = flywire_latestid(Putative_LC9_161016_46645_2160_input$pre_id[i])
  Putative_LC9_161016_46645_2160_input$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_LC9_161016_46645_2160_input$post_id[i],Putative_LC9_161016_46645_2160_input$pre_id[i]))}","klickable_link")')
} 

# get names 
Putative_LC9_161016_46645_2160_input$partner_name = flywire_neuron_name$name[match(Putative_LC9_161016_46645_2160_input$pre_id,flywire_neuron_name$seg_id)]
Putative_LC9_161016_46645_2160_input$partner_type = flywire_neuron_name$type[match(Putative_LC9_161016_46645_2160_input$pre_id,flywire_neuron_name$seg_id)]



# writing the connectivity tables back to google sheets
sheet_write(Putative_LC9_161016_46645_2160_output, ss="1O1q48IxN90ZBENaFefw-deEhJasS7XDdSPHkq_ZG_x4", sheet = "output")
sheet_write(Putative_LC9_161016_46645_2160_input, ss="1O1q48IxN90ZBENaFefw-deEhJasS7XDdSPHkq_ZG_x4", sheet = "input")
# sheet_write(Putative_LC9_161016_46645_2160_exclude, ss= "1O1q48IxN90ZBENaFefw-deEhJasS7XDdSPHkq_ZG_x4", sheet = "exclude")



# ------------------------------------------------------------------------
# LC9 Samra 5 --------------------------------- Last run 30/01/2022
# Putative_LC9_112766_ 48940_ 2889

# 4 LC9 : https://docs.google.com/spreadsheets/d/1BEETGXKQLQXuEXivt6sWiRwEQGbcPue1CBQm3RgX95o/edit?usp=sharing
Putative_LC9_112766_48940_2889_id = "720575940617467485"

# getting the newest ID if changes happened
Putative_LC9_112766_48940_2889_id = flywire_latestid(Putative_LC9_112766_48940_2889_id)

# read mesh from flywire
Putative_LC9_112766_48940_2889_mesh = read_cloudvolume_meshes(Putative_LC9_112766_48940_2889_id) 


## DOWNSTREAM PARTNERS based on buhmann synapses 
Putative_LC9_112766_48940_2889_output = flywire_partner_summary(Putative_LC9_112766_48940_2889_id,partners = "output", cleft.threshold = 100)
Putative_LC9_112766_48940_2889_output$pre_id = Putative_LC9_112766_48940_2889_id

# check for the latest ID for each partner and add a flywire_URL
Putative_LC9_112766_48940_2889_output$partner_type = ""
Putative_LC9_112766_48940_2889_output$partner_name = ""
Putative_LC9_112766_48940_2889_output$flywire_URL = ""
for (i in 1:length(Putative_LC9_112766_48940_2889_output$post_id)){
  Putative_LC9_112766_48940_2889_output$post_id[i] = flywire_latestid(Putative_LC9_112766_48940_2889_output$post_id[i])
  Putative_LC9_112766_48940_2889_output$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_LC9_112766_48940_2889_output$pre_id[i],Putative_LC9_112766_48940_2889_output$post_id[i]))}","klickable_link")')
} 

# get names and types from flywire_neuron_names table
Putative_LC9_112766_48940_2889_output$partner_name = flywire_neuron_name$name[match(Putative_LC9_112766_48940_2889_output$post_id,flywire_neuron_name$seg_id)]
Putative_LC9_112766_48940_2889_output$partner_type = flywire_neuron_name$type[match(Putative_LC9_112766_48940_2889_output$post_id,flywire_neuron_name$seg_id)]



## UPSTREAM PARTNERS based on buhmann synapses 
Putative_LC9_112766_48940_2889_input = flywire_partner_summary(Putative_LC9_112766_48940_2889_id,partners = "input", cleft.threshold = 100)
Putative_LC9_112766_48940_2889_input$post_id = Putative_LC9_112766_48940_2889_id

# check for the latest ID for each partner and add a flywire_URL
Putative_LC9_112766_48940_2889_input$partner_type = ""
Putative_LC9_112766_48940_2889_input$partner_name = ""
Putative_LC9_112766_48940_2889_input$flywire_URL = ""
for (i in 1:length(Putative_LC9_112766_48940_2889_input$pre_id)){
  Putative_LC9_112766_48940_2889_input$pre_id[i] = flywire_latestid(Putative_LC9_112766_48940_2889_input$pre_id[i])
  Putative_LC9_112766_48940_2889_input$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_LC9_112766_48940_2889_input$post_id[i],Putative_LC9_112766_48940_2889_input$pre_id[i]))}","klickable_link")')
} 

# get names 
Putative_LC9_112766_48940_2889_input$partner_name = flywire_neuron_name$name[match(Putative_LC9_112766_48940_2889_input$pre_id,flywire_neuron_name$seg_id)]
Putative_LC9_112766_48940_2889_input$partner_type = flywire_neuron_name$type[match(Putative_LC9_112766_48940_2889_input$pre_id,flywire_neuron_name$seg_id)]



# writing the connectivity tables back to google sheets
sheet_write(Putative_LC9_112766_48940_2889_output, ss="1BEETGXKQLQXuEXivt6sWiRwEQGbcPue1CBQm3RgX95o", sheet = "output")
sheet_write(Putative_LC9_112766_48940_2889_input, ss="1BEETGXKQLQXuEXivt6sWiRwEQGbcPue1CBQm3RgX95o", sheet = "input")
# sheet_write(Putative_LC9_112766_48940_2889_exclude, ss= "1BEETGXKQLQXuEXivt6sWiRwEQGbcPue1CBQm3RgX95o", sheet = "exclude")



# ------------------------------------------------------------------------
# LC9 Samra 6 --------------------------------- Last run 30/01/2022
# Putative_LC9_160922_46337_2160

# 4 LC9 : https://docs.google.com/spreadsheets/d/189HNY8Ds7KAO5iTmoXr9eByHi96G7GZA2f9kzMOZS68/edit?usp=sharing
Putative_LC9_160922_46337_2160_id = "720575940626613609"

# getting the newest ID if changes happened
Putative_LC9_160922_46337_2160_id = flywire_latestid(Putative_LC9_160922_46337_2160_id)

# read mesh from flywire
Putative_LC9_160922_46337_2160_mesh = read_cloudvolume_meshes(Putative_LC9_160922_46337_2160_id) 


## DOWNSTREAM PARTNERS based on buhmann synapses 
Putative_LC9_160922_46337_2160_output = flywire_partner_summary(Putative_LC9_160922_46337_2160_id,partners = "output", cleft.threshold = 100)
Putative_LC9_160922_46337_2160_output$pre_id = Putative_LC9_160922_46337_2160_id

# check for the latest ID for each partner and add a flywire_URL
Putative_LC9_160922_46337_2160_output$partner_type = ""
Putative_LC9_160922_46337_2160_output$partner_name = ""
Putative_LC9_160922_46337_2160_output$flywire_URL = ""
for (i in 1:length(Putative_LC9_160922_46337_2160_output$post_id)){
  Putative_LC9_160922_46337_2160_output$post_id[i] = flywire_latestid(Putative_LC9_160922_46337_2160_output$post_id[i])
  Putative_LC9_160922_46337_2160_output$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_LC9_160922_46337_2160_output$pre_id[i],Putative_LC9_160922_46337_2160_output$post_id[i]))}","klickable_link")')
} 

# get names and types from flywire_neuron_names table
Putative_LC9_160922_46337_2160_output$partner_name = flywire_neuron_name$name[match(Putative_LC9_160922_46337_2160_output$post_id,flywire_neuron_name$seg_id)]
Putative_LC9_160922_46337_2160_output$partner_type = flywire_neuron_name$type[match(Putative_LC9_160922_46337_2160_output$post_id,flywire_neuron_name$seg_id)]



## UPSTREAM PARTNERS based on buhmann synapses 
Putative_LC9_160922_46337_2160_input = flywire_partner_summary(Putative_LC9_160922_46337_2160_id,partners = "input", cleft.threshold = 100)
Putative_LC9_160922_46337_2160_input$post_id = Putative_LC9_160922_46337_2160_id

# check for the latest ID for each partner and add a flywire_URL
Putative_LC9_160922_46337_2160_input$partner_type = ""
Putative_LC9_160922_46337_2160_input$partner_name = ""
Putative_LC9_160922_46337_2160_input$flywire_URL = ""
for (i in 1:length(Putative_LC9_160922_46337_2160_input$pre_id)){
  Putative_LC9_160922_46337_2160_input$pre_id[i] = flywire_latestid(Putative_LC9_160922_46337_2160_input$pre_id[i])
  Putative_LC9_160922_46337_2160_input$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_LC9_160922_46337_2160_input$post_id[i],Putative_LC9_160922_46337_2160_input$pre_id[i]))}","klickable_link")')
} 

# get names 
Putative_LC9_160922_46337_2160_input$partner_name = flywire_neuron_name$name[match(Putative_LC9_160922_46337_2160_input$pre_id,flywire_neuron_name$seg_id)]
Putative_LC9_160922_46337_2160_input$partner_type = flywire_neuron_name$type[match(Putative_LC9_160922_46337_2160_input$pre_id,flywire_neuron_name$seg_id)]



# writing the connectivity tables back to google sheets
sheet_write(Putative_LC9_160922_46337_2160_output, ss="189HNY8Ds7KAO5iTmoXr9eByHi96G7GZA2f9kzMOZS68", sheet = "output")
sheet_write(Putative_LC9_160922_46337_2160_input, ss="189HNY8Ds7KAO5iTmoXr9eByHi96G7GZA2f9kzMOZS68", sheet = "input")
# sheet_write(Putative_LC9_160922_46337_2160_exclude, ss= "189HNY8Ds7KAO5iTmoXr9eByHi96G7GZA2f9kzMOZS68", sheet = "exclude")


#LC9 input


# ------------------------------------------------------------------------
# PVLP_to_PLP Matanat 1 --------------------------------- Last run 30/01/2022
# Putative_PVLP_to_PLP_165632_40188_2802

# 1 PVLP_to_PLP : https://docs.google.com/spreadsheets/d/1sGpl8guN04lbnvilApo4DwZnE-scaBJgKbPROtvZkUc/edit?usp=sharing
Putative_PVLP_to_PLP_165632_40188_2802_id = "720575940625694034"

# getting the newest ID if changes happened
Putative_PVLP_to_PLP_165632_40188_2802_id = flywire_latestid(Putative_PVLP_to_PLP_165632_40188_2802_id)

# read mesh from flywire
Putative_PVLP_to_PLP_165632_40188_2802_mesh = read_cloudvolume_meshes(Putative_PVLP_to_PLP_165632_40188_2802_id) 


## DOWNSTREAM PARTNERS based on buhmann synapses 
Putative_PVLP_to_PLP_165632_40188_2802_output = flywire_partner_summary(Putative_PVLP_to_PLP_165632_40188_2802_id,partners = "output", cleft.threshold = 100)
Putative_PVLP_to_PLP_165632_40188_2802_output$pre_id = Putative_PVLP_to_PLP_165632_40188_2802_id

# check for the latest ID for each partner and add a flywire_URL
Putative_PVLP_to_PLP_165632_40188_2802_output$partner_type = ""
Putative_PVLP_to_PLP_165632_40188_2802_output$partner_name = ""
Putative_PVLP_to_PLP_165632_40188_2802_output$flywire_URL = ""
for (i in 1:length(Putative_PVLP_to_PLP_165632_40188_2802_output$post_id)){
  Putative_PVLP_to_PLP_165632_40188_2802_output$post_id[i] = flywire_latestid(Putative_PVLP_to_PLP_165632_40188_2802_output$post_id[i])
  Putative_PVLP_to_PLP_165632_40188_2802_output$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_PVLP_to_PLP_165632_40188_2802_output$pre_id[i],Putative_PVLP_to_PLP_165632_40188_2802_output$post_id[i]))}","klickable_link")')
} 

# get names and types from flywire_neuron_names table
Putative_PVLP_to_PLP_165632_40188_2802_output$partner_name = flywire_neuron_name$name[match(Putative_PVLP_to_PLP_165632_40188_2802_output$post_id,flywire_neuron_name$seg_id)]
Putative_PVLP_to_PLP_165632_40188_2802_output$partner_type = flywire_neuron_name$type[match(Putative_PVLP_to_PLP_165632_40188_2802_output$post_id,flywire_neuron_name$seg_id)]



## UPSTREAM PARTNERS based on buhmann synapses 
Putative_PVLP_to_PLP_165632_40188_2802_input = flywire_partner_summary(Putative_PVLP_to_PLP_165632_40188_2802_id,partners = "input", cleft.threshold = 100)
Putative_PVLP_to_PLP_165632_40188_2802_input$post_id = Putative_PVLP_to_PLP_165632_40188_2802_id

# check for the latest ID for each partner and add a flywire_URL
Putative_PVLP_to_PLP_165632_40188_2802_input$partner_type = ""
Putative_PVLP_to_PLP_165632_40188_2802_input$partner_name = ""
Putative_PVLP_to_PLP_165632_40188_2802_input$flywire_URL = ""
for (i in 1:length(Putative_PVLP_to_PLP_165632_40188_2802_input$pre_id)){
  Putative_PVLP_to_PLP_165632_40188_2802_input$pre_id[i] = flywire_latestid(Putative_PVLP_to_PLP_165632_40188_2802_input$pre_id[i])
  Putative_PVLP_to_PLP_165632_40188_2802_input$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_PVLP_to_PLP_165632_40188_2802_input$post_id[i],Putative_PVLP_to_PLP_165632_40188_2802_input$pre_id[i]))}","klickable_link")')
} 

# get names 
Putative_PVLP_to_PLP_165632_40188_2802_input$partner_name = flywire_neuron_name$name[match(Putative_PVLP_to_PLP_165632_40188_2802_input$pre_id,flywire_neuron_name$seg_id)]
Putative_PVLP_to_PLP_165632_40188_2802_input$partner_type = flywire_neuron_name$type[match(Putative_PVLP_to_PLP_165632_40188_2802_input$pre_id,flywire_neuron_name$seg_id)]



# writing the connectivity tables back to google sheets
sheet_write(Putative_PVLP_to_PLP_165632_40188_2802_output, ss="1sGpl8guN04lbnvilApo4DwZnE-scaBJgKbPROtvZkUc", sheet = "output")
sheet_write(Putative_PVLP_to_PLP_165632_40188_2802_input, ss="1sGpl8guN04lbnvilApo4DwZnE-scaBJgKbPROtvZkUc", sheet = "input")
# sheet_write(Putative_PVLP_to_PLP_165632_40188_2802_exclude, ss= "1sGpl8guN04lbnvilApo4DwZnE-scaBJgKbPROtvZkUc", sheet = "exclude")



# ------------------------------------------------------------------------
# PVLP_to_PLP Matanat 2 --------------------------------- Last run 30/01/2022
# Putative_PVLP_to_PLP_165885_40225_2761

# 2 PVLP_to_PLP : https://docs.google.com/spreadsheets/d/1Qpw3hsXOnZ9pCuYOb-0jGBQyLSS_HE47xDN6Ueduy9I/edit?usp=sharing
Putative_PVLP_to_PLP_165885_40225_2761_id = "720575940632614881"

# getting the newest ID if changes happened
Putative_PVLP_to_PLP_165885_40225_2761_id = flywire_latestid(Putative_PVLP_to_PLP_165885_40225_2761_id)

# read mesh from flywire
Putative_PVLP_to_PLP_165885_40225_2761_mesh = read_cloudvolume_meshes(Putative_PVLP_to_PLP_165885_40225_2761_id) 


## DOWNSTREAM PARTNERS based on buhmann synapses 
Putative_PVLP_to_PLP_165885_40225_2761_output = flywire_partner_summary(Putative_PVLP_to_PLP_165885_40225_2761_id,partners = "output", cleft.threshold = 100)
Putative_PVLP_to_PLP_165885_40225_2761_output$pre_id = Putative_PVLP_to_PLP_165885_40225_2761_id

# check for the latest ID for each partner and add a flywire_URL
Putative_PVLP_to_PLP_165885_40225_2761_output$partner_type = ""
Putative_PVLP_to_PLP_165885_40225_2761_output$partner_name = ""
Putative_PVLP_to_PLP_165885_40225_2761_output$flywire_URL = ""
for (i in 1:length(Putative_PVLP_to_PLP_165885_40225_2761_output$post_id)){
  Putative_PVLP_to_PLP_165885_40225_2761_output$post_id[i] = flywire_latestid(Putative_PVLP_to_PLP_165885_40225_2761_output$post_id[i])
  Putative_PVLP_to_PLP_165885_40225_2761_output$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_PVLP_to_PLP_165885_40225_2761_output$pre_id[i],Putative_PVLP_to_PLP_165885_40225_2761_output$post_id[i]))}","klickable_link")')
} 

# get names and types from flywire_neuron_names table
Putative_PVLP_to_PLP_165885_40225_2761_output$partner_name = flywire_neuron_name$name[match(Putative_PVLP_to_PLP_165885_40225_2761_output$post_id,flywire_neuron_name$seg_id)]
Putative_PVLP_to_PLP_165885_40225_2761_output$partner_type = flywire_neuron_name$type[match(Putative_PVLP_to_PLP_165885_40225_2761_output$post_id,flywire_neuron_name$seg_id)]



## UPSTREAM PARTNERS based on buhmann synapses 
Putative_PVLP_to_PLP_165885_40225_2761_input = flywire_partner_summary(Putative_PVLP_to_PLP_165885_40225_2761_id,partners = "input", cleft.threshold = 100)
Putative_PVLP_to_PLP_165885_40225_2761_input$post_id = Putative_PVLP_to_PLP_165885_40225_2761_id

# check for the latest ID for each partner and add a flywire_URL
Putative_PVLP_to_PLP_165885_40225_2761_input$partner_type = ""
Putative_PVLP_to_PLP_165885_40225_2761_input$partner_name = ""
Putative_PVLP_to_PLP_165885_40225_2761_input$flywire_URL = ""
for (i in 1:length(Putative_PVLP_to_PLP_165885_40225_2761_input$pre_id)){
  Putative_PVLP_to_PLP_165885_40225_2761_input$pre_id[i] = flywire_latestid(Putative_PVLP_to_PLP_165885_40225_2761_input$pre_id[i])
  Putative_PVLP_to_PLP_165885_40225_2761_input$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_PVLP_to_PLP_165885_40225_2761_input$post_id[i],Putative_PVLP_to_PLP_165885_40225_2761_input$pre_id[i]))}","klickable_link")')
} 

# get names 
Putative_PVLP_to_PLP_165885_40225_2761_input$partner_name = flywire_neuron_name$name[match(Putative_PVLP_to_PLP_165885_40225_2761_input$pre_id,flywire_neuron_name$seg_id)]
Putative_PVLP_to_PLP_165885_40225_2761_input$partner_type = flywire_neuron_name$type[match(Putative_PVLP_to_PLP_165885_40225_2761_input$pre_id,flywire_neuron_name$seg_id)]



# writing the connectivity tables back to google sheets
sheet_write(Putative_PVLP_to_PLP_165885_40225_2761_output, ss="1Qpw3hsXOnZ9pCuYOb-0jGBQyLSS_HE47xDN6Ueduy9I", sheet = "output")
sheet_write(Putative_PVLP_to_PLP_165885_40225_2761_input, ss="1Qpw3hsXOnZ9pCuYOb-0jGBQyLSS_HE47xDN6Ueduy9I", sheet = "input")
# sheet_write(Putative_PVLP_to_PLP_165885_40225_2761_exclude, ss= "1Qpw3hsXOnZ9pCuYOb-0jGBQyLSS_HE47xDN6Ueduy9I", sheet = "exclude")


# ------------------------------------------------------------------------
# PVLP_to_PLP Matanat 3 --------------------------------- Last run 30/01/2022
# Putative_PVLP_to_PLP_163993_44573_2982

# 3 PVLP_to_PLP : https://docs.google.com/spreadsheets/d/1kWWzFpASwVGCHW7gmRHJdk_QlTlIVje0GTqXpUXbNxM/edit?usp=sharing
Putative_PVLP_to_PLP_163993_44573_2982_id = "720575940637114458"

# getting the newest ID if changes happened
Putative_PVLP_to_PLP_163993_44573_2982_id = flywire_latestid(Putative_PVLP_to_PLP_163993_44573_2982_id)

# read mesh from flywire
Putative_PVLP_to_PLP_163993_44573_2982_mesh = read_cloudvolume_meshes(Putative_PVLP_to_PLP_163993_44573_2982_id) 


## DOWNSTREAM PARTNERS based on buhmann synapses 
Putative_PVLP_to_PLP_163993_44573_2982_output = flywire_partner_summary(Putative_PVLP_to_PLP_163993_44573_2982_id,partners = "output", cleft.threshold = 100)
Putative_PVLP_to_PLP_163993_44573_2982_output$pre_id = Putative_PVLP_to_PLP_163993_44573_2982_id

# check for the latest ID for each partner and add a flywire_URL
Putative_PVLP_to_PLP_163993_44573_2982_output$partner_type = ""
Putative_PVLP_to_PLP_163993_44573_2982_output$partner_name = ""
Putative_PVLP_to_PLP_163993_44573_2982_output$flywire_URL = ""
for (i in 1:length(Putative_PVLP_to_PLP_163993_44573_2982_output$post_id)){
  Putative_PVLP_to_PLP_163993_44573_2982_output$post_id[i] = flywire_latestid(Putative_PVLP_to_PLP_163993_44573_2982_output$post_id[i])
  Putative_PVLP_to_PLP_163993_44573_2982_output$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_PVLP_to_PLP_163993_44573_2982_output$pre_id[i],Putative_PVLP_to_PLP_163993_44573_2982_output$post_id[i]))}","klickable_link")')
} 

# get names and types from flywire_neuron_names table
Putative_PVLP_to_PLP_163993_44573_2982_output$partner_name = flywire_neuron_name$name[match(Putative_PVLP_to_PLP_163993_44573_2982_output$post_id,flywire_neuron_name$seg_id)]
Putative_PVLP_to_PLP_163993_44573_2982_output$partner_type = flywire_neuron_name$type[match(Putative_PVLP_to_PLP_163993_44573_2982_output$post_id,flywire_neuron_name$seg_id)]



## UPSTREAM PARTNERS based on buhmann synapses 
Putative_PVLP_to_PLP_163993_44573_2982_input = flywire_partner_summary(Putative_PVLP_to_PLP_163993_44573_2982_id,partners = "input", cleft.threshold = 100)
Putative_PVLP_to_PLP_163993_44573_2982_input$post_id = Putative_PVLP_to_PLP_163993_44573_2982_id

# check for the latest ID for each partner and add a flywire_URL
Putative_PVLP_to_PLP_163993_44573_2982_input$partner_type = ""
Putative_PVLP_to_PLP_163993_44573_2982_input$partner_name = ""
Putative_PVLP_to_PLP_163993_44573_2982_input$flywire_URL = ""
for (i in 1:length(Putative_PVLP_to_PLP_163993_44573_2982_input$pre_id)){
  Putative_PVLP_to_PLP_163993_44573_2982_input$pre_id[i] = flywire_latestid(Putative_PVLP_to_PLP_163993_44573_2982_input$pre_id[i])
  Putative_PVLP_to_PLP_163993_44573_2982_input$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_PVLP_to_PLP_163993_44573_2982_input$post_id[i],Putative_PVLP_to_PLP_163993_44573_2982_input$pre_id[i]))}","klickable_link")')
} 

# get names 
Putative_PVLP_to_PLP_163993_44573_2982_input$partner_name = flywire_neuron_name$name[match(Putative_PVLP_to_PLP_163993_44573_2982_input$pre_id,flywire_neuron_name$seg_id)]
Putative_PVLP_to_PLP_163993_44573_2982_input$partner_type = flywire_neuron_name$type[match(Putative_PVLP_to_PLP_163993_44573_2982_input$pre_id,flywire_neuron_name$seg_id)]



# writing the connectivity tables back to google sheets
sheet_write(Putative_PVLP_to_PLP_163993_44573_2982_output, ss="1kWWzFpASwVGCHW7gmRHJdk_QlTlIVje0GTqXpUXbNxM", sheet = "output")
sheet_write(Putative_PVLP_to_PLP_163993_44573_2982_input, ss="1kWWzFpASwVGCHW7gmRHJdk_QlTlIVje0GTqXpUXbNxM", sheet = "input")
# sheet_write(Putative_PVLP_to_PLP_163993_44573_2982_exclude, ss= "1kWWzFpASwVGCHW7gmRHJdk_QlTlIVje0GTqXpUXbNxM", sheet = "exclude")


# LC14a output

# ------------------------------------------------------------------------
# Lobula to medulla Samra 1 --------------------------------- Last run 30/01/2023
# Putative_Lobula_to_medulla_85584_58922_4400

# First Lobula to medulla : https://docs.google.com/spreadsheets/d/1nhCJEE_Uw6FmANEw4dEVLzj7dVX4p3-FIyxi2QwXOn0/edit?usp=sharing
Putative_Lobula_to_medulla_85584_58922_4400_id = "720575940631063756"

# getting the newest ID if changes happened
Putative_Lobula_to_medulla_85584_58922_4400_id = flywire_latestid(Putative_Lobula_to_medulla_85584_58922_4400_id)

# read mesh from flywire
Putative_Lobula_to_medulla_85584_58922_4400_mesh = read_cloudvolume_meshes(Putative_Lobula_to_medulla_85584_58922_4400_id) 


## DOWNSTREAM PARTNERS based on buhmann synapses 
Putative_Lobula_to_medulla_85584_58922_4400_output = flywire_partner_summary(Putative_Lobula_to_medulla_85584_58922_4400_id,partners = "output", cleft.threshold = 100)
Putative_Lobula_to_medulla_85584_58922_4400_output$pre_id = Putative_Lobula_to_medulla_85584_58922_4400_id

# check for the latest ID for each partner and add a flywire_URL
Putative_Lobula_to_medulla_85584_58922_4400_output$partner_type = ""
Putative_Lobula_to_medulla_85584_58922_4400_output$partner_name = ""
Putative_Lobula_to_medulla_85584_58922_4400_output$flywire_URL = ""
for (i in 1:length(Putative_Lobula_to_medulla_85584_58922_4400_output$post_id)){
  Putative_Lobula_to_medulla_85584_58922_4400_output$post_id[i] = flywire_latestid(Putative_Lobula_to_medulla_85584_58922_4400_output$post_id[i])
  Putative_Lobula_to_medulla_85584_58922_4400_output$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_Lobula_to_medulla_85584_58922_4400_output$pre_id[i],Putative_Lobula_to_medulla_85584_58922_4400_output$post_id[i]))}","klickable_link")')
} 

# get names and types from flywire_neuron_names table
Putative_Lobula_to_medulla_85584_58922_4400_output$partner_name = flywire_neuron_name$name[match(Putative_Lobula_to_medulla_85584_58922_4400_output$post_id,flywire_neuron_name$seg_id)]
Putative_Lobula_to_medulla_85584_58922_4400_output$partner_type = flywire_neuron_name$type[match(Putative_Lobula_to_medulla_85584_58922_4400_output$post_id,flywire_neuron_name$seg_id)]



## UPSTREAM PARTNERS based on buhmann synapses 
Putative_Lobula_to_medulla_85584_58922_4400_input = flywire_partner_summary(Putative_Lobula_to_medulla_85584_58922_4400_id,partners = "input", cleft.threshold = 100)
Putative_Lobula_to_medulla_85584_58922_4400_input$post_id = Putative_Lobula_to_medulla_85584_58922_4400_id

# check for the latest ID for each partner and add a flywire_URL
Putative_Lobula_to_medulla_85584_58922_4400_input$partner_type = ""
Putative_Lobula_to_medulla_85584_58922_4400_input$partner_name = ""
Putative_Lobula_to_medulla_85584_58922_4400_input$flywire_URL = ""
for (i in 1:length(Putative_Lobula_to_medulla_85584_58922_4400_input$pre_id)){
  Putative_Lobula_to_medulla_85584_58922_4400_input$pre_id[i] = flywire_latestid(Putative_Lobula_to_medulla_85584_58922_4400_input$pre_id[i])
  Putative_Lobula_to_medulla_85584_58922_4400_input$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_Lobula_to_medulla_85584_58922_4400_input$post_id[i],Putative_Lobula_to_medulla_85584_58922_4400_input$pre_id[i]))}","klickable_link")')
} 

# get names 
Putative_Lobula_to_medulla_85584_58922_4400_input$partner_name = flywire_neuron_name$name[match(Putative_Lobula_to_medulla_85584_58922_4400_input$pre_id,flywire_neuron_name$seg_id)]
Putative_Lobula_to_medulla_85584_58922_4400_input$partner_type = flywire_neuron_name$type[match(Putative_Lobula_to_medulla_85584_58922_4400_input$pre_id,flywire_neuron_name$seg_id)]



# writing the connectivity tables back to google sheets
sheet_write(Putative_Lobula_to_medulla_85584_58922_4400_output, ss="1nhCJEE_Uw6FmANEw4dEVLzj7dVX4p3-FIyxi2QwXOn0", sheet = "output")
sheet_write(Putative_Lobula_to_medulla_85584_58922_4400_input, ss="1nhCJEE_Uw6FmANEw4dEVLzj7dVX4p3-FIyxi2QwXOn0", sheet = "input")
# sheet_write(Putative_Lobula_to_medulla_85584_58922_4400_exclude, ss= "1AJYJZm7yR0yfm7uz88ptYMeeWkm4yezKwN_dHqNtBRk", sheet = "exclude")





# ------------------------------------------------------------------------
# Lobula to medulla Samra 2 --------------------------------- Last run 30/01/2023
# Putative_Lobula_to_medulla_192544_81628_3983

# First Lobula to medulla : https://docs.google.com/spreadsheets/d/1uUf8gpLPi0xR3uZaQQF3ODMUwvn5zKNcKbgh31slTus/edit?usp=sharing
Putative_Lobula_to_medulla_192544_81628_3983_id = "720575940616059485"

# getting the newest ID if changes happened
Putative_Lobula_to_medulla_192544_81628_3983_id = flywire_latestid(Putative_Lobula_to_medulla_192544_81628_3983_id)

# read mesh from flywire
Putative_Lobula_to_medulla_192544_81628_3983_mesh = read_cloudvolume_meshes(Putative_Lobula_to_medulla_192544_81628_3983_id) 


## DOWNSTREAM PARTNERS based on buhmann synapses 
Putative_Lobula_to_medulla_192544_81628_3983_output = flywire_partner_summary(Putative_Lobula_to_medulla_192544_81628_3983_id,partners = "output", cleft.threshold = 100)
Putative_Lobula_to_medulla_192544_81628_3983_output$pre_id = Putative_Lobula_to_medulla_192544_81628_3983_id

# check for the latest ID for each partner and add a flywire_URL
Putative_Lobula_to_medulla_192544_81628_3983_output$partner_type = ""
Putative_Lobula_to_medulla_192544_81628_3983_output$partner_name = ""
Putative_Lobula_to_medulla_192544_81628_3983_output$flywire_URL = ""
for (i in 1:length(Putative_Lobula_to_medulla_192544_81628_3983_output$post_id)){
  Putative_Lobula_to_medulla_192544_81628_3983_output$post_id[i] = flywire_latestid(Putative_Lobula_to_medulla_192544_81628_3983_output$post_id[i])
  Putative_Lobula_to_medulla_192544_81628_3983_output$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_Lobula_to_medulla_192544_81628_3983_output$pre_id[i],Putative_Lobula_to_medulla_192544_81628_3983_output$post_id[i]))}","klickable_link")')
} 

# get names and types from flywire_neuron_names table
Putative_Lobula_to_medulla_192544_81628_3983_output$partner_name = flywire_neuron_name$name[match(Putative_Lobula_to_medulla_192544_81628_3983_output$post_id,flywire_neuron_name$seg_id)]
Putative_Lobula_to_medulla_192544_81628_3983_output$partner_type = flywire_neuron_name$type[match(Putative_Lobula_to_medulla_192544_81628_3983_output$post_id,flywire_neuron_name$seg_id)]



## UPSTREAM PARTNERS based on buhmann synapses 
Putative_Lobula_to_medulla_192544_81628_3983_input = flywire_partner_summary(Putative_Lobula_to_medulla_192544_81628_3983_id,partners = "input", cleft.threshold = 100)
Putative_Lobula_to_medulla_192544_81628_3983_input$post_id = Putative_Lobula_to_medulla_192544_81628_3983_id

# check for the latest ID for each partner and add a flywire_URL
Putative_Lobula_to_medulla_192544_81628_3983_input$partner_type = ""
Putative_Lobula_to_medulla_192544_81628_3983_input$partner_name = ""
Putative_Lobula_to_medulla_192544_81628_3983_input$flywire_URL = ""
for (i in 1:length(Putative_Lobula_to_medulla_192544_81628_3983_input$pre_id)){
  Putative_Lobula_to_medulla_192544_81628_3983_input$pre_id[i] = flywire_latestid(Putative_Lobula_to_medulla_192544_81628_3983_input$pre_id[i])
  Putative_Lobula_to_medulla_192544_81628_3983_input$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_Lobula_to_medulla_192544_81628_3983_input$post_id[i],Putative_Lobula_to_medulla_192544_81628_3983_input$pre_id[i]))}","klickable_link")')
} 

# get names 
Putative_Lobula_to_medulla_192544_81628_3983_input$partner_name = flywire_neuron_name$name[match(Putative_Lobula_to_medulla_192544_81628_3983_input$pre_id,flywire_neuron_name$seg_id)]
Putative_Lobula_to_medulla_192544_81628_3983_input$partner_type = flywire_neuron_name$type[match(Putative_Lobula_to_medulla_192544_81628_3983_input$pre_id,flywire_neuron_name$seg_id)]


# writing the connectivity tables back to google sheets
sheet_write(Putative_Lobula_to_medulla_192544_81628_3983_output, ss="1uUf8gpLPi0xR3uZaQQF3ODMUwvn5zKNcKbgh31slTus", sheet = "output")
sheet_write(Putative_Lobula_to_medulla_192544_81628_3983_input, ss="1uUf8gpLPi0xR3uZaQQF3ODMUwvn5zKNcKbgh31slTus", sheet = "input")
# sheet_write(Putative_Lobula_to_medulla_192544_81628_3983_exclude, ss= "1uUf8gpLPi0xR3uZaQQF3ODMUwvn5zKNcKbgh31slTus", sheet = "exclude")



# ------------------------------------------------------------------------
# Lobula to medulla Samra 3 --------------------------------- Last run 30/01/2023
# Putative_Lobula_to_medulla_176134_72366_3203

# Third Lobula to medulla : https://docs.google.com/spreadsheets/d/1w2QmqlxvEo5RdDe7YeJOEFyd_CYDTXcSOMwTML1DH9Q/edit?usp=sharing
Putative_Lobula_to_medulla_176134_72366_3203_id = "720575940613910951"

# getting the newest ID if changes happened
Putative_Lobula_to_medulla_176134_72366_3203_id = flywire_latestid(Putative_Lobula_to_medulla_176134_72366_3203_id)

# read mesh from flywire
Putative_Lobula_to_medulla_176134_72366_3203_mesh = read_cloudvolume_meshes(Putative_Lobula_to_medulla_176134_72366_3203_id) 


## DOWNSTREAM PARTNERS based on buhmann synapses 
Putative_Lobula_to_medulla_176134_72366_3203_output = flywire_partner_summary(Putative_Lobula_to_medulla_176134_72366_3203_id,partners = "output", cleft.threshold = 100)
Putative_Lobula_to_medulla_176134_72366_3203_output$pre_id = Putative_Lobula_to_medulla_176134_72366_3203_id

# check for the latest ID for each partner and add a flywire_URL
Putative_Lobula_to_medulla_176134_72366_3203_output$partner_type = ""
Putative_Lobula_to_medulla_176134_72366_3203_output$partner_name = ""
Putative_Lobula_to_medulla_176134_72366_3203_output$flywire_URL = ""
for (i in 1:length(Putative_Lobula_to_medulla_176134_72366_3203_output$post_id)){
  Putative_Lobula_to_medulla_176134_72366_3203_output$post_id[i] = flywire_latestid(Putative_Lobula_to_medulla_176134_72366_3203_output$post_id[i])
  Putative_Lobula_to_medulla_176134_72366_3203_output$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_Lobula_to_medulla_176134_72366_3203_output$pre_id[i],Putative_Lobula_to_medulla_176134_72366_3203_output$post_id[i]))}","klickable_link")')
} 

# get names and types from flywire_neuron_names table
Putative_Lobula_to_medulla_176134_72366_3203_output$partner_name = flywire_neuron_name$name[match(Putative_Lobula_to_medulla_176134_72366_3203_output$post_id,flywire_neuron_name$seg_id)]
Putative_Lobula_to_medulla_176134_72366_3203_output$partner_type = flywire_neuron_name$type[match(Putative_Lobula_to_medulla_176134_72366_3203_output$post_id,flywire_neuron_name$seg_id)]



## UPSTREAM PARTNERS based on buhmann synapses 
Putative_Lobula_to_medulla_176134_72366_3203_input = flywire_partner_summary(Putative_Lobula_to_medulla_176134_72366_3203_id,partners = "input", cleft.threshold = 100)
Putative_Lobula_to_medulla_176134_72366_3203_input$post_id = Putative_Lobula_to_medulla_176134_72366_3203_id

# check for the latest ID for each partner and add a flywire_URL
Putative_Lobula_to_medulla_176134_72366_3203_input$partner_type = ""
Putative_Lobula_to_medulla_176134_72366_3203_input$partner_name = ""
Putative_Lobula_to_medulla_176134_72366_3203_input$flywire_URL = ""
for (i in 1:length(Putative_Lobula_to_medulla_176134_72366_3203_input$pre_id)){
  Putative_Lobula_to_medulla_176134_72366_3203_input$pre_id[i] = flywire_latestid(Putative_Lobula_to_medulla_176134_72366_3203_input$pre_id[i])
  Putative_Lobula_to_medulla_176134_72366_3203_input$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_Lobula_to_medulla_176134_72366_3203_input$post_id[i],Putative_Lobula_to_medulla_176134_72366_3203_input$pre_id[i]))}","klickable_link")')
} 

# get names 
Putative_Lobula_to_medulla_176134_72366_3203_input$partner_name = flywire_neuron_name$name[match(Putative_Lobula_to_medulla_176134_72366_3203_input$pre_id,flywire_neuron_name$seg_id)]
Putative_Lobula_to_medulla_176134_72366_3203_input$partner_type = flywire_neuron_name$type[match(Putative_Lobula_to_medulla_176134_72366_3203_input$pre_id,flywire_neuron_name$seg_id)]


# writing the connectivity tables back to google sheets
sheet_write(Putative_Lobula_to_medulla_176134_72366_3203_output, ss="1w2QmqlxvEo5RdDe7YeJOEFyd_CYDTXcSOMwTML1DH9Q", sheet = "output")
sheet_write(Putative_Lobula_to_medulla_176134_72366_3203_input, ss="1w2QmqlxvEo5RdDe7YeJOEFyd_CYDTXcSOMwTML1DH9Q", sheet = "input")
# sheet_write(Putative_Lobula_to_medulla_176134_72366_3203_exclude, ss= "1w2QmqlxvEo5RdDe7YeJOEFyd_CYDTXcSOMwTML1DH9Q", sheet = "exclude")




# LC14b input

# ------------------------------------------------------------------------
# T2-a Samra 1 --------------------------------- Last run 30/01/2023
# Putative_T2a_193425_79506_5458

# First T2-a : https://docs.google.com/spreadsheets/d/14SDD_5dUhgeyKU-i-xysdONtFdUeIOB1r93BTlWMe4U/edit?usp=sharing
Putative_T2a_193425_79506_5458_id = "720575940627318683"

# getting the newest ID if changes happened
Putative_T2a_193425_79506_5458_id = flywire_latestid(Putative_T2a_193425_79506_5458_id)

# read mesh from flywire
Putative_T2a_193425_79506_5458_mesh = read_cloudvolume_meshes(Putative_T2a_193425_79506_5458_id) 


## DOWNSTREAM PARTNERS based on buhmann synapses 
Putative_T2a_193425_79506_5458_output = flywire_partner_summary(Putative_T2a_193425_79506_5458_id,partners = "output", cleft.threshold = 100)
Putative_T2a_193425_79506_5458_output$pre_id = Putative_T2a_193425_79506_5458_id

# check for the latest ID for each partner and add a flywire_URL
Putative_T2a_193425_79506_5458_output$partner_type = ""
Putative_T2a_193425_79506_5458_output$partner_name = ""
Putative_T2a_193425_79506_5458_output$flywire_URL = ""
for (i in 1:length(Putative_T2a_193425_79506_5458_output$post_id)){
  Putative_T2a_193425_79506_5458_output$post_id[i] = flywire_latestid(Putative_T2a_193425_79506_5458_output$post_id[i])
  Putative_T2a_193425_79506_5458_output$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_T2a_193425_79506_5458_output$pre_id[i],Putative_T2a_193425_79506_5458_output$post_id[i]))}","klickable_link")')
} 

# get names and types from flywire_neuron_names table
Putative_T2a_193425_79506_5458_output$partner_name = flywire_neuron_name$name[match(Putative_T2a_193425_79506_5458_output$post_id,flywire_neuron_name$seg_id)]
Putative_T2a_193425_79506_5458_output$partner_type = flywire_neuron_name$type[match(Putative_T2a_193425_79506_5458_output$post_id,flywire_neuron_name$seg_id)]



## UPSTREAM PARTNERS based on buhmann synapses 
Putative_T2a_193425_79506_5458_input = flywire_partner_summary(Putative_T2a_193425_79506_5458_id,partners = "input", cleft.threshold = 100)
Putative_T2a_193425_79506_5458_input$post_id = Putative_T2a_193425_79506_5458_id

# check for the latest ID for each partner and add a flywire_URL
Putative_T2a_193425_79506_5458_input$partner_type = ""
Putative_T2a_193425_79506_5458_input$partner_name = ""
Putative_T2a_193425_79506_5458_input$flywire_URL = ""
for (i in 1:length(Putative_T2a_193425_79506_5458_input$pre_id)){
  Putative_T2a_193425_79506_5458_input$pre_id[i] = flywire_latestid(Putative_T2a_193425_79506_5458_input$pre_id[i])
  Putative_T2a_193425_79506_5458_input$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_T2a_193425_79506_5458_input$post_id[i],Putative_T2a_193425_79506_5458_input$pre_id[i]))}","klickable_link")')
} 

# get names 
Putative_T2a_193425_79506_5458_input$partner_name = flywire_neuron_name$name[match(Putative_T2a_193425_79506_5458_input$pre_id,flywire_neuron_name$seg_id)]
Putative_T2a_193425_79506_5458_input$partner_type = flywire_neuron_name$type[match(Putative_T2a_193425_79506_5458_input$pre_id,flywire_neuron_name$seg_id)]



# writing the connectivity tables back to google sheets
sheet_write(Putative_T2a_193425_79506_5458_output, ss="14SDD_5dUhgeyKU-i-xysdONtFdUeIOB1r93BTlWMe4U", sheet = "output")
sheet_write(Putative_T2a_193425_79506_5458_input, ss="14SDD_5dUhgeyKU-i-xysdONtFdUeIOB1r93BTlWMe4U", sheet = "input")
# sheet_write(Putative_T2a_193425_79506_5458_exclude, ss= "14SDD_5dUhgeyKU-i-xysdONtFdUeIOB1r93BTlWMe4U", sheet = "exclude")




# ------------------------------------------------------------------------
# T2-a Samra 2 --------------------------------- Last run 30/01/2023
# Putative_T2a_191926_81565_5405

# Second T2-a : https://docs.google.com/spreadsheets/d/1O0iAsJ1r-3akZ3TA9nWIMJ6cub2ckXvq5A7Urh9-DFk/edit?usp=sharing
Putative_T2a_191926_81565_5405_id = "720575940622434539"

# getting the newest ID if changes happened
Putative_T2a_191926_81565_5405_id = flywire_latestid(Putative_T2a_191926_81565_5405_id)

# read mesh from flywire
Putative_T2a_191926_81565_5405_mesh = read_cloudvolume_meshes(Putative_T2a_191926_81565_5405_id) 


## DOWNSTREAM PARTNERS based on buhmann synapses 
Putative_T2a_191926_81565_5405_output = flywire_partner_summary(Putative_T2a_191926_81565_5405_id,partners = "output", cleft.threshold = 100)
Putative_T2a_191926_81565_5405_output$pre_id = Putative_T2a_191926_81565_5405_id

# check for the latest ID for each partner and add a flywire_URL
Putative_T2a_191926_81565_5405_output$partner_type = ""
Putative_T2a_191926_81565_5405_output$partner_name = ""
Putative_T2a_191926_81565_5405_output$flywire_URL = ""
for (i in 1:length(Putative_T2a_191926_81565_5405_output$post_id)){
  Putative_T2a_191926_81565_5405_output$post_id[i] = flywire_latestid(Putative_T2a_191926_81565_5405_output$post_id[i])
  Putative_T2a_191926_81565_5405_output$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_T2a_191926_81565_5405_output$pre_id[i],Putative_T2a_191926_81565_5405_output$post_id[i]))}","klickable_link")')
} 

# get names and types from flywire_neuron_names table
Putative_T2a_191926_81565_5405_output$partner_name = flywire_neuron_name$name[match(Putative_T2a_191926_81565_5405_output$post_id,flywire_neuron_name$seg_id)]
Putative_T2a_191926_81565_5405_output$partner_type = flywire_neuron_name$type[match(Putative_T2a_191926_81565_5405_output$post_id,flywire_neuron_name$seg_id)]



## UPSTREAM PARTNERS based on buhmann synapses 
Putative_T2a_191926_81565_5405_input = flywire_partner_summary(Putative_T2a_191926_81565_5405_id,partners = "input", cleft.threshold = 100)
Putative_T2a_191926_81565_5405_input$post_id = Putative_T2a_191926_81565_5405_id

# check for the latest ID for each partner and add a flywire_URL
Putative_T2a_191926_81565_5405_input$partner_type = ""
Putative_T2a_191926_81565_5405_input$partner_name = ""
Putative_T2a_191926_81565_5405_input$flywire_URL = ""
for (i in 1:length(Putative_T2a_191926_81565_5405_input$pre_id)){
  Putative_T2a_191926_81565_5405_input$pre_id[i] = flywire_latestid(Putative_T2a_191926_81565_5405_input$pre_id[i])
  Putative_T2a_191926_81565_5405_input$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_T2a_191926_81565_5405_input$post_id[i],Putative_T2a_191926_81565_5405_input$pre_id[i]))}","klickable_link")')
} 

# get names 
Putative_T2a_191926_81565_5405_input$partner_name = flywire_neuron_name$name[match(Putative_T2a_191926_81565_5405_input$pre_id,flywire_neuron_name$seg_id)]
Putative_T2a_191926_81565_5405_input$partner_type = flywire_neuron_name$type[match(Putative_T2a_191926_81565_5405_input$pre_id,flywire_neuron_name$seg_id)]



# writing the connectivity tables back to google sheets
sheet_write(Putative_T2a_191926_81565_5405_output, ss="1O0iAsJ1r-3akZ3TA9nWIMJ6cub2ckXvq5A7Urh9-DFk", sheet = "output")
sheet_write(Putative_T2a_191926_81565_5405_input, ss="1O0iAsJ1r-3akZ3TA9nWIMJ6cub2ckXvq5A7Urh9-DFk", sheet = "input")
# sheet_write(Putative_T2a_191926_81565_5405_exclude, ss= "1O0iAsJ1r-3akZ3TA9nWIMJ6cub2ckXvq5A7Urh9-DFk", sheet = "exclude")






# ------------------------------------------------------------------------
# T2-a Samra 3 --------------------------------- Last run 30/01/2023
# Putative_T2a_68142_77422_6799

# Third T2-a : https://docs.google.com/spreadsheets/d/1hlRSA5KpkbMeRvQb_rF-RTEVwWzb1uEqJTLtzA7L4wg/edit?usp=sharing
Putative_T2a_68142_77422_6799_id = "720575940632787391"

# getting the newest ID if changes happened
Putative_T2a_68142_77422_6799_id = flywire_latestid(Putative_T2a_68142_77422_6799_id)

# read mesh from flywire
Putative_T2a_68142_77422_6799_mesh = read_cloudvolume_meshes(Putative_T2a_68142_77422_6799_id) 


## DOWNSTREAM PARTNERS based on buhmann synapses 
Putative_T2a_68142_77422_6799_output = flywire_partner_summary(Putative_T2a_68142_77422_6799_id,partners = "output", cleft.threshold = 100)
Putative_T2a_68142_77422_6799_output$pre_id = Putative_T2a_68142_77422_6799_id

# check for the latest ID for each partner and add a flywire_URL
Putative_T2a_68142_77422_6799_output$partner_type = ""
Putative_T2a_68142_77422_6799_output$partner_name = ""
Putative_T2a_68142_77422_6799_output$flywire_URL = ""
for (i in 1:length(Putative_T2a_68142_77422_6799_output$post_id)){
  Putative_T2a_68142_77422_6799_output$post_id[i] = flywire_latestid(Putative_T2a_68142_77422_6799_output$post_id[i])
  Putative_T2a_68142_77422_6799_output$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_T2a_68142_77422_6799_output$pre_id[i],Putative_T2a_68142_77422_6799_output$post_id[i]))}","klickable_link")')
} 

# get names and types from flywire_neuron_names table
Putative_T2a_68142_77422_6799_output$partner_name = flywire_neuron_name$name[match(Putative_T2a_68142_77422_6799_output$post_id,flywire_neuron_name$seg_id)]
Putative_T2a_68142_77422_6799_output$partner_type = flywire_neuron_name$type[match(Putative_T2a_68142_77422_6799_output$post_id,flywire_neuron_name$seg_id)]



## UPSTREAM PARTNERS based on buhmann synapses 
Putative_T2a_68142_77422_6799_input = flywire_partner_summary(Putative_T2a_68142_77422_6799_id,partners = "input", cleft.threshold = 100)
Putative_T2a_68142_77422_6799_input$post_id = Putative_T2a_68142_77422_6799_id

# check for the latest ID for each partner and add a flywire_URL
Putative_T2a_68142_77422_6799_input$partner_type = ""
Putative_T2a_68142_77422_6799_input$partner_name = ""
Putative_T2a_68142_77422_6799_input$flywire_URL = ""
for (i in 1:length(Putative_T2a_68142_77422_6799_input$pre_id)){
  Putative_T2a_68142_77422_6799_input$pre_id[i] = flywire_latestid(Putative_T2a_68142_77422_6799_input$pre_id[i])
  Putative_T2a_68142_77422_6799_input$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_T2a_68142_77422_6799_input$post_id[i],Putative_T2a_68142_77422_6799_input$pre_id[i]))}","klickable_link")')
} 

# get names 
Putative_T2a_68142_77422_6799_input$partner_name = flywire_neuron_name$name[match(Putative_T2a_68142_77422_6799_input$pre_id,flywire_neuron_name$seg_id)]
Putative_T2a_68142_77422_6799_input$partner_type = flywire_neuron_name$type[match(Putative_T2a_68142_77422_6799_input$pre_id,flywire_neuron_name$seg_id)]




# writing the connectivity tables back to google sheets
sheet_write(Putative_T2a_68142_77422_6799_output, ss="1hlRSA5KpkbMeRvQb_rF-RTEVwWzb1uEqJTLtzA7L4wg", sheet = "output")
sheet_write(Putative_T2a_68142_77422_6799_input, ss="1hlRSA5KpkbMeRvQb_rF-RTEVwWzb1uEqJTLtzA7L4wg", sheet = "input")
# sheet_write(Putative_T2a_68142_77422_6799_exclude, ss= "1hlRSA5KpkbMeRvQb_rF-RTEVwWzb1uEqJTLtzA7L4wg", sheet = "exclude")


#T2a input

# ------------------------------------------------------------------------
# Mi1 Matanat 1 --------------------------------- Last run 30/01/2023
# Putative_Mi1_206478_85928_5325

# Third T2-a : https://docs.google.com/spreadsheets/d/1Eh8Wrkxk-rI_8HA7_TsUnFw6sHVFgcU9oNaR1TYMFP4/edit?usp=sharing
Putative_Mi1_206478_85928_5325_id = "720575940628748816"

# getting the newest ID if changes happened
Putative_Mi1_206478_85928_5325_id = flywire_latestid(Putative_Mi1_206478_85928_5325_id)

# read mesh from flywire
Putative_Mi1_206478_85928_5325_mesh = read_cloudvolume_meshes(Putative_Mi1_206478_85928_5325_id) 


## DOWNSTREAM PARTNERS based on buhmann synapses 
Putative_Mi1_206478_85928_5325_output = flywire_partner_summary(Putative_Mi1_206478_85928_5325_id,partners = "output", cleft.threshold = 100)
Putative_Mi1_206478_85928_5325_output$pre_id = Putative_Mi1_206478_85928_5325_id

# check for the latest ID for each partner and add a flywire_URL
Putative_Mi1_206478_85928_5325_output$partner_type = ""
Putative_Mi1_206478_85928_5325_output$partner_name = ""
Putative_Mi1_206478_85928_5325_output$flywire_URL = ""
for (i in 1:length(Putative_Mi1_206478_85928_5325_output$post_id)){
  Putative_Mi1_206478_85928_5325_output$post_id[i] = flywire_latestid(Putative_Mi1_206478_85928_5325_output$post_id[i])
  Putative_Mi1_206478_85928_5325_output$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_Mi1_206478_85928_5325_output$pre_id[i],Putative_Mi1_206478_85928_5325_output$post_id[i]))}","klickable_link")')
} 

# get names and types from flywire_neuron_names table
Putative_Mi1_206478_85928_5325_output$partner_name = flywire_neuron_name$name[match(Putative_Mi1_206478_85928_5325_output$post_id,flywire_neuron_name$seg_id)]
Putative_Mi1_206478_85928_5325_output$partner_type = flywire_neuron_name$type[match(Putative_Mi1_206478_85928_5325_output$post_id,flywire_neuron_name$seg_id)]



## UPSTREAM PARTNERS based on buhmann synapses 
Putative_Mi1_206478_85928_5325_input = flywire_partner_summary(Putative_Mi1_206478_85928_5325_id,partners = "input", cleft.threshold = 100)
Putative_Mi1_206478_85928_5325_input$post_id = Putative_Mi1_206478_85928_5325_id

# check for the latest ID for each partner and add a flywire_URL
Putative_Mi1_206478_85928_5325_input$partner_type = ""
Putative_Mi1_206478_85928_5325_input$partner_name = ""
Putative_Mi1_206478_85928_5325_input$flywire_URL = ""
for (i in 1:length(Putative_Mi1_206478_85928_5325_input$pre_id)){
  Putative_Mi1_206478_85928_5325_input$pre_id[i] = flywire_latestid(Putative_Mi1_206478_85928_5325_input$pre_id[i])
  Putative_Mi1_206478_85928_5325_input$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_Mi1_206478_85928_5325_input$post_id[i],Putative_Mi1_206478_85928_5325_input$pre_id[i]))}","klickable_link")')
} 

# get names 
Putative_Mi1_206478_85928_5325_input$partner_name = flywire_neuron_name$name[match(Putative_Mi1_206478_85928_5325_input$pre_id,flywire_neuron_name$seg_id)]
Putative_Mi1_206478_85928_5325_input$partner_type = flywire_neuron_name$type[match(Putative_Mi1_206478_85928_5325_input$pre_id,flywire_neuron_name$seg_id)]




# writing the connectivity tables back to google sheets
sheet_write(Putative_Mi1_206478_85928_5325_output, ss="1Eh8Wrkxk-rI_8HA7_TsUnFw6sHVFgcU9oNaR1TYMFP4", sheet = "output")
sheet_write(Putative_Mi1_206478_85928_5325_input, ss="1Eh8Wrkxk-rI_8HA7_TsUnFw6sHVFgcU9oNaR1TYMFP4", sheet = "input")
# sheet_write(Putative_Mi1_206478_85928_5325_exclude, ss= "1Eh8Wrkxk-rI_8HA7_TsUnFw6sHVFgcU9oNaR1TYMFP4", sheet = "exclude")



# ------------------------------------------------------------------------
# Mi1 Matanat 2 --------------------------------- Last run 30/01/2023
# Putative_Mi1_207231_86367_5047

# 2 Mi2 : https://docs.google.com/spreadsheets/d/18-egDavLW-pQ6Cq6E2_Y_u7W0z1lsRCfvqIFPGsY2VA/edit?usp=sharing
Putative_Mi1_207231_86367_5047_id = "720575940629588891"

# getting the newest ID if changes happened
Putative_Mi1_207231_86367_5047_id = flywire_latestid(Putative_Mi1_207231_86367_5047_id)

# read mesh from flywire
Putative_Mi1_207231_86367_5047_mesh = read_cloudvolume_meshes(Putative_Mi1_207231_86367_5047_id) 


## DOWNSTREAM PARTNERS based on buhmann synapses 
Putative_Mi1_207231_86367_5047_output = flywire_partner_summary(Putative_Mi1_207231_86367_5047_id,partners = "output", cleft.threshold = 100)
Putative_Mi1_207231_86367_5047_output$pre_id = Putative_Mi1_207231_86367_5047_id

# check for the latest ID for each partner and add a flywire_URL
Putative_Mi1_207231_86367_5047_output$partner_type = ""
Putative_Mi1_207231_86367_5047_output$partner_name = ""
Putative_Mi1_207231_86367_5047_output$flywire_URL = ""
for (i in 1:length(Putative_Mi1_207231_86367_5047_output$post_id)){
  Putative_Mi1_207231_86367_5047_output$post_id[i] = flywire_latestid(Putative_Mi1_207231_86367_5047_output$post_id[i])
  Putative_Mi1_207231_86367_5047_output$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_Mi1_207231_86367_5047_output$pre_id[i],Putative_Mi1_207231_86367_5047_output$post_id[i]))}","klickable_link")')
} 

# get names and types from flywire_neuron_names table
Putative_Mi1_207231_86367_5047_output$partner_name = flywire_neuron_name$name[match(Putative_Mi1_207231_86367_5047_output$post_id,flywire_neuron_name$seg_id)]
Putative_Mi1_207231_86367_5047_output$partner_type = flywire_neuron_name$type[match(Putative_Mi1_207231_86367_5047_output$post_id,flywire_neuron_name$seg_id)]



## UPSTREAM PARTNERS based on buhmann synapses 
Putative_Mi1_207231_86367_5047_input = flywire_partner_summary(Putative_Mi1_207231_86367_5047_id,partners = "input", cleft.threshold = 100)
Putative_Mi1_207231_86367_5047_input$post_id = Putative_Mi1_207231_86367_5047_id

# check for the latest ID for each partner and add a flywire_URL
Putative_Mi1_207231_86367_5047_input$partner_type = ""
Putative_Mi1_207231_86367_5047_input$partner_name = ""
Putative_Mi1_207231_86367_5047_input$flywire_URL = ""
for (i in 1:length(Putative_Mi1_207231_86367_5047_input$pre_id)){
  Putative_Mi1_207231_86367_5047_input$pre_id[i] = flywire_latestid(Putative_Mi1_207231_86367_5047_input$pre_id[i])
  Putative_Mi1_207231_86367_5047_input$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_Mi1_207231_86367_5047_input$post_id[i],Putative_Mi1_207231_86367_5047_input$pre_id[i]))}","klickable_link")')
} 

# get names 
Putative_Mi1_207231_86367_5047_input$partner_name = flywire_neuron_name$name[match(Putative_Mi1_207231_86367_5047_input$pre_id,flywire_neuron_name$seg_id)]
Putative_Mi1_207231_86367_5047_input$partner_type = flywire_neuron_name$type[match(Putative_Mi1_207231_86367_5047_input$pre_id,flywire_neuron_name$seg_id)]




# writing the connectivity tables back to google sheets
sheet_write(Putative_Mi1_207231_86367_5047_output, ss="18-egDavLW-pQ6Cq6E2_Y_u7W0z1lsRCfvqIFPGsY2VA", sheet = "output")
sheet_write(Putative_Mi1_207231_86367_5047_input, ss="18-egDavLW-pQ6Cq6E2_Y_u7W0z1lsRCfvqIFPGsY2VA", sheet = "input")
# sheet_write(Putative_Mi1_207231_86367_5047_exclude, ss= "18-egDavLW-pQ6Cq6E2_Y_u7W0z1lsRCfvqIFPGsY2VA", sheet = "exclude")



# ------------------------------------------------------------------------
# Mi1 Matanat 3 --------------------------------- Last run 30/01/2023
# Putative_Mi1_208741_85071_5001

# 3 Mi2 : https://docs.google.com/spreadsheets/d/1dWmII5aH69R8140KBd3XYDCcvZnKTxRarHC5JprvNSs/edit?usp=sharing
Putative_Mi1_208741_85071_5001_id = "720575940637571812"

# getting the newest ID if changes happened
Putative_Mi1_208741_85071_5001_id = flywire_latestid(Putative_Mi1_208741_85071_5001_id)

# read mesh from flywire
Putative_Mi1_208741_85071_5001_mesh = read_cloudvolume_meshes(Putative_Mi1_208741_85071_5001_id) 


## DOWNSTREAM PARTNERS based on buhmann synapses 
Putative_Mi1_208741_85071_5001_output = flywire_partner_summary(Putative_Mi1_208741_85071_5001_id,partners = "output", cleft.threshold = 100)
Putative_Mi1_208741_85071_5001_output$pre_id = Putative_Mi1_208741_85071_5001_id

# check for the latest ID for each partner and add a flywire_URL
Putative_Mi1_208741_85071_5001_output$partner_type = ""
Putative_Mi1_208741_85071_5001_output$partner_name = ""
Putative_Mi1_208741_85071_5001_output$flywire_URL = ""
for (i in 1:length(Putative_Mi1_208741_85071_5001_output$post_id)){
  Putative_Mi1_208741_85071_5001_output$post_id[i] = flywire_latestid(Putative_Mi1_208741_85071_5001_output$post_id[i])
  Putative_Mi1_208741_85071_5001_output$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_Mi1_208741_85071_5001_output$pre_id[i],Putative_Mi1_208741_85071_5001_output$post_id[i]))}","klickable_link")')
} 

# get names and types from flywire_neuron_names table
Putative_Mi1_208741_85071_5001_output$partner_name = flywire_neuron_name$name[match(Putative_Mi1_208741_85071_5001_output$post_id,flywire_neuron_name$seg_id)]
Putative_Mi1_208741_85071_5001_output$partner_type = flywire_neuron_name$type[match(Putative_Mi1_208741_85071_5001_output$post_id,flywire_neuron_name$seg_id)]



## UPSTREAM PARTNERS based on buhmann synapses 
Putative_Mi1_208741_85071_5001_input = flywire_partner_summary(Putative_Mi1_208741_85071_5001_id,partners = "input", cleft.threshold = 100)
Putative_Mi1_208741_85071_5001_input$post_id = Putative_Mi1_208741_85071_5001_id

# check for the latest ID for each partner and add a flywire_URL
Putative_Mi1_208741_85071_5001_input$partner_type = ""
Putative_Mi1_208741_85071_5001_input$partner_name = ""
Putative_Mi1_208741_85071_5001_input$flywire_URL = ""
for (i in 1:length(Putative_Mi1_208741_85071_5001_input$pre_id)){
  Putative_Mi1_208741_85071_5001_input$pre_id[i] = flywire_latestid(Putative_Mi1_208741_85071_5001_input$pre_id[i])
  Putative_Mi1_208741_85071_5001_input$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_Mi1_208741_85071_5001_input$post_id[i],Putative_Mi1_208741_85071_5001_input$pre_id[i]))}","klickable_link")')
} 

# get names 
Putative_Mi1_208741_85071_5001_input$partner_name = flywire_neuron_name$name[match(Putative_Mi1_208741_85071_5001_input$pre_id,flywire_neuron_name$seg_id)]
Putative_Mi1_208741_85071_5001_input$partner_type = flywire_neuron_name$type[match(Putative_Mi1_208741_85071_5001_input$pre_id,flywire_neuron_name$seg_id)]




# writing the connectivity tables back to google sheets
sheet_write(Putative_Mi1_208741_85071_5001_output, ss="1dWmII5aH69R8140KBd3XYDCcvZnKTxRarHC5JprvNSs", sheet = "output")
sheet_write(Putative_Mi1_208741_85071_5001_input, ss="1dWmII5aH69R8140KBd3XYDCcvZnKTxRarHC5JprvNSs", sheet = "input")
# sheet_write(Putative_Mi1_208741_85071_5001_exclude, ss= "1dWmII5aH69R8140KBd3XYDCcvZnKTxRarHC5JprvNSs", sheet = "exclude")



# LC14b output




# ------------------------------------------------------------------------
# Li2 Samra 1 --------------------------------- Last run 30/01/2023
# Putative_Li2_180762_71427_3783

# 1 Li2 : https://docs.google.com/spreadsheets/d/1wu20XyoN0DCTVE_efCh1r5lK7fjikJzmr1YI6OA4SL4/edit?usp=sharing
Putative_Li2_180762_71427_3783_id = "720575940615926489"

# getting the newest ID if changes happened
Putative_Li2_180762_71427_3783_id = flywire_latestid(Putative_Li2_180762_71427_3783_id)

# read mesh from flywire
Putative_Li2_180762_71427_3783_mesh = read_cloudvolume_meshes(Putative_Li2_180762_71427_3783_id) 


## DOWNSTREAM PARTNERS based on buhmann synapses 
Putative_Li2_180762_71427_3783_output = flywire_partner_summary(Putative_Li2_180762_71427_3783_id,partners = "output", cleft.threshold = 100)
Putative_Li2_180762_71427_3783_output$pre_id = Putative_Li2_180762_71427_3783_id

# check for the latest ID for each partner and add a flywire_URL
Putative_Li2_180762_71427_3783_output$partner_type = ""
Putative_Li2_180762_71427_3783_output$partner_name = ""
Putative_Li2_180762_71427_3783_output$flywire_URL = ""
for (i in 1:length(Putative_Li2_180762_71427_3783_output$post_id)){
  Putative_Li2_180762_71427_3783_output$post_id[i] = flywire_latestid(Putative_Li2_180762_71427_3783_output$post_id[i])
  Putative_Li2_180762_71427_3783_output$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_Li2_180762_71427_3783_output$pre_id[i],Putative_Li2_180762_71427_3783_output$post_id[i]))}","klickable_link")')
} 

# get names and types from flywire_neuron_names table
Putative_Li2_180762_71427_3783_output$partner_name = flywire_neuron_name$name[match(Putative_Li2_180762_71427_3783_output$post_id,flywire_neuron_name$seg_id)]
Putative_Li2_180762_71427_3783_output$partner_type = flywire_neuron_name$type[match(Putative_Li2_180762_71427_3783_output$post_id,flywire_neuron_name$seg_id)]



## UPSTREAM PARTNERS based on buhmann synapses 
Putative_Li2_180762_71427_3783_input = flywire_partner_summary(Putative_Li2_180762_71427_3783_id,partners = "input", cleft.threshold = 100)
Putative_Li2_180762_71427_3783_input$post_id = Putative_Li2_180762_71427_3783_id

# check for the latest ID for each partner and add a flywire_URL
Putative_Li2_180762_71427_3783_input$partner_type = ""
Putative_Li2_180762_71427_3783_input$partner_name = ""
Putative_Li2_180762_71427_3783_input$flywire_URL = ""
for (i in 1:length(Putative_Li2_180762_71427_3783_input$pre_id)){
  Putative_Li2_180762_71427_3783_input$pre_id[i] = flywire_latestid(Putative_Li2_180762_71427_3783_input$pre_id[i])
  Putative_Li2_180762_71427_3783_input$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_Li2_180762_71427_3783_input$post_id[i],Putative_Li2_180762_71427_3783_input$pre_id[i]))}","klickable_link")')
} 

# get names 
Putative_Li2_180762_71427_3783_input$partner_name = flywire_neuron_name$name[match(Putative_Li2_180762_71427_3783_input$pre_id,flywire_neuron_name$seg_id)]
Putative_Li2_180762_71427_3783_input$partner_type = flywire_neuron_name$type[match(Putative_Li2_180762_71427_3783_input$pre_id,flywire_neuron_name$seg_id)]




# writing the connectivity tables back to google sheets
sheet_write(Putative_Li2_180762_71427_3783_output, ss="1wu20XyoN0DCTVE_efCh1r5lK7fjikJzmr1YI6OA4SL4", sheet = "output")
sheet_write(Putative_Li2_180762_71427_3783_input, ss="1wu20XyoN0DCTVE_efCh1r5lK7fjikJzmr1YI6OA4SL4", sheet = "input")
# sheet_write(Putative_Li2_180762_71427_3783_exclude, ss= "1wu20XyoN0DCTVE_efCh1r5lK7fjikJzmr1YI6OA4SL4", sheet = "exclude")



# ------------------------------------------------------------------------
# Li2 Samra 2 --------------------------------- Last run 30/01/2023
# Putative_Li2M_81942_68905_4656

# 1 Li2 : https://docs.google.com/spreadsheets/d/1ilnwXpwF2e_kwW88BwbVrRgzZAxUof4oqNoADNHg0Og/edit?usp=sharing
Putative_Li2M_81942_68905_4656_id = "720575940616293259"

# getting the newest ID if changes happened
Putative_Li2M_81942_68905_4656_id = flywire_latestid(Putative_Li2M_81942_68905_4656_id)

# read mesh from flywire
Putative_Li2M_81942_68905_4656_mesh = read_cloudvolume_meshes(Putative_Li2M_81942_68905_4656_id) 


## DOWNSTREAM PARTNERS based on buhmann synapses 
Putative_Li2M_81942_68905_4656_output = flywire_partner_summary(Putative_Li2M_81942_68905_4656_id,partners = "output", cleft.threshold = 100)
Putative_Li2M_81942_68905_4656_output$pre_id = Putative_Li2M_81942_68905_4656_id

# check for the latest ID for each partner and add a flywire_URL
Putative_Li2M_81942_68905_4656_output$partner_type = ""
Putative_Li2M_81942_68905_4656_output$partner_name = ""
Putative_Li2M_81942_68905_4656_output$flywire_URL = ""
for (i in 1:length(Putative_Li2M_81942_68905_4656_output$post_id)){
  Putative_Li2M_81942_68905_4656_output$post_id[i] = flywire_latestid(Putative_Li2M_81942_68905_4656_output$post_id[i])
  Putative_Li2M_81942_68905_4656_output$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_Li2M_81942_68905_4656_output$pre_id[i],Putative_Li2M_81942_68905_4656_output$post_id[i]))}","klickable_link")')
} 

# get names and types from flywire_neuron_names table
Putative_Li2M_81942_68905_4656_output$partner_name = flywire_neuron_name$name[match(Putative_Li2M_81942_68905_4656_output$post_id,flywire_neuron_name$seg_id)]
Putative_Li2M_81942_68905_4656_output$partner_type = flywire_neuron_name$type[match(Putative_Li2M_81942_68905_4656_output$post_id,flywire_neuron_name$seg_id)]



## UPSTREAM PARTNERS based on buhmann synapses 
Putative_Li2M_81942_68905_4656_input = flywire_partner_summary(Putative_Li2M_81942_68905_4656_id,partners = "input", cleft.threshold = 100)
Putative_Li2M_81942_68905_4656_input$post_id = Putative_Li2M_81942_68905_4656_id

# check for the latest ID for each partner and add a flywire_URL
Putative_Li2M_81942_68905_4656_input$partner_type = ""
Putative_Li2M_81942_68905_4656_input$partner_name = ""
Putative_Li2M_81942_68905_4656_input$flywire_URL = ""
for (i in 1:length(Putative_Li2M_81942_68905_4656_input$pre_id)){
  Putative_Li2M_81942_68905_4656_input$pre_id[i] = flywire_latestid(Putative_Li2M_81942_68905_4656_input$pre_id[i])
  Putative_Li2M_81942_68905_4656_input$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_Li2M_81942_68905_4656_input$post_id[i],Putative_Li2M_81942_68905_4656_input$pre_id[i]))}","klickable_link")')
} 

# get names 
Putative_Li2M_81942_68905_4656_input$partner_name = flywire_neuron_name$name[match(Putative_Li2M_81942_68905_4656_input$pre_id,flywire_neuron_name$seg_id)]
Putative_Li2M_81942_68905_4656_input$partner_type = flywire_neuron_name$type[match(Putative_Li2M_81942_68905_4656_input$pre_id,flywire_neuron_name$seg_id)]




# writing the connectivity tables back to google sheets
sheet_write(Putative_Li2M_81942_68905_4656_output, ss="1ilnwXpwF2e_kwW88BwbVrRgzZAxUof4oqNoADNHg0Og", sheet = "output")
sheet_write(Putative_Li2M_81942_68905_4656_input, ss="1ilnwXpwF2e_kwW88BwbVrRgzZAxUof4oqNoADNHg0Og", sheet = "input")
# sheet_write(Putative_Li2M_81942_68905_4656_exclude, ss= "1ilnwXpwF2e_kwW88BwbVrRgzZAxUof4oqNoADNHg0Og", sheet = "exclude")



# ------------------------------------------------------------------------
# Li2 Samra 3 --------------------------------- Last run 31/01/2023
# Putative_Li2XXL_93821_56473_4787

# 1 Li2 : https://docs.google.com/spreadsheets/d/11yIkts2DqMR6i54d64t4T4K-lBuaIdtla6Yy54AaQs8/edit?usp=sharing
Putative_Li2XXL_93821_56473_4787_id = "720575940615179074"

# getting the newest ID if changes happened
Putative_Li2XXL_93821_56473_4787_id = flywire_latestid(Putative_Li2XXL_93821_56473_4787_id)

# read mesh from flywire
Putative_Li2XXL_93821_56473_4787_mesh = read_cloudvolume_meshes(Putative_Li2XXL_93821_56473_4787_id) 


## DOWNSTREAM PARTNERS based on buhmann synapses 
Putative_Li2XXL_93821_56473_4787_output = flywire_partner_summary(Putative_Li2XXL_93821_56473_4787_id,partners = "output", cleft.threshold = 100)
Putative_Li2XXL_93821_56473_4787_output$pre_id = Putative_Li2XXL_93821_56473_4787_id

# check for the latest ID for each partner and add a flywire_URL
Putative_Li2XXL_93821_56473_4787_output$partner_type = ""
Putative_Li2XXL_93821_56473_4787_output$partner_name = ""
Putative_Li2XXL_93821_56473_4787_output$flywire_URL = ""
for (i in 1:length(Putative_Li2XXL_93821_56473_4787_output$post_id)){
  Putative_Li2XXL_93821_56473_4787_output$post_id[i] = flywire_latestid(Putative_Li2XXL_93821_56473_4787_output$post_id[i])
  Putative_Li2XXL_93821_56473_4787_output$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_Li2XXL_93821_56473_4787_output$pre_id[i],Putative_Li2XXL_93821_56473_4787_output$post_id[i]))}","klickable_link")')
} 

# get names and types from flywire_neuron_names table
Putative_Li2XXL_93821_56473_4787_output$partner_name = flywire_neuron_name$name[match(Putative_Li2XXL_93821_56473_4787_output$post_id,flywire_neuron_name$seg_id)]
Putative_Li2XXL_93821_56473_4787_output$partner_type = flywire_neuron_name$type[match(Putative_Li2XXL_93821_56473_4787_output$post_id,flywire_neuron_name$seg_id)]



## UPSTREAM PARTNERS based on buhmann synapses 
Putative_Li2XXL_93821_56473_4787_input = flywire_partner_summary(Putative_Li2XXL_93821_56473_4787_id,partners = "input", cleft.threshold = 100)
Putative_Li2XXL_93821_56473_4787_input$post_id = Putative_Li2XXL_93821_56473_4787_id

# check for the latest ID for each partner and add a flywire_URL
Putative_Li2XXL_93821_56473_4787_input$partner_type = ""
Putative_Li2XXL_93821_56473_4787_input$partner_name = ""
Putative_Li2XXL_93821_56473_4787_input$flywire_URL = ""
for (i in 1:length(Putative_Li2XXL_93821_56473_4787_input$pre_id)){
  Putative_Li2XXL_93821_56473_4787_input$pre_id[i] = flywire_latestid(Putative_Li2XXL_93821_56473_4787_input$pre_id[i])
  Putative_Li2XXL_93821_56473_4787_input$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_Li2XXL_93821_56473_4787_input$post_id[i],Putative_Li2XXL_93821_56473_4787_input$pre_id[i]))}","klickable_link")')
} 

# get names 
Putative_Li2XXL_93821_56473_4787_input$partner_name = flywire_neuron_name$name[match(Putative_Li2XXL_93821_56473_4787_input$pre_id,flywire_neuron_name$seg_id)]
Putative_Li2XXL_93821_56473_4787_input$partner_type = flywire_neuron_name$type[match(Putative_Li2XXL_93821_56473_4787_input$pre_id,flywire_neuron_name$seg_id)]




# writing the connectivity tables back to google sheets
sheet_write(Putative_Li2XXL_93821_56473_4787_output, ss="11yIkts2DqMR6i54d64t4T4K-lBuaIdtla6Yy54AaQs8", sheet = "output")
sheet_write(Putative_Li2XXL_93821_56473_4787_input, ss="11yIkts2DqMR6i54d64t4T4K-lBuaIdtla6Yy54AaQs8", sheet = "input")
# sheet_write(Putative_Li2XXL_93821_56473_4787_exclude, ss= "11yIkts2DqMR6i54d64t4T4K-lBuaIdtla6Yy54AaQs8", sheet = "exclude")



# ------------------------------------------------------------------------
# LC17 Matanat 1 --------------------------------- Last run 31/01/2023
# Putative_LC17_180383_68074_3564

# 1 LC17 : https://docs.google.com/spreadsheets/d/1aRy0Zvfhc4OHXm4Ep5d8SNPpsYL-k5v97TVZkdyNvmo/edit?usp=sharing
Putative_LC17_180383_68074_3564_id = "720575940613168534"

# getting the newest ID if changes happened
Putative_LC17_180383_68074_3564_id = flywire_latestid(Putative_LC17_180383_68074_3564_id)

# read mesh from flywire
Putative_LC17_180383_68074_3564_mesh = read_cloudvolume_meshes(Putative_LC17_180383_68074_3564_id) 


## DOWNSTREAM PARTNERS based on buhmann synapses 
Putative_LC17_180383_68074_3564_output = flywire_partner_summary(Putative_LC17_180383_68074_3564_id,partners = "output", cleft.threshold = 100)
Putative_LC17_180383_68074_3564_output$pre_id = Putative_LC17_180383_68074_3564_id

# check for the latest ID for each partner and add a flywire_URL
Putative_LC17_180383_68074_3564_output$partner_type = ""
Putative_LC17_180383_68074_3564_output$partner_name = ""
Putative_LC17_180383_68074_3564_output$flywire_URL = ""
for (i in 1:length(Putative_LC17_180383_68074_3564_output$post_id)){
  Putative_LC17_180383_68074_3564_output$post_id[i] = flywire_latestid(Putative_LC17_180383_68074_3564_output$post_id[i])
  Putative_LC17_180383_68074_3564_output$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_LC17_180383_68074_3564_output$pre_id[i],Putative_LC17_180383_68074_3564_output$post_id[i]))}","klickable_link")')
} 

# get names and types from flywire_neuron_names table
Putative_LC17_180383_68074_3564_output$partner_name = flywire_neuron_name$name[match(Putative_LC17_180383_68074_3564_output$post_id,flywire_neuron_name$seg_id)]
Putative_LC17_180383_68074_3564_output$partner_type = flywire_neuron_name$type[match(Putative_LC17_180383_68074_3564_output$post_id,flywire_neuron_name$seg_id)]



## UPSTREAM PARTNERS based on buhmann synapses 
Putative_LC17_180383_68074_3564_input = flywire_partner_summary(Putative_LC17_180383_68074_3564_id,partners = "input", cleft.threshold = 100)
Putative_LC17_180383_68074_3564_input$post_id = Putative_LC17_180383_68074_3564_id

# check for the latest ID for each partner and add a flywire_URL
Putative_LC17_180383_68074_3564_input$partner_type = ""
Putative_LC17_180383_68074_3564_input$partner_name = ""
Putative_LC17_180383_68074_3564_input$flywire_URL = ""
for (i in 1:length(Putative_LC17_180383_68074_3564_input$pre_id)){
  Putative_LC17_180383_68074_3564_input$pre_id[i] = flywire_latestid(Putative_LC17_180383_68074_3564_input$pre_id[i])
  Putative_LC17_180383_68074_3564_input$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_LC17_180383_68074_3564_input$post_id[i],Putative_LC17_180383_68074_3564_input$pre_id[i]))}","klickable_link")')
} 

# get names 
Putative_LC17_180383_68074_3564_input$partner_name = flywire_neuron_name$name[match(Putative_LC17_180383_68074_3564_input$pre_id,flywire_neuron_name$seg_id)]
Putative_LC17_180383_68074_3564_input$partner_type = flywire_neuron_name$type[match(Putative_LC17_180383_68074_3564_input$pre_id,flywire_neuron_name$seg_id)]




# writing the connectivity tables back to google sheets
sheet_write(Putative_LC17_180383_68074_3564_output, ss="1aRy0Zvfhc4OHXm4Ep5d8SNPpsYL-k5v97TVZkdyNvmo", sheet = "output")
sheet_write(Putative_LC17_180383_68074_3564_input, ss="1aRy0Zvfhc4OHXm4Ep5d8SNPpsYL-k5v97TVZkdyNvmo", sheet = "input")
# sheet_write(Putative_LC17_180383_68074_3564_exclude, ss= "1aRy0Zvfhc4OHXm4Ep5d8SNPpsYL-k5v97TVZkdyNvmo", sheet = "exclude")


# ------------------------------------------------------------------------
# LC17 Matanat 2 --------------------------------- Last run 31/01/2023
# Putative_LC17_91335_58232_4404

# 2 LC17 : https://docs.google.com/spreadsheets/d/1RpMerU9QQ9cJL8X8z6hrsTCyFPpEqmkhqYfP1w0v1Ek/edit?usp=sharing
Putative_LC17_91335_58232_4404_id = "720575940614430623"

# getting the newest ID if changes happened
Putative_LC17_91335_58232_4404_id = flywire_latestid(Putative_LC17_91335_58232_4404_id)

# read mesh from flywire
Putative_LC17_91335_58232_4404_mesh = read_cloudvolume_meshes(Putative_LC17_91335_58232_4404_id) 


## DOWNSTREAM PARTNERS based on buhmann synapses 
Putative_LC17_91335_58232_4404_output = flywire_partner_summary(Putative_LC17_91335_58232_4404_id,partners = "output", cleft.threshold = 100)
Putative_LC17_91335_58232_4404_output$pre_id = Putative_LC17_91335_58232_4404_id

# check for the latest ID for each partner and add a flywire_URL
Putative_LC17_91335_58232_4404_output$partner_type = ""
Putative_LC17_91335_58232_4404_output$partner_name = ""
Putative_LC17_91335_58232_4404_output$flywire_URL = ""
for (i in 1:length(Putative_LC17_91335_58232_4404_output$post_id)){
  Putative_LC17_91335_58232_4404_output$post_id[i] = flywire_latestid(Putative_LC17_91335_58232_4404_output$post_id[i])
  Putative_LC17_91335_58232_4404_output$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_LC17_91335_58232_4404_output$pre_id[i],Putative_LC17_91335_58232_4404_output$post_id[i]))}","klickable_link")')
} 

# get names and types from flywire_neuron_names table
Putative_LC17_91335_58232_4404_output$partner_name = flywire_neuron_name$name[match(Putative_LC17_91335_58232_4404_output$post_id,flywire_neuron_name$seg_id)]
Putative_LC17_91335_58232_4404_output$partner_type = flywire_neuron_name$type[match(Putative_LC17_91335_58232_4404_output$post_id,flywire_neuron_name$seg_id)]



## UPSTREAM PARTNERS based on buhmann synapses 
Putative_LC17_91335_58232_4404_input = flywire_partner_summary(Putative_LC17_91335_58232_4404_id,partners = "input", cleft.threshold = 100)
Putative_LC17_91335_58232_4404_input$post_id = Putative_LC17_91335_58232_4404_id

# check for the latest ID for each partner and add a flywire_URL
Putative_LC17_91335_58232_4404_input$partner_type = ""
Putative_LC17_91335_58232_4404_input$partner_name = ""
Putative_LC17_91335_58232_4404_input$flywire_URL = ""
for (i in 1:length(Putative_LC17_91335_58232_4404_input$pre_id)){
  Putative_LC17_91335_58232_4404_input$pre_id[i] = flywire_latestid(Putative_LC17_91335_58232_4404_input$pre_id[i])
  Putative_LC17_91335_58232_4404_input$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_LC17_91335_58232_4404_input$post_id[i],Putative_LC17_91335_58232_4404_input$pre_id[i]))}","klickable_link")')
} 

# get names 
Putative_LC17_91335_58232_4404_input$partner_name = flywire_neuron_name$name[match(Putative_LC17_91335_58232_4404_input$pre_id,flywire_neuron_name$seg_id)]
Putative_LC17_91335_58232_4404_input$partner_type = flywire_neuron_name$type[match(Putative_LC17_91335_58232_4404_input$pre_id,flywire_neuron_name$seg_id)]




# writing the connectivity tables back to google sheets
sheet_write(Putative_LC17_91335_58232_4404_output, ss="1RpMerU9QQ9cJL8X8z6hrsTCyFPpEqmkhqYfP1w0v1Ek", sheet = "output")
sheet_write(Putative_LC17_91335_58232_4404_input, ss="1RpMerU9QQ9cJL8X8z6hrsTCyFPpEqmkhqYfP1w0v1Ek", sheet = "input")
# sheet_write(Putative_LC17_91335_58232_4404_exclude, ss= "1RpMerU9QQ9cJL8X8z6hrsTCyFPpEqmkhqYfP1w0v1Ek", sheet = "exclude")


# ------------------------------------------------------------------------
# LC17 Matanat 3 --------------------------------- Last run 31/01/2023
# Putative_LC17_85460_67815_4300

# 3 LC17 : https://docs.google.com/spreadsheets/d/1QAav0adK0aMkHKRrtqJ3TT9wVQbenCnArWyL_PjYlng/edit?usp=sharing
Putative_LC17_85460_67815_4300_id = "720575940617708885"

# getting the newest ID if changes happened
Putative_LC17_85460_67815_4300_id = flywire_latestid(Putative_LC17_85460_67815_4300_id)

# read mesh from flywire
Putative_LC17_85460_67815_4300_mesh = read_cloudvolume_meshes(Putative_LC17_85460_67815_4300_id) 


## DOWNSTREAM PARTNERS based on buhmann synapses 
Putative_LC17_85460_67815_4300_output = flywire_partner_summary(Putative_LC17_85460_67815_4300_id,partners = "output", cleft.threshold = 100)
Putative_LC17_85460_67815_4300_output$pre_id = Putative_LC17_85460_67815_4300_id

# check for the latest ID for each partner and add a flywire_URL
Putative_LC17_85460_67815_4300_output$partner_type = ""
Putative_LC17_85460_67815_4300_output$partner_name = ""
Putative_LC17_85460_67815_4300_output$flywire_URL = ""
for (i in 1:length(Putative_LC17_85460_67815_4300_output$post_id)){
  Putative_LC17_85460_67815_4300_output$post_id[i] = flywire_latestid(Putative_LC17_85460_67815_4300_output$post_id[i])
  Putative_LC17_85460_67815_4300_output$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_LC17_85460_67815_4300_output$pre_id[i],Putative_LC17_85460_67815_4300_output$post_id[i]))}","klickable_link")')
} 

# get names and types from flywire_neuron_names table
Putative_LC17_85460_67815_4300_output$partner_name = flywire_neuron_name$name[match(Putative_LC17_85460_67815_4300_output$post_id,flywire_neuron_name$seg_id)]
Putative_LC17_85460_67815_4300_output$partner_type = flywire_neuron_name$type[match(Putative_LC17_85460_67815_4300_output$post_id,flywire_neuron_name$seg_id)]



## UPSTREAM PARTNERS based on buhmann synapses 
Putative_LC17_85460_67815_4300_input = flywire_partner_summary(Putative_LC17_85460_67815_4300_id,partners = "input", cleft.threshold = 100)
Putative_LC17_85460_67815_4300_input$post_id = Putative_LC17_85460_67815_4300_id

# check for the latest ID for each partner and add a flywire_URL
Putative_LC17_85460_67815_4300_input$partner_type = ""
Putative_LC17_85460_67815_4300_input$partner_name = ""
Putative_LC17_85460_67815_4300_input$flywire_URL = ""
for (i in 1:length(Putative_LC17_85460_67815_4300_input$pre_id)){
  Putative_LC17_85460_67815_4300_input$pre_id[i] = flywire_latestid(Putative_LC17_85460_67815_4300_input$pre_id[i])
  Putative_LC17_85460_67815_4300_input$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_LC17_85460_67815_4300_input$post_id[i],Putative_LC17_85460_67815_4300_input$pre_id[i]))}","klickable_link")')
} 

# get names 
Putative_LC17_85460_67815_4300_input$partner_name = flywire_neuron_name$name[match(Putative_LC17_85460_67815_4300_input$pre_id,flywire_neuron_name$seg_id)]
Putative_LC17_85460_67815_4300_input$partner_type = flywire_neuron_name$type[match(Putative_LC17_85460_67815_4300_input$pre_id,flywire_neuron_name$seg_id)]




# writing the connectivity tables back to google sheets
sheet_write(Putative_LC17_85460_67815_4300_output, ss="1QAav0adK0aMkHKRrtqJ3TT9wVQbenCnArWyL_PjYlng", sheet = "output")
sheet_write(Putative_LC17_85460_67815_4300_input, ss="1QAav0adK0aMkHKRrtqJ3TT9wVQbenCnArWyL_PjYlng", sheet = "input")
# sheet_write(Putative_LC17_85460_67815_4300_exclude, ss= "1QAav0adK0aMkHKRrtqJ3TT9wVQbenCnArWyL_PjYlng", sheet = "exclude")


#2st most important targets:


# LC14a input



# ------------------------------------------------------------------------
# Li1 Samra 1 --------------------------------- Last run 31/01/2023
# Putative_Li1_170637_66919_3787

# 1 Li2 : https://docs.google.com/spreadsheets/d/1GyHqc0AfipANkw0NJboHG40pJXbNPxQ15N-9_1GbTME/edit?usp=sharing
Putative_Li1_170637_66919_3787_id = "720575940634915696"

# getting the newest ID if changes happened
Putative_Li1_170637_66919_3787_id = flywire_latestid(Putative_Li1_170637_66919_3787_id)

# read mesh from flywire
Putative_Li1_170637_66919_3787_mesh = read_cloudvolume_meshes(Putative_Li1_170637_66919_3787_id) 


## DOWNSTREAM PARTNERS based on buhmann synapses 
Putative_Li1_170637_66919_3787_output = flywire_partner_summary(Putative_Li1_170637_66919_3787_id,partners = "output", cleft.threshold = 100)
Putative_Li1_170637_66919_3787_output$pre_id = Putative_Li1_170637_66919_3787_id

# check for the latest ID for each partner and add a flywire_URL
Putative_Li1_170637_66919_3787_output$partner_type = ""
Putative_Li1_170637_66919_3787_output$partner_name = ""
Putative_Li1_170637_66919_3787_output$flywire_URL = ""
for (i in 1:length(Putative_Li1_170637_66919_3787_output$post_id)){
  Putative_Li1_170637_66919_3787_output$post_id[i] = flywire_latestid(Putative_Li1_170637_66919_3787_output$post_id[i])
  Putative_Li1_170637_66919_3787_output$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_Li1_170637_66919_3787_output$pre_id[i],Putative_Li1_170637_66919_3787_output$post_id[i]))}","klickable_link")')
} 

# get names and types from flywire_neuron_names table
Putative_Li1_170637_66919_3787_output$partner_name = flywire_neuron_name$name[match(Putative_Li1_170637_66919_3787_output$post_id,flywire_neuron_name$seg_id)]
Putative_Li1_170637_66919_3787_output$partner_type = flywire_neuron_name$type[match(Putative_Li1_170637_66919_3787_output$post_id,flywire_neuron_name$seg_id)]



## UPSTREAM PARTNERS based on buhmann synapses 
Putative_Li1_170637_66919_3787_input = flywire_partner_summary(Putative_Li1_170637_66919_3787_id,partners = "input", cleft.threshold = 100)
Putative_Li1_170637_66919_3787_input$post_id = Putative_Li1_170637_66919_3787_id

# check for the latest ID for each partner and add a flywire_URL
Putative_Li1_170637_66919_3787_input$partner_type = ""
Putative_Li1_170637_66919_3787_input$partner_name = ""
Putative_Li1_170637_66919_3787_input$flywire_URL = ""
for (i in 1:length(Putative_Li1_170637_66919_3787_input$pre_id)){
  Putative_Li1_170637_66919_3787_input$pre_id[i] = flywire_latestid(Putative_Li1_170637_66919_3787_input$pre_id[i])
  Putative_Li1_170637_66919_3787_input$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_Li1_170637_66919_3787_input$post_id[i],Putative_Li1_170637_66919_3787_input$pre_id[i]))}","klickable_link")')
} 

# get names 
Putative_Li1_170637_66919_3787_input$partner_name = flywire_neuron_name$name[match(Putative_Li1_170637_66919_3787_input$pre_id,flywire_neuron_name$seg_id)]
Putative_Li1_170637_66919_3787_input$partner_type = flywire_neuron_name$type[match(Putative_Li1_170637_66919_3787_input$pre_id,flywire_neuron_name$seg_id)]




# writing the connectivity tables back to google sheets
sheet_write(Putative_Li1_170637_66919_3787_output, ss="1GyHqc0AfipANkw0NJboHG40pJXbNPxQ15N-9_1GbTME", sheet = "output")
sheet_write(Putative_Li1_170637_66919_3787_input, ss="1GyHqc0AfipANkw0NJboHG40pJXbNPxQ15N-9_1GbTME", sheet = "input")
# sheet_write(Putative_Li1_170637_66919_3787_exclude, ss= "1GyHqc0AfipANkw0NJboHG40pJXbNPxQ15N-9_1GbTME", sheet = "exclude")


# ------------------------------------------------------------------------
# Li1 Matanat 2 --------------------------------- Last run 31/01/2023
# Putative_Li1_177445_58663_3553

# 2 Li2 : https://docs.google.com/spreadsheets/d/132elZveGuRGYeYN9cTVNkGfzwKE_Km24H9ns0ya1oBY/edit?usp=sharing
Putative_Li1_177445_58663_3553_id = "720575940623183436"

# getting the newest ID if changes happened
Putative_Li1_177445_58663_3553_id = flywire_latestid(Putative_Li1_177445_58663_3553_id)

# read mesh from flywire
Putative_Li1_177445_58663_3553_mesh = read_cloudvolume_meshes(Putative_Li1_177445_58663_3553_id) 


## DOWNSTREAM PARTNERS based on buhmann synapses 
Putative_Li1_177445_58663_3553_output = flywire_partner_summary(Putative_Li1_177445_58663_3553_id,partners = "output", cleft.threshold = 100)
Putative_Li1_177445_58663_3553_output$pre_id = Putative_Li1_177445_58663_3553_id

# check for the latest ID for each partner and add a flywire_URL
Putative_Li1_177445_58663_3553_output$partner_type = ""
Putative_Li1_177445_58663_3553_output$partner_name = ""
Putative_Li1_177445_58663_3553_output$flywire_URL = ""
for (i in 1:length(Putative_Li1_177445_58663_3553_output$post_id)){
  Putative_Li1_177445_58663_3553_output$post_id[i] = flywire_latestid(Putative_Li1_177445_58663_3553_output$post_id[i])
  Putative_Li1_177445_58663_3553_output$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_Li1_177445_58663_3553_output$pre_id[i],Putative_Li1_177445_58663_3553_output$post_id[i]))}","klickable_link")')
} 

# get names and types from flywire_neuron_names table
Putative_Li1_177445_58663_3553_output$partner_name = flywire_neuron_name$name[match(Putative_Li1_177445_58663_3553_output$post_id,flywire_neuron_name$seg_id)]
Putative_Li1_177445_58663_3553_output$partner_type = flywire_neuron_name$type[match(Putative_Li1_177445_58663_3553_output$post_id,flywire_neuron_name$seg_id)]



## UPSTREAM PARTNERS based on buhmann synapses 
Putative_Li1_177445_58663_3553_input = flywire_partner_summary(Putative_Li1_177445_58663_3553_id,partners = "input", cleft.threshold = 100)
Putative_Li1_177445_58663_3553_input$post_id = Putative_Li1_177445_58663_3553_id

# check for the latest ID for each partner and add a flywire_URL
Putative_Li1_177445_58663_3553_input$partner_type = ""
Putative_Li1_177445_58663_3553_input$partner_name = ""
Putative_Li1_177445_58663_3553_input$flywire_URL = ""
for (i in 1:length(Putative_Li1_177445_58663_3553_input$pre_id)){
  Putative_Li1_177445_58663_3553_input$pre_id[i] = flywire_latestid(Putative_Li1_177445_58663_3553_input$pre_id[i])
  Putative_Li1_177445_58663_3553_input$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_Li1_177445_58663_3553_input$post_id[i],Putative_Li1_177445_58663_3553_input$pre_id[i]))}","klickable_link")')
} 

# get names 
Putative_Li1_177445_58663_3553_input$partner_name = flywire_neuron_name$name[match(Putative_Li1_177445_58663_3553_input$pre_id,flywire_neuron_name$seg_id)]
Putative_Li1_177445_58663_3553_input$partner_type = flywire_neuron_name$type[match(Putative_Li1_177445_58663_3553_input$pre_id,flywire_neuron_name$seg_id)]




# writing the connectivity tables back to google sheets
sheet_write(Putative_Li1_177445_58663_3553_output, ss="132elZveGuRGYeYN9cTVNkGfzwKE_Km24H9ns0ya1oBY", sheet = "output")
sheet_write(Putative_Li1_177445_58663_3553_input, ss="132elZveGuRGYeYN9cTVNkGfzwKE_Km24H9ns0ya1oBY", sheet = "input")
# sheet_write(Putative_Li1_177445_58663_3553_exclude, ss= "132elZveGuRGYeYN9cTVNkGfzwKE_Km24H9ns0ya1oBY", sheet = "exclude")



# ------------------------------------------------------------------------
# Li1 Matanat 3 --------------------------------- Last run 31/01/2023
# Putative_Li1_86162_62789_4245

# 3 Li2 : https://docs.google.com/spreadsheets/d/1uyJIrAeXUz6dd2YImW0NLq7aTO4ovlzfxYyg2GW0HaQ/edit?usp=sharing
Putative_Li1_86162_62789_4245_id = "720575940633185619"

# getting the newest ID if changes happened
Putative_Li1_86162_62789_4245_id = flywire_latestid(Putative_Li1_86162_62789_4245_id)

# read mesh from flywire
Putative_Li1_86162_62789_4245_mesh = read_cloudvolume_meshes(Putative_Li1_86162_62789_4245_id) 


## DOWNSTREAM PARTNERS based on buhmann synapses 
Putative_Li1_86162_62789_4245_output = flywire_partner_summary(Putative_Li1_86162_62789_4245_id,partners = "output", cleft.threshold = 100)
Putative_Li1_86162_62789_4245_output$pre_id = Putative_Li1_86162_62789_4245_id

# check for the latest ID for each partner and add a flywire_URL
Putative_Li1_86162_62789_4245_output$partner_type = ""
Putative_Li1_86162_62789_4245_output$partner_name = ""
Putative_Li1_86162_62789_4245_output$flywire_URL = ""
for (i in 1:length(Putative_Li1_86162_62789_4245_output$post_id)){
  Putative_Li1_86162_62789_4245_output$post_id[i] = flywire_latestid(Putative_Li1_86162_62789_4245_output$post_id[i])
  Putative_Li1_86162_62789_4245_output$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_Li1_86162_62789_4245_output$pre_id[i],Putative_Li1_86162_62789_4245_output$post_id[i]))}","klickable_link")')
} 

# get names and types from flywire_neuron_names table
Putative_Li1_86162_62789_4245_output$partner_name = flywire_neuron_name$name[match(Putative_Li1_86162_62789_4245_output$post_id,flywire_neuron_name$seg_id)]
Putative_Li1_86162_62789_4245_output$partner_type = flywire_neuron_name$type[match(Putative_Li1_86162_62789_4245_output$post_id,flywire_neuron_name$seg_id)]



## UPSTREAM PARTNERS based on buhmann synapses 
Putative_Li1_86162_62789_4245_input = flywire_partner_summary(Putative_Li1_86162_62789_4245_id,partners = "input", cleft.threshold = 100)
Putative_Li1_86162_62789_4245_input$post_id = Putative_Li1_86162_62789_4245_id

# check for the latest ID for each partner and add a flywire_URL
Putative_Li1_86162_62789_4245_input$partner_type = ""
Putative_Li1_86162_62789_4245_input$partner_name = ""
Putative_Li1_86162_62789_4245_input$flywire_URL = ""
for (i in 1:length(Putative_Li1_86162_62789_4245_input$pre_id)){
  Putative_Li1_86162_62789_4245_input$pre_id[i] = flywire_latestid(Putative_Li1_86162_62789_4245_input$pre_id[i])
  Putative_Li1_86162_62789_4245_input$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_Li1_86162_62789_4245_input$post_id[i],Putative_Li1_86162_62789_4245_input$pre_id[i]))}","klickable_link")')
} 

# get names 
Putative_Li1_86162_62789_4245_input$partner_name = flywire_neuron_name$name[match(Putative_Li1_86162_62789_4245_input$pre_id,flywire_neuron_name$seg_id)]
Putative_Li1_86162_62789_4245_input$partner_type = flywire_neuron_name$type[match(Putative_Li1_86162_62789_4245_input$pre_id,flywire_neuron_name$seg_id)]




# writing the connectivity tables back to google sheets
sheet_write(Putative_Li1_86162_62789_4245_output, ss="1uyJIrAeXUz6dd2YImW0NLq7aTO4ovlzfxYyg2GW0HaQ", sheet = "output")
sheet_write(Putative_Li1_86162_62789_4245_input, ss="1uyJIrAeXUz6dd2YImW0NLq7aTO4ovlzfxYyg2GW0HaQ", sheet = "input")
# sheet_write(Putative_Li1_86162_62789_4245_exclude, ss= "1uyJIrAeXUz6dd2YImW0NLq7aTO4ovlzfxYyg2GW0HaQ", sheet = "exclude")


# LC14a output

# ------------------------------------------------------------------------
# Li2 Samra 4 --------------------------------- Last run 31/01/2023
# Putative_Li2M_80829_64916_4761

# 1 Li2 : https://docs.google.com/spreadsheets/d/1wVbLJfyxiJSX9DRfB9-g18o6qDzYS8vqNslR28_3i7g/edit?usp=sharing
Putative_Li2M_80829_64916_4761_id = "720575940629378883"

# getting the newest ID if changes happened
Putative_Li2M_80829_64916_4761_id = flywire_latestid(Putative_Li2M_80829_64916_4761_id)

# read mesh from flywire
Putative_Li2M_80829_64916_4761_mesh = read_cloudvolume_meshes(Putative_Li2M_80829_64916_4761_id) 


## DOWNSTREAM PARTNERS based on buhmann synapses 
Putative_Li2M_80829_64916_4761_output = flywire_partner_summary(Putative_Li2M_80829_64916_4761_id,partners = "output", cleft.threshold = 100)
Putative_Li2M_80829_64916_4761_output$pre_id = Putative_Li2M_80829_64916_4761_id

# check for the latest ID for each partner and add a flywire_URL
Putative_Li2M_80829_64916_4761_output$partner_type = ""
Putative_Li2M_80829_64916_4761_output$partner_name = ""
Putative_Li2M_80829_64916_4761_output$flywire_URL = ""
for (i in 1:length(Putative_Li2M_80829_64916_4761_output$post_id)){
  Putative_Li2M_80829_64916_4761_output$post_id[i] = flywire_latestid(Putative_Li2M_80829_64916_4761_output$post_id[i])
  Putative_Li2M_80829_64916_4761_output$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_Li2M_80829_64916_4761_output$pre_id[i],Putative_Li2M_80829_64916_4761_output$post_id[i]))}","klickable_link")')
} 

# get names and types from flywire_neuron_names table
Putative_Li2M_80829_64916_4761_output$partner_name = flywire_neuron_name$name[match(Putative_Li2M_80829_64916_4761_output$post_id,flywire_neuron_name$seg_id)]
Putative_Li2M_80829_64916_4761_output$partner_type = flywire_neuron_name$type[match(Putative_Li2M_80829_64916_4761_output$post_id,flywire_neuron_name$seg_id)]



## UPSTREAM PARTNERS based on buhmann synapses 
Putative_Li2M_80829_64916_4761_input = flywire_partner_summary(Putative_Li2M_80829_64916_4761_id,partners = "input", cleft.threshold = 100)
Putative_Li2M_80829_64916_4761_input$post_id = Putative_Li2M_80829_64916_4761_id

# check for the latest ID for each partner and add a flywire_URL
Putative_Li2M_80829_64916_4761_input$partner_type = ""
Putative_Li2M_80829_64916_4761_input$partner_name = ""
Putative_Li2M_80829_64916_4761_input$flywire_URL = ""
for (i in 1:length(Putative_Li2M_80829_64916_4761_input$pre_id)){
  Putative_Li2M_80829_64916_4761_input$pre_id[i] = flywire_latestid(Putative_Li2M_80829_64916_4761_input$pre_id[i])
  Putative_Li2M_80829_64916_4761_input$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_Li2M_80829_64916_4761_input$post_id[i],Putative_Li2M_80829_64916_4761_input$pre_id[i]))}","klickable_link")')
} 

# get names 
Putative_Li2M_80829_64916_4761_input$partner_name = flywire_neuron_name$name[match(Putative_Li2M_80829_64916_4761_input$pre_id,flywire_neuron_name$seg_id)]
Putative_Li2M_80829_64916_4761_input$partner_type = flywire_neuron_name$type[match(Putative_Li2M_80829_64916_4761_input$pre_id,flywire_neuron_name$seg_id)]




# writing the connectivity tables back to google sheets
sheet_write(Putative_Li2M_80829_64916_4761_output, ss="1wVbLJfyxiJSX9DRfB9-g18o6qDzYS8vqNslR28_3i7g", sheet = "output")
sheet_write(Putative_Li2M_80829_64916_4761_input, ss="1wVbLJfyxiJSX9DRfB9-g18o6qDzYS8vqNslR28_3i7g", sheet = "input")
# sheet_write(Putative_Li2M_80829_64916_4761_exclude, ss= "1wVbLJfyxiJSX9DRfB9-g18o6qDzYS8vqNslR28_3i7g", sheet = "exclude")


# ------------------------------------------------------------------------
# Li2 Matanat 5 --------------------------------- Last run 31/01/2023
# Putative_Li2_79661_60574_4766

# 2 Li2 : https://docs.google.com/spreadsheets/d/1tMxIvkkZ8t-_r9zIDTi9xsddM1jPc6eKkK-4RFHPggw/edit?usp=sharing
Putative_Li2_79661_60574_4766_id = "720575940632490336"

# getting the newest ID if changes happened
Putative_Li2_79661_60574_4766_id = flywire_latestid(Putative_Li2_79661_60574_4766_id)

# read mesh from flywire
Putative_Li2_79661_60574_4766_mesh = read_cloudvolume_meshes(Putative_Li2_79661_60574_4766_id) 


## DOWNSTREAM PARTNERS based on buhmann synapses 
Putative_Li2_79661_60574_4766_output = flywire_partner_summary(Putative_Li2_79661_60574_4766_id,partners = "output", cleft.threshold = 100)
Putative_Li2_79661_60574_4766_output$pre_id = Putative_Li2_79661_60574_4766_id

# check for the latest ID for each partner and add a flywire_URL
Putative_Li2_79661_60574_4766_output$partner_type = ""
Putative_Li2_79661_60574_4766_output$partner_name = ""
Putative_Li2_79661_60574_4766_output$flywire_URL = ""
for (i in 1:length(Putative_Li2_79661_60574_4766_output$post_id)){
  Putative_Li2_79661_60574_4766_output$post_id[i] = flywire_latestid(Putative_Li2_79661_60574_4766_output$post_id[i])
  Putative_Li2_79661_60574_4766_output$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_Li2_79661_60574_4766_output$pre_id[i],Putative_Li2_79661_60574_4766_output$post_id[i]))}","klickable_link")')
} 

# get names and types from flywire_neuron_names table
Putative_Li2_79661_60574_4766_output$partner_name = flywire_neuron_name$name[match(Putative_Li2_79661_60574_4766_output$post_id,flywire_neuron_name$seg_id)]
Putative_Li2_79661_60574_4766_output$partner_type = flywire_neuron_name$type[match(Putative_Li2_79661_60574_4766_output$post_id,flywire_neuron_name$seg_id)]



## UPSTREAM PARTNERS based on buhmann synapses 
Putative_Li2_79661_60574_4766_input = flywire_partner_summary(Putative_Li2_79661_60574_4766_id,partners = "input", cleft.threshold = 100)
Putative_Li2_79661_60574_4766_input$post_id = Putative_Li2_79661_60574_4766_id

# check for the latest ID for each partner and add a flywire_URL
Putative_Li2_79661_60574_4766_input$partner_type = ""
Putative_Li2_79661_60574_4766_input$partner_name = ""
Putative_Li2_79661_60574_4766_input$flywire_URL = ""
for (i in 1:length(Putative_Li2_79661_60574_4766_input$pre_id)){
  Putative_Li2_79661_60574_4766_input$pre_id[i] = flywire_latestid(Putative_Li2_79661_60574_4766_input$pre_id[i])
  Putative_Li2_79661_60574_4766_input$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_Li2_79661_60574_4766_input$post_id[i],Putative_Li2_79661_60574_4766_input$pre_id[i]))}","klickable_link")')
} 

# get names 
Putative_Li2_79661_60574_4766_input$partner_name = flywire_neuron_name$name[match(Putative_Li2_79661_60574_4766_input$pre_id,flywire_neuron_name$seg_id)]
Putative_Li2_79661_60574_4766_input$partner_type = flywire_neuron_name$type[match(Putative_Li2_79661_60574_4766_input$pre_id,flywire_neuron_name$seg_id)]




# writing the connectivity tables back to google sheets
sheet_write(Putative_Li2_79661_60574_4766_output, ss="1tMxIvkkZ8t-_r9zIDTi9xsddM1jPc6eKkK-4RFHPggw", sheet = "output")
sheet_write(Putative_Li2_79661_60574_4766_input, ss="1tMxIvkkZ8t-_r9zIDTi9xsddM1jPc6eKkK-4RFHPggw", sheet = "input")
# sheet_write(Putative_Li2_79661_60574_4766_exclude, ss= "1tMxIvkkZ8t-_r9zIDTi9xsddM1jPc6eKkK-4RFHPggw", sheet = "exclude")


# ------------------------------------------------------------------------
# Li2 Matanat 6 --------------------------------- Last run 31/01/2023
# Putative_Li2_180762_71427_3783

# 3 Li2 : https://docs.google.com/spreadsheets/d/19wfcWnT1B7A__AK2kcP-zokCXvZHdfTqyoVyqtDk-Pc/edit?usp=sharing
Putative_Li2_180762_71427_3783_id = "720575940645454116"

# getting the newest ID if changes happened
Putative_Li2_180762_71427_3783_id = flywire_latestid(Putative_Li2_180762_71427_3783_id)

# read mesh from flywire
Putative_Li2_180762_71427_3783_mesh = read_cloudvolume_meshes(Putative_Li2_180762_71427_3783_id) 


## DOWNSTREAM PARTNERS based on buhmann synapses 
Putative_Li2_180762_71427_3783_output = flywire_partner_summary(Putative_Li2_180762_71427_3783_id,partners = "output", cleft.threshold = 100)
Putative_Li2_180762_71427_3783_output$pre_id = Putative_Li2_180762_71427_3783_id

# check for the latest ID for each partner and add a flywire_URL
Putative_Li2_180762_71427_3783_output$partner_type = ""
Putative_Li2_180762_71427_3783_output$partner_name = ""
Putative_Li2_180762_71427_3783_output$flywire_URL = ""
for (i in 1:length(Putative_Li2_180762_71427_3783_output$post_id)){
  Putative_Li2_180762_71427_3783_output$post_id[i] = flywire_latestid(Putative_Li2_180762_71427_3783_output$post_id[i])
  Putative_Li2_180762_71427_3783_output$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_Li2_180762_71427_3783_output$pre_id[i],Putative_Li2_180762_71427_3783_output$post_id[i]))}","klickable_link")')
} 

# get names and types from flywire_neuron_names table
Putative_Li2_180762_71427_3783_output$partner_name = flywire_neuron_name$name[match(Putative_Li2_180762_71427_3783_output$post_id,flywire_neuron_name$seg_id)]
Putative_Li2_180762_71427_3783_output$partner_type = flywire_neuron_name$type[match(Putative_Li2_180762_71427_3783_output$post_id,flywire_neuron_name$seg_id)]



## UPSTREAM PARTNERS based on buhmann synapses 
Putative_Li2_180762_71427_3783_input = flywire_partner_summary(Putative_Li2_180762_71427_3783_id,partners = "input", cleft.threshold = 100)
Putative_Li2_180762_71427_3783_input$post_id = Putative_Li2_180762_71427_3783_id

# check for the latest ID for each partner and add a flywire_URL
Putative_Li2_180762_71427_3783_input$partner_type = ""
Putative_Li2_180762_71427_3783_input$partner_name = ""
Putative_Li2_180762_71427_3783_input$flywire_URL = ""
for (i in 1:length(Putative_Li2_180762_71427_3783_input$pre_id)){
  Putative_Li2_180762_71427_3783_input$pre_id[i] = flywire_latestid(Putative_Li2_180762_71427_3783_input$pre_id[i])
  Putative_Li2_180762_71427_3783_input$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_Li2_180762_71427_3783_input$post_id[i],Putative_Li2_180762_71427_3783_input$pre_id[i]))}","klickable_link")')
} 

# get names 
Putative_Li2_180762_71427_3783_input$partner_name = flywire_neuron_name$name[match(Putative_Li2_180762_71427_3783_input$pre_id,flywire_neuron_name$seg_id)]
Putative_Li2_180762_71427_3783_input$partner_type = flywire_neuron_name$type[match(Putative_Li2_180762_71427_3783_input$pre_id,flywire_neuron_name$seg_id)]




# writing the connectivity tables back to google sheets
sheet_write(Putative_Li2_180762_71427_3783_output, ss="19wfcWnT1B7A__AK2kcP-zokCXvZHdfTqyoVyqtDk-Pc", sheet = "output")
sheet_write(Putative_Li2_180762_71427_3783_input, ss="19wfcWnT1B7A__AK2kcP-zokCXvZHdfTqyoVyqtDk-Pc", sheet = "input")
# sheet_write(Putative_Li2_180762_71427_3783_exclude, ss= "19wfcWnT1B7A__AK2kcP-zokCXvZHdfTqyoVyqtDk-Pc", sheet = "exclude")


# LC14b input

# ------------------------------------------------------------------------
# Y3 Samra 1 --------------------------------- Last run 15/07/2023
# Putative_Y3_188363_75142_5430

# 1 Y3 : https://docs.google.com/spreadsheets/d/1PUHgwV1aNILWQOyklojstNaVDsqAF-O582sgjcKXefs/edit?usp=sharing
Putative_Y3_188363_75142_5430_id = "720575940625460241"

# getting the newest ID if changes happened
Putative_Y3_188363_75142_5430_id = flywire_latestid(Putative_Y3_188363_75142_5430_id)

# read mesh from flywire
Putative_Y3_188363_75142_5430_mesh = read_cloudvolume_meshes(Putative_Y3_188363_75142_5430_id) 


## DOWNSTREAM PARTNERS based on buhmann synapses 
Putative_Y3_188363_75142_5430_output = flywire_partner_summary(Putative_Y3_188363_75142_5430_id,partners = "output", cleft.threshold = 100)
Putative_Y3_188363_75142_5430_output$pre_id = Putative_Y3_188363_75142_5430_id

# check for the latest ID for each partner and add a flywire_URL
Putative_Y3_188363_75142_5430_output$partner_type = ""
Putative_Y3_188363_75142_5430_output$partner_name = ""
Putative_Y3_188363_75142_5430_output$flywire_URL = ""
for (i in 1:length(Putative_Y3_188363_75142_5430_output$post_id)){
  Putative_Y3_188363_75142_5430_output$post_id[i] = flywire_latestid(Putative_Y3_188363_75142_5430_output$post_id[i])
  Putative_Y3_188363_75142_5430_output$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_Y3_188363_75142_5430_output$pre_id[i],Putative_Y3_188363_75142_5430_output$post_id[i]))}","klickable_link")')
} 

# get names and types from flywire_neuron_names table
Putative_Y3_188363_75142_5430_output$partner_name = flywire_neuron_name$name[match(Putative_Y3_188363_75142_5430_output$post_id,flywire_neuron_name$seg_id)]
Putative_Y3_188363_75142_5430_output$partner_type = flywire_neuron_name$type[match(Putative_Y3_188363_75142_5430_output$post_id,flywire_neuron_name$seg_id)]



## UPSTREAM PARTNERS based on buhmann synapses 
Putative_Y3_188363_75142_5430_input = flywire_partner_summary(Putative_Y3_188363_75142_5430_id,partners = "input", cleft.threshold = 100)
Putative_Y3_188363_75142_5430_input$post_id = Putative_Y3_188363_75142_5430_id

# check for the latest ID for each partner and add a flywire_URL
Putative_Y3_188363_75142_5430_input$partner_type = ""
Putative_Y3_188363_75142_5430_input$partner_name = ""
Putative_Y3_188363_75142_5430_input$flywire_URL = ""
for (i in 1:length(Putative_Y3_188363_75142_5430_input$pre_id)){
  Putative_Y3_188363_75142_5430_input$pre_id[i] = flywire_latestid(Putative_Y3_188363_75142_5430_input$pre_id[i])
  Putative_Y3_188363_75142_5430_input$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_Y3_188363_75142_5430_input$post_id[i],Putative_Y3_188363_75142_5430_input$pre_id[i]))}","klickable_link")')
} 

# get names 
Putative_Y3_188363_75142_5430_input$partner_name = flywire_neuron_name$name[match(Putative_Y3_188363_75142_5430_input$pre_id,flywire_neuron_name$seg_id)]
Putative_Y3_188363_75142_5430_input$partner_type = flywire_neuron_name$type[match(Putative_Y3_188363_75142_5430_input$pre_id,flywire_neuron_name$seg_id)]




# writing the connectivity tables back to google sheets
sheet_write(Putative_Y3_188363_75142_5430_output, ss="1W0pbyB_p-qPKQmstBg6g9CZHqW2u4bJxQC0tChe4Juw", sheet = "output")
sheet_write(Putative_Y3_188363_75142_5430_input, ss="1W0pbyB_p-qPKQmstBg6g9CZHqW2u4bJxQC0tChe4Juw", sheet = "input")
# sheet_write(Putative_Y3_188363_75142_5430_exclude, ss= "1W0pbyB_p-qPKQmstBg6g9CZHqW2u4bJxQC0tChe4Juw", sheet = "exclude")


# ------------------------------------------------------------------------
# Y3 Matanat 2 --------------------------------- Last run 15/07/2023
# Putative_Y3_69594_70446_6252

# 2 Y3 : https://docs.google.com/spreadsheets/d/1PUHgwV1aNILWQOyklojstNaVDsqAF-O582sgjcKXefs/edit?usp=sharing
Putative_Y3_69594_70446_6252_id = "720575940625560590"

# getting the newest ID if changes happened
Putative_Y3_69594_70446_6252_id = flywire_latestid(Putative_Y3_69594_70446_6252_id)

# read mesh from flywire
Putative_Y3_69594_70446_6252_mesh = read_cloudvolume_meshes(Putative_Y3_69594_70446_6252_id) 


## DOWNSTREAM PARTNERS based on buhmann synapses 
Putative_Y3_69594_70446_6252_output = flywire_partner_summary(Putative_Y3_69594_70446_6252_id,partners = "output", cleft.threshold = 100)
Putative_Y3_69594_70446_6252_output$pre_id = Putative_Y3_69594_70446_6252_id

# check for the latest ID for each partner and add a flywire_URL
Putative_Y3_69594_70446_6252_output$partner_type = ""
Putative_Y3_69594_70446_6252_output$partner_name = ""
Putative_Y3_69594_70446_6252_output$flywire_URL = ""
for (i in 1:length(Putative_Y3_69594_70446_6252_output$post_id)){
  Putative_Y3_69594_70446_6252_output$post_id[i] = flywire_latestid(Putative_Y3_69594_70446_6252_output$post_id[i])
  Putative_Y3_69594_70446_6252_output$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_Y3_69594_70446_6252_output$pre_id[i],Putative_Y3_69594_70446_6252_output$post_id[i]))}","klickable_link")')
} 

# get names and types from flywire_neuron_names table
Putative_Y3_69594_70446_6252_output$partner_name = flywire_neuron_name$name[match(Putative_Y3_69594_70446_6252_output$post_id,flywire_neuron_name$seg_id)]
Putative_Y3_69594_70446_6252_output$partner_type = flywire_neuron_name$type[match(Putative_Y3_69594_70446_6252_output$post_id,flywire_neuron_name$seg_id)]



## UPSTREAM PARTNERS based on buhmann synapses 
Putative_Y3_69594_70446_6252_input = flywire_partner_summary(Putative_Y3_69594_70446_6252_id,partners = "input", cleft.threshold = 100)
Putative_Y3_69594_70446_6252_input$post_id = Putative_Y3_69594_70446_6252_id

# check for the latest ID for each partner and add a flywire_URL
Putative_Y3_69594_70446_6252_input$partner_type = ""
Putative_Y3_69594_70446_6252_input$partner_name = ""
Putative_Y3_69594_70446_6252_input$flywire_URL = ""
for (i in 1:length(Putative_Y3_69594_70446_6252_input$pre_id)){
  Putative_Y3_69594_70446_6252_input$pre_id[i] = flywire_latestid(Putative_Y3_69594_70446_6252_input$pre_id[i])
  Putative_Y3_69594_70446_6252_input$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_Y3_69594_70446_6252_input$post_id[i],Putative_Y3_69594_70446_6252_input$pre_id[i]))}","klickable_link")')
} 

# get names 
Putative_Y3_69594_70446_6252_input$partner_name = flywire_neuron_name$name[match(Putative_Y3_69594_70446_6252_input$pre_id,flywire_neuron_name$seg_id)]
Putative_Y3_69594_70446_6252_input$partner_type = flywire_neuron_name$type[match(Putative_Y3_69594_70446_6252_input$pre_id,flywire_neuron_name$seg_id)]




# writing the connectivity tables back to google sheets
sheet_write(Putative_Y3_69594_70446_6252_output, ss="1PUHgwV1aNILWQOyklojstNaVDsqAF-O582sgjcKXefs", sheet = "output")
sheet_write(Putative_Y3_69594_70446_6252_input, ss="1PUHgwV1aNILWQOyklojstNaVDsqAF-O582sgjcKXefs", sheet = "input")
# sheet_write(Putative_Y3_69594_70446_6252_exclude, ss= "1PUHgwV1aNILWQOyklojstNaVDsqAF-O582sgjcKXefs", sheet = "exclude")


# ------------------------------------------------------------------------
# Y3 Matanat 3 --------------------------------- Last run 15/07/2023
# Putative_Y3_72350_69627_5903

# 3 Y3 : https://docs.google.com/spreadsheets/d/1-QA0HYZaTIh9o2ioILpSfi3-JUG6V8P-cOGXAO4JHfc/edit?usp=sharing
Putative_Y3_72350_69627_5903_id = "720575940630726460"

# getting the newest ID if changes happened
Putative_Y3_72350_69627_5903_id = flywire_latestid(Putative_Y3_72350_69627_5903_id)

# read mesh from flywire
Putative_Y3_72350_69627_5903_mesh = read_cloudvolume_meshes(Putative_Y3_72350_69627_5903_id) 


## DOWNSTREAM PARTNERS based on buhmann synapses 
Putative_Y3_72350_69627_5903_output = flywire_partner_summary(Putative_Y3_72350_69627_5903_id,partners = "output", cleft.threshold = 100)
Putative_Y3_72350_69627_5903_output$pre_id = Putative_Y3_72350_69627_5903_id

# check for the latest ID for each partner and add a flywire_URL
Putative_Y3_72350_69627_5903_output$partner_type = ""
Putative_Y3_72350_69627_5903_output$partner_name = ""
Putative_Y3_72350_69627_5903_output$flywire_URL = ""
for (i in 1:length(Putative_Y3_72350_69627_5903_output$post_id)){
  Putative_Y3_72350_69627_5903_output$post_id[i] = flywire_latestid(Putative_Y3_72350_69627_5903_output$post_id[i])
  Putative_Y3_72350_69627_5903_output$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_Y3_72350_69627_5903_output$pre_id[i],Putative_Y3_72350_69627_5903_output$post_id[i]))}","klickable_link")')
} 

# get names and types from flywire_neuron_names table
Putative_Y3_72350_69627_5903_output$partner_name = flywire_neuron_name$name[match(Putative_Y3_72350_69627_5903_output$post_id,flywire_neuron_name$seg_id)]
Putative_Y3_72350_69627_5903_output$partner_type = flywire_neuron_name$type[match(Putative_Y3_72350_69627_5903_output$post_id,flywire_neuron_name$seg_id)]



## UPSTREAM PARTNERS based on buhmann synapses 
Putative_Y3_72350_69627_5903_input = flywire_partner_summary(Putative_Y3_72350_69627_5903_id,partners = "input", cleft.threshold = 100)
Putative_Y3_72350_69627_5903_input$post_id = Putative_Y3_72350_69627_5903_id

# check for the latest ID for each partner and add a flywire_URL
Putative_Y3_72350_69627_5903_input$partner_type = ""
Putative_Y3_72350_69627_5903_input$partner_name = ""
Putative_Y3_72350_69627_5903_input$flywire_URL = ""
for (i in 1:length(Putative_Y3_72350_69627_5903_input$pre_id)){
  Putative_Y3_72350_69627_5903_input$pre_id[i] = flywire_latestid(Putative_Y3_72350_69627_5903_input$pre_id[i])
  Putative_Y3_72350_69627_5903_input$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_Y3_72350_69627_5903_input$post_id[i],Putative_Y3_72350_69627_5903_input$pre_id[i]))}","klickable_link")')
} 

# get names 
Putative_Y3_72350_69627_5903_input$partner_name = flywire_neuron_name$name[match(Putative_Y3_72350_69627_5903_input$pre_id,flywire_neuron_name$seg_id)]
Putative_Y3_72350_69627_5903_input$partner_type = flywire_neuron_name$type[match(Putative_Y3_72350_69627_5903_input$pre_id,flywire_neuron_name$seg_id)]




# writing the connectivity tables back to google sheets
sheet_write(Putative_Y3_72350_69627_5903_output, ss="1-QA0HYZaTIh9o2ioILpSfi3-JUG6V8P-cOGXAO4JHfc", sheet = "output")
sheet_write(Putative_Y3_72350_69627_5903_input, ss="1-QA0HYZaTIh9o2ioILpSfi3-JUG6V8P-cOGXAO4JHfc", sheet = "input")
# sheet_write(Putative_Y3_72350_69627_5903_exclude, ss= "1-QA0HYZaTIh9o2ioILpSfi3-JUG6V8P-cOGXAO4JHfc", sheet = "exclude")


# LC14b output

# ------------------------------------------------------------------------
# Dm3 Samra 1 --------------------------------- Last run 15/07/2023
# Putative_Dm3_62413_84498_6391

# 1 Li2 : https://docs.google.com/spreadsheets/d/1QshdZKac3ZW9Wntx4fFpnD6Vn2k9xFeW-Oihltfw7cM/edit?usp=sharing
Putative_Dm3_62413_84498_6391_id = "720575940621327498"

# getting the newest ID if changes happened
Putative_Dm3_62413_84498_6391_id = flywire_latestid(Putative_Dm3_62413_84498_6391_id)

# read mesh from flywire
Putative_Dm3_62413_84498_6391_mesh = read_cloudvolume_meshes(Putative_Dm3_62413_84498_6391_id) 


## DOWNSTREAM PARTNERS based on buhmann synapses 
Putative_Dm3_62413_84498_6391_output = flywire_partner_summary(Putative_Dm3_62413_84498_6391_id,partners = "output", cleft.threshold = 100)
Putative_Dm3_62413_84498_6391_output$pre_id = Putative_Dm3_62413_84498_6391_id

# check for the latest ID for each partner and add a flywire_URL
Putative_Dm3_62413_84498_6391_output$partner_type = ""
Putative_Dm3_62413_84498_6391_output$partner_name = ""
Putative_Dm3_62413_84498_6391_output$flywire_URL = ""
for (i in 1:length(Putative_Dm3_62413_84498_6391_output$post_id)){
  Putative_Dm3_62413_84498_6391_output$post_id[i] = flywire_latestid(Putative_Dm3_62413_84498_6391_output$post_id[i])
  Putative_Dm3_62413_84498_6391_output$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_Dm3_62413_84498_6391_output$pre_id[i],Putative_Dm3_62413_84498_6391_output$post_id[i]))}","klickable_link")')
} 

# get names and types from flywire_neuron_names table
Putative_Dm3_62413_84498_6391_output$partner_name = flywire_neuron_name$name[match(Putative_Dm3_62413_84498_6391_output$post_id,flywire_neuron_name$seg_id)]
Putative_Dm3_62413_84498_6391_output$partner_type = flywire_neuron_name$type[match(Putative_Dm3_62413_84498_6391_output$post_id,flywire_neuron_name$seg_id)]



## UPSTREAM PARTNERS based on buhmann synapses 
Putative_Dm3_62413_84498_6391_input = flywire_partner_summary(Putative_Dm3_62413_84498_6391_id,partners = "input", cleft.threshold = 100)
Putative_Dm3_62413_84498_6391_input$post_id = Putative_Dm3_62413_84498_6391_id

# check for the latest ID for each partner and add a flywire_URL
Putative_Dm3_62413_84498_6391_input$partner_type = ""
Putative_Dm3_62413_84498_6391_input$partner_name = ""
Putative_Dm3_62413_84498_6391_input$flywire_URL = ""
for (i in 1:length(Putative_Dm3_62413_84498_6391_input$pre_id)){
  Putative_Dm3_62413_84498_6391_input$pre_id[i] = flywire_latestid(Putative_Dm3_62413_84498_6391_input$pre_id[i])
  Putative_Dm3_62413_84498_6391_input$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_Dm3_62413_84498_6391_input$post_id[i],Putative_Dm3_62413_84498_6391_input$pre_id[i]))}","klickable_link")')
} 

# get names 
Putative_Dm3_62413_84498_6391_input$partner_name = flywire_neuron_name$name[match(Putative_Dm3_62413_84498_6391_input$pre_id,flywire_neuron_name$seg_id)]
Putative_Dm3_62413_84498_6391_input$partner_type = flywire_neuron_name$type[match(Putative_Dm3_62413_84498_6391_input$pre_id,flywire_neuron_name$seg_id)]




# writing the connectivity tables back to google sheets
sheet_write(Putative_Dm3_62413_84498_6391_output, ss="1QshdZKac3ZW9Wntx4fFpnD6Vn2k9xFeW-Oihltfw7cM", sheet = "output")
sheet_write(Putative_Dm3_62413_84498_6391_input, ss="1QshdZKac3ZW9Wntx4fFpnD6Vn2k9xFeW-Oihltfw7cM", sheet = "input")
# sheet_write(Putative_Dm3_62413_84498_6391_exclude, ss= "1QshdZKac3ZW9Wntx4fFpnD6Vn2k9xFeW-Oihltfw7cM", sheet = "exclude")


# ------------------------------------------------------------------------
# Dm3 Matanat 2 --------------------------------- Last run 15/07/2023
# Putative_Dm3_202211_87930_5110

# 2 Dm3 : https://docs.google.com/spreadsheets/d/1pUtLSRczu0_0DCECngApn9piXEK7z4F3jvxQBSGb2OY/edit?usp=sharing
Putative_Dm3_202211_87930_5110_id = "720575940623760166"

# getting the newest ID if changes happened
Putative_Dm3_202211_87930_5110_id = flywire_latestid(Putative_Dm3_202211_87930_5110_id)

# read mesh from flywire
Putative_Dm3_202211_87930_5110_mesh = read_cloudvolume_meshes(Putative_Dm3_202211_87930_5110_id) 


## DOWNSTREAM PARTNERS based on buhmann synapses 
Putative_Dm3_202211_87930_5110_output = flywire_partner_summary(Putative_Dm3_202211_87930_5110_id,partners = "output", cleft.threshold = 100)
Putative_Dm3_202211_87930_5110_output$pre_id = Putative_Dm3_202211_87930_5110_id

# check for the latest ID for each partner and add a flywire_URL
Putative_Dm3_202211_87930_5110_output$partner_type = ""
Putative_Dm3_202211_87930_5110_output$partner_name = ""
Putative_Dm3_202211_87930_5110_output$flywire_URL = ""
for (i in 1:length(Putative_Dm3_202211_87930_5110_output$post_id)){
  Putative_Dm3_202211_87930_5110_output$post_id[i] = flywire_latestid(Putative_Dm3_202211_87930_5110_output$post_id[i])
  Putative_Dm3_202211_87930_5110_output$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_Dm3_202211_87930_5110_output$pre_id[i],Putative_Dm3_202211_87930_5110_output$post_id[i]))}","klickable_link")')
} 

# get names and types from flywire_neuron_names table
Putative_Dm3_202211_87930_5110_output$partner_name = flywire_neuron_name$name[match(Putative_Dm3_202211_87930_5110_output$post_id,flywire_neuron_name$seg_id)]
Putative_Dm3_202211_87930_5110_output$partner_type = flywire_neuron_name$type[match(Putative_Dm3_202211_87930_5110_output$post_id,flywire_neuron_name$seg_id)]



## UPSTREAM PARTNERS based on buhmann synapses 
Putative_Dm3_202211_87930_5110_input = flywire_partner_summary(Putative_Dm3_202211_87930_5110_id,partners = "input", cleft.threshold = 100)
Putative_Dm3_202211_87930_5110_input$post_id = Putative_Dm3_202211_87930_5110_id

# check for the latest ID for each partner and add a flywire_URL
Putative_Dm3_202211_87930_5110_input$partner_type = ""
Putative_Dm3_202211_87930_5110_input$partner_name = ""
Putative_Dm3_202211_87930_5110_input$flywire_URL = ""
for (i in 1:length(Putative_Dm3_202211_87930_5110_input$pre_id)){
  Putative_Dm3_202211_87930_5110_input$pre_id[i] = flywire_latestid(Putative_Dm3_202211_87930_5110_input$pre_id[i])
  Putative_Dm3_202211_87930_5110_input$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_Dm3_202211_87930_5110_input$post_id[i],Putative_Dm3_202211_87930_5110_input$pre_id[i]))}","klickable_link")')
} 

# get names 
Putative_Dm3_202211_87930_5110_input$partner_name = flywire_neuron_name$name[match(Putative_Dm3_202211_87930_5110_input$pre_id,flywire_neuron_name$seg_id)]
Putative_Dm3_202211_87930_5110_input$partner_type = flywire_neuron_name$type[match(Putative_Dm3_202211_87930_5110_input$pre_id,flywire_neuron_name$seg_id)]




# writing the connectivity tables back to google sheets
sheet_write(Putative_Dm3_202211_87930_5110_output, ss="1pUtLSRczu0_0DCECngApn9piXEK7z4F3jvxQBSGb2OY", sheet = "output")
sheet_write(Putative_Dm3_202211_87930_5110_input, ss="1pUtLSRczu0_0DCECngApn9piXEK7z4F3jvxQBSGb2OY", sheet = "input")
# sheet_write(Putative_Dm3_202211_87930_5110_exclude, ss= "1pUtLSRczu0_0DCECngApn9piXEK7z4F3jvxQBSGb2OY", sheet = "exclude")


# ------------------------------------------------------------------------
# Dm3 Matanat 3 --------------------------------- Last run 15/07/2023
# Putative_Dm3FDB_206468_79698_5421

# 3 Dm3 : https://docs.google.com/spreadsheets/d/1udTpatxpnvSz_oihhNJESrdp01O6K7Fcna2hXleCOhk/edit?usp=sharing
Putative_Dm3FDB_206468_79698_5421_id = "720575940626898746"

# getting the newest ID if changes happened
Putative_Dm3FDB_206468_79698_5421_id = flywire_latestid(Putative_Dm3FDB_206468_79698_5421_id)

# read mesh from flywire
Putative_Dm3FDB_206468_79698_5421_mesh = read_cloudvolume_meshes(Putative_Dm3FDB_206468_79698_5421_id) 


## DOWNSTREAM PARTNERS based on buhmann synapses 
Putative_Dm3FDB_206468_79698_5421_output = flywire_partner_summary(Putative_Dm3FDB_206468_79698_5421_id,partners = "output", cleft.threshold = 100)
Putative_Dm3FDB_206468_79698_5421_output$pre_id = Putative_Dm3FDB_206468_79698_5421_id

# check for the latest ID for each partner and add a flywire_URL
Putative_Dm3FDB_206468_79698_5421_output$partner_type = ""
Putative_Dm3FDB_206468_79698_5421_output$partner_name = ""
Putative_Dm3FDB_206468_79698_5421_output$flywire_URL = ""
for (i in 1:length(Putative_Dm3FDB_206468_79698_5421_output$post_id)){
  Putative_Dm3FDB_206468_79698_5421_output$post_id[i] = flywire_latestid(Putative_Dm3FDB_206468_79698_5421_output$post_id[i])
  Putative_Dm3FDB_206468_79698_5421_output$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_Dm3FDB_206468_79698_5421_output$pre_id[i],Putative_Dm3FDB_206468_79698_5421_output$post_id[i]))}","klickable_link")')
} 

# get names and types from flywire_neuron_names table
Putative_Dm3FDB_206468_79698_5421_output$partner_name = flywire_neuron_name$name[match(Putative_Dm3FDB_206468_79698_5421_output$post_id,flywire_neuron_name$seg_id)]
Putative_Dm3FDB_206468_79698_5421_output$partner_type = flywire_neuron_name$type[match(Putative_Dm3FDB_206468_79698_5421_output$post_id,flywire_neuron_name$seg_id)]



## UPSTREAM PARTNERS based on buhmann synapses 
Putative_Dm3FDB_206468_79698_5421_input = flywire_partner_summary(Putative_Dm3FDB_206468_79698_5421_id,partners = "input", cleft.threshold = 100)
Putative_Dm3FDB_206468_79698_5421_input$post_id = Putative_Dm3FDB_206468_79698_5421_id

# check for the latest ID for each partner and add a flywire_URL
Putative_Dm3FDB_206468_79698_5421_input$partner_type = ""
Putative_Dm3FDB_206468_79698_5421_input$partner_name = ""
Putative_Dm3FDB_206468_79698_5421_input$flywire_URL = ""
for (i in 1:length(Putative_Dm3FDB_206468_79698_5421_input$pre_id)){
  Putative_Dm3FDB_206468_79698_5421_input$pre_id[i] = flywire_latestid(Putative_Dm3FDB_206468_79698_5421_input$pre_id[i])
  Putative_Dm3FDB_206468_79698_5421_input$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_Dm3FDB_206468_79698_5421_input$post_id[i],Putative_Dm3FDB_206468_79698_5421_input$pre_id[i]))}","klickable_link")')
} 

# get names 
Putative_Dm3FDB_206468_79698_5421_input$partner_name = flywire_neuron_name$name[match(Putative_Dm3FDB_206468_79698_5421_input$pre_id,flywire_neuron_name$seg_id)]
Putative_Dm3FDB_206468_79698_5421_input$partner_type = flywire_neuron_name$type[match(Putative_Dm3FDB_206468_79698_5421_input$pre_id,flywire_neuron_name$seg_id)]




# writing the connectivity tables back to google sheets
sheet_write(Putative_Dm3FDB_206468_79698_5421_output, ss="1udTpatxpnvSz_oihhNJESrdp01O6K7Fcna2hXleCOhk", sheet = "output")
sheet_write(Putative_Dm3FDB_206468_79698_5421_input, ss="1udTpatxpnvSz_oihhNJESrdp01O6K7Fcna2hXleCOhk", sheet = "input")
# sheet_write(Putative_Dm3FDB_206468_79698_5421_exclude, ss= "1udTpatxpnvSz_oihhNJESrdp01O6K7Fcna2hXleCOhk", sheet = "exclude")


# ------------------------------------------------------------------------
# T2 Samra 1 --------------------------------- Last run 31/01/2023
# Putative_T2_195858_74302_5455

# First T2-a : https://docs.google.com/spreadsheets/d/1AJYJZm7yR0yfm7uz88ptYMeeWkm4yezKwN_dHqNtBRk/edit?usp=sharing
Putative_T2_195858_74302_5455_id = "720575940609091704"

# getting the newest ID if changes happened
Putative_T2_195858_74302_5455_id = flywire_latestid(Putative_T2_195858_74302_5455_id)

# read mesh from flywire
Putative_T2_195858_74302_5455_mesh = read_cloudvolume_meshes(Putative_T2_195858_74302_5455_id) 


## DOWNSTREAM PARTNERS based on buhmann synapses 
Putative_T2_195858_74302_5455_output = flywire_partner_summary(Putative_T2_195858_74302_5455_id,partners = "output", cleft.threshold = 100)
Putative_T2_195858_74302_5455_output$pre_id = Putative_T2_195858_74302_5455_id

# check for the latest ID for each partner and add a flywire_URL
Putative_T2_195858_74302_5455_output$partner_type = ""
Putative_T2_195858_74302_5455_output$partner_name = ""
Putative_T2_195858_74302_5455_output$flywire_URL = ""
for (i in 1:length(Putative_T2_195858_74302_5455_output$post_id)){
  Putative_T2_195858_74302_5455_output$post_id[i] = flywire_latestid(Putative_T2_195858_74302_5455_output$post_id[i])
  Putative_T2_195858_74302_5455_output$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_T2_195858_74302_5455_output$pre_id[i],Putative_T2_195858_74302_5455_output$post_id[i]))}","klickable_link")')
} 

# get names and types from flywire_neuron_names table
Putative_T2_195858_74302_5455_output$partner_name = flywire_neuron_name$name[match(Putative_T2_195858_74302_5455_output$post_id,flywire_neuron_name$seg_id)]
Putative_T2_195858_74302_5455_output$partner_type = flywire_neuron_name$type[match(Putative_T2_195858_74302_5455_output$post_id,flywire_neuron_name$seg_id)]



## UPSTREAM PARTNERS based on buhmann synapses 
Putative_T2_195858_74302_5455_input = flywire_partner_summary(Putative_T2_195858_74302_5455_id,partners = "input", cleft.threshold = 100)
Putative_T2_195858_74302_5455_input$post_id = Putative_T2_195858_74302_5455_id

# check for the latest ID for each partner and add a flywire_URL
Putative_T2_195858_74302_5455_input$partner_type = ""
Putative_T2_195858_74302_5455_input$partner_name = ""
Putative_T2_195858_74302_5455_input$flywire_URL = ""
for (i in 1:length(Putative_T2_195858_74302_5455_input$pre_id)){
  Putative_T2_195858_74302_5455_input$pre_id[i] = flywire_latestid(Putative_T2_195858_74302_5455_input$pre_id[i])
  Putative_T2_195858_74302_5455_input$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_T2_195858_74302_5455_input$post_id[i],Putative_T2_195858_74302_5455_input$pre_id[i]))}","klickable_link")')
} 

# get names 
Putative_T2_195858_74302_5455_input$partner_name = flywire_neuron_name$name[match(Putative_T2_195858_74302_5455_input$pre_id,flywire_neuron_name$seg_id)]
Putative_T2_195858_74302_5455_input$partner_type = flywire_neuron_name$type[match(Putative_T2_195858_74302_5455_input$pre_id,flywire_neuron_name$seg_id)]





# writing the connectivity tables back to google sheets
sheet_write(Putative_T2_195858_74302_5455_output, ss="1AJYJZm7yR0yfm7uz88ptYMeeWkm4yezKwN_dHqNtBRk", sheet = "output")
sheet_write(Putative_T2_195858_74302_5455_input, ss="1AJYJZm7yR0yfm7uz88ptYMeeWkm4yezKwN_dHqNtBRk", sheet = "input")
# sheet_write(Putative_T2_195858_74302_5455_exclude, ss= "1AJYJZm7yR0yfm7uz88ptYMeeWkm4yezKwN_dHqNtBRk", sheet = "exclude")


#3rd most important targets:


# LC14a input


# ------------------------------------------------------------------------
# LoME Matanat 1 --------------------------------- Last run 30/10/2023
# Putative_Lobula_to_medulla_170637_66919_3787

# First LoMe : https://docs.google.com/spreadsheets/d/1gn7b0-KtV25s3GY8xQ8FoBGImKsrbFm-EfNwI059aDw/edit?usp=sharing
Putative_Lobula_to_medulla_170637_66919_3787_id = "720575940631845151"

# getting the newest ID if changes happened
Putative_Lobula_to_medulla_170637_66919_3787_id = flywire_latestid(Putative_Lobula_to_medulla_170637_66919_3787_id)

# read mesh from flywire
Putative_Lobula_to_medulla_170637_66919_3787_mesh = read_cloudvolume_meshes(Putative_Lobula_to_medulla_170637_66919_3787_id) 


## DOWNSTREAM PARTNERS based on buhmann synapses 
Putative_Lobula_to_medulla_170637_66919_3787_output = flywire_partner_summary(Putative_Lobula_to_medulla_170637_66919_3787_id,partners = "output", cleft.threshold = 100)
Putative_Lobula_to_medulla_170637_66919_3787_output$pre_id = Putative_Lobula_to_medulla_170637_66919_3787_id

# check for the latest ID for each partner and add a flywire_URL
Putative_Lobula_to_medulla_170637_66919_3787_output$partner_type = ""
Putative_Lobula_to_medulla_170637_66919_3787_output$partner_name = ""
Putative_Lobula_to_medulla_170637_66919_3787_output$flywire_URL = ""
for (i in 1:length(Putative_Lobula_to_medulla_170637_66919_3787_output$post_id)){
  Putative_Lobula_to_medulla_170637_66919_3787_output$post_id[i] = flywire_latestid(Putative_Lobula_to_medulla_170637_66919_3787_output$post_id[i])
  Putative_Lobula_to_medulla_170637_66919_3787_output$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_Lobula_to_medulla_170637_66919_3787_output$pre_id[i],Putative_Lobula_to_medulla_170637_66919_3787_output$post_id[i]))}","klickable_link")')
} 

# get names and types from flywire_neuron_names table
Putative_Lobula_to_medulla_170637_66919_3787_output$partner_name = flywire_neuron_name$name[match(Putative_Lobula_to_medulla_170637_66919_3787_output$post_id,flywire_neuron_name$seg_id)]
Putative_Lobula_to_medulla_170637_66919_3787_output$partner_type = flywire_neuron_name$type[match(Putative_Lobula_to_medulla_170637_66919_3787_output$post_id,flywire_neuron_name$seg_id)]



## UPSTREAM PARTNERS based on buhmann synapses 
Putative_Lobula_to_medulla_170637_66919_3787_input = flywire_partner_summary(Putative_Lobula_to_medulla_170637_66919_3787_id,partners = "input", cleft.threshold = 100)
Putative_Lobula_to_medulla_170637_66919_3787_input$post_id = Putative_Lobula_to_medulla_170637_66919_3787_id

# check for the latest ID for each partner and add a flywire_URL
Putative_Lobula_to_medulla_170637_66919_3787_input$partner_type = ""
Putative_Lobula_to_medulla_170637_66919_3787_input$partner_name = ""
Putative_Lobula_to_medulla_170637_66919_3787_input$flywire_URL = ""
for (i in 1:length(Putative_Lobula_to_medulla_170637_66919_3787_input$pre_id)){
  Putative_Lobula_to_medulla_170637_66919_3787_input$pre_id[i] = flywire_latestid(Putative_Lobula_to_medulla_170637_66919_3787_input$pre_id[i])
  Putative_Lobula_to_medulla_170637_66919_3787_input$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_Lobula_to_medulla_170637_66919_3787_input$post_id[i],Putative_Lobula_to_medulla_170637_66919_3787_input$pre_id[i]))}","klickable_link")')
} 

# get names 
Putative_Lobula_to_medulla_170637_66919_3787_input$partner_name = flywire_neuron_name$name[match(Putative_Lobula_to_medulla_170637_66919_3787_input$pre_id,flywire_neuron_name$seg_id)]
Putative_Lobula_to_medulla_170637_66919_3787_input$partner_type = flywire_neuron_name$type[match(Putative_Lobula_to_medulla_170637_66919_3787_input$pre_id,flywire_neuron_name$seg_id)]





# writing the connectivity tables back to google sheets
sheet_write(Putative_Lobula_to_medulla_170637_66919_3787_output, ss="1gn7b0-KtV25s3GY8xQ8FoBGImKsrbFm-EfNwI059aDw", sheet = "output")
sheet_write(Putative_Lobula_to_medulla_170637_66919_3787_input, ss="1gn7b0-KtV25s3GY8xQ8FoBGImKsrbFm-EfNwI059aDw", sheet = "input")
# sheet_write(Putative_Lobula_to_medulla_170637_66919_3787_exclude, ss= "1gn7b0-KtV25s3GY8xQ8FoBGImKsrbFm-EfNwI059aDw", sheet = "exclude")




# ------------------------------------------------------------------------
# LoME Matanat 2 --------------------------------- Last run 30/10/2023
# Putative_Lobula_to_medulla_99510_65662_4878

# 2 LoMe : https://docs.google.com/spreadsheets/d/1kqMB9v1BeWh4yV6Z-ao1rRwDOsuGxa9V9gRQQoqoXhk/edit?usp=sharing
Putative_Lobula_to_medulla_99510_65662_4878_id = "720575940640815219"

# getting the newest ID if changes happened
Putative_Lobula_to_medulla_99510_65662_4878_id = flywire_latestid(Putative_Lobula_to_medulla_99510_65662_4878_id)

# read mesh from flywire
Putative_Lobula_to_medulla_99510_65662_4878_mesh = read_cloudvolume_meshes(Putative_Lobula_to_medulla_99510_65662_4878_id) 


## DOWNSTREAM PARTNERS based on buhmann synapses 
Putative_Lobula_to_medulla_99510_65662_4878_output = flywire_partner_summary(Putative_Lobula_to_medulla_99510_65662_4878_id,partners = "output", cleft.threshold = 100)
Putative_Lobula_to_medulla_99510_65662_4878_output$pre_id = Putative_Lobula_to_medulla_99510_65662_4878_id

# check for the latest ID for each partner and add a flywire_URL
Putative_Lobula_to_medulla_99510_65662_4878_output$partner_type = ""
Putative_Lobula_to_medulla_99510_65662_4878_output$partner_name = ""
Putative_Lobula_to_medulla_99510_65662_4878_output$flywire_URL = ""
for (i in 1:length(Putative_Lobula_to_medulla_99510_65662_4878_output$post_id)){
  Putative_Lobula_to_medulla_99510_65662_4878_output$post_id[i] = flywire_latestid(Putative_Lobula_to_medulla_99510_65662_4878_output$post_id[i])
  Putative_Lobula_to_medulla_99510_65662_4878_output$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_Lobula_to_medulla_99510_65662_4878_output$pre_id[i],Putative_Lobula_to_medulla_99510_65662_4878_output$post_id[i]))}","klickable_link")')
} 

# get names and types from flywire_neuron_names table
Putative_Lobula_to_medulla_99510_65662_4878_output$partner_name = flywire_neuron_name$name[match(Putative_Lobula_to_medulla_99510_65662_4878_output$post_id,flywire_neuron_name$seg_id)]
Putative_Lobula_to_medulla_99510_65662_4878_output$partner_type = flywire_neuron_name$type[match(Putative_Lobula_to_medulla_99510_65662_4878_output$post_id,flywire_neuron_name$seg_id)]



## UPSTREAM PARTNERS based on buhmann synapses 
Putative_Lobula_to_medulla_99510_65662_4878_input = flywire_partner_summary(Putative_Lobula_to_medulla_99510_65662_4878_id,partners = "input", cleft.threshold = 100)
Putative_Lobula_to_medulla_99510_65662_4878_input$post_id = Putative_Lobula_to_medulla_99510_65662_4878_id

# check for the latest ID for each partner and add a flywire_URL
Putative_Lobula_to_medulla_99510_65662_4878_input$partner_type = ""
Putative_Lobula_to_medulla_99510_65662_4878_input$partner_name = ""
Putative_Lobula_to_medulla_99510_65662_4878_input$flywire_URL = ""
for (i in 1:length(Putative_Lobula_to_medulla_99510_65662_4878_input$pre_id)){
  Putative_Lobula_to_medulla_99510_65662_4878_input$pre_id[i] = flywire_latestid(Putative_Lobula_to_medulla_99510_65662_4878_input$pre_id[i])
  Putative_Lobula_to_medulla_99510_65662_4878_input$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_Lobula_to_medulla_99510_65662_4878_input$post_id[i],Putative_Lobula_to_medulla_99510_65662_4878_input$pre_id[i]))}","klickable_link")')
} 

# get names 
Putative_Lobula_to_medulla_99510_65662_4878_input$partner_name = flywire_neuron_name$name[match(Putative_Lobula_to_medulla_99510_65662_4878_input$pre_id,flywire_neuron_name$seg_id)]
Putative_Lobula_to_medulla_99510_65662_4878_input$partner_type = flywire_neuron_name$type[match(Putative_Lobula_to_medulla_99510_65662_4878_input$pre_id,flywire_neuron_name$seg_id)]





# writing the connectivity tables back to google sheets
sheet_write(Putative_Lobula_to_medulla_99510_65662_4878_output, ss="1kqMB9v1BeWh4yV6Z-ao1rRwDOsuGxa9V9gRQQoqoXhk", sheet = "output")
sheet_write(Putative_Lobula_to_medulla_99510_65662_4878_input, ss="1kqMB9v1BeWh4yV6Z-ao1rRwDOsuGxa9V9gRQQoqoXhk", sheet = "input")
# sheet_write(Putative_Lobula_to_medulla_99510_65662_4878_exclude, ss= "1kqMB9v1BeWh4yV6Z-ao1rRwDOsuGxa9V9gRQQoqoXhk", sheet = "exclude")



# ------------------------------------------------------------------------
# LoME Matanat 3 --------------------------------- Last run 30/10/2023
# Putative_Lobula_to_medulla_192544_81628_3983

# 3 LoMe : https://docs.google.com/spreadsheets/d/1rDVxYIT1AUeZd3NJSLYKjwJd4d5OCnijqzy52Jt0P_o/edit?usp=sharing
Putative_Lobula_to_medulla_192544_81628_3983_id = "720575940647674676"

# getting the newest ID if changes happened
Putative_Lobula_to_medulla_192544_81628_3983_id = flywire_latestid(Putative_Lobula_to_medulla_192544_81628_3983_id)

# read mesh from flywire
Putative_Lobula_to_medulla_192544_81628_3983_mesh = read_cloudvolume_meshes(Putative_Lobula_to_medulla_192544_81628_3983_id) 


## DOWNSTREAM PARTNERS based on buhmann synapses 
Putative_Lobula_to_medulla_192544_81628_3983_output = flywire_partner_summary(Putative_Lobula_to_medulla_192544_81628_3983_id,partners = "output", cleft.threshold = 100)
Putative_Lobula_to_medulla_192544_81628_3983_output$pre_id = Putative_Lobula_to_medulla_192544_81628_3983_id

# check for the latest ID for each partner and add a flywire_URL
Putative_Lobula_to_medulla_192544_81628_3983_output$partner_type = ""
Putative_Lobula_to_medulla_192544_81628_3983_output$partner_name = ""
Putative_Lobula_to_medulla_192544_81628_3983_output$flywire_URL = ""
for (i in 1:length(Putative_Lobula_to_medulla_192544_81628_3983_output$post_id)){
  Putative_Lobula_to_medulla_192544_81628_3983_output$post_id[i] = flywire_latestid(Putative_Lobula_to_medulla_192544_81628_3983_output$post_id[i])
  Putative_Lobula_to_medulla_192544_81628_3983_output$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_Lobula_to_medulla_192544_81628_3983_output$pre_id[i],Putative_Lobula_to_medulla_192544_81628_3983_output$post_id[i]))}","klickable_link")')
} 

# get names and types from flywire_neuron_names table
Putative_Lobula_to_medulla_192544_81628_3983_output$partner_name = flywire_neuron_name$name[match(Putative_Lobula_to_medulla_192544_81628_3983_output$post_id,flywire_neuron_name$seg_id)]
Putative_Lobula_to_medulla_192544_81628_3983_output$partner_type = flywire_neuron_name$type[match(Putative_Lobula_to_medulla_192544_81628_3983_output$post_id,flywire_neuron_name$seg_id)]



## UPSTREAM PARTNERS based on buhmann synapses 
Putative_Lobula_to_medulla_192544_81628_3983_input = flywire_partner_summary(Putative_Lobula_to_medulla_192544_81628_3983_id,partners = "input", cleft.threshold = 100)
Putative_Lobula_to_medulla_192544_81628_3983_input$post_id = Putative_Lobula_to_medulla_192544_81628_3983_id

# check for the latest ID for each partner and add a flywire_URL
Putative_Lobula_to_medulla_192544_81628_3983_input$partner_type = ""
Putative_Lobula_to_medulla_192544_81628_3983_input$partner_name = ""
Putative_Lobula_to_medulla_192544_81628_3983_input$flywire_URL = ""
for (i in 1:length(Putative_Lobula_to_medulla_192544_81628_3983_input$pre_id)){
  Putative_Lobula_to_medulla_192544_81628_3983_input$pre_id[i] = flywire_latestid(Putative_Lobula_to_medulla_192544_81628_3983_input$pre_id[i])
  Putative_Lobula_to_medulla_192544_81628_3983_input$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_Lobula_to_medulla_192544_81628_3983_input$post_id[i],Putative_Lobula_to_medulla_192544_81628_3983_input$pre_id[i]))}","klickable_link")')
} 

# get names 
Putative_Lobula_to_medulla_192544_81628_3983_input$partner_name = flywire_neuron_name$name[match(Putative_Lobula_to_medulla_192544_81628_3983_input$pre_id,flywire_neuron_name$seg_id)]
Putative_Lobula_to_medulla_192544_81628_3983_input$partner_type = flywire_neuron_name$type[match(Putative_Lobula_to_medulla_192544_81628_3983_input$pre_id,flywire_neuron_name$seg_id)]





# writing the connectivity tables back to google sheets
sheet_write(Putative_Lobula_to_medulla_192544_81628_3983_output, ss="1rDVxYIT1AUeZd3NJSLYKjwJd4d5OCnijqzy52Jt0P_o", sheet = "output")
sheet_write(Putative_Lobula_to_medulla_192544_81628_3983_input, ss="1rDVxYIT1AUeZd3NJSLYKjwJd4d5OCnijqzy52Jt0P_o", sheet = "input")
# sheet_write(Putative_Lobula_to_medulla_192544_81628_3983_exclude, ss= "1rDVxYIT1AUeZd3NJSLYKjwJd4d5OCnijqzy52Jt0P_o", sheet = "exclude")



# LC14a output


# ------------------------------------------------------------------------
# Li1L Matanat 1 --------------------------------- Last run 30/10/2023
# Putative_Li1L_87230_60015_4247

# 3 LoMe : https://docs.google.com/spreadsheets/d/1cVUjBlHPwCAQ6UCHIMHOQxBYxq38W2L9rLgM_D8iRm0/edit?usp=sharing
Putative_Li1L_87230_60015_4247_id = "720575940628258577"

# getting the newest ID if changes happened
Putative_Li1L_87230_60015_4247_id = flywire_latestid(Putative_Li1L_87230_60015_4247_id)

# read mesh from flywire
Putative_Li1L_87230_60015_4247_mesh = read_cloudvolume_meshes(Putative_Li1L_87230_60015_4247_id) 


## DOWNSTREAM PARTNERS based on buhmann synapses 
Putative_Li1L_87230_60015_4247_output = flywire_partner_summary(Putative_Li1L_87230_60015_4247_id,partners = "output", cleft.threshold = 100)
Putative_Li1L_87230_60015_4247_output$pre_id = Putative_Li1L_87230_60015_4247_id

# check for the latest ID for each partner and add a flywire_URL
Putative_Li1L_87230_60015_4247_output$partner_type = ""
Putative_Li1L_87230_60015_4247_output$partner_name = ""
Putative_Li1L_87230_60015_4247_output$flywire_URL = ""
for (i in 1:length(Putative_Li1L_87230_60015_4247_output$post_id)){
  Putative_Li1L_87230_60015_4247_output$post_id[i] = flywire_latestid(Putative_Li1L_87230_60015_4247_output$post_id[i])
  Putative_Li1L_87230_60015_4247_output$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_Li1L_87230_60015_4247_output$pre_id[i],Putative_Li1L_87230_60015_4247_output$post_id[i]))}","klickable_link")')
} 

# get names and types from flywire_neuron_names table
Putative_Li1L_87230_60015_4247_output$partner_name = flywire_neuron_name$name[match(Putative_Li1L_87230_60015_4247_output$post_id,flywire_neuron_name$seg_id)]
Putative_Li1L_87230_60015_4247_output$partner_type = flywire_neuron_name$type[match(Putative_Li1L_87230_60015_4247_output$post_id,flywire_neuron_name$seg_id)]



## UPSTREAM PARTNERS based on buhmann synapses 
Putative_Li1L_87230_60015_4247_input = flywire_partner_summary(Putative_Li1L_87230_60015_4247_id,partners = "input", cleft.threshold = 100)
Putative_Li1L_87230_60015_4247_input$post_id = Putative_Li1L_87230_60015_4247_id

# check for the latest ID for each partner and add a flywire_URL
Putative_Li1L_87230_60015_4247_input$partner_type = ""
Putative_Li1L_87230_60015_4247_input$partner_name = ""
Putative_Li1L_87230_60015_4247_input$flywire_URL = ""
for (i in 1:length(Putative_Li1L_87230_60015_4247_input$pre_id)){
  Putative_Li1L_87230_60015_4247_input$pre_id[i] = flywire_latestid(Putative_Li1L_87230_60015_4247_input$pre_id[i])
  Putative_Li1L_87230_60015_4247_input$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_Li1L_87230_60015_4247_input$post_id[i],Putative_Li1L_87230_60015_4247_input$pre_id[i]))}","klickable_link")')
} 

# get names 
Putative_Li1L_87230_60015_4247_input$partner_name = flywire_neuron_name$name[match(Putative_Li1L_87230_60015_4247_input$pre_id,flywire_neuron_name$seg_id)]
Putative_Li1L_87230_60015_4247_input$partner_type = flywire_neuron_name$type[match(Putative_Li1L_87230_60015_4247_input$pre_id,flywire_neuron_name$seg_id)]





# writing the connectivity tables back to google sheets
sheet_write(Putative_Li1L_87230_60015_4247_output, ss="1cVUjBlHPwCAQ6UCHIMHOQxBYxq38W2L9rLgM_D8iRm0", sheet = "output")
sheet_write(Putative_Li1L_87230_60015_4247_input, ss="1cVUjBlHPwCAQ6UCHIMHOQxBYxq38W2L9rLgM_D8iRm0", sheet = "input")
# sheet_write(Putative_Li1L_87230_60015_4247_exclude, ss= "1cVUjBlHPwCAQ6UCHIMHOQxBYxq38W2L9rLgM_D8iRm0", sheet = "exclude")



# ------------------------------------------------------------------------
# Li1L Matanat 2 --------------------------------- Last run 30/10/2023
# Putative_Li1L_93523_66810_4558

# 2 li1L : https://docs.google.com/spreadsheets/d/11LvbADEHe8H-XF-3Hm7LDc-kgnbEpcZHYXgqD8jLC3E/edit?usp=sharing
Putative_Li1L_93523_66810_4558_id = "720575940629179900"

# getting the newest ID if changes happened
Putative_Li1L_93523_66810_4558_id = flywire_latestid(Putative_Li1L_93523_66810_4558_id)

# read mesh from flywire
Putative_Li1L_93523_66810_4558_mesh = read_cloudvolume_meshes(Putative_Li1L_93523_66810_4558_id) 


## DOWNSTREAM PARTNERS based on buhmann synapses 
Putative_Li1L_93523_66810_4558_output = flywire_partner_summary(Putative_Li1L_93523_66810_4558_id,partners = "output", cleft.threshold = 100)
Putative_Li1L_93523_66810_4558_output$pre_id = Putative_Li1L_93523_66810_4558_id

# check for the latest ID for each partner and add a flywire_URL
Putative_Li1L_93523_66810_4558_output$partner_type = ""
Putative_Li1L_93523_66810_4558_output$partner_name = ""
Putative_Li1L_93523_66810_4558_output$flywire_URL = ""
for (i in 1:length(Putative_Li1L_93523_66810_4558_output$post_id)){
  Putative_Li1L_93523_66810_4558_output$post_id[i] = flywire_latestid(Putative_Li1L_93523_66810_4558_output$post_id[i])
  Putative_Li1L_93523_66810_4558_output$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_Li1L_93523_66810_4558_output$pre_id[i],Putative_Li1L_93523_66810_4558_output$post_id[i]))}","klickable_link")')
} 

# get names and types from flywire_neuron_names table
Putative_Li1L_93523_66810_4558_output$partner_name = flywire_neuron_name$name[match(Putative_Li1L_93523_66810_4558_output$post_id,flywire_neuron_name$seg_id)]
Putative_Li1L_93523_66810_4558_output$partner_type = flywire_neuron_name$type[match(Putative_Li1L_93523_66810_4558_output$post_id,flywire_neuron_name$seg_id)]



## UPSTREAM PARTNERS based on buhmann synapses 
Putative_Li1L_93523_66810_4558_input = flywire_partner_summary(Putative_Li1L_93523_66810_4558_id,partners = "input", cleft.threshold = 100)
Putative_Li1L_93523_66810_4558_input$post_id = Putative_Li1L_93523_66810_4558_id

# check for the latest ID for each partner and add a flywire_URL
Putative_Li1L_93523_66810_4558_input$partner_type = ""
Putative_Li1L_93523_66810_4558_input$partner_name = ""
Putative_Li1L_93523_66810_4558_input$flywire_URL = ""
for (i in 1:length(Putative_Li1L_93523_66810_4558_input$pre_id)){
  Putative_Li1L_93523_66810_4558_input$pre_id[i] = flywire_latestid(Putative_Li1L_93523_66810_4558_input$pre_id[i])
  Putative_Li1L_93523_66810_4558_input$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_Li1L_93523_66810_4558_input$post_id[i],Putative_Li1L_93523_66810_4558_input$pre_id[i]))}","klickable_link")')
} 

# get names 
Putative_Li1L_93523_66810_4558_input$partner_name = flywire_neuron_name$name[match(Putative_Li1L_93523_66810_4558_input$pre_id,flywire_neuron_name$seg_id)]
Putative_Li1L_93523_66810_4558_input$partner_type = flywire_neuron_name$type[match(Putative_Li1L_93523_66810_4558_input$pre_id,flywire_neuron_name$seg_id)]





# writing the connectivity tables back to google sheets
sheet_write(Putative_Li1L_93523_66810_4558_output, ss="11LvbADEHe8H-XF-3Hm7LDc-kgnbEpcZHYXgqD8jLC3E", sheet = "output")
sheet_write(Putative_Li1L_93523_66810_4558_input, ss="11LvbADEHe8H-XF-3Hm7LDc-kgnbEpcZHYXgqD8jLC3E", sheet = "input")
# sheet_write(Putative_Li1L_93523_66810_4558_exclude, ss= "11LvbADEHe8H-XF-3Hm7LDc-kgnbEpcZHYXgqD8jLC3E", sheet = "exclude")


# ------------------------------------------------------------------------
# Li1L Matanat 3 --------------------------------- Last run 30/10/2023
# Putative_Li1L_165450_74613_3761

# 3 li1L : https://docs.google.com/spreadsheets/d/1ZkRRcZXHsqaBbPWr9fjhJJYIBgMlGnlJG2GrLL_-HmI/edit?usp=sharing
Putative_Li1L_165450_74613_3761_id = "720575940606608230"

# getting the newest ID if changes happened
Putative_Li1L_165450_74613_3761_id = flywire_latestid(Putative_Li1L_165450_74613_3761_id)

# read mesh from flywire
Putative_Li1L_165450_74613_3761_mesh = read_cloudvolume_meshes(Putative_Li1L_165450_74613_3761_id) 


## DOWNSTREAM PARTNERS based on buhmann synapses 
Putative_Li1L_165450_74613_3761_output = flywire_partner_summary(Putative_Li1L_165450_74613_3761_id,partners = "output", cleft.threshold = 100)
Putative_Li1L_165450_74613_3761_output$pre_id = Putative_Li1L_165450_74613_3761_id

# check for the latest ID for each partner and add a flywire_URL
Putative_Li1L_165450_74613_3761_output$partner_type = ""
Putative_Li1L_165450_74613_3761_output$partner_name = ""
Putative_Li1L_165450_74613_3761_output$flywire_URL = ""
for (i in 1:length(Putative_Li1L_165450_74613_3761_output$post_id)){
  Putative_Li1L_165450_74613_3761_output$post_id[i] = flywire_latestid(Putative_Li1L_165450_74613_3761_output$post_id[i])
  Putative_Li1L_165450_74613_3761_output$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_Li1L_165450_74613_3761_output$pre_id[i],Putative_Li1L_165450_74613_3761_output$post_id[i]))}","klickable_link")')
} 

# get names and types from flywire_neuron_names table
Putative_Li1L_165450_74613_3761_output$partner_name = flywire_neuron_name$name[match(Putative_Li1L_165450_74613_3761_output$post_id,flywire_neuron_name$seg_id)]
Putative_Li1L_165450_74613_3761_output$partner_type = flywire_neuron_name$type[match(Putative_Li1L_165450_74613_3761_output$post_id,flywire_neuron_name$seg_id)]



## UPSTREAM PARTNERS based on buhmann synapses 
Putative_Li1L_165450_74613_3761_input = flywire_partner_summary(Putative_Li1L_165450_74613_3761_id,partners = "input", cleft.threshold = 100)
Putative_Li1L_165450_74613_3761_input$post_id = Putative_Li1L_165450_74613_3761_id

# check for the latest ID for each partner and add a flywire_URL
Putative_Li1L_165450_74613_3761_input$partner_type = ""
Putative_Li1L_165450_74613_3761_input$partner_name = ""
Putative_Li1L_165450_74613_3761_input$flywire_URL = ""
for (i in 1:length(Putative_Li1L_165450_74613_3761_input$pre_id)){
  Putative_Li1L_165450_74613_3761_input$pre_id[i] = flywire_latestid(Putative_Li1L_165450_74613_3761_input$pre_id[i])
  Putative_Li1L_165450_74613_3761_input$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_Li1L_165450_74613_3761_input$post_id[i],Putative_Li1L_165450_74613_3761_input$pre_id[i]))}","klickable_link")')
} 

# get names 
Putative_Li1L_165450_74613_3761_input$partner_name = flywire_neuron_name$name[match(Putative_Li1L_165450_74613_3761_input$pre_id,flywire_neuron_name$seg_id)]
Putative_Li1L_165450_74613_3761_input$partner_type = flywire_neuron_name$type[match(Putative_Li1L_165450_74613_3761_input$pre_id,flywire_neuron_name$seg_id)]





# writing the connectivity tables back to google sheets
sheet_write(Putative_Li1L_165450_74613_3761_output, ss="1ZkRRcZXHsqaBbPWr9fjhJJYIBgMlGnlJG2GrLL_-HmI", sheet = "output")
sheet_write(Putative_Li1L_165450_74613_3761_input, ss="1ZkRRcZXHsqaBbPWr9fjhJJYIBgMlGnlJG2GrLL_-HmI", sheet = "input")
# sheet_write(Putative_Li1L_165450_74613_3761_exclude, ss= "1ZkRRcZXHsqaBbPWr9fjhJJYIBgMlGnlJG2GrLL_-HmI", sheet = "exclude")



# LC14b input


# ------------------------------------------------------------------------
# TmY5 Matanat 1 --------------------------------- Last run 30/10/2023
# Putative_TmY5_61241_75697_6407

# 1 TmY5 : https://docs.google.com/spreadsheets/d/1V2FtkHcraOAvoru2oarJjL3vQoBHgSE7xueYl6sOe9E/edit?usp=sharing
Putative_TmY5_61241_75697_6407_id = "720575940610998136"

# getting the newest ID if changes happened
Putative_TmY5_61241_75697_6407_id = flywire_latestid(Putative_TmY5_61241_75697_6407_id)

# read mesh from flywire
Putative_TmY5_61241_75697_6407_mesh = read_cloudvolume_meshes(Putative_TmY5_61241_75697_6407_id) 


## DOWNSTREAM PARTNERS based on buhmann synapses 
Putative_TmY5_61241_75697_6407_output = flywire_partner_summary(Putative_TmY5_61241_75697_6407_id,partners = "output", cleft.threshold = 100)
Putative_TmY5_61241_75697_6407_output$pre_id = Putative_TmY5_61241_75697_6407_id

# check for the latest ID for each partner and add a flywire_URL
Putative_TmY5_61241_75697_6407_output$partner_type = ""
Putative_TmY5_61241_75697_6407_output$partner_name = ""
Putative_TmY5_61241_75697_6407_output$flywire_URL = ""
for (i in 1:length(Putative_TmY5_61241_75697_6407_output$post_id)){
  Putative_TmY5_61241_75697_6407_output$post_id[i] = flywire_latestid(Putative_TmY5_61241_75697_6407_output$post_id[i])
  Putative_TmY5_61241_75697_6407_output$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_TmY5_61241_75697_6407_output$pre_id[i],Putative_TmY5_61241_75697_6407_output$post_id[i]))}","klickable_link")')
} 

# get names and types from flywire_neuron_names table
Putative_TmY5_61241_75697_6407_output$partner_name = flywire_neuron_name$name[match(Putative_TmY5_61241_75697_6407_output$post_id,flywire_neuron_name$seg_id)]
Putative_TmY5_61241_75697_6407_output$partner_type = flywire_neuron_name$type[match(Putative_TmY5_61241_75697_6407_output$post_id,flywire_neuron_name$seg_id)]



## UPSTREAM PARTNERS based on buhmann synapses 
Putative_TmY5_61241_75697_6407_input = flywire_partner_summary(Putative_TmY5_61241_75697_6407_id,partners = "input", cleft.threshold = 100)
Putative_TmY5_61241_75697_6407_input$post_id = Putative_TmY5_61241_75697_6407_id

# check for the latest ID for each partner and add a flywire_URL
Putative_TmY5_61241_75697_6407_input$partner_type = ""
Putative_TmY5_61241_75697_6407_input$partner_name = ""
Putative_TmY5_61241_75697_6407_input$flywire_URL = ""
for (i in 1:length(Putative_TmY5_61241_75697_6407_input$pre_id)){
  Putative_TmY5_61241_75697_6407_input$pre_id[i] = flywire_latestid(Putative_TmY5_61241_75697_6407_input$pre_id[i])
  Putative_TmY5_61241_75697_6407_input$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_TmY5_61241_75697_6407_input$post_id[i],Putative_TmY5_61241_75697_6407_input$pre_id[i]))}","klickable_link")')
} 

# get names 
Putative_TmY5_61241_75697_6407_input$partner_name = flywire_neuron_name$name[match(Putative_TmY5_61241_75697_6407_input$pre_id,flywire_neuron_name$seg_id)]
Putative_TmY5_61241_75697_6407_input$partner_type = flywire_neuron_name$type[match(Putative_TmY5_61241_75697_6407_input$pre_id,flywire_neuron_name$seg_id)]





# writing the connectivity tables back to google sheets
sheet_write(Putative_TmY5_61241_75697_6407_output, ss="1V2FtkHcraOAvoru2oarJjL3vQoBHgSE7xueYl6sOe9E", sheet = "output")
sheet_write(Putative_TmY5_61241_75697_6407_input, ss="1V2FtkHcraOAvoru2oarJjL3vQoBHgSE7xueYl6sOe9E", sheet = "input")
# sheet_write(Putative_TmY5_61241_75697_6407_exclude, ss= "1V2FtkHcraOAvoru2oarJjL3vQoBHgSE7xueYl6sOe9E", sheet = "exclude")




# ------------------------------------------------------------------------
# TmY5 Matanat 2 --------------------------------- Last run 30/10/2023
# Putative_TmY5_56518_75138_5791

# 2 TmY5 : https://docs.google.com/spreadsheets/d/13z6BnOv_l9aBavvJK3hda50Vx4SOHLM4zjMNOs-gNW0/edit?usp=sharing
Putative_TmY5_56518_75138_5791_id = "720575940631319353"

# getting the newest ID if changes happened
Putative_TmY5_56518_75138_5791_id = flywire_latestid(Putative_TmY5_56518_75138_5791_id)

# read mesh from flywire
Putative_TmY5_56518_75138_5791_mesh = read_cloudvolume_meshes(Putative_TmY5_56518_75138_5791_id) 


## DOWNSTREAM PARTNERS based on buhmann synapses 
Putative_TmY5_56518_75138_5791_output = flywire_partner_summary(Putative_TmY5_56518_75138_5791_id,partners = "output", cleft.threshold = 100)
Putative_TmY5_56518_75138_5791_output$pre_id = Putative_TmY5_56518_75138_5791_id

# check for the latest ID for each partner and add a flywire_URL
Putative_TmY5_56518_75138_5791_output$partner_type = ""
Putative_TmY5_56518_75138_5791_output$partner_name = ""
Putative_TmY5_56518_75138_5791_output$flywire_URL = ""
for (i in 1:length(Putative_TmY5_56518_75138_5791_output$post_id)){
  Putative_TmY5_56518_75138_5791_output$post_id[i] = flywire_latestid(Putative_TmY5_56518_75138_5791_output$post_id[i])
  Putative_TmY5_56518_75138_5791_output$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_TmY5_56518_75138_5791_output$pre_id[i],Putative_TmY5_56518_75138_5791_output$post_id[i]))}","klickable_link")')
} 

# get names and types from flywire_neuron_names table
Putative_TmY5_56518_75138_5791_output$partner_name = flywire_neuron_name$name[match(Putative_TmY5_56518_75138_5791_output$post_id,flywire_neuron_name$seg_id)]
Putative_TmY5_56518_75138_5791_output$partner_type = flywire_neuron_name$type[match(Putative_TmY5_56518_75138_5791_output$post_id,flywire_neuron_name$seg_id)]



## UPSTREAM PARTNERS based on buhmann synapses 
Putative_TmY5_56518_75138_5791_input = flywire_partner_summary(Putative_TmY5_56518_75138_5791_id,partners = "input", cleft.threshold = 100)
Putative_TmY5_56518_75138_5791_input$post_id = Putative_TmY5_56518_75138_5791_id

# check for the latest ID for each partner and add a flywire_URL
Putative_TmY5_56518_75138_5791_input$partner_type = ""
Putative_TmY5_56518_75138_5791_input$partner_name = ""
Putative_TmY5_56518_75138_5791_input$flywire_URL = ""
for (i in 1:length(Putative_TmY5_56518_75138_5791_input$pre_id)){
  Putative_TmY5_56518_75138_5791_input$pre_id[i] = flywire_latestid(Putative_TmY5_56518_75138_5791_input$pre_id[i])
  Putative_TmY5_56518_75138_5791_input$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_TmY5_56518_75138_5791_input$post_id[i],Putative_TmY5_56518_75138_5791_input$pre_id[i]))}","klickable_link")')
} 

# get names 
Putative_TmY5_56518_75138_5791_input$partner_name = flywire_neuron_name$name[match(Putative_TmY5_56518_75138_5791_input$pre_id,flywire_neuron_name$seg_id)]
Putative_TmY5_56518_75138_5791_input$partner_type = flywire_neuron_name$type[match(Putative_TmY5_56518_75138_5791_input$pre_id,flywire_neuron_name$seg_id)]





# writing the connectivity tables back to google sheets
sheet_write(Putative_TmY5_56518_75138_5791_output, ss="13z6BnOv_l9aBavvJK3hda50Vx4SOHLM4zjMNOs-gNW0", sheet = "output")
sheet_write(Putative_TmY5_56518_75138_5791_input, ss="13z6BnOv_l9aBavvJK3hda50Vx4SOHLM4zjMNOs-gNW0", sheet = "input")
# sheet_write(Putative_TmY5_56518_75138_5791_exclude, ss= "13z6BnOv_l9aBavvJK3hda50Vx4SOHLM4zjMNOs-gNW0", sheet = "exclude")



# ------------------------------------------------------------------------
# TmY5 Matanat 3 --------------------------------- Last run 30/10/2023
# Putative_TmY5_198402_82280_4704

# 3 TmY5 : https://docs.google.com/spreadsheets/d/1A8m-pCT_wiZ6_diRdcmByp-fwbXtoxAa7jlMgXofx2Q/edit?usp=sharing
Putative_TmY5_198402_82280_4704_id = "720575940624088840"

# getting the newest ID if changes happened
Putative_TmY5_198402_82280_4704_id = flywire_latestid(Putative_TmY5_198402_82280_4704_id)

# read mesh from flywire
Putative_TmY5_198402_82280_4704_mesh = read_cloudvolume_meshes(Putative_TmY5_198402_82280_4704_id) 


## DOWNSTREAM PARTNERS based on buhmann synapses 
Putative_TmY5_198402_82280_4704_output = flywire_partner_summary(Putative_TmY5_198402_82280_4704_id,partners = "output", cleft.threshold = 100)
Putative_TmY5_198402_82280_4704_output$pre_id = Putative_TmY5_198402_82280_4704_id

# check for the latest ID for each partner and add a flywire_URL
Putative_TmY5_198402_82280_4704_output$partner_type = ""
Putative_TmY5_198402_82280_4704_output$partner_name = ""
Putative_TmY5_198402_82280_4704_output$flywire_URL = ""
for (i in 1:length(Putative_TmY5_198402_82280_4704_output$post_id)){
  Putative_TmY5_198402_82280_4704_output$post_id[i] = flywire_latestid(Putative_TmY5_198402_82280_4704_output$post_id[i])
  Putative_TmY5_198402_82280_4704_output$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_TmY5_198402_82280_4704_output$pre_id[i],Putative_TmY5_198402_82280_4704_output$post_id[i]))}","klickable_link")')
} 

# get names and types from flywire_neuron_names table
Putative_TmY5_198402_82280_4704_output$partner_name = flywire_neuron_name$name[match(Putative_TmY5_198402_82280_4704_output$post_id,flywire_neuron_name$seg_id)]
Putative_TmY5_198402_82280_4704_output$partner_type = flywire_neuron_name$type[match(Putative_TmY5_198402_82280_4704_output$post_id,flywire_neuron_name$seg_id)]



## UPSTREAM PARTNERS based on buhmann synapses 
Putative_TmY5_198402_82280_4704_input = flywire_partner_summary(Putative_TmY5_198402_82280_4704_id,partners = "input", cleft.threshold = 100)
Putative_TmY5_198402_82280_4704_input$post_id = Putative_TmY5_198402_82280_4704_id

# check for the latest ID for each partner and add a flywire_URL
Putative_TmY5_198402_82280_4704_input$partner_type = ""
Putative_TmY5_198402_82280_4704_input$partner_name = ""
Putative_TmY5_198402_82280_4704_input$flywire_URL = ""
for (i in 1:length(Putative_TmY5_198402_82280_4704_input$pre_id)){
  Putative_TmY5_198402_82280_4704_input$pre_id[i] = flywire_latestid(Putative_TmY5_198402_82280_4704_input$pre_id[i])
  Putative_TmY5_198402_82280_4704_input$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_TmY5_198402_82280_4704_input$post_id[i],Putative_TmY5_198402_82280_4704_input$pre_id[i]))}","klickable_link")')
} 

# get names 
Putative_TmY5_198402_82280_4704_input$partner_name = flywire_neuron_name$name[match(Putative_TmY5_198402_82280_4704_input$pre_id,flywire_neuron_name$seg_id)]
Putative_TmY5_198402_82280_4704_input$partner_type = flywire_neuron_name$type[match(Putative_TmY5_198402_82280_4704_input$pre_id,flywire_neuron_name$seg_id)]





# writing the connectivity tables back to google sheets
sheet_write(Putative_TmY5_198402_82280_4704_output, ss="1A8m-pCT_wiZ6_diRdcmByp-fwbXtoxAa7jlMgXofx2Q", sheet = "output")
sheet_write(Putative_TmY5_198402_82280_4704_input, ss="1A8m-pCT_wiZ6_diRdcmByp-fwbXtoxAa7jlMgXofx2Q", sheet = "input")
# sheet_write(Putative_TmY5_198402_82280_4704_exclude, ss= "1A8m-pCT_wiZ6_diRdcmByp-fwbXtoxAa7jlMgXofx2Q", sheet = "exclude")


# LC14b output


# ------------------------------------------------------------------------
# Li1 Matanat 1 --------------------------------- Last run 30/10/2023
# Putative_Li1XL_93737_56398_4823

# 1 li1 : https://docs.google.com/spreadsheets/d/1tDcU2ytkOWH55d7qdBznG5-bTFV1A1vQNinMHjUJndU/edit?usp=sharing
Putative_Li1XL_93737_56398_4823_id = "720575940644823456"

# getting the newest ID if changes happened
Putative_Li1XL_93737_56398_4823_id = flywire_latestid(Putative_Li1XL_93737_56398_4823_id)

# read mesh from flywire
Putative_Li1XL_93737_56398_4823_mesh = read_cloudvolume_meshes(Putative_Li1XL_93737_56398_4823_id) 


## DOWNSTREAM PARTNERS based on buhmann synapses 
Putative_Li1XL_93737_56398_4823_output = flywire_partner_summary(Putative_Li1XL_93737_56398_4823_id,partners = "output", cleft.threshold = 100)
Putative_Li1XL_93737_56398_4823_output$pre_id = Putative_Li1XL_93737_56398_4823_id

# check for the latest ID for each partner and add a flywire_URL
Putative_Li1XL_93737_56398_4823_output$partner_type = ""
Putative_Li1XL_93737_56398_4823_output$partner_name = ""
Putative_Li1XL_93737_56398_4823_output$flywire_URL = ""
for (i in 1:length(Putative_Li1XL_93737_56398_4823_output$post_id)){
  Putative_Li1XL_93737_56398_4823_output$post_id[i] = flywire_latestid(Putative_Li1XL_93737_56398_4823_output$post_id[i])
  Putative_Li1XL_93737_56398_4823_output$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_Li1XL_93737_56398_4823_output$pre_id[i],Putative_Li1XL_93737_56398_4823_output$post_id[i]))}","klickable_link")')
} 

# get names and types from flywire_neuron_names table
Putative_Li1XL_93737_56398_4823_output$partner_name = flywire_neuron_name$name[match(Putative_Li1XL_93737_56398_4823_output$post_id,flywire_neuron_name$seg_id)]
Putative_Li1XL_93737_56398_4823_output$partner_type = flywire_neuron_name$type[match(Putative_Li1XL_93737_56398_4823_output$post_id,flywire_neuron_name$seg_id)]



## UPSTREAM PARTNERS based on buhmann synapses 
Putative_Li1XL_93737_56398_4823_input = flywire_partner_summary(Putative_Li1XL_93737_56398_4823_id,partners = "input", cleft.threshold = 100)
Putative_Li1XL_93737_56398_4823_input$post_id = Putative_Li1XL_93737_56398_4823_id

# check for the latest ID for each partner and add a flywire_URL
Putative_Li1XL_93737_56398_4823_input$partner_type = ""
Putative_Li1XL_93737_56398_4823_input$partner_name = ""
Putative_Li1XL_93737_56398_4823_input$flywire_URL = ""
for (i in 1:length(Putative_Li1XL_93737_56398_4823_input$pre_id)){
  Putative_Li1XL_93737_56398_4823_input$pre_id[i] = flywire_latestid(Putative_Li1XL_93737_56398_4823_input$pre_id[i])
  Putative_Li1XL_93737_56398_4823_input$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_Li1XL_93737_56398_4823_input$post_id[i],Putative_Li1XL_93737_56398_4823_input$pre_id[i]))}","klickable_link")')
} 

# get names 
Putative_Li1XL_93737_56398_4823_input$partner_name = flywire_neuron_name$name[match(Putative_Li1XL_93737_56398_4823_input$pre_id,flywire_neuron_name$seg_id)]
Putative_Li1XL_93737_56398_4823_input$partner_type = flywire_neuron_name$type[match(Putative_Li1XL_93737_56398_4823_input$pre_id,flywire_neuron_name$seg_id)]





# writing the connectivity tables back to google sheets
sheet_write(Putative_Li1XL_93737_56398_4823_output, ss="1tDcU2ytkOWH55d7qdBznG5-bTFV1A1vQNinMHjUJndU", sheet = "output")
sheet_write(Putative_Li1XL_93737_56398_4823_input, ss="1tDcU2ytkOWH55d7qdBznG5-bTFV1A1vQNinMHjUJndU", sheet = "input")
# sheet_write(Putative_Li1XL_93737_56398_4823_exclude, ss= "1tDcU2ytkOWH55d7qdBznG5-bTFV1A1vQNinMHjUJndU", sheet = "exclude")




# ------------------------------------------------------------------------
# Li1 Matanat 2 --------------------------------- Last run 30/10/2023
# Putative_Li1Y_165030_69393_3946

# 2 li1 : https://docs.google.com/spreadsheets/d/1an1aQKjHYZHP17TOidaByxlqbneet7Cy8COx6C4XxKI/edit?usp=sharing
Putative_Li1Y_165030_69393_3946_id = "720575940661420929"

# getting the newest ID if changes happened
Putative_Li1Y_165030_69393_3946_id = flywire_latestid(Putative_Li1Y_165030_69393_3946_id)

# read mesh from flywire
Putative_Li1Y_165030_69393_3946_mesh = read_cloudvolume_meshes(Putative_Li1Y_165030_69393_3946_id) 


## DOWNSTREAM PARTNERS based on buhmann synapses 
Putative_Li1Y_165030_69393_3946_output = flywire_partner_summary(Putative_Li1Y_165030_69393_3946_id,partners = "output", cleft.threshold = 100)
Putative_Li1Y_165030_69393_3946_output$pre_id = Putative_Li1Y_165030_69393_3946_id

# check for the latest ID for each partner and add a flywire_URL
Putative_Li1Y_165030_69393_3946_output$partner_type = ""
Putative_Li1Y_165030_69393_3946_output$partner_name = ""
Putative_Li1Y_165030_69393_3946_output$flywire_URL = ""
for (i in 1:length(Putative_Li1Y_165030_69393_3946_output$post_id)){
  Putative_Li1Y_165030_69393_3946_output$post_id[i] = flywire_latestid(Putative_Li1Y_165030_69393_3946_output$post_id[i])
  Putative_Li1Y_165030_69393_3946_output$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_Li1Y_165030_69393_3946_output$pre_id[i],Putative_Li1Y_165030_69393_3946_output$post_id[i]))}","klickable_link")')
} 

# get names and types from flywire_neuron_names table
Putative_Li1Y_165030_69393_3946_output$partner_name = flywire_neuron_name$name[match(Putative_Li1Y_165030_69393_3946_output$post_id,flywire_neuron_name$seg_id)]
Putative_Li1Y_165030_69393_3946_output$partner_type = flywire_neuron_name$type[match(Putative_Li1Y_165030_69393_3946_output$post_id,flywire_neuron_name$seg_id)]



## UPSTREAM PARTNERS based on buhmann synapses 
Putative_Li1Y_165030_69393_3946_input = flywire_partner_summary(Putative_Li1Y_165030_69393_3946_id,partners = "input", cleft.threshold = 100)
Putative_Li1Y_165030_69393_3946_input$post_id = Putative_Li1Y_165030_69393_3946_id

# check for the latest ID for each partner and add a flywire_URL
Putative_Li1Y_165030_69393_3946_input$partner_type = ""
Putative_Li1Y_165030_69393_3946_input$partner_name = ""
Putative_Li1Y_165030_69393_3946_input$flywire_URL = ""
for (i in 1:length(Putative_Li1Y_165030_69393_3946_input$pre_id)){
  Putative_Li1Y_165030_69393_3946_input$pre_id[i] = flywire_latestid(Putative_Li1Y_165030_69393_3946_input$pre_id[i])
  Putative_Li1Y_165030_69393_3946_input$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_Li1Y_165030_69393_3946_input$post_id[i],Putative_Li1Y_165030_69393_3946_input$pre_id[i]))}","klickable_link")')
} 

# get names 
Putative_Li1Y_165030_69393_3946_input$partner_name = flywire_neuron_name$name[match(Putative_Li1Y_165030_69393_3946_input$pre_id,flywire_neuron_name$seg_id)]
Putative_Li1Y_165030_69393_3946_input$partner_type = flywire_neuron_name$type[match(Putative_Li1Y_165030_69393_3946_input$pre_id,flywire_neuron_name$seg_id)]





# writing the connectivity tables back to google sheets
sheet_write(Putative_Li1Y_165030_69393_3946_output, ss="1an1aQKjHYZHP17TOidaByxlqbneet7Cy8COx6C4XxKI", sheet = "output")
sheet_write(Putative_Li1Y_165030_69393_3946_input, ss="1an1aQKjHYZHP17TOidaByxlqbneet7Cy8COx6C4XxKI", sheet = "input")
# sheet_write(Putative_Li1Y_165030_69393_3946_exclude, ss= "1an1aQKjHYZHP17TOidaByxlqbneet7Cy8COx6C4XxKI", sheet = "exclude")



# ------------------------------------------------------------------------
# Li1 Matanat 3 --------------------------------- Last run 30/10/2023
# Putative_Li1_174692_69984_3320

# 3 li1 : https://docs.google.com/spreadsheets/d/1R3V52LMywa1up9XdKyE3yp2ah-7AmFxjWo-esXnFvd8/edit?usp=sharing
Putative_Li1_174692_69984_3320_id = "720575940622031201"

# getting the newest ID if changes happened
Putative_Li1_174692_69984_3320_id = flywire_latestid(Putative_Li1_174692_69984_3320_id)

# read mesh from flywire
Putative_Li1_174692_69984_3320_mesh = read_cloudvolume_meshes(Putative_Li1_174692_69984_3320_id) 


## DOWNSTREAM PARTNERS based on buhmann synapses 
Putative_Li1_174692_69984_3320_output = flywire_partner_summary(Putative_Li1_174692_69984_3320_id,partners = "output", cleft.threshold = 100)
Putative_Li1_174692_69984_3320_output$pre_id = Putative_Li1_174692_69984_3320_id

# check for the latest ID for each partner and add a flywire_URL
Putative_Li1_174692_69984_3320_output$partner_type = ""
Putative_Li1_174692_69984_3320_output$partner_name = ""
Putative_Li1_174692_69984_3320_output$flywire_URL = ""
for (i in 1:length(Putative_Li1_174692_69984_3320_output$post_id)){
  Putative_Li1_174692_69984_3320_output$post_id[i] = flywire_latestid(Putative_Li1_174692_69984_3320_output$post_id[i])
  Putative_Li1_174692_69984_3320_output$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_Li1_174692_69984_3320_output$pre_id[i],Putative_Li1_174692_69984_3320_output$post_id[i]))}","klickable_link")')
} 

# get names and types from flywire_neuron_names table
Putative_Li1_174692_69984_3320_output$partner_name = flywire_neuron_name$name[match(Putative_Li1_174692_69984_3320_output$post_id,flywire_neuron_name$seg_id)]
Putative_Li1_174692_69984_3320_output$partner_type = flywire_neuron_name$type[match(Putative_Li1_174692_69984_3320_output$post_id,flywire_neuron_name$seg_id)]



## UPSTREAM PARTNERS based on buhmann synapses 
Putative_Li1_174692_69984_3320_input = flywire_partner_summary(Putative_Li1_174692_69984_3320_id,partners = "input", cleft.threshold = 100)
Putative_Li1_174692_69984_3320_input$post_id = Putative_Li1_174692_69984_3320_id

# check for the latest ID for each partner and add a flywire_URL
Putative_Li1_174692_69984_3320_input$partner_type = ""
Putative_Li1_174692_69984_3320_input$partner_name = ""
Putative_Li1_174692_69984_3320_input$flywire_URL = ""
for (i in 1:length(Putative_Li1_174692_69984_3320_input$pre_id)){
  Putative_Li1_174692_69984_3320_input$pre_id[i] = flywire_latestid(Putative_Li1_174692_69984_3320_input$pre_id[i])
  Putative_Li1_174692_69984_3320_input$flywire_URL[i] = glue('=HYPERLINK("{flywire_url_func(c(Putative_Li1_174692_69984_3320_input$post_id[i],Putative_Li1_174692_69984_3320_input$pre_id[i]))}","klickable_link")')
} 

# get names 
Putative_Li1_174692_69984_3320_input$partner_name = flywire_neuron_name$name[match(Putative_Li1_174692_69984_3320_input$pre_id,flywire_neuron_name$seg_id)]
Putative_Li1_174692_69984_3320_input$partner_type = flywire_neuron_name$type[match(Putative_Li1_174692_69984_3320_input$pre_id,flywire_neuron_name$seg_id)]





# writing the connectivity tables back to google sheets
sheet_write(Putative_Li1_174692_69984_3320_output, ss="1R3V52LMywa1up9XdKyE3yp2ah-7AmFxjWo-esXnFvd8", sheet = "output")
sheet_write(Putative_Li1_174692_69984_3320_input, ss="1R3V52LMywa1up9XdKyE3yp2ah-7AmFxjWo-esXnFvd8", sheet = "input")
# sheet_write(Putative_Li1_174692_69984_3320_exclude, ss= "1R3V52LMywa1up9XdKyE3yp2ah-7AmFxjWo-esXnFvd8", sheet = "exclude")



