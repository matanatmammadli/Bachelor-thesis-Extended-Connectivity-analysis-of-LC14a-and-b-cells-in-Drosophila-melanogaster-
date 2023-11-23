library(natverse)
library(tidyverse)
library(fafbseg)
library(gargle)
library(dplyr)
library(glue)
library(reticulate)
library(RColorBrewer)
dr_fafbseg()



#1 cell = "720575940620585548"
#2 cell = "720575940625287994"
#3 cell = "720575940637511485"
##cell = "720575940630105903"
#5 cell = "720575940616048413"
#6 cell = "720575940618594542"
#7 cell = "720575940621434506"
#8 cell = "720575940624457091"

##cell = "720575940633789779"          ##Lobula_to_medulla cell (LC14a output)

##cell = "720575940635014807"          ##LC14a cell

##cell = "720575940629791275"          ##LC9 cell (LC14a input)

##cell =  "720575940623183436"         ## L1 cell (LC14a input)

##cell = "720575940633185619"          ## another L1 cell (Lc14a input)

##cell = "720575940631845151"          ## Lobula_to_medulla (LC14a input)

##cell = "720575940629378883"          ## Li2 cell (LC14a output)

##cell = "720575940628258577"          ## Li1 (LC14a output)     

##cell = "720575940660340353"          ## LC14b cell

##cell = "720575940642564749"          ## T2a (LC14b input)

cell = "720575940637962864"          ## Y3 cell (LC14b input)

##cell = "720575940631319353"           ## TmY5 (LC14b input)


cell = flywire_latestid(cell)


end <- 17 #the n'th cell when order them in decreasing number of synapses
z <- 10 #what is the number of synapses of the cell in the "end" place


#only cells with synapse numbers between high and low
# replace filter(n >= z) with filter(n >= low, n<= high)"
high <- 100
low<- 1
# couldnt figure out how to change it automatically


post <- flywire_partners(cell, details = TRUE, partners = c("outputs")) %>% filter(cleft_scores >= 100)
post <- data.frame(post[,c(2,3,4,5,6,7,18,19)])
A <- post %>% add_count(post_id)
post <-A %>% arrange(desc(n)) %>% group_by(post_id)
z <- unique(post[,c('n','post_id')])[end,1] %>% arrange(desc(n)) 
post <- post  %>% filter(n >= z)
DAT <- data.frame(post[,c(1,2,3,7)])
postnest <- DAT %>% nest(pre_x,pre_y,pre_z)

pre <- flywire_partners(cell, details = TRUE, partners = c("inputs")) %>% filter(cleft_scores >= 100)
pre <- data.frame(pre[,c(2,3,4,5,6,7,18,19)])
A <- pre %>% add_count(pre_id)
pre <-A %>% arrange(desc(n)) %>% group_by(pre_id)
z <- unique(pre[,c('n','pre_id')])[end,1] %>% arrange(desc(n)) 
pre <- pre  %>% filter(n >= z)
DAT <- data.frame(pre[,c(1,2,3,7)])
prenest <- DAT %>% nest(pre_x,pre_y,pre_z)

root<-  read_cloudvolume_meshes(cell)



listpost <- read_cloudvolume_meshes(unique(post$post_id))
i=1

nopen3d()
par3d('windowRect' = c(100,100,2000,1100))

for (i in 1:end){
    shade3d(as.mesh3d(FAFB14NP.surf), alpha=0.05, col='gray', lit=T)
  plot3d(root, col = "blue", lit=F)
  plot3d(listpost[i],col="red",lit=F)
  spheres3d(post[which(post$post_id==postnest[i,1]),1:3],col="black",radius = 500,lit=F)
  view3d(fov=0,zoom=0.5, userMatrix= rotationMatrix(0/180*pi,0,0,1) %*% rotationMatrix(180/180*pi,1,0,0))
  rgl.snapshot(filename= paste("try/lc14b_post","(", cell,")","_to_",unique(post$post_id)[i], "front.png", sep=""))
  view3d(fov=0,zoom=0.5, userMatrix= rotationMatrix(0/180*pi,0,0,1) %*% rotationMatrix(-90/180*pi,1,0,0))
  rgl.snapshot(filename= paste("try/lc14b_post","(", cell,")","_to_",unique(post$post_id)[i], "top.png", sep=""))
  clear3d()
  }

listpre <- read_cloudvolume_meshes(unique(pre$pre_id))
i=1
nopen3d()
par3d('windowRect' = c(100,100,2000,1100))

for (i in 1:end){
  shade3d(as.mesh3d(FAFB14NP.surf), alpha=0.05, col='gray', lit=T)
  plot3d(root, col = "green", lit=F)
  plot3d(listpre[i],col="magenta",lit=F)
  spheres3d(pre[which(pre$pre_id==prenest[i,1]),1:3],col="black",radius = 500,lit=F)
  view3d(fov=0,zoom=0.5, userMatrix= rotationMatrix(0/180*pi,0,0,1) %*% rotationMatrix(180/180*pi,1,0,0))
  rgl.snapshot(filename= paste("try/y3_pre","(", cell,")","_to_",unique(pre$pre_id)[i], "front.png", sep=""))
  view3d(fov=0,zoom=0.5, userMatrix= rotationMatrix(0/180*pi,0,0,1) %*% rotationMatrix(-90/180*pi,1,0,0))
  rgl.snapshot(filename= paste("try/y3_pre","(", cell,")","_to_",unique(pre$pre_id)[i], "top.png", sep=""))
  clear3d()
  }

