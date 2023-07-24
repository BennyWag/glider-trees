# Review - Ecography ------------------------------------------------------

library(raster)
library(exactextractr)
library(sf)
library(effects)
library(tidyverse)
library(ggpubr)
library(patchwork)

gliding_all<-raster('//research-cifs/6030-users/bwagner2/gliding species/rasters/final_new/all_species.tif')
vert_stack<-stack('//research-cifs/6030-users/bwagner2/gliding species/rasters/total richness (review)/final/vert_all_1x1.tif')

names(vert_stack)<-c('vert_all', 'mam_all', 'amp_all', 'rep_all')

model_data<-read.csv('data/review/data_1x1_RS.csv')%>%
  st_as_sf(coords = c('x', 'y'), crs = crs(vert_stack))


#extract vertebrate richness

vert_terra<-terra::rast(vert_stack)

model_data_vert<-terra::extract(vert_terra, model_data, fun = NULL , xy = T, ID = T)

model_data_vert_bind<-model_data_vert%>%mutate_all(~replace(., is.na(.), 0))%>%
  rename(X = ID)

#combine and save

model_data_review<-left_join(model_data, model_data_vert_bind, by = 'X')%>%st_drop_geometry()

write.csv(model_data_review, 'outputs/final/new/review/data_1x1_RS+verts.csv', row.names = F)


# test model --------------------------------------------------------------

#all gliding

values_sample<-model_data_review%>%sample_n(10000)

test_mod<-glm(all~tree_height+tree_density, family = 'poisson', data = values_sample)

summary(test_mod)
plot(allEffects(test_mod), type = 'response')

#all vert

vert_mod<-glm(vert_all~tree_height+tree_density, family = 'poisson', data = values_sample)

summary(vert_mod)
plot(allEffects(vert_mod), type = 'response')

       