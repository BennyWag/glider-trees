
# new layers --------------------------------------------------------------

library(raster)
library(exactextractr)
library(sf)
library(effects)
library(tidyverse)

rasterOptions(maxmemory = 2e+10, tmpdir = 'Y:/gliding species/raster_temp', timer = T, progress = 'text')


# load data ---------------------------------------------------------------

IFL<-st_read('data/all/IFL_ALL_cntr+biogeoregion+islands_diss.shp')

stack_gliding<-stack('Y:/gliding species/rasters/final_new/all_species.tif',
                     'Y:/gliding species/rasters/final_new/all_genus.tif',
                     'Y:/gliding species/rasters/final_new/mam_species.tif',
                     'Y:/gliding species/rasters/final_new/mam_genus.tif',
                     'Y:/gliding species/rasters/final_new/amph_species.tif',
                     'Y:/gliding species/rasters/final_new/amph_genus.tif',
                     'Y:/gliding species/rasters/final_new/rep_species.tif',
                     'Y:/gliding species/rasters/final_new/rep_genus.tif')

crs(stack_gliding)

names(stack_gliding)<-c('all', 'all_genus', 
                        'mam', 'mam_genus', 
                        'amph', 'amph_genus',
                        'rep', 'rep_genus')

stack_trees<-stack("outputs/final/stack_trees.tif")

#resample to 1x1

stack_trees_1x1<-resample(stack_trees, stack_gliding, progress = 'text')

names(stack_trees_1x1)<-c("tree_height",  "tree_density", "elevation")

writeRaster(stack_trees_1x1, "outputs/final/new/stack_trees_1x1.tif", options="COMPRESS=LZW", progress = 'text')

#add climate

temp <- getData('worldclim', var='tmean', res=2.5)
precip <- getData('worldclim', var='prec', res=2.5)

temp_mean<-mean(temp)
precip_mean<-mean(precip)

stack_climate<-stack(temp_mean, precip_mean)
names(stack_climate)<-c('temp', 'precip')

climate_1x1<-resample(stack_climate, stack_gliding, progress = 'text')

#stack

stack_data<-stack(stack_trees_1x1, climate_1x1)

stack_all<-stack(stack_gliding, stack_data)

writeRaster(stack_all, "outputs/final/new/stack_all_1x1.tif", options="COMPRESS=LZW", progress = 'text')

stacknames<-names(stack_all)

saveRDS(stacknames, "outputs/final/new/stacknames.rds")

# test data extract -------------------------------------------------------

IFL_test<-IFL%>%filter(CNTRY_NAME %in% c('Australia', 'Indonesia'))

# extraction

values_test<-exact_extract(stack_all, IFL_test, fun = NULL , include_xy = T)

for(i in seq_along(values_test)){
  
  values_test[[i]]$ID <- seq_along(values_test)[i]
  
}

#compile into dataset

values_bind_test<-dplyr::bind_rows(values_test)%>%dplyr::select(-16)%>%
  mutate_all(~replace(., is.na(.), 0))%>%mutate_at(1:8, funs(round(.)))%>%mutate(temp = temp/10)

#add country name

values_bind_test$country <- as.factor(IFL_test$CNTRY_NAME[values_bind_test$ID])
values_bind_test$island <- as.factor(IFL_test$ISL_NAME[values_bind_test$ID])
values_bind_test$BGR <- as.factor(IFL_test$CLU[values_bind_test$ID])

values_bind_test<-values_bind_test%>%droplevels()

#test

values_sample<-values_bind_test%>%sample_n(10000)

test_mod<-glm(all~tree_height+tree_density, family = 'poisson', data = values_sample)

summary(test_mod)
plot(allEffects(test_mod), type = 'response')

# extract and save all ----------------------------------------------------

IFL_crop<-st_crop(IFL, stack_all)

# full res #

values<-exact_extract(stack_all, IFL_crop, fun = NULL , include_xy = T)

for(i in seq_along(values)){
  
  values[[i]]$ID <- seq_along(values)[i]
  
}

#compile into dataset

values_bind<-dplyr::bind_rows(values)%>%dplyr::select(-16)%>%
  mutate_all(~replace(., is.na(.), 0))%>%
  mutate_at(1:8, funs(round(.)))%>%
  mutate(temp = temp/10)

#add country name

values_bind$country <- as.factor(IFL_crop$CNTRY_NAME[values_bind$ID])
values_bind$island <- as.factor(IFL_crop$ISL_NAME[values_bind$ID])
values_bind$BGR <- as.factor(IFL_crop$CLU[values_bind$ID])

values_bind<-values_bind%>%droplevels()

#test

values_sample<-values_bind%>%sample_n(10000)

test_mod<-glm(all~tree_height+tree_density, family = 'poisson', data = values_sample)

summary(test_mod)
plot(allEffects(test_mod), type = 'response')

write.csv(values_bind, 'outputs/final/new/data_1x1.csv', row.names = F)

# aggregate rasters -------------------------------------------------------

stack_all_5x5<-aggregate(stack_all, 5, fun = max)
stack_all_10x10<-aggregate(stack_all, 10, fun = max)
stack_all_25x25<-aggregate(stack_all, 25, fun = max)
stack_all_50x50<-aggregate(stack_all, 50, fun = max)
stack_all_100x100<-aggregate(stack_all, 100, fun = max)


# lower res extract -------------------------------------------------------

# 5x5 #

values<-exact_extract(stack_all_5x5, IFL_crop, fun = NULL , include_xy = T)

for(i in seq_along(values)){
  
  values[[i]]$ID <- seq_along(values)[i]
  
}

#compile into dataset

values_bind<-dplyr::bind_rows(values)%>%dplyr::select(-16)%>%
  mutate_all(~replace(., is.na(.), 0))%>%
  mutate_at(1:8, funs(round(.)))%>%
  mutate(temp = temp/10)

#add country name

values_bind$country <- as.factor(IFL_crop$CNTRY_NAME[values_bind$ID])
values_bind$island <- as.factor(IFL_crop$ISL_NAME[values_bind$ID])
values_bind$BGR <- as.factor(IFL_crop$CLU[values_bind$ID])

values_bind<-values_bind%>%droplevels()

#test

values_sample<-values_bind%>%sample_n(10000)

test_mod<-glm(all~tree_height+tree_density, family = 'poisson', data = values_sample)

summary(test_mod)
plot(allEffects(test_mod), type = 'response')

write.csv(values_bind, 'outputs/final/new/data_5x5.csv', row.names = F)

# 10 x 10 #

values<-exact_extract(stack_all_10x10, IFL_crop, fun = NULL , include_xy = T)

for(i in seq_along(values)){
  
  values[[i]]$ID <- seq_along(values)[i]
  
}

#compile into dataset

values_bind<-dplyr::bind_rows(values)%>%dplyr::select(-16)%>%
  mutate_all(~replace(., is.na(.), 0))%>%
  mutate_at(1:8, funs(round(.)))%>%
  mutate(temp = temp/10)

#add country name

values_bind$country <- as.factor(IFL_crop$CNTRY_NAME[values_bind$ID])
values_bind$island <- as.factor(IFL_crop$ISL_NAME[values_bind$ID])
values_bind$BGR <- as.factor(IFL_crop$CLU[values_bind$ID])

values_bind<-values_bind%>%droplevels()

#test

values_sample<-values_bind%>%sample_n(10000)

test_mod<-glm(all~tree_height+tree_density, family = 'poisson', data = values_sample)

summary(test_mod)
plot(allEffects(test_mod), type = 'response')

write.csv(values_bind, 'outputs/final/new/data_10x10.csv', row.names = F)

# 25 x 25 #

values<-exact_extract(stack_all_25x25, IFL_crop, fun = NULL , include_xy = T)

for(i in seq_along(values)){
  
  values[[i]]$ID <- seq_along(values)[i]
  
}

#compile into dataset

values_bind<-dplyr::bind_rows(values)%>%dplyr::select(-16)%>%
  mutate_all(~replace(., is.na(.), 0))%>%
  mutate_at(1:8, funs(round(.)))%>%
  mutate(temp = temp/10)

#add country name

values_bind$country <- as.factor(IFL_crop$CNTRY_NAME[values_bind$ID])
values_bind$island <- as.factor(IFL_crop$ISL_NAME[values_bind$ID])
values_bind$BGR <- as.factor(IFL_crop$CLU[values_bind$ID])

values_bind<-values_bind%>%droplevels()

#test

values_sample<-values_bind%>%sample_n(10000)

test_mod<-glm(all~tree_height+tree_density, family = 'poisson', data = values_sample)

summary(test_mod)
plot(allEffects(test_mod), type = 'response')

write.csv(values_bind, 'outputs/final/new/data_25x25.csv', row.names = F)

# 50 x 50 #

values<-exact_extract(stack_all_50x50, IFL_crop, fun = NULL , include_xy = T)

for(i in seq_along(values)){
  
  values[[i]]$ID <- seq_along(values)[i]
  
}

#compile into dataset

values_bind<-dplyr::bind_rows(values)%>%dplyr::select(-16)%>%
  mutate_all(~replace(., is.na(.), 0))%>%
  mutate_at(1:8, funs(round(.)))%>%
  mutate(temp = temp/10)

#add country name

values_bind$country <- as.factor(IFL_crop$CNTRY_NAME[values_bind$ID])
values_bind$island <- as.factor(IFL_crop$ISL_NAME[values_bind$ID])
values_bind$BGR <- as.factor(IFL_crop$CLU[values_bind$ID])

values_bind<-values_bind%>%droplevels()

#test

values_sample<-values_bind%>%sample_n(5000)

test_mod<-glm(all~tree_height+tree_density, family = 'poisson', data = values_sample)

summary(test_mod)
plot(allEffects(test_mod), type = 'response')

write.csv(values_bind, 'outputs/final/new/data_50x50.csv', row.names = F)

# 100 x 100

values<-exact_extract(stack_all_100x100, IFL_crop, fun = NULL , include_xy = T)

for(i in seq_along(values)){
  
  values[[i]]$ID <- seq_along(values)[i]
  
}

#compile into dataset

values_bind<-dplyr::bind_rows(values)%>%dplyr::select(-16)%>%
  mutate_all(~replace(., is.na(.), 0))%>%
  mutate_at(1:8, funs(round(.)))%>%
  mutate(temp = temp/10)

#add country name

values_bind$country <- as.factor(IFL_crop$CNTRY_NAME[values_bind$ID])
values_bind$island <- as.factor(IFL_crop$ISL_NAME[values_bind$ID])
values_bind$BGR <- as.factor(IFL_crop$CLU[values_bind$ID])

values_bind<-values_bind%>%droplevels()

#test

values_sample<-values_bind%>%sample_n(1000)

test_mod<-glm(all~tree_height+tree_density, family = 'poisson', data = values_bind)

summary(test_mod)
plot(allEffects(test_mod), type = 'response',)

write.csv(values_bind, 'outputs/final/new/data_100x100.csv', row.names = F)

ggplot(values_bind, aes(x = tree_height, y = all, color = as.factor(BGR)))+
  geom_point()+
  geom_smooth(method = 'lm')


ggplot(values_bind%>%filter(all>0), aes(x = log10(tree_density), y = log10(all), color = as.factor(BGR)))+
  geom_point()+
  geom_smooth(method = 'lm')

# descriptive stats -------------------------------------------------------

values_bind<-read.csv('outputs/final/new/data_100x100.csv')

values_bind%>%filter(all>=1)%>%nrow()

values_bind%>%filter(all>=1)%>%summarise(min = min(tree_height),
                                                 max = max(tree_height),
                                                 median = median(tree_height),
                                                 mean = mean(tree_height),
                                                 sd = sd(tree_height))

values_bind%>%filter(all>=1)%>%summarise(min = min(tree_density),
                                         max = max(tree_density),
                                         median = median(tree_density),
                                         mean = mean(tree_density),
                                         sd = sd(tree_density))

values_bind%>%filter(all>=1, BGR %in% c(11,12,13))%>%summarise(min = min(tree_height),
                                                                       max = max(tree_height),
                                                                       median = median(tree_height),
                                                                       mean = mean(tree_height),
                                                                       sd = sd(tree_height))

values_bind%>%filter(all>=1, BGR %in% c(11,12,13))%>%summarise(min = min(tree_density),
                                                                       max = max(tree_density),
                                                                       median = median(tree_density),
                                                                       mean = mean(tree_density),
                                                                       sd = sd(tree_density))

values_bind%>%filter(all>=1)%>%summarise(min = min(elevation),
                                                 max = max(elevation),
                                                 median = median(elevation),
                                                 mean = mean(elevation),
                                                 sd = sd(elevation))

values_bind%>%filter(all>=1, BGR %in% c(11,12,13))%>%summarise(min = min(elevation),
                                                                       max = max(elevation),
                                                                       median = median(elevation),
                                                                       mean = mean(elevation),
                                                                       sd = sd(elevation))

values_bind%>%filter(all>=1, BGR %in% c(11,12,13))%>%nrow()

BGRs<-as.data.frame(summary(values_bind$BGR))

BGRs<-as.data.frame(summary(values_bind$BGR[values_bind$all>=1]))


