############# Raster data in R ###############

#stars is a package for raster data
#install.packages("stars")
library(stars)
library(ggplot2)

#loading raster data using the read_stars() function
#digital terrain model
dtm_harv <- read_stars("HARV_dtmCrop.tif")

#look at the object - has an x and y dimension (2 dimensions) and one attribute (just the elevation)
dtm_harv

#there is a geom in ggplot for plotting stars data
#by default with geom_stars, it already knows the aesthetic so you don't need to specify the mapping

ggplot() + 
  geom_stars(data = dtm_harv) 

#change the colour ramp
ggplot() + 
  geom_stars(data = dtm_harv) +
  scale_fill_viridis_c()

#digital surface model - digital terrain model to create a canopy height model
#gives you the height of the trees
#can just subtract the two rasters

#digital surface model
dsm_harv <- read_stars("HARV_dsmCrop.tif")
dsm_harv

#subtracts the value of each pixel in the dtm raster from the corresponding pixel value in dsm
chm_harv <- dsm_harv - dtm_harv

#plot the canopy height model
ggplot() +
  geom_stars(data = chm_harv)

#see some areas with no trees, some very tall trees (up to almost 30m)

#this is how we can do math with rasters to make a new raster

############# Vector data in R ###############
#sf package

library(sf)

#loading vector data
plots_harv <- st_read("harv_plots.shp")
View(plots_harv)

#the last field is where the spatial information is stored
#since this is point data, it only has a pair of x/y coordinates for the point

#we don't have to define the mapping between the data and the plot here either
ggplot() +
  geom_sf(data = plots_harv)

#can change the colour of the points based on a variable in the dataset
ggplot() +
  geom_sf(data = plots_harv, mapping = aes(color = plot_type))

#usually would like to see some kind of background behind the points
boundary_harv <- st_read("harv_boundary.shp")

View(boundary_harv)
#this is a multipart polygon, so it has a series of x, y coordinates connected by a series of lines

#can plot the points and the boundary
ggplot() +
  geom_sf(data = plots_harv, mapping = aes(color = plot_type)) +
  geom_sf(data = boundary_harv)

#can't see the points - why? need to pay attention to the order of the layers
ggplot() +
  geom_sf(data = boundary_harv) +
  geom_sf(data = plots_harv, mapping = aes(color = plot_type))

#or can change transparency of a layer to help with this
ggplot() +
  geom_sf(data = plots_harv, mapping = aes(color = plot_type)) +
  geom_sf(data = boundary_harv, alpha = 0.5)

#projections
#plot the raster again
ggplot() +
  geom_stars(data = dtm_harv)

#plot the points again
ggplot() +
  geom_sf(data = plots_harv)

#put them together
ggplot() +
  geom_stars(data = dtm_harv) +
  geom_sf(data = plots_harv)


#that is not what we wanted!
#the two datasets must have different projections/coordinate reference systems)
#can press the back arrow in the Plots panel to look - vector is lat long and raster seems to be UTM
#check the coordinate reference system (crs)
st_crs(dtm_harv)
st_crs(plots_harv)

#they are different so need to transform so that they are in the same projection
dtm_harv_lat_long <- st_transform(dtm_harv, 4326)
st_crs(dtm_harv_lat_long)

#often it is easiest to match all of the objects to one of the coordinate reference systems you're using

#transform the vector data crs
#here you are looking up the reference system for dtm_harv and applying it to plots_harv
plots_harv_utm <- st_transform(plots_harv, st_crs(dtm_harv))
plots_harv_utm
plots_harv

#now both should have the same coordinate reference system and the map should work
ggplot() +
  geom_stars(data = dtm_harv) +
  geom_sf(data = plots_harv_utm)

#What if you want to extract data from rasters for specific points e.g., want information on elevation from a raster layer at your sampling point
#important that the data has the same projection before you try to do this

#use the aggregate function
#takes three main arguments, the raster object that you want to extract the information from
#the second is the vector object indicating where on the raster you want the information from
#then the function that we use for combining the values from multiple pixels if there are multiple pixels associated with each location in the vector data

plot_elevations <- aggregate(dtm_harv, plots_harv_utm, mean, as_points = FALSE)
plot_elevations
#see that there is a list of elevations, one for each point

#we can look at those directly with the $ sign
plot_elevations$HARV_dtmCrop.tif
#get out the seven elevations
View(plots_harv_utm)

library(dplyr) #add the new elevation data to the points dataset
plottest <- mutate(plots_harv_utm, elevations = plot_elevations$HARV_dtmCrop.tif)
head(plottest)

####
#soils data
harv_soils <- st_read("harv_soils.shp")
#can see that it is polygon dta and there are 246 polygons and there are 5 columns of data

#if we look at the object, we see it is a table with information on soil type and drainage and the geometry of the polygons
View(harv_soils)

#map the polygons, coloured by soil type (still using the simple features sf geom)
#fill for the aesthetic changes the fill colour
ggplot() +
  geom_sf(data = harv_soils, mapping = aes(fill = TYPE_))

#can change the colour scheme, use the viridis colour ramp again (need to have the d at the end for discrete)
ggplot() +
  geom_sf(data = harv_soils, mapping = aes(fill = TYPE_)) +
  scale_fill_viridis_d()

#can make different subplots by soil type with facet_wrap
ggplot() +
  geom_sf(data = harv_soils) +
  facet_wrap(~TYPE_)

#press the zoom button in plots to see a larger version

#aggregating raster data in polygons, can use the aggregate function
#get the full dataset for the digital terrain model
harv_dtm <- read_stars("HARV/HARV_dtmFull.tif")

ggplot() +
  geom_stars(data = harv_dtm) +
  geom_sf(data = harv_soils, alpha = 0)

#can see that we have a raster that's bigger than the entire site and then we have the soil polygons within that

#for the arguments in the aggregate function, we first have the raster, then the vector, then the function that we want to calculate
#there are a lot of pixels within one polygon and we want the mean
#similar to group_by and summarize functions in dplyr, but spatial
elevs_by_soil <- aggregate(harv_dtm, harv_soils, mean)

#elevations for each polygon are stored in elevs_by_soil, but they are in an object inside there so we need to use the $ sign
elevs_by_soil$HARV_dtmFull.tif

#these elevations are in the same order as in harv_soils, so we can add them into that dataframe
harv_soils <- mutate(harv_soils, elevation = elevs_by_soil$HARV_dtmFull.tif)
head(harv_soils)

#now can see the elevation column in harv_soils
#use it to make a new map, with polygons coloured by elevation
ggplot() +
  geom_sf(data = harv_soils, mapping = aes(fill = elevation)) + 
  scale_fill_viridis_c()

#making plots that maintain the units of the data we're working with
#check coordinate systems
st_crs(harv_soils)

st_crs(harv_dtm)
#both have the same EPSG code and description of the CRS

#if we plot the raster data, we see that it maintains those units
ggplot() +
  geom_stars(data = harv_dtm)

#if we add the soils data, the UTM northing and easting numbers are replaced by lat/long
ggplot() +
  geom_stars(data = harv_dtm) +
  geom_sf(data = harv_soils, alpha = 0)

#by default geom_sf changes the units to latitude and longitude values
#often that's fine, but what if we want UTMs instead
ggplot() +
  geom_stars(data = harv_dtm) +
  geom_sf(data = harv_soils, alpha = 0) +
  coord_sf(datum = st_crs(harv_soils))


#making a vector point dataset when you don't have a spatial object to start with, it's just a .csv
#often need to do this with points data (e.g., collected with a gps)
harv_plots <- st_read("harv_plots.csv",
                      options = c("X_POSSIBLE_NAMES=longitude", 
                                  "Y_POSSIBLE_NAMES=latitude"),
                      crs = 4326) #this is the code for lat/long data

#now if you click on harv_plots it will look like the other simple features dataframes
#to export your data:
st_write(harv_plots, "harv_plots_new.shp")


#cropping data to make it the correct size for our project (study region)
#cropping harv_dtm to harv_boundary
#plot the data to look at it again

harv_boundary <- st_read("harv_boundary.shp")
ggplot() +
  geom_stars(data = harv_dtm) + 
  scale_fill_viridis_c() +
  geom_sf(data = harv_boundary, alpha = 0)

#can crop the raster and keep only the part that is within the polygon with the st_crop function
#the first argument is the raster that we want to crop
#the second argument is the vector that we want to crop the raster to


harv_dtm_cropped <- st_crop(harv_dtm, harv_boundary)
harv_dtm #it is 150 x 150 square
harv_dtm_cropped #ranges from 9 to 127 on the x and 2 to 146 on y so it is smaller

#now plot the cropped data to look at it
ggplot() +
  geom_stars(data = harv_dtm_cropped) + 
  scale_fill_viridis_c() +
  geom_sf(data = harv_boundary, alpha = 0)

#still see grey cells around the edge, which are null values in the raster
#can set the null value colour to transparent so we don't see them
ggplot() +
  geom_stars(data = harv_dtm_cropped) +
  scale_fill_viridis_c(na.value = "transparent") +
  geom_sf(data = harv_boundary, alpha = 0) 

#masking data -sometimes we want to keep all the cells outside but just set them to na
harv_dtm_cropped <- st_crop(harv_dtm, harv_boundary, crop = FALSE)
#this keeps the full extent of the raster

#or we can crop without a polygon layer, and instead crop to a square region

ggplot() +
  geom_stars(data = harv_dtm_cropped) +
  scale_fill_viridis_c(na.value = "transparent") +
  geom_sf(data = harv_boundary, alpha = 0) +
  coord_sf(datum = st_crs(harv_boundary)) #need this to see the coordinates so that you can figure out what to crop to

#create a boundary box using the st_bbox function, with a vector for the edges of the box
bbox <- st_bbox(c(xmin = 731000, ymin = 4713000, xmax = 732000, ymax = 471400),
                crs = st_crs(harv_dtm))
harv_dtm_small <- st_crop(harv_dtm, bbox)

#can do this cropping with vector data too
harv_soils <- st_read("harv_soils.shp")
harv_soils_small <- st_crop(harv_soils, bbox)

#plot the cropped raster and vector data together (soil polygons and dtm)
ggplot() +
  geom_stars(data = harv_dtm_small) +
  scale_fill_viridis_c() +
  geom_sf(data = harv_soils_small, alpha = 0.5)


#export the raster data, the file you want to save to is in the quotes
write_stars(harv_dtm_cropped, "harv_dtm_cropped.tif")
read_stars("harv_dtm_cropped.tif")
#can see it is the cropped version because both x and y don't go to 150

#export the vector data
st_write(harv_soils_small, "harv_soils_small.shp")

#both take two arguments - the name of the object we want to write (export), then the name we want it saved as

#rnaturalearth is a good package for background maps
#install.packages(c("rnaturalearth", "rnaturalearthdata"))
library("rnaturalearth")
library("rnaturalearthdata")

#install.packages("rgeos")
library("rgeos")
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

#add your points
ggplot(data = world) +
  geom_sf() +
  geom_sf(data = plots_harv_utm)



