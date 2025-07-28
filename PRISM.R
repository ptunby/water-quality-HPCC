# Load the required packages:
library(raster)
library(sf)
library(terra)
library(sp)
library(prism)
library(exactextractr)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tibble)

# Set Raster Directory
prism_set_dl_dir("F:/Summer_2024_Ch1_Data")

# Select Data Parameter
param = 'tmean'

  # Parameter Description
  # tmean	    Mean temperature
  # tmax	    Maximum temperature
  # tmin	    Minimum temperature
  # tdmean  	Mean dew point temperature
  # ppt	      Total precipitation (rain and snow)
  # vpdmin	  Daily minimum vapor pressure deficit
  # vpdmax	  Daily maximum vapor pressure deficit

# Set date range
start_date = "2022-04-28"
end_date = "2023-09-22"

# Download 30-year Normal Data

#get_prism_normals(type= param, resolution = "4km", mon = 1:12, keepZip = FALSE)

# Download Daily, Monthly, and Annual Data: YYYY-MM-DD

  get_prism_dailys(type = param, minDate = start_date, maxDate = end_date, keepZip = FALSE)
  #get_prism_monthlys(type = param, year = 1992:2023, mon = 5:10, keepZip = FALSE)
  #get_prism_annual(param, years = 2007:2023, keepZip = FALSE)
  
# Read the PRISM data:
  prism_data <- prism_archive_subset(param, "daily", 
                                   minDate = start_date, 
                                   maxDate = end_date
  )
  prism_stack <- pd_stack(prism_data)
  #crs(prism_stack) <- "+proj=longlat +datum=WGS84 +no_defs"

# Load shapefiles
Mont_shp <- st_read(dsn = "C:/Users/ptunb/OneDrive - University of New Mexico/PhD - Wildfire Research/Ch1 CQ Plots/Watershed and Burn Data Files/Montezuma_USGS08380500/globalwatershed.shp")
Lour_shp <- st_read(dsn = "C:/Users/ptunb/OneDrive - University of New Mexico/PhD - Wildfire Research/Ch1 CQ Plots/Watershed and Burn Data Files/Lourdes_USGS08382000/globalwatershed.shp")
SRUp_shp <- st_read(dsn = "C:/Users/ptunb/OneDrive - University of New Mexico/PhD - Wildfire Research/Ch1 CQ Plots/Watershed and Burn Data Files/SantaRosaUp_USGS08382650/globalwatershed.shp")

# Set the projection:
#shapefile <- read_sf("F:/Summer_2024_Ch1_Data/Continental US Medium Resolution/area-of-interest.shp")
Mont_shp_reproj <- st_transform(Mont_shp, crs = projection(prism_stack)) #Project to the same coordinate reference system as prism stack
Lour_shp_reproj <- st_transform(Lour_shp, crs = projection(prism_stack)) #Project to the same coordinate reference system as prism stack
SRUp_shp_reproj <- st_transform(SRUp_shp, crs = projection(prism_stack)) #Project to the same coordinate reference system as prism stack

#Plot the PRISM data to view
plot(prism_stack[[60]])
plot(SRUp_shp_reproj, add = TRUE, col = "red")


# Get the extent of the shapefile with the largest watershed, SRUp
shapefile_extent <- extent(SRUp_shp_reproj)
# Print the original extent in degrees
print(shapefile_extent)
# Define the amount to extend the extent (e.g., 0.1 degrees) to contain the entire reporojected shapefile
extend_amount <- 0.05
# Extend the top and bottom of the extent
extended_extent <- extent(
  xmin(shapefile_extent) - extend_amount,
  xmax(shapefile_extent) + extend_amount,
  ymin(shapefile_extent) - extend_amount,
  ymax(shapefile_extent) + extend_amount
)
# Print the extended extent in degrees
print(extended_extent)
# Crop the raster stack to the extended extent
prism_stack_crop <- crop(prism_stack, extended_extent)

#Plot the PRISM data that was cropped to the area of interest
plot(prism_stack_crop[[60]])
plot(SRUp_shp_reproj, add = TRUE, col = "red")


# Disaggregate to create a raster with a larger resolution
prism_stack_dissag <-disaggregate(prism_stack_crop, fact = 10)


hist(prism_stack_dissag[[60]], main="Tmean", 
     col= "purple", 
     maxpixels=100000)

# Extract mean values within the SRUp shapefile bounds
##extracted_rasters <- crop(prism_stack_dissag, shapefile_reprojected)
Mont_prism_stack <- mask(prism_stack_dissag, Mont_shp_reproj)
Lour_prism_stack <- mask(prism_stack_dissag, Lour_shp_reproj)
SRUp_prism_stack <- mask(prism_stack_dissag, SRUp_shp_reproj)

plot(SRUp_prism_stack[[60]])
plot(Lour_prism_stack[[60]])
plot(Mont_prism_stack[[60]])

# Define output file path
Mont_output_file <- "C:/Users/ptunb/OneDrive - University of New Mexico/PhD - Wildfire Research/Manuscript_CQ/Final Figures for Manuscript/Mont_prism_stack_60_raised.tif"
Lour_output_file <- "C:/Users/ptunb/OneDrive - University of New Mexico/PhD - Wildfire Research/Manuscript_CQ/Final Figures for Manuscript/Lour_prism_stack_60_raised.tif"
SRUp_output_file <- "C:/Users/ptunb/OneDrive - University of New Mexico/PhD - Wildfire Research/Manuscript_CQ/Final Figures for Manuscript/SRUp_prism_stack_60_raised.tif"

# Save the modified raster
writeRaster(Mont_prism_stack[[60]], filename = Mont_output_file, format = "GTiff", overwrite = TRUE)
writeRaster(Lour_prism_stack[[60]], filename = Lour_output_file, format = "GTiff", overwrite = TRUE)
writeRaster(SRUp_prism_stack[[60]], filename = SRUp_output_file, format = "GTiff", overwrite = TRUE)


# Initialize a vector to store the total sum for each layer
temp_avg_SRUp <- numeric(nlayers(SRUp_prism_stack))
# Loop through each layer
for (i in 1:nlayers(SRUp_prism_stack)) {
  # Extract values from the current layer
  values <- getValues(SRUp_prism_stack[[i]])
  # Calculate the average value, ignoring NA values
  temp_avg_SRUp[i] <- mean(values, na.rm = TRUE)
}
# Print the total sum for each layer
print(temp_avg_SRUp)

# Initialize a vector to store the total sum for each layer
temp_avg_Mont <- numeric(nlayers(Mont_prism_stack))
# Loop through each layer
for (i in 1:nlayers(Mont_prism_stack)) {
  # Extract values from the current layer
  values <- getValues(Mont_prism_stack[[i]])
  # Calculate the average value, ignoring NA values
  temp_avg_Mont[i] <- mean(values, na.rm = TRUE)
}
# Print the total sum for each layer
print(temp_avg_Mont)
# Initialize a vector to store the total sum for each layer
temp_avg_Lour <- numeric(nlayers(Lour_prism_stack))
# Loop through each layer
for (i in 1:nlayers(Lour_prism_stack)) {
  # Extract values from the current layer
  values <- getValues(Lour_prism_stack[[i]])
  # Calculate the average value, ignoring NA values
  temp_avg_Lour[i] <- mean(values, na.rm = TRUE)
}
# Print the total sum for each layer
print(temp_avg_Lour)
plot(SRUp_prism_stack[[1]])
plot(Mont_prism_stack[[1]])
plot(Lour_prism_stack[[1]])

dates_string <- as.Date(sub(".*_(\\d{8})_.*", "\\1", prism_data), format = "%Y%m%d")

# Create a data frame with dates and total sums
results_df_Mont <- data.frame(Date = dates_string, TotalSum = temp_avg_Mont)
# Print the results
print(results_df_Mont)
# Create a data frame with dates and total sums
results_df_Lour <- data.frame(Date = dates_string, TotalSum = temp_avg_Lour)
# Create a data frame with dates and total sums
results_df_SRUp <- data.frame(Date = dates_string, TotalSum = temp_avg_SRUp)

# Export the data
write.csv(results_df_Mont, "F:/Summer_2024_Ch1_Data/PRISM_value_summary/Daily_Tavg_Mont_20220428_20230922.csv", row.names = FALSE)
write.csv(results_df_Lour, "F:/Summer_2024_Ch1_Data/PRISM_value_summary/Daily_Tavg_Lour_20220408_20230922.csv", row.names = FALSE)
write.csv(results_df_SRUp, "F:/Summer_2024_Ch1_Data/PRISM_value_summary/Daily_Tavg_SRUp_20220408_20230922.csv", row.names = FALSE)
# Check Projection
plot(Mont_prism_stack[[1]])
plot(Mont_shp_reproj, add = TRUE, border = "black", col = NA)


# Define your custom date range
start_date <- as.Date(start_date)
end_date <- as.Date(end_date)

# Create a time series plot with the custom date range
ggplot(data = results_df, aes(x = Date, y = Value)) +
  geom_bar(aes(Date, TotalSum, fill = "Precip"), stat = "identity") +
  labs(x = "Date", y = "mm") +
  ggtitle("") +
  theme_classic() +
  scale_x_date(expand = expansion(0), limits = c(start_date, end_date)) +
  scale_y_continuous(expand = expansion(add = c(0, 0)))



