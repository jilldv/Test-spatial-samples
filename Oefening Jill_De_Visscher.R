# SCRIPT: OEFENING SELECTIEPROCEDURE RESEARCH SOFTWARE ENGINEER --------------------------------------------------------------------


# Load packages -----------------------------------------------------------

# Install missing packages and load them
packages <- c("googledrive", "dplyr", "n2khab", "ggplot2")

for(i in packages) {
  if( ! i %in% rownames(installed.packages()) ) { install.packages( i ) }
  library(i, character.only = TRUE)
}



# Download datasets with spatial samples ---------------------------------------------------------------

# Authentification not needed for public folders
drive_deauth()

# Load file "spatial_samples.csv" for a specific version
read_spatial_samples <- function(root_url, version) {
  
  # Main folder on shared google drive
  root <- drive_get(root_url)
  
  # Identify correct version folder
  version_folder <- drive_ls(root) %>% #List all folders in main folder
    filter(name==version) #Select folder of correct version
  
  # Identify "samples" subfolder
  samples_folder <- drive_ls(version_folder$id) %>% 
    filter(name == "samples")
  
  # Identify "spatial_samples.csv" file
  file <- drive_ls(samples_folder$id) %>% 
    filter(name == "spatial_samples.csv")
  
  # Download the file
  tmp <- tempfile(fileext = ".csv")
  drive_download(as_id(file$id), path = tmp, overwrite = TRUE) #Overwrite the file in case a file with same name already exists
  
  # Load the file in R
  read.csv(tmp)
}

# Test read_spatial_samples function
samples_012 <- read_spatial_samples(root_url = "https://drive.google.com/drive/folders/1gzrB-5AG-KYHmiQUThyTEhpsboMPHXeT",
  version  = "poc_0.12.0")



# Filter data based on scheme and water management class ------------------

filter_spatial_samples <- function(spatial_samples, scheme_keep, hydr_keep) {
  
  # Find water management class of a stratum (type)
  types <- n2khab::read_types() %>% 
    select(type, hydr_class)
  
  spatial_samples %>% 
    left_join(types, by = c("stratum" = "type"))  %>% #Link water management class to each stratum (type)
    filter(scheme %in% scheme_keep, hydr_class %in% hydr_keep) #Filter based on wanted scheme and water management class
}

# Test filter_spatial_samples function
filtered_012 <- filter_spatial_samples(spatial_samples = samples_012, scheme_keep = "GW_03.3", hydr_keep   = "HC3")



# Compare sample sizes per stratum between versions -----------------------

# Get sample sizes per stratum and compare between versions
compare_versions <- function(root_url,
                             version_a,
                             version_b,
                             scheme_keep,
                             hydr_keep) {
  
  bind_rows( #Combine 2 versions
    read_spatial_samples(root_url, version_a) %>% #Load data of first version
      filter_spatial_samples(scheme_keep, hydr_keep) %>% #Filter by scheme and water management class
      summarize(sample_size=length(unique(grts_address)), .by = stratum) %>% #Calculate sample size as number of unique values of grts_address
      mutate(version = version_a), #Add name of version
    
    read_spatial_samples(root_url, version_b) %>% #Load data of second version
      filter_spatial_samples(scheme_keep, hydr_keep) %>% 
      summarize(sample_size=length(unique(grts_address)), .by = stratum) %>%
      mutate(version = version_b))
}

# Visualize sample sizes per stratum anc compare between versions
plot_sample_size_comparison <- function(df) {
  
  ggplot(df, aes(x = sample_size, y = stratum, colour = version)) +
    geom_point(size = 2) +
    geom_line(aes(group = stratum), colour = "black") +
    scale_colour_brewer(palette = "Set1") +
    labs(x = "Steekproefgrootte",y = "Stratum", colour = "POC-versie", title = "Vergelijking steekproefgroottes per type") +
    theme_bw()
}

# Compare sample sizes for versions poc_0.13.1 and poc_0.14.0, in schemes GW_03.3 and for the water management classes HC1, HC12 or HC2
df_comparison <- compare_versions(root_url = "https://drive.google.com/drive/folders/1gzrB-5AG-KYHmiQUThyTEhpsboMPHXeT", 
                                  version_a = "poc_0.13.1", version_b = "poc_0.14.0", 
                                  scheme_keep = "GW_03.3", hydr_keep = c("HC1", "HC12", "HC2"))
plot_sample_size_comparison(df_comparison)

