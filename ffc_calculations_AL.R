# ======================================================================================================================
#### SET-UP : Load libaries, set token, and import data sources
# ======================================================================================================================
#### libraries
# devtools::install_github('ceff-tech/ffc_api_client/ffcAPIClient')
library(devtools)
library(usethis)
library(tidyverse)
library(readxl)
library(fs)
library(ffcAPIClient)

#### set-up token for use
ffctoken <- 'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJmaXJzdE5hbWUiOiJBZHJpYW5hIiwibGFzdE5hbWUiOiJMZSBDb21wdGUiLCJlbWFpbCI6ImFkcmlhbmFsc0BzY2N3cnAub3JnIiwicm9sZSI6IlVTRVIiLCJpYXQiOjE2OTMyNTA2Njl9.mjM3WqZJXJdaJHEqN5e5Fh90JFgERzERqxiksSCpGbE'
ffctoken <- ffcAPIClient::set_token(Sys.getenv("EFLOWS_TOKEN", ""))
ffc <- ffcAPIClient::FFCProcessor$new()
# ffc$set_up(token = ffctoken) - gives error "need either a gage ID or a timeseries of data to proceed"
# ffc$run()

#### read-in data and lookup tables
lu_model <- read_xlsx("C:/Users/adrianal/SCCWRP/Cannabis E-Flows - Data/Working/Watershed_Delineation_Tool/Modeled_Flow/Eel_River/Lookup_Tables/LOOKUP_table_all_v2 - Copy.xlsx", sheet = "LU_model")
lu_gage <- read_xlsx("C:/Users/adrianal/SCCWRP/Cannabis E-Flows - Data/Working/Watershed_Delineation_Tool/Modeled_Flow/Eel_River/Lookup_Tables/LOOKUP_table_all_v2 - Copy.xlsx", sheet = "LU_gage")
model_data <- read.csv("C:/Users/adrianal/SCCWRP/Cannabis E-Flows - Data/Working/Watershed_Delineation_Tool/Modeled_Flow/Eel_River/MODEL/eel_subbasins.sub_cfs.csv", check.names = F)
gage_data <- ("C:/Users/adrianal/SCCWRP/Cannabis E-Flows - Data/Working/Watershed_Delineation_Tool/Modeled_Flow/Eel_River/GAGES")
metric_categories_file <- read.csv("C:/Users/adrianal/SCCWRP/Cannabis E-Flows - Data/Working/Watershed_Delineation_Tool/Modeled_Flow/Eel_River/Lookup_Tables/all_metric_def_list_FFMs_v2 1.csv")

#### user specify the Model_Watershed abbreviation (Eel River is ER, Redwood Creek is RWC, Little River is LR, Red Wood Creek is RWC, Mad River is MR) based on the model_data csv name
Model_Watershed_eval <- "ER"


# ======================================================================================================================
#### LOOP 1 : loop through gage files for ffc calculations (loop takes approx 2 minutes to run for 15 files)
# ======================================================================================================================
# list the timeseries files you want to loop through
gage_file_list <- list.files(gage_data, full.names = T)

# create empty dataframe to place results into
output.gage <- data.frame()

error_files <- c()

for(i in 1:length(gage_file_list)) {
  
  tmp <- read.csv(gage_file_list[i])
  names(tmp) <- tolower(names(tmp))
  gage_file <- tmp %>%
    rename(flow = obs) %>% 
    mutate(date = ymd(date)) %>% 
    mutate(date = format(date, "%m/%d/%Y"))
  
  name <- path_file(gage_file_list[i])
  gage_ID <- unlist(lapply(strsplit(name, "_"), function(x) paste(nth(x, -2), sep = "_")))
  COMID_df <- lu_gage %>% filter(gageID == gage_ID)
  
  result <- tryCatch(
    expr = {    
      example_ffc <- ffcAPIClient::evaluate_alteration(
      timeseries_df = gage_file, 
      token = ffctoken,
      comid = COMID_df$COMID)
      },
    error = function(e){
      cat("An error occurred:", conditionMessage(e), "/n")
      error_files <- append(error_files, gage_file_list[i])
      result <- NA 
    },
    finally = {
      cat("Execution completed./n")
    }
  )
  
  ex_ffm_results <- example_ffc$ffc_results
  pivoted_results <- ex_ffm_results %>% pivot_longer(cols= DS_Dur_WS:Peak_Fre_5, names_to = "ffm", values_to = "gage.value") %>% 
    mutate(gageID = gage_ID) %>% 
    mutate(COMID = COMID_df$COMID)
  
  output.gage <- output.gage  %>% 
    bind_rows(pivoted_results)

}

write.csv(output.gage, paste("C:/Users/adrianal/SCCWRP/Cannabis E-Flows - Data/Working/Watershed_Delineation_Tool/Modeled_Flow/FFC_outputs/csv_results/", "gage", "ffc_results.csv", sep = "_"), row.names = F)

# ======================================================================================================================
#### LOOP 2 : loop through model columns for ffc calculations (loop takes approx 13 minutes to run for 122 columns)
# ======================================================================================================================
model.columns <- colnames(model_data)
output.model <- data.frame()

for(k in 2:length(model.columns)) {
  
  model_run <- model_data %>% 
    select(Date, model.columns[k]) %>% 
    rename(date = Date) %>% 
    rename(flow = model.columns[k])
  
  subbasin <- (model.columns[k])
  soi <- lu_model %>% 
    filter(Model_Subbasin == subbasin,
           Model_Watershed == Model_Watershed_eval)
  
  result <- tryCatch(
    expr = {    
      ffc_model_run <- ffcAPIClient::evaluate_alteration(
        timeseries_df = model_run, 
        token = ffctoken,
        comid = soi$COMID)
    },
    error = function(e){
      cat("An error occurred:", conditionMessage(e), "/n")
      result <- NA 
    },
    finally = {
      cat("Execution completed./n")
    }
  )
  
  ffm_model_results <- ffc_model_run$ffc_results
  pivoted_model_results <- ffm_model_results %>% pivot_longer(cols= DS_Dur_WS:Peak_Fre_5, names_to = "ffm", values_to = "model.value") %>% 
    mutate(model_ID = soi$model_ID,
           scenario = "unimpaired",
           COMID = soi$COMID) 
  
  output.model <- output.model  %>% 
    bind_rows(pivoted_model_results)

}

write.csv(output.model, paste("C:/Users/adrianal/SCCWRP/Cannabis E-Flows - Data/Working/Watershed_Delineation_Tool/Modeled_Flow/FFC_outputs/csv_results/", "model", "ffc_results.csv", sep = "_"), row.names = F)

# ======================================================================================================================
#### LOOP 3 : loop through boxplots to compare gaged and modeled data
# ======================================================================================================================
output.model.2 <- output.model %>%
  rename(value = model.value) %>%
  mutate(data_type = "predicted")

output.gage.2 <- output.gage %>%
  rename(value = gage.value) %>%
  mutate(data_type = "observed")

metrics <-  metric_categories_file %>% select(flow_metric, title_component) %>% 
  rename(ffm = flow_metric)

joined_df_col <- bind_rows(output.gage.2, output.model.2) %>% 
  left_join(metrics, by = c("ffm"))

#joined_df_boxplot <- bind_cols(output.gage, output.model)
COMID_list <- unique(joined_df_col$COMID)

for(p in 1:length(COMID_list)){

  df.1 <-  joined_df_col %>%
    filter(COMID %in% COMID_list[p])
  
  unique.component <- unique(metrics$title_component)
  
  gageID.p <- unique(na.omit(as.numeric(df.1$gageID)))
  
  gage.type.p <- lu_gage %>% filter(gageID == gageID.p)
  
  modelID.p <- unique(na.omit(df.1$model_ID))
  
  for (y in 1:length(unique.component)){
    
    df.2 <- df.1 %>%
      filter(title_component %in% unique.component[y])
    
    boxplot <- df.2 %>%
      ggplot(aes(x = data_type, y = value, fill = data_type))+
      geom_boxplot()+
      facet_wrap(~ffm, scales = "free")+
      labs(title = paste0("COMID:", df.1$COMID, ", Gage:", df.1$gageID, ", Model ID:", modelID.p),
           subtitle = paste0("Gage type: ", gage.type.p$gage_type))
    # ggsave(file = paste("C:/Users/adrianal/SCCWRP/Cannabis E-Flows - Data/Working/Watershed_Delineation_Tool/Modeled_Flow/FFC_outputs/boxplots/", unique(df.2$COMID), unique(df.2$title_component), "boxplot.jpg", sep = "_"), dpi = 150, boxplot)
    print(boxplot)
    
  }
  
}

# ======================================================================================================================
#### LOOP 4 : loop through creating scatterplots and residual plots to compare gaged and modeled data
# ======================================================================================================================
joined_df_scatter <- inner_join(output.gage, output.model, by = c("COMID", "ffm", "Year")) %>% 
  left_join(metrics, by = "ffm")

COMID_list <- unique(joined_df_scatter$COMID)

for(r in 1:length(COMID_list)){
  
  new_df <- joined_df_scatter %>% 
    mutate(residual = (gage.value - model.value))
  
  df.1 <-  new_df %>%
    filter(COMID %in% COMID_list[r])
  
  unique.component <- unique(metrics$title_component)
  
  gageID.r <- unique(na.omit(as.numeric(df.1$gageID)))
  
  gage.type.r <- lu_gage %>% filter(gageID == gageID.r)
  
  modelID.r <- unique(na.omit(df.1$model_ID))

  for (z in 1:length(unique.component)){
    
    df.2 <- df.1 %>%
      filter(title_component %in% unique.component[z])
    
    scatter_plot <- df.2 %>%
      ggplot(aes(x = gage.value, y = model.value))+
      geom_point()+
      facet_wrap(~ffm, scales = "free")+
      geom_abline(intercept = 0, slope = 1)+
      labs(title = paste0(unique.component[z]),
           subtitle = paste0("COMID:", df.1$COMID, ", Model ID:", modelID.r, ", Gage ID:", df.1$gageID, ", Gage type: ", gage.type.r$gage_type))
    # ggsave(file = paste("C:/Users/adrianal/SCCWRP/Cannabis E-Flows - Data/Working/Watershed_Delineation_Tool/Modeled_Flow/FFC_outputs/scatterplots/", unique(df.2$COMID), unique(df.2$title_component), "scatterplot.jpg", sep = "_"), dpi = 150, scatter_plot)
    print(scatter_plot)
    
    residual_plot <- ggplot(df.2, aes(x = gage.value, y = residual))+
      geom_point()+
      facet_wrap(~ffm, scales = "free")+
      geom_hline(yintercept = 0, linetype = 2)+
      labs(title = paste0(unique.component[z]),
           subtitle = paste0("COMID:", df.1$COMID, ", Model ID:", modelID.r, ", Gage ID:", df.1$gageID, ", Gage type: ", gage.type.r$gage_type))
    # ggsave(file = paste("C:/Users/adrianal/SCCWRP/Cannabis E-Flows - Data/Working/Watershed_Delineation_Tool/Modeled_Flow/FFC_outputs/residuals/", unique(df.2$COMID), unique(df.2$title_component), "residualplot.jpg", sep = "_"), dpi = 150, residual_plot)
    print(residual_plot)
    
  }
  
}
