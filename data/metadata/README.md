# README for SEV OUP Climate Chapter 

Author: KM Hall  

----  

These steps need to occur in order:  
1.  download data from EDI using the program ./scripts/download_sev_met_from_edi.R and make the resulting raw data files in ./data/raw_data read-only  
2.  gap fill hourly data and run anomalize package on it to see if there are any major issues of concern using ./scripts/anomalize_hourly_met_data.R  
3. Look at how often various data flags are setting in ./Rmd/test_hourly_gap_filling.Rmd
4. Create gap-filled daily, monthly, and yearly summaries in ./met_create_dly_mthly_yrly_data.R



----  

Other steps that do not have an associated order of operations:
- ./scripts/met_basic_analysis.R and ./Rmd/met_basic_analysis.Rmd produce some basic graphs of the met data at various time scales without using any specialized time series R packages.  
- 