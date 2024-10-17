## Hajian-Forooshani_etal_2024_Consumer_Resource_Turing_Demography

#### Description of code and data from: “The population dynamics of clustered consumer-resource spatial patterns: insights from the demographics of a Turing mechanism” (2024) by Zachary Hajian-Forooshani, Iris Saraeny Rivera Salinas, Ivette Perfecto and John Vandermeer. With these files all of the analysis can be recreated. 

**R script:** *“2024_10_16_Phorid_survey_analysis_figures_final”* 
- Script for statistical analysis of the Phoridae field surveys. 
 
- **Relies on data from:** “Azteca_DSP_Phorid_dynamics_survey_data.csv”

**R script:** *“2024_10_16_DSP_test_for_empirical_data”*
- Script for Demographic Spatial Patterning test and randomization procedure as outlined in Figure 2 of the manuscript and described in Supplementary Material 

- **Relies on data from:**
  - “Azteca_census_data_Jan_2018.csv”
- **Produces:**
  - “dsp.df.final.csv”
  - “dsp.randomizations.df.final.csv”

**R script:** *“2024_10_16_DSP_results_Fig3”*
- Plots the results from the DSP test for Fig 3. 

- **Relies on data from:**
    - “dsp.df.final.csv”
    - “dsp.randomizations.df.final.csv”

**R script:** *“2024_10_16_Azteca_DSP_age_specific_mortality_calculations”*
- Calculates the age specific mortalities in the empirical data. 

- **Produces:** ”2023_11_15_Azteca_DSP_empirical_death_rates_full_df.csv”

**R script:** *“2024_10_16_density_dependence_in_clusters_for_SupMat”*
- Script with analysis to show that older nests tend to be in the center of clusters for supplementary material. 

- **Relies on data from:**
    - “Azteca_census_data_Jan_2018.csv”

**R script:** *“2024_10_16_Processing_death_rate_data_from_netlogo_model”*
- Script to process death rate trends from the netlogo consumer-resource model.

- **Relies on data from:**
    - “Netlogo_consumer_resrouce_model_output.csv"

- **Produces:**
    - "2023_11_19_azteca_DSP_netlogo_mortality_df_with_replicates.csv"

**R script:** *“2024_10_16_Netlogo_spatial_output_Fig6_landscape”*
- Makes figures from the output of the netlogo consumer-resource model. Data is embedded in the script which was copied directly from runs of the netlogo model to get spatial explicit information on consumers and resources. 

- **Relies on data from:**
   - “Netlogo_consumer_resource_model_output.csv"

**R script:** *“2024_10_16_Empirical_and_model_comparisions_Death_rates_and_Spatial_patterns_Fig5_Fig8_Fig9”*
- Script to compare death rate and spatial patterning trends from empirical data and netlogo consumer-resource model. 

- **Relies on data from:**
    - “azteca_phorid_model_data_for_paper_Feb27_202.csv”
    - “2023_11_15_Azteca_DSP_empirical_death_rates_full_df.csv”
    - “2023_11_19_azteca_DSP_netlogo_mortality_df_with_replicates.csv”

**R script:** *“2024_10_16_Azteca_DSP_netlogo_age_specific_death_rate_threshold_sensitivity_analysis”*
- Sensitivity analysis for the death rate trends in the empirical data and model as described in the supplementary material. 

- **Relies on data from:**
    - “2023_11_19_azteca_DSP_netlogo_mortality_df_with_replicates.csv”
    - “Azteca_census_data_Jan_2018.csv”

**R script:** *“2024_10_16_Frequency_distribution_empirical_model_comparison_Fig7"*
- Script to make Fig 7 comparing the landscape scale spatial patterns between the model and the empirical data. 

- **Relies on data from:**
    - “Netlogo_consumer_resource_model_output.csv”
    - “Azteca_census_data_Jan_2018.csv”

**Netlogo file:** *“2024_10_16_Netlogo_consumer_resource_model.nlogo”*
- Implementation of the spatially explicit consumer-resource model that tracks demography of resources. 
