
# data
This README is the explanation of each column of the dataset.

* data_all_Dec 
  - date
  - region
  - date_analysis - date of analysis
  - confirm - PCR-confirmed cases in Hong Kong
  - temperature (°C)
  - humidity - Relative humidity (%)
  - wind_speed - Wind speed (km/h)
  - StringencyIndex_WeightedAverage - The index records the strictness of ‘lockdown style’ policies that primarily restrict people’s behavior. 
  - GovernmentResponseIndex_WeightedAverage - The index records how the response of governments has varied over all indicators in the database, becoming stronger or weaker over the course of the outbreak.
  - ContainmentHealthIndex_WeightedAverage - The index combines ‘lockdown’ restrictions and closures with measures such as testing policy and contact tracing, short term investment in healthcare, as well investments in vaccines. 
  - EconomicSupportIndex - The index records measures such as income support and debt relief. It is calculated using all ordinal economic policies indicators. 
  

* data_rt_cov_all 
  - true_rt - $R_t$ effective reproductive number at 'date' t estimated with data of the whole study period.
  - temp_rt - $R_t^t$ effective reproductive number at 'date' t estimated with data up to 'date' t.
  - rt_temp_past_m - $R_{t-m}^{t}$ effective reproductive number at past m day estimated with data up to 'date' t. 

* ride_rt
  - rt_temp_RIDE - Effective reproductive number at 'date' estimated with data up to 'date_analysis' using RIDE method.
  
* bp_rt
  - rt_temp_BP - Effective reproductive number at 'date' estimated with data up to 'date_analysis' using RIDE method.