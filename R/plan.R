plan <- drake_plan(
  
  man_obj = table_management_objectives(),
  
  comm_revenue_ne = plot_total_comm_rev(epu_abbr = epu_abbr),
  
  bennet_index = plot_bennet_index(epu_abbr = epu_abbr),
  
  comm_landings_ne = plot_total_comm_land(epu_abbr = epu_abbr),
  
  report = rmarkdown::render(
    knitr_in("SOE-NEFMC-2019.Rmd"),
    output_file = file_out("SOE-NEFMC-2019.pdf"),
    quiet = TRUE
  )

)
