

#extract legend
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}


#A function for plotting daily chlorophyll and PP data
interp_chl_pp <- function(epu, year = 2018, Variable){
  out <- ecodata::chl_pp %>% 
    filter(str_detect(Var,Variable),
           EPU == epu) %>% 
    separate(.,Time, c("Year","Week"),sep = 4) %>% 
    filter(Year == year) %>% 
    group_by(EPU) %>% 
    mutate(Time = 1:length(Year))
  
  ltm_out <- ecodata::chl_pp %>% 
    filter(str_detect(Var,Variable),
           EPU == epu) %>% 
    separate(.,Time, c("Year","Week"),sep = 4) %>% 
    group_by(Week) %>% 
    dplyr::summarise(LTM = mean(Value, na.rm = T),
                     SD = sd(Value, na.rm = T)) %>% 
    mutate(Time = 1:length(Week),
           sd.low = LTM - SD,
           sd.high = LTM + SD) %>% 
    left_join(.,out, by = c("Time")) %>% 
    mutate(status = ifelse(Value < sd.high & Value > sd.low, "near_mean",
                           ifelse(Value > sd.high, "high",
                                  ifelse(Value < sd.low,"low",NA))),
           group = "PLOT")
  
  return(ltm_out)
}



table_management_objectives <- function(){
  mng_obj <- data.frame("Objective Categories" = c("Seafood Production??",
                                                   "Profits","Recreation",
                                                   "Stability","Social & Cultural",
                                                   "Biomass","Productivity",
                                                   "Trophic structure","Habitat"),
                        "Indicators reported here" = c("Landings by feeding guild","Revenue by feeding guild",
                                                       "Number of anglers and trips; recreational catch",
                                                       "Diversity indices (fishery and species)",
                                                       "Commercial and recreational reliance",
                                                       "Biomass or abundance by feeding guild from surveys",
                                                       "Condition and recruitment of MAFMC managed species",
                                                       "Relative biomass of feeding guilds, primary productivity",
                                                       "Estuarine and offshore habitat conditions"))
  
  knitr::kable(mng_obj,
               col.names = c("Objective Categories","Indicators reported here"),
               caption = "Established ecosystem-scale objectives in New England",
               #align = 'c',
               booktabs = T) %>%
    kable_styling(latex_options = c("hold_position", "scale_down")) %>%
    # column_spec(c(2), width = c("25em")) %>%
    row_spec(0, bold = TRUE)
}



plot_total_comm_rev <- function(epu_abbr){
  
  #Filtering and aggregation step
  rev_agg <- ecodata::comdat %>% 
    filter(str_detect(Var, "Revenue"),
           !str_detect(Var, "prop|Other|MAFMC"), #Remove proportions, "Other" category species, NEFMC managed species in MAB
           EPU %in% epu_abbr,
           Time >= 1986) %>% 
    mutate(Status = ifelse(str_detect(Var, "Revenue weight"), 
                           "Managed","Total")) %>% #Create groups for aggregation
    group_by(EPU,Status, Time) %>% 
    dplyr::summarise(Total = sum(Value)) %>% 
    group_by(EPU,Status) %>% 
    mutate(hline = mean(Total))
  
  #Plotting
  gom_rev_agg <- rev_agg %>% filter(EPU == "GOM") %>% 
    ggplot() +
    
    #Highlight last ten years
    annotate("rect", fill = shade.fill, alpha = shade.alpha,
             xmin = x.shade.min , xmax = x.shade.max,
             ymin = -Inf, ymax = Inf)+  
    
    #lines
    geom_gls(aes(x = Time, y = Total,
                 group = Status),
             alpha = trend.alpha, size = trend.size) +
    geom_line(aes(x = Time, y = Total, color = Status), size = lwd) +
    geom_point(aes(x = Time, y = Total, color = Status), size = pcex) +
    
    #axes
    scale_y_continuous(labels = function(l){trans = l / 1000000})+
    scale_x_continuous(breaks = seq(1985, 2015, by = 5), expand = c(0.01, 0.01)) +
    scale_color_manual(values = c("indianred","black"), aesthetics = "color")+
    guides(color = FALSE) +
    ylab(expression("Revenue (10"^6*"USD)")) +
    geom_hline(aes(yintercept = hline,
                   color = Status),
               size = hline.size,
               alpha = hline.alpha,
               linetype = hline.lty) +
    theme_ts() +
    ggtitle("Gulf of Maine")
  
  gb_rev_agg <- rev_agg %>% filter(EPU == "GB") %>% 
    ggplot() +
    
    #Highlight last ten years
    annotate("rect", fill = shade.fill, alpha = shade.alpha,
             xmin = x.shade.min , xmax = x.shade.max,
             ymin = -Inf, ymax = Inf)+  
    
    #lines
    geom_gls(aes(x = Time, y = Total,
                 group = Status),
             alpha = trend.alpha, size = trend.size) +
    geom_line(aes(x = Time, y = Total, color = Status), size = lwd) +
    geom_point(aes(x = Time, y = Total, color = Status), size = pcex) +
    
    #axes
    scale_y_continuous(labels = function(l){trans = l / 1000000})+
    scale_x_continuous(breaks = seq(1985, 2015, by = 5), expand = c(0.01, 0.01)) +
    scale_color_manual(values = c("indianred","black"), aesthetics = "color")+
    guides(color = FALSE) +
    ylab(expression("Revenue (10"^6*"USD)")) +
    geom_hline(aes(yintercept = hline,
                   color = Status),
               size = hline.size,
               alpha = hline.alpha,
               linetype = hline.lty) +
    theme_ts() +
    ggtitle("Georges Bank")
  
  return(cowplot::plot_grid(gom_rev_agg, gb_rev_agg, ncol = 2))
}


## Bennet index 

plot_bennet_index <- function(epu_abbr){
  
  #Filter data into two dataframes for plotting
  indicators <- ecodata::bennet %>% 
    filter(EPU %in% epu_abbr,
           Var %in% c("Benthivore VI",
                      "Benthivore PI", 
                      "Benthos VI",
                      "Benthos PI")) %>% 
    mutate(Var, Var = plyr::mapvalues(Var, from = c("Benthivore VI","Benthivore PI",
                                                    "Benthos VI", "Benthos PI"),
                                      to = c("Benthivore Volume","Benthivore Price",
                                             "Volume","Price")))
  
  revchange <- ecodata::bennet %>% 
    filter(EPU %in% epu_abbr,
           Var %in% c("REVCHANGE EPU aggregate"))
  
  #custom bar fill color (color-blind friendly)
  ind_fill <- c("#a6cee3", "#b2df8a")
  
  #limits
  y.lim <- c(-350,350)
  
  #plot
  
  gom_bennet <- indicators %>% filter(EPU == "GOM" & Var %in% c("Benthivore Volume","Benthivore Price")) %>% 
    ggplot()+
    
    #Highlight last ten years
    annotate("rect", fill = shade.fill, alpha = shade.alpha,
             xmin = x.shade.min , xmax = x.shade.max,
             ymin = -Inf, ymax = Inf)+
    #guides(color = F, fill = F)+
    geom_bar(aes(x=Time, y= Value, fill = Var), stat="identity")+
    scale_fill_manual(name = "Indicators", values = ind_fill, guide = FALSE) +
    geom_line(data = revchange[revchange$EPU == "GOM",], aes(x = Time, y = Value, colour="$"))+
    scale_colour_grey(name ="Total Revenue Change") +
    ggtitle("Gulf of Maine Benthivores Component")+
    labs(y="Value $1,000,000 ($2015)") +
    scale_x_continuous(breaks = seq(1965, 2015, by = 10), expand = c(0.01, 0.01)) +
    scale_y_continuous(breaks = seq(y.lim[1], y.lim[2], by = 100), limits = y.lim, expand = c(0.01, 0.01)) +
    theme_ts() +
    theme(title = element_text(size = 10)) +
    theme(legend.position="bottom", legend.direction = "horizontal", legend.background = element_rect(fill = "transparent"), legend.title = element_blank(), legend.text = element_blank())
  
  gb_bennet <- indicators %>% filter(EPU == "GB" & Var %in% c("Volume","Price")) %>% 
    ggplot()+
    
    #Highlight last ten years
    annotate("rect", fill = shade.fill, alpha = shade.alpha,
             xmin = x.shade.min , xmax = x.shade.max,
             ymin = -Inf, ymax = Inf)+
    
    geom_bar(aes(x=Time, y= Value, fill = Var), stat="identity")+
    scale_fill_manual(name = "Indicators", values = ind_fill) +
    geom_line(data = revchange[revchange$EPU == "GB",], aes(x = Time, y = Value, colour="$"))+
    scale_colour_grey(name ="Total Revenue Change") +
    ggtitle("Georges Bank Benthos Component")+
    labs(y="") +
    scale_x_continuous(breaks = seq(1965, 2015, by = 10), expand = c(0.01, 0.01)) +
    scale_y_continuous(breaks = seq(y.lim[1], y.lim[2], by = 100), limits = y.lim, expand = c(0.01, 0.01)) +
    theme_ts() +
    theme(title = element_text(size = 10)) +
    theme(legend.position="bottom", legend.direction = "horizontal", legend.background = element_rect(fill = "transparent"), legend.title = element_text(size = 8), legend.text = element_text(size = 8)) +
    guides(color = guide_legend(order = 1),
           fill = guide_legend(order = 0))
  
  #cowplot::plot_grid(gom_bennet, gb_bennet, ncol = 2, rel_widths = c(1,1))
  
  mylegend<-g_legend(gb_bennet)
  
  p3 <- gridExtra::grid.arrange(gridExtra::arrangeGrob(gom_bennet + theme(legend.position="none"),
                                                       gb_bennet + theme(legend.position="none"),
                                                       nrow=1),
                                mylegend, nrow=2,heights=c(6, 1))
  return(p3)
}

plot_total_comm_land <- function(epu_abbr){
  #Managed landings
  managed_landings <- ecodata::comdat  %>%
    filter(str_detect(Var, paste0(council_abbr," managed species - Landings weight|JOINT managed species - Landings weight")),
           !str_detect(Var, "Other"),
           Time >= 1986,
           EPU %in% epu_abbr)
  
  #Total landings
  total_landings <- ecodata::comdat  %>%
    filter(!str_detect(Var, "managed species"),
           !str_detect(Var, "Other"),
           str_detect(Var, "Landings"),
           Time >= 1986,
           EPU %in% epu_abbr)
  
  total_landings_agg <- total_landings %>%
    group_by(EPU,Time) %>%
    dplyr::summarise(Value = sum(Value)) %>% 
    mutate(Var = "Total",hline = mean(Value))
  managed_landings_agg <- managed_landings %>%
    group_by(EPU,Time) %>%
    dplyr::summarise(Value = sum(Value)) %>% 
    mutate(Var = "Managed",hline = mean(Value))
  
  landings_agg <- rbind(total_landings_agg, managed_landings_agg) 
  
  gom_total <- landings_agg %>% filter(EPU == "GOM") %>% 
    ggplot()+
    
    #Highlight last ten years
    annotate("rect", fill = shade.fill, alpha = shade.alpha,
             xmin = x.shade.min , xmax = x.shade.max,
             ymin = -Inf, ymax = Inf) +
    geom_gls(aes(x = Time, y = Value,
                 group = Var),
             alpha = trend.alpha, size = trend.size) +
    geom_line(aes(x = Time, y = Value, color = Var), size = lwd) +
    geom_point(aes(x = Time, y = Value, color = Var), size = pcex) +
    
    scale_y_continuous(labels = function(l){trans = l / 1000})+
    scale_x_continuous(breaks = seq(1985, 2015, by = 5), expand = c(0.01, 0.01)) +
    scale_color_manual(values = c("indianred","black"), aesthetics = "color")+
    guides(color = FALSE) +
    ylab(expression("Landings (10"^3*"metric tons)")) +
    
    geom_hline(aes(yintercept = hline,
                   
                   color = Var),
               size = hline.size,
               alpha = hline.alpha,
               linetype = hline.lty) +
    theme_ts() +
    ggtitle("Gulf of Maine")
  
  gb_total <- landings_agg %>% filter(EPU == "GB") %>% 
    ggplot()+
    
    #Highlight last ten years
    annotate("rect", fill = shade.fill, alpha = shade.alpha,
             xmin = x.shade.min , xmax = x.shade.max,
             ymin = -Inf, ymax = Inf) +
    geom_gls(aes(x = Time, y = Value,
                 group = Var),
             alpha = trend.alpha, size = trend.size) +
    geom_line(aes(x = Time, y = Value, color = Var), size = lwd) +
    geom_point(aes(x = Time, y = Value, color = Var), size = pcex) +
    
    scale_y_continuous(labels = function(l){trans = l / 1000})+
    scale_x_continuous(breaks = seq(1985, 2015, by = 5), expand = c(0.01, 0.01)) +
    scale_color_manual(values = c("indianred","black"), aesthetics = "color")+
    guides(color = FALSE) +
    ylab(expression("Landings (10"^3*"metric tons)")) +
    
    geom_hline(aes(yintercept = hline,
                   
                   color = Var),
               size = hline.size,
               alpha = hline.alpha,
               linetype = hline.lty) +
    theme_ts() +
    ggtitle("Georges Bank")
  
  return(cowplot::plot_grid(gom_total, gb_total, ncol = 2))
}


#Function for custom ggplot facet labels
label_func <- function(variable,value){
  return(comm_facet_names[value])
}

