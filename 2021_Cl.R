  ### Emma Kok
  
  # clear environment and open the waterbalans
  rm(list = ls())
  source("C:/Users/Kok30/OneDrive - Waternet Amsterdam/Documents/Rstudio/waterbalans_versie_november2022.R")
  rm(list=setdiff(ls(), "df.ts"))
  
  df.ts <- df.ts[,c("datum", setdiff(names(df.ts),"datum"))] # date as column
  
  api_token <- "73938989yQWarcwr2eOnBrTzQ88xPe4S"
  
  area_balans <- "Polder Waardassacker en Holendrecht"
  
  server <- "engine"
  
  # Oppervlaktes ----------------------------------------------------------------
  oppervlakte_totaal <- 6027093.304 #m2
  oppervlakte_water <- 494326.951  #m2
  oppervlakte_land <- oppervlakte_totaal - oppervlakte_water #m2

  # Grondwater en waterpeil -----------------------------------------------------
  # Read the data
  data <- read.table("C:/Users/Kok30/OneDrive - Waternet Amsterdam/groundwaterlevel.txt", header = TRUE, sep = "\t")
  
  # Calculate relative groundwater levels
  tot_grondwater <- data$`tot Grondwaterstand` / oppervlakte_totaal
  wat_grondwater <- data$`wat Grondwaterstand` / oppervlakte_water
  land_grondwater <- data$`lan Grondwaterstand` / oppervlakte_land
  
  

  # Remove the last row from data (if necessary)
  data_grondwater <- head(data, -1)
  
  
  # WATERSTANDEN ---------------------------------------------------------------------------
  # Set y-axis limits
  y_min <- min(data_grondwater$Grondwater..gemiddeld.gemeten..m.NAP., data_grondwater$Berekend, na.rm = TRUE)
  y_max <- max(data_grondwater$Grondwater..gemiddeld.gemeten..m.NAP., data_grondwater$Berekend, na.rm = TRUE)
  ylim <- c(y_min, y_max)
  
  # Convert data to data frame
  data_grondwater <- data.frame(x = 1:nrow(data_grondwater),
                                Grondwater = data_grondwater$Grondwater..gemiddeld.gemeten..m.NAP.,
                                Berekend = data_grondwater$Berekend)
  
  # Melt the data for plotting
  data_grondwater_long <- melt(data_grondwater, id.vars = "x")
  
  # Plot the groundwater data
  plot_grondwater <- ggplot(data_grondwater_long, aes(x = x, y = value, color = variable)) +
    geom_line() +
    scale_color_manual(values = c("brown", "orange"),
                       labels = c("Measured groundwater level", "Calculated groundwater level"),
                       name = "Legend") +
    ylim(ylim) +
    scale_x_continuous(breaks = c(1, 32, 60, 91, 121, 152, 183, 213, 244, 275, 305, 336),
                       labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
    labs(x = "Date", y = "Groundwater level (m NAP)") +
    ggtitle("Groundwater level measured vs calculated in the Waardassacker") +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
    theme(legend.position = "bottom", 
          legend.justification = "right", 
          legend.box.just = "right",
          plot.title = element_text(hjust = 0.5))
  
  # Display the plot
  plot_grondwater
  
  
  
  data_waterpeil <- read.table("C:/Users/Kok30/OneDrive - Waternet Amsterdam/waterpeil.txt", header = TRUE, sep = "\t")
  # Remove the last row from data (if necessary)
  #data_grondwater <- head(data, -1)
  
  # Set y-axis limits
  y_min <- min(data_waterpeil$Gemeten.waterpeil, data_waterpeil$Berekend.waterpeil , na.rm = TRUE)
  y_max <- max(data_waterpeil$Gemeten.waterpeil, data_waterpeil$Berekend.waterpeil , na.rm = TRUE)
  ylim <- c(y_min, y_max)
  
  # Convert data to data frame
  data_waterpeil <- data.frame(x = 1:nrow(data_waterpeil),
                                Waterpeil_gemeten = data_waterpeil$Gemeten.waterpeil,
                                Waterpeil_berkend = data_waterpeil$Berekend.waterpeil)
  
  # Melt the data for plotting
  data_waterpeil_long <- melt(data_waterpeil, id.vars = "x")
  
  # Plot the groundwater data
  plot_waterpeil <- ggplot(data_waterpeil_long, aes(x = x, y = value, color = variable)) +
    geom_line() +
    scale_color_manual(values = c("maroon", "lightgoldenrod"),
                       labels = c("Measured waterlevel", "Calculated waterlevel"),
                       name = "Legend") +
    ylim(ylim) +
    scale_x_continuous(breaks = c(1, 32, 60, 91, 121, 152, 183, 213, 244, 275, 305, 336),
                       labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
    labs(x = "Date", y = "Waterlevel") +
    ggtitle("Waterlevel measured vs calculated in the Waardassacker") +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
    theme(legend.position = "bottom", 
          legend.justification = "right", 
          legend.box.just = "right",
          plot.title = element_text(hjust = 0.5))
  
  # Display the plot
  plot_waterpeil
  
  

  # Waterbalans veranderen -----------------------------------------------------
  df.ts$`Grondwaterstand in m` <- data$Stand_20
  
  # Neerslag in m3 per tijdstap
  df.ts$`tot neerslag in m3` <- df.ts$`tot 1. Grondwater` - lag(df.ts$`tot 1. Grondwater`)
  df.ts$`wat neerslag in m3` <- df.ts$`wat 1. Grondwater` - lag(df.ts$`wat 1. Grondwater`)
  df.ts$`lan neerslag in m3` <- df.ts$`lan 1. Grondwater` - lag(df.ts$`lan 1. Grondwater`)
  
  # Eerste waarde een value geven, anders NA en loopt alles mis
  df.ts$`tot neerslag in m3`[1] <- df.ts$`tot 1. Grondwater`[1]
  df.ts$`wat neerslag in m3`[1] <- df.ts$`wat 1. Grondwater`[1]
  df.ts$`lan neerslag in m3`[1] <- df.ts$`lan 1. Grondwater`[1]
  
  # delta verdamping in m3
  df.ts$`tot verdamping in m3` <- df.ts$`tot Verdamping` - lag(df.ts$`tot Verdamping`)
  df.ts$`wat verdamping in m3` <- df.ts$`wat Verdamping`- lag(df.ts$`wat Verdamping`)
  df.ts$`lan verdamping in m3` <- df.ts$`lan Verdamping`- lag(df.ts$`lan Verdamping`)
  
  # berging grondwater
  df.ts$`tot Grondwater laatste berging [m3]` <- df.ts$`tot Grondwater laatste berging` - lag(df.ts$`tot Grondwater laatste berging`)
  df.ts$`wat Grondwater laatste berging [m3]` <- df.ts$`wat Grondwater laatste berging` - lag(df.ts$`wat Grondwater laatste berging`)
  df.ts$`lan Grondwater laatste berging [m3]` <- df.ts$`lan Grondwater laatste berging` - lag(df.ts$`lan Grondwater laatste berging`)
  
  # per tijdstap
  df.ts$`tot berging opp [m3]` <- df.ts$`tot Oppervlak laatste waarde` - lag(df.ts$`tot Oppervlak laatste waarde`) #### vragen aan jan
  df.ts$`wat berging opp [m3]` <- df.ts$`wat Oppervlak laatste waarde` - lag(df.ts$`wat Oppervlak laatste waarde`)
  df.ts$`lan berging opp [m3]` <- df.ts$`lan Oppervlak laatste waarde` - lag(df.ts$`lan Oppervlak laatste waarde`)
  
  # WATERBALANS ----------------------------------------------------------------
  df.ts$wegzijging <- ifelse(df.ts$`wat Grondwater kwel` > 0, 0, -df.ts$`wat Grondwater kwel`)
  df.ts$`wat drainage + afspoeling` <- df.ts$`wat berging opp [m3]`- df.ts$`wat neerslag in m3` + df.ts$`wat verdamping in m3` - df.ts$wegzijging - df.ts$inlaat + df.ts$`uitlaat`
  df.ts$`wat intrek` <- ifelse(df.ts$`wat drainage + afspoeling`<0, -df.ts$`wat drainage + afspoeling`,0)
  
  
  neerslag <- sum(df.ts$`wat neerslag in m3`)
  kwel <- ifelse(sum(na.omit(df.ts$`wat Grondwater kwel`))>0, sum(na.omit(df.ts$`wat Grondwater kwel`)), 0)
  inlaat <- sum(df.ts$inlaat)
  drainage_afspoeling <- sum(df.ts$`wat drainage + afspoeling`,na.rm = T)
  totaal_in <- sum(neerslag, kwel, inlaat, drainage_afspoeling)
  
  # Calculate output components
  verdamping <- sum(na.omit(df.ts$`wat verdamping in m3`))
  wegzijging <- ifelse(sum(na.omit(df.ts$`wat Grondwater kwel`))>0, 0, -sum(na.omit(df.ts$`wat Grondwater kwel`)))
  uitlaat <- sum(df.ts$uitlaat)
  intrek <- sum(df.ts$`wat intrek`,na.rm=T)
  totaal_uit <- sum(verdamping, wegzijging, intrek, uitlaat)
  
  # Create a data frame with input and output components
  df <- data.frame(
    In = c("Neerslag", "Kwel", "Inlaat","Drainage + Afspoeling", "Totaal"),
    Value = round(c(neerslag, kwel, inlaat, drainage_afspoeling, totaal_in), 1),
    Unit = rep("m³", 5),
    Uit = c("Verdamping", "Wegzijging", "Afvoer", "Intrek", "Totaal"),
    Value_uit = round(c(verdamping, wegzijging, uitlaat, intrek, totaal_uit), 1),
    Unit_uit = rep("m³", 5)
  )
  
  check_balans <- totaal_in - totaal_uit  
  df_check_balans <- data.frame(
    In = c("Check balans"),
    Value = round(check_balans, 1),
    Unit = "m³",
    Uit = "",
    Value_uit = "",
    Unit_uit = ""
  )
  
  df <- rbind(df, df_check_balans)
  
  print(df)
  
  
    
  # Check gemaaldata ---------------------------------------------------------------
  
  data_gemaal <- read.table("C:/Users/Kok30/OneDrive - Waternet Amsterdam/gemaaldata.txt", header = TRUE, sep = "\t")
  
  # Compute the cumulative sums of the vectors
  cumsum_uitlaat <- cumsum(df.ts$uitlaat)
  cumsum_fews <- cumsum(data_gemaal$fews)
  
  y_min <- min(cumsum_uitlaat, cumsum_fews, na.rm = TRUE)
  y_max <- max(cumsum_uitlaat, cumsum_fews, na.rm = TRUE)
  ylim <- c(y_min, y_max)
  
  # Create a data frame for the cumulative sums
  df_outlet <- data.frame(
    time = 1:length(cumsum_uitlaat),
    cumsum_uitlaat = cumsum_uitlaat,
    cumsum_fews = cumsum_fews
  )
  
  # Melt the data frame
  data_outlet_long <- melt(df_outlet, id.vars = "time")
  
  # Create the plot
  outlet_data <- ggplot(data_outlet_long, aes(x = time, y = value, color = variable)) +
    geom_line() +
    scale_color_manual(
      values = c("blue", "green"),
      labels = c("Calculated gemaal data", "Measured gemaal data"),
      name = "Legend"
    ) +
    ylim(ylim) +
    scale_x_continuous(
      breaks = c(1, 32, 60, 91, 121, 152, 183, 213, 244, 275, 305, 336),
      labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    ) +
    labs(x = "Date", y = "Cumulative volume [m3]") +
    ggtitle("Cumulative volume comparison between calculated and measured outflow") +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    theme(legend.position = "bottom", 
          legend.justification = "right", 
          legend.box.just = "right",
          plot.title = element_text(hjust = 0.5))
  
  # Display the plot
  outlet_data
  
  
  diff_gemaal <- cumsum_uitlaat - cumsum_fews
  
  sum(diff_gemaal)
  
  

  

  
  # initial concentrations-------------------------------------------------------
  df_Cl <- data.frame(datum = df.ts$datum)
  
  chloride_inlaat <- c("1" = 220, "2" = 275, "3" = 420, "4" = 410,
                       "5" = 480, "6" = 410, "7" = 420, "8" = 410,
                       "9" = 410, "10" = 340, "11" = 200, "12" = 190)
  
  
  df_Cl$month <- month(as.Date(df_Cl$datum))
  
  chloride_neerslag <- 0 #mg/l
  chloride_initial_value <- 211 #mg/l
  chloride_verdamping <- 0 #mg/l
  
  # INPUT chloride -----------------------------------------------------------
  
  #delta neerslag in m3
  df_Cl$`tot chloride neerslag [g]` <- df.ts$`tot neerslag in m3`* chloride_neerslag
  df_Cl$`wat chloride neerslag [g]` <- df.ts$`wat neerslag in m3` * chloride_neerslag
  df_Cl$`lan chloride neerslag [g]` <- df.ts$`lan neerslag in m3` * chloride_neerslag
  
  #inlaat water, nu dus overal dezelfde chloride inlaat
  df_Cl$`inlaat chloride [g]` <- df.ts$inlaat * chloride_inlaat[df_Cl$month]
  
  # Input drainage en afspoeling
  min_value <- 0
  max_value <- 80
  mid_value <- 40
  min_gws <- min(df.ts$`Grondwaterstand in m`)
  max_gws <- max(df.ts$`Grondwaterstand in m`)
  
  # Scale the values based on 'Grondwaterstand in m'
  # (max_value - min_value) / (max_gws - min_gws): This expression calculates the scaling factor
  # determines how much each unit change in the Grondwaterstand in m variable should correspond 
  # to a change in the scaled value between min_value and max_value.
  
  # (df.ts$Grondwaterstand in m - min_gws): This part subtracts the minimum value of Grondwaterstand 
  # in m (min_gws) from each value in the Grondwaterstand in m variable. This step is necessary to align 
  # the scaling with the range of Grondwaterstand in m so that the minimum value in Grondwaterstand in m maps 
  # to min_value in the scaled values.
  
  scaled_value <- min_value + ((max_value - min_value) / (max_gws - min_gws)) * (df.ts$`Grondwaterstand in m` - min_gws)
  scaled_value <- pmax(pmin(scaled_value, max_value), min_value)  # Limit values to the desired range
  
  # Assign the scaled values to 'drainage chloride' variable in 'df_Cl' dataframe
  df_Cl$`drainage chloride` <- scaled_value
  


  #####
  df_Cl$`wat drainage en afspoeling chloride [g]` <- df_Cl$`drainage chloride`* df.ts$`wat drainage + afspoeling`
  df_Cl$`in chloride` <- df_Cl$`wat chloride neerslag [g]` + df_Cl$`inlaat chloride [g]` + ifelse(is.na(df_Cl$`wat drainage en afspoeling chloride [g]`), 0, df_Cl$`wat drainage en afspoeling chloride [g]`)
  
  
  # OUTPUT chloride ------------------------------------------------------------
  # calculate verdamping chloride and add it as new columns to df.ts
  #df_Cl$`tot chloride verdamping [g]` <- df.ts$`tot verdamping in m3` * chloride_neerslag
  #df_Cl$`wat chloride verdamping [g]` <- df.ts$`wat verdamping in m3` * chloride_neerslag
  #df_Cl$`lan chloride verdamping [g]` <- df.ts$`lan verdamping in m3` * chloride_neerslag
  
  
  # gemaal, intrek, wegzijging
  df_Cl$`gemaal chloride [g]`[1] <- df.ts$uitlaat[1]* chloride_initial_value
  df_Cl$`intrek chloride [g]`[1] <- NA
  
  df_Cl$`uit chloride`[1] <- df_Cl$`gemaal chloride [g]`[1]
  
  # calculate berging sloot [g]
  df_Cl$`berging sloot [g]`[1] <- df.ts$`wat Oppervlak laatste waarde`[1] * chloride_initial_value
  
  # calculate sloot [mg/l]
  df_Cl$`sloot [mg/l]`[1] <- chloride_initial_value
  
  df_Cl$`wegzijging factor` <- ifelse(df.ts$wegzijging > 0, 1, 0)
  df_Cl$`wegzijging factor`[is.na(df_Cl$`wegzijging factor`)] <- 0
  
  df.ts$wegzijging[is.na(df.ts$wegzijging)] <- 0
  
  # calculate delta berging sloot [g]
  df_Cl$`delta berging sloot [g]`[1] <- NA
  
  for (i in 2:nrow(df_Cl)) {
    df_Cl$`berging sloot [g]`[i] <- df_Cl$`berging sloot [g]`[i-1] + df_Cl$`in chloride`[i-1] - df_Cl$`uit chloride`[i-1]
    df_Cl$`sloot [mg/l]`[i] <- df_Cl$`berging sloot [g]`[i] / df.ts$`wat Oppervlak laatste waarde`[i]
    df_Cl$`gemaal chloride [g]`[i] <- df.ts$`uitlaat`[i] * df_Cl$`sloot [mg/l]`[i]
    df_Cl$`wegzijging`[i] <- df_Cl$`wegzijging factor`[i]*df.ts$wegzijging[i]* df_Cl$`sloot [mg/l]`[i]
    df_Cl$`intrek chloride [g]`[i] <- df.ts$`wat intrek`[i] * df_Cl$`sloot [mg/l]`[i]
    df_Cl$`uit chloride`[i] <- df_Cl$`gemaal chloride [g]`[i] + df_Cl$`intrek chloride [g]`[i] + df_Cl$`wegzijging`[i]
    df_Cl$`delta berging sloot [g]`[i] <- df_Cl$`berging sloot [g]`[i] - df_Cl$`berging sloot [g]`[i-1]
  }
  
  df_Cl$`verschil` = df_Cl$`in chloride`- df_Cl$`uit chloride`
  
  som_verschil <- sum(df_Cl$`in chloride`)-sum(df_Cl$`uit chloride`)
  som_verschil
  
  # Plot -----------------------------------------------------------------------
  df_Cl$datum <- as.Date(df_Cl$datum, format = "%d-%m-%Y")
  
  # Create a dataframe of the measured values
  measured_values <- data.frame(date = as.Date(c("2021-01-14", "2021-02-18", "2021-03-11", "2021-04-15",
                                                 "2021-05-10", "2021-06-07", "2021-07-12", "2021-08-11",
                                                 "2021-09-09", "2021-10-13", "2021-11-11", "2021-12-03")),
                                Cl_mg_l = c(141.268, 106.688, 192.693, 255.603, 355.291, 268.239, 358.309,
                                            291.499, 382.512, 212.959, 138.42, 83.737))
  
  # Plot sloot using ggplot
  plot_Cl <- ggplot(df_Cl, aes(x = datum)) +
    geom_line(aes(y = `sloot [mg/l]`, color = "Calculated concentration")) +
    geom_point(data = measured_values, aes(x = date, y = Cl_mg_l, color = "Measured concentration")) +
    labs(x = "Date", y = "Concentration chloride [mg/l]") +
    theme_minimal() +
    theme(axis.line = element_line(size = 0.8), 
          axis.text = element_text(size = 12),
          plot.title = element_text(hjust = 0.5)) +  # Set hjust to 0.5 for center alignment
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
    ggtitle("Cl concentration in the Waardassacker in 2021") +
    scale_color_manual(name = "Legend",
                       values = c("Calculated concentration" = "brown", "Measured concentration" = "red"))
  
  plot_Cl + ylim(80, 650)
  
  # Install the openxlsx package if not already installed -------------------------------
  install.packages("openxlsx")
  
  # Load the openxlsx package
  library(openxlsx)
  
  # Specify the file path where you want to save the Excel file
  file_path <- "C:/Users/Kok30/OneDrive - Waternet Amsterdam/df_Cl_2021_meerinlaat.xlsx"
  
  # Create a new workbook
  wb <- createWorkbook()
  
  # Add the dataframe to the workbook
  addWorksheet(wb, "Sheet1")
  writeData(wb, "Sheet1", df_Cl)
  
  # Save the workbook as an Excel file
  saveWorkbook(wb, file_path)
  

  
  
  
  # plotten van de kunstwerken -----------------------------------------------
  
  kunstwerken <- tyg_kunstwerken_timeseries(API_token = "73938989yQWarcwr2eOnBrTzQ88xPe4S",
                                            server = "engine", overlay_type = "GROUNDWATER")
  
  ggplot(filter(kunstwerken, startsWith(NAME, "KIN")), 
         aes(x=datum, y=debiet)) +
    #ylim(-100,100)+
    theme_bw() +
    facet_wrap(~NAME) +
    geom_line()
  
  # Fracties  ---------------------------------------------------------------------
  
  df_frac1 <- data.frame(datum = df.ts$datum)
  
  df_frac1$`S in` <- df.ts$`wat neerslag in m3` +  df.ts$inlaat + df.ts$`wat drainage + afspoeling` # S in inlaat + neerslag + drainage en afspoeling
  df_frac1$`S uit` <- df.ts$uitlaat + df.ts$`wat verdamping in m3` + df.ts$`wat intrek` # S uit gemaal + verdamping + intrek
  df_frac1$S[1] <- df.ts$`wat Oppervlak laatste waarde`[1]  # S is berging opp
  
  df_frac1$`S initieel`[1] <- 0.22 * df_frac1$S[1]
  df_frac1$`initieel procent`[1] <- 0.22
  
  df_frac1$`S inlaat`[1] <-  0.21 * df_frac1$S[1]
  df_frac1$`inlaat procent`[1] <- 0.21
  
  df_frac1$`S neerslag`[1] <-  0.12 * df_frac1$S[1]
  df_frac1$`neerslag procent`[1] <- 0.12
  
  df_frac1$`S bodem`[1] <- 0.45 * df_frac1$S[1]
  df_frac1$`bodem procent`[1] <- 0.45
  
  df_frac1$`Som fracties`[1] <- round(df_frac1$`initieel procent`[1] + df_frac1$`inlaat procent`[1] + df_frac1$`neerslag procent`[1] + df_frac1$`bodem procent`[1])
  
  
  
  for (i in 2:nrow(df_Cl)) {
    df_frac1$S[i] <- df_frac1$S[i-1] + df_frac1$`S in`[i] - df_frac1$`S uit`[i]
    
    df_frac1$`S initieel`[i] <- df_frac1$`S initieel`[i-1] - (df_frac1$`initieel procent`[i-1] * df_frac1$`S uit`[i])
    df_frac1$`initieel procent`[i] <- max(min(df_frac1$`S initieel`[i] / df_frac1$S[i], 1), 0)
    
    df_frac1$`S inlaat` [i] <-  df_frac1$`S inlaat` [i-1] + df.ts$inlaat [i] - (df_frac1$`inlaat procent` [i-1]*df_frac1$`S uit`[i])
    df_frac1$`inlaat procent` [i] <- max(min(df_frac1$`S inlaat`[i] / df_frac1$S[i], 1), 0)
    
    df_frac1$`S neerslag` [i] <- df_frac1$`S neerslag` [i-1] + df.ts$`wat neerslag in m3` [i]-(df_frac1$`neerslag procent`[i-1]*df_frac1$`S uit`[i])
    df_frac1$`neerslag procent` [i] <- max(min(df_frac1$`S neerslag`[i] / df_frac1$S[i], 1), 0)
    
    df_frac1$`S bodem` [i] <- df_frac1$`S bodem` [i-1] + df.ts$`wat drainage + afspoeling`[i]-(df_frac1$`bodem procent` [i-1]*df_frac1$`S uit`[i])
    df_frac1$`bodem procent` [i] <- ifelse(df_frac1$`S bodem`[i] / df_frac1$S[i] < 0,0, (max(min(df_frac1$`S bodem`[i] / df_frac1$S[i], 1), 0)))  
    
    df_frac1$`Som fracties` [i] <- round(df_frac1$`initieel procent`[i] + df_frac1$`inlaat procent`[i] + df_frac1$`neerslag procent`[i] + df_frac1$`bodem procent`[i])
  }
  
  
  row_id <- seq(1, nrow(df_Cl))
  df_Cl$row <- row_id
  
  # create a data frame with the four lines to plot
  df_lines <- data.frame(x = 1:nrow(df_frac1),
                         y1 = df_frac1$`initieel procent`,
                         y2 = df_frac1$`inlaat procent`,
                         y3 = df_frac1$`neerslag procent`,
                         y4 = df_frac1$`bodem procent`)
  
  df_lines_long <- melt(df_lines, id.vars = "x")
  
  plot_fracties <- ggplot() +
    geom_area(data = df_lines_long, aes(x = x, y = value * 100, fill = variable),position = "stack", color = "black") +
    geom_line(data= df_Cl, aes(x=row, y=(`sloot [mg/l]`)/4))
    theme_bw() +
    #scale_x_continuous(
     # breaks = c(1, 32, 60, 91, 121, 152, 183, 213, 244, 275, 305, 336),
      #labels = c("jan", "feb", "mar", "apr", "mei", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
    #) +
    theme(axis.title.x = element_blank()) +
    labs(y = "fractie (%)")
  
  plot_fracties
  

  
  
  #CHLORIDE BALANS -----------------------------------------------------------
  Cl_neerslag <- sum(df_Cl$`wat chloride neerslag [g]`)
  Cl_kwel <- 0
  Cl_inlaat <- sum(df_Cl$`inlaat chloride [g]`,na.rm=T)
  Cl_drainage_afspoeling <- sum(df_Cl$`wat drainage en afspoeling chloride [g]`,na.rm=T)
  Cl_totaal_in <- sum(Cl_neerslag, Cl_inlaat, Cl_drainage_afspoeling)
  
  # Calculate output components
  Cl_verdamping <- 0
  Cl_wegzijging <- sum(df_Cl$`wegzijging`)
  Cl_uitlaat <- sum(df_Cl$`gemaal chloride [g]`,na.rm=T)
  Cl_intrek <- sum(df_Cl$`intrek chloride [g]`,na.rm=T)
  Cl_totaal_uit <- sum(Cl_wegzijging,Cl_uitlaat,Cl_intrek)
    
  
  
  # Create a data frame with input and output components
  Cl_df <- data.frame(
    In = c("Neerslag", "Kwel", "Inlaat","Drainage + Afspoeling", "Totaal"),
    Value = round(c(Cl_neerslag, Cl_kwel, Cl_inlaat, Cl_drainage_afspoeling, Cl_totaal_in), 1),
    Unit = rep("g", 5),
    Uit = c("Verdamping", "Wegzijging", "Afvoer", "Intrek", "Totaal"),
    Value_uit = round(c(Cl_verdamping, Cl_wegzijging, Cl_uitlaat, Cl_intrek, Cl_totaal_uit), 1),
    Unit_uit = rep("g", 5)
  )
  
  Cl_check_balans <- Cl_totaal_in - Cl_totaal_uit  
  df_check_balans <- data.frame(
    In = c("Check balans"),
    Value = round(Cl_check_balans, 1),
    Unit = "g",
    Uit = "",
    Value_uit = "",
    Unit_uit = ""
  )
  
  Cl_df <- rbind(Cl_df, df_check_balans)
  
  print(Cl_df)
  
  

  
  
  # Fracties Cl ---------------------------------------------------------------------
  
  df_frac <- data.frame(datum = df.ts$datum)
  
  df_frac$`Cl In` <- df_Cl$`in chloride` # S in inlaat + neerslag + drainage en afspoeling
  df_frac$`Cl Uit` <- df_Cl$`uit chloride` # S uit gemaal + verdamping + intrek
  df_frac$S[1] <- df_Cl$`berging sloot [g]`[1]  # S is berging opp
  
  df_frac$`S initieel`[1] <- 0.01 * df_frac$S[1]
  df_frac$`initieel procent`[1] <- 0.01
  
  df_frac$`S inlaat`[1] <-  0.46 * df_frac$S[1]
  df_frac$`inlaat procent`[1] <- 0.46
  
  df_frac$`S neerslag`[1] <-  0.05 * df_frac$S[1]
  df_frac$`neerslag procent`[1] <- 0.05
  
  df_frac$`S bodem`[1] <- 0.47 * df_frac$S[1]
  df_frac$`bodem procent`[1] <- 0.47
  
  df_frac$`Som fracties`[1] <- round(df_frac$`initieel procent`[1] + df_frac$`inlaat procent`[1] + df_frac$`neerslag procent`[1] + df_frac$`bodem procent`[1])
  
  
  
  for (i in 2:nrow(df_Cl)) {
    df_frac$S[i] <- df_frac$S[i-1] + df_frac$`Cl In`[i] - df_frac$`Cl Uit`[i]
    
    df_frac$`S initieel`[i] <- df_frac$`S initieel`[i-1] - (df_frac$`initieel procent`[i-1] * df_frac$`Cl Uit`[i-1])
    df_frac$`initieel procent`[i] <- max(min(df_frac$`S initieel`[i] / df_frac$S[i], 1), 0)
    
    df_frac$`S inlaat` [i] <-  df_frac$`S inlaat` [i-1] + df_Cl$`inlaat chloride [g]`[i] - (df_frac$`inlaat procent` [i-1]*df_frac$`Cl Uit`[i])
    df_frac$`inlaat procent` [i] <- max(min(df_frac$`S inlaat`[i] / df_frac$S[i], 1), 0)
      
    df_frac$`S neerslag` [i] <- df_frac$`S neerslag` [i-1] + df_Cl$`wat chloride neerslag [g]`[i]-(df_frac$`neerslag procent`[i-1]*df_frac$`Cl Uit`[i])
    df_frac$`neerslag procent` [i] <- max(min(df_frac$`S neerslag`[i] / df_frac$S[i], 1), 0)
  
    df_frac$`S bodem` [i] <- df_frac$`S bodem` [i-1] + df_Cl$`wat drainage en afspoeling chloride [g]`[i]-(df_frac$`bodem procent` [i-1]*df_frac$`Cl Uit`[i])
    df_frac$`bodem procent` [i] <- ifelse(df_frac$`S bodem`[i] / df_frac$S[i] < 0,0, (max(min(df_frac$`S bodem`[i] / df_frac$S[i], 1), 0)))  
    
    df_frac$`Som fracties` [i] <- round(df_frac$`initieel procent`[i] + df_frac$`inlaat procent`[i] + df_frac$`neerslag procent`[i] + df_frac$`bodem procent`[i])
  }
  
  
  
  # create a data frame with the four lines to plot
  df_linesCl <- data.frame(x = 1:nrow(df_frac),
                         y1 = df_frac$`initieel procent`,
                         y2 = df_frac$`inlaat procent`,
                         y3 = df_frac$`neerslag procent`,
                         y4 = df_frac$`bodem procent`)
  
  df_linesCl_long <- melt(df_linesCl, id.vars = "x")
  
  ggplot(df_linesCl_long, aes(x=x, y=value*100, fill=variable)) +
    geom_area(position = "stack", color="black") +
    theme_bw() +
    scale_x_continuous(breaks = c(1, 32, 60, 91, 121, 152, 183, 213, 244, 275, 305, 336),
                       labels = c("jan", "feb", "mar", "apr", "mei", "jun", "jul", "aug", "sep", "oct", "nov", "dec")) +
    theme(axis.title.x = element_blank()) +
    labs(y="fractie (%)")
  



# Plot graph ------------------------------------------------------------------
#plot(cumsum(df.ts$uitlaat))# Set plot parameters
#plot(cumsum(df.ts$uitlaat), type = "l", col = "blue", 
#     xlab = "Date", ylab = "Debiet Waardassacker",
#     main = "Debiet Gemaal Waardassacker")

# Add a legend
#legend("topright", legend = "Uitlaat", col = "blue", lty = 1)

##### Extra notes --------------------------------------------------------------

#df.ts$`neerslag tot [m]`= (df.ts$`tot 1. Grondwater` / oppervlakte_totaal)
#df.ts$`neerslag wat [m]`= (df.ts$`wat 1. Grondwater` / oppervlakte_water)
#df.ts$`neerslag lan [m]`= (df.ts$`lan 1. Grondwater` / oppervlakte_land)

# Calculate rainfall per time period
#df.ts$`neerslag tot [m per timestap]` <- c(df.ts$`neerslag tot [m]`[1], diff(df.ts$`neerslag tot [m]`))
#df.ts$`neerslag wat [m per timestap]` <- c(df.ts$`neerslag wat [m]`[1], diff(df.ts$`neerslag wat [m]`))
#df.ts$`neerslag lan [m per timestap]` <- c(df.ts$`neerslag lan [m]`[1], diff(df.ts$`neerslag lan [m]`))

# Delete original columns by name
#df.ts <- df.ts[, !names(df.ts) %in% c("tot 1. Grondwater", "wat 1. Grondwater", "lan 1. Grondwater")]


# OUTPUT chloride -----------------------------------------------------------
# calculate verdamping chloride and add it as new columns to df.ts
#df.ts$`tot chloride verdamping [g]` <- (df.ts$`tot verdamping in m3`* chloride_neerslag * 1000) / 1000
#df.ts$`wat chloride verdamping [g]` <- (df.ts$`wat verdamping in m3` * chloride_neerslag * 1000) / 1000
#df.ts$`lan chloride verdamping [g]` <- (df.ts$`lan verdamping in m3` * chloride_neerslag * 1000) / 1000


#first value of column sloot: 
#df.ts$`sloot [mg/l]` <- 50
#rest of the values of column sloot: 
#df.ts$`sloot [mg/l]` <- ((df.ts$`berging sloot [g]`*1000)/1000)/df.ts$`delta tot berging opp [m3]`

# calculate intrek chloride and add it as a new column to df.ts
#df.ts$intrek_chloride <- (df.ts$`intrek` * df.ts$`sloot [mg/l]` * 1000) / 1000


# calculate gemaal chloride [g] and add it as a new column to df.ts
#df.ts$`gemaal chloride [g]` <- (df.ts$uitlaat * df.ts$`sloot [mg/l]` * 1000) / 1000

# calculate uit chloride and add it as a new column to df.ts
#df.ts$`uit chloride` <- df.ts$`gemaal chloride [g]` + df.ts$intrek_chloride

#first value of column berging sloot:
#df.ts$`berging sloot [g]` <- (df.ts$`delta wat berging opp [m3]`* (df.ts$`sloot [mg/l]`)*1000)/1000
#rest of the values of columm bergin sloot
#df.ts$`berging sloot [g]` <- previous timestep value of berging sloot [g] + previous timestep value of df.ts$`in chloride` - previous timestep value of df.ts$`uit chloride`

# first value of delta berging sloot [g] = non existing (because previous timestep)
#delta_berging_sloot <- c(NA, diff(df.ts$`berging sloot [g]`))
#df.ts$`delta berging sloot [g]` <- delta_berging_sloot


# calculate intrek chloride and add it as a new column to df.ts
#df.ts$intrek_chloride <- (df.ts$`intrek` * df.ts$`sloot [mg/l]` * 1000) / 1000



#WEGZIJGING

