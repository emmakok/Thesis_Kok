### Emma Kok
### Dit is een opzet van een fosfaat balans

# Clear environment and open the waterbalans
rm(list = ls())
source("C:/Users/Kok30/OneDrive - Waternet Amsterdam/Documents/Rstudio/waterbalans_versie_november2022.R")
rm(list=setdiff(ls(), "df.ts"))

df.ts <- df.ts[,c("datum", setdiff(names(df.ts),"datum"))] # date as column

api_token <- "72342404vjiLM6IfwQiKZmzSDeMHrgPJ"

area_balans <- "Polder Waardassacker en Holendrecht"

server <- "engine"

df.ts$temperatuur <- c(6.8, 6.5, 8.8, 8.2, 6.4, 4.5, 1, 1.1, 3, 6.9, 5.7, 5, 4.2, 2.9, 5.2, 5.2, 4.6, 4.7, 3.7, 2.2, 3, 6.3, 8.1, 11.7, 7.7, 5.4, 
                       5.8, 9.7, 8.8, 4.7, 6.5, 3.4, 4.4, 2, 1.7, 1, -2.1, -2.2, 1, 0.6, 2.9, 4.5, 3, 2, 1.9, 4.3, 3.2, 0.9, 0.7, 1.2, 0.8, 0.3, 
                       0.2, -0.5, -0.4, -2.2, -3.3, -3.6, -6.6, -4.7, -3.7, -1.7, 5, 6.6, 5.6, 5.9, 4.8, 6.5, 11.4, 10.2, 10.3, 5.6, 5.8, 7.3, 
                       3.8, -1.4, -1, -0.2, 3.8, 3.6, 6.4, 6.2, 6.5, 5.4, 4.7, 5.6, 5.9, 6.5, 9, 7.2, 4.8, 7.7, 12.7, 10.3, 5.9, 8.8, 13.9, 15.2, 
                       12.6, 15.5, 12.7, 10.4, 10, 11.5, 12.7, 11.4, 12.8, 16.3, 19.2, 18.3, 15.5, 17.9, 13.1, 12.9, 11.5, 10.3, 10.3, 11.7, 9.2, 
                       9.5, 7.9, 10.9, 10.4, 11.9, 14.7, 17.2, 18, 19.1, 17.9, 12.1, 12.3, 17, 13.1, 18.7, 19.2, 15.3, 11.4, 10.9, 11, 15.4, 19.2, 
                       18.3, 18.8, 18.3, 20.6, 23, 20.3, 22.5, 22.1, 21.5, 20.2, 17.6, 17, 19.3, 18.2, 16.7, 19.8, 22.4, 18.5, 19.8, 18.2, 16.9, 
                       14.8, 14.7, 15.7, 18.8, 17.5, 15.6, 16.7, 17.7, 19.1, 14.4, 13.3, 15.1, 14.7, 16.5, 16, 17.9, 21.2, 19.5, 21.3, 21.3, 20.6, 
                       19.6, 19.2, 18.9, 19.5, 18.3, 18.6, 17.5, 16.1, 17.8, 19, 18.5, 18.8, 20.8, 22.6, 20, 19.1, 19.6, 19.1, 21.1, 20.6, 22.5, 24.6, 
                       24.4, 27.7, 29.7, 20.3, 21.3, 24, 20.6, 20.4, 22.6, 23.9, 22.4, 20.5, 23.1, 25.9, 19.6, 17.2, 16.5, 15.6, 19.8, 18.7, 18.8, 
                       19.7, 18.4, 17.4, 17.8, 19.4, 19.6, 20, 19.5, 18.3, 15.5, 12.7, 14.5, 17.1, 16.7, 14.7, 15, 12.4, 13.5, 15.9, 18.6, 20.6, 19, 
                       16.6, 14.6, 14.8, 18.3, 17.4, 18.8, 15.3, 13, 13.5, 14.7, 14.6, 15.3, 18.9, 19.1, 19.7, 15.4, 12.1, 9.5, 9.9, 9.4, 11.9, 
                       13.2, 11.4, 7.5, 9.9, 9.5, 11.7, 12.6, 14.7, 12.8, 13.9, 10.1, 9.5, 10.9, 15.1, 17.6, 18.7, 20, 17.4, 16.1, 15.3, 13.2, 12.2, 
                       9.2, 8.6, 12.2, 10.8, 12.8, 14, 12.1, 9.5, 7.1, 4, 4.4, 5.7, 8.5, 10.4, 7.8, 4.3, 5.9, 7.7, 12.4, 11.1, 9.2, 9, 11.6, 11.4, 10, 
                       10.2, 8, 6.9, 3.4, 4.1, 2.9, 4.9, 3.4, 3.4, 3.2, 3.2, 2.4, 3.4, 4.9, 3.3, 6.1, 10.2, 9.6, 7.7, 12, 11.4, 4.8, 5.5, 11.1, 10, 9.1, 
                       8.1, 5.5, 3.9, 0.8, 0.8, 0.9, 0.3, 1.4, 5.6, 6.4, 7.4, 7.5, 9.7, 9.6, 7.1, 5.7, 5.6, 4, 0.8, 2.3, 7.9, 8.5, 8.9)

# Oppervlaktes ----------------------------------------------------------------
oppervlakte_totaal <- 6027093.304 #m2
oppervlakte_water <- 494326.951  #m2
oppervlakte_land <- oppervlakte_totaal - oppervlakte_water #m2

# Grondwater ------------------------------------------------------------------

data <- read.table("C:/Users/Kok30/OneDrive - Waternet Amsterdam/grondwstand_2018.txt", header = TRUE, sep = "\t")

# Remove the last row from data (if necessary)
data_grondwater <- head(data, -1)




# Waterbalans veranderen -----------------------------------------------------
df.ts$`Grondwaterstand in m` <- data$stand_20                                                                                                      

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


# initial concentrations-------------------------------------------------------
df_PO <- data.frame(datum = df.ts$datum)

fosfaat_inlaat <- c("1" = 0.2, "2" = 0.2, "3" = 0.17, "4" = 0.18,
                    "5" = 0.1, "6" = 0.05, "7" = 0.19, "8" = 0.05,
                    "9" = 0.16, "10" = 0.1, "11" = 0.11, "12" = 0.12)

df_PO$month <- month(as.Date(df_PO$datum))



fosfaat_neerslag <- 0.1 #mg/l
fosfaat_initial_value <- 0.2 #mg/l
fosfaat_verdamping <- 0.1 #mg/l

# INPUT fosfaat -----------------------------------------------------------

#delta neerslag in m3
df_PO$`tot fosfaat neerslag [g]` <- df.ts$`tot neerslag in m3`* fosfaat_neerslag
df_PO$`wat fosfaat neerslag [g]` <- df.ts$`wat neerslag in m3` * fosfaat_neerslag
df_PO$`lan fosfaat neerslag [g]` <- df.ts$`lan neerslag in m3` * fosfaat_neerslag

#inlaat water, nu dus overal dezelfde fosfaat inlaat
df_PO$`inlaat fosfaat [g]` <- df.ts$inlaat * fosfaat_inlaat[df_PO$month]




# Input drainage en afspoeling
min_value <- 0
max_value <- 0.6
mid_value <- 0.3
min_gws <- min(df.ts$`Grondwaterstand in m`)
max_gws <- max(df.ts$`Grondwaterstand in m`)


scaled_value <- min_value + ((max_value - min_value) / (max_gws - min_gws)) * (df.ts$`Grondwaterstand in m` - min_gws)
scaled_value <- pmax(pmin(scaled_value, max_value), min_value)  # Limit values to the desired range

# Assign the scaled values to 'drainage chloride' variable in 'df_Cl' dataframe
df_PO$`drainage fosfaat` <- scaled_value


#####
df_PO$`wat drainage en afspoeling fosfaat [g]` <- df_PO$`drainage fosfaat`* df.ts$`wat drainage + afspoeling`
df_PO$`in fosfaat` <- df_PO$`wat fosfaat neerslag [g]` + df_PO$`inlaat fosfaat [g]` + ifelse(is.na(df_PO$`wat drainage en afspoeling fosfaat [g]`), 0, df_PO$`wat drainage en afspoeling fosfaat [g]`)


# OUTPUT fosfaat ------------------------------------------------------------
# calculate verdamping fosfaat and add it as new columns to df.ts
#df_PO$`tot fosfaat verdamping [g]` <- df.ts$`tot verdamping in m3` * fosfaat_neerslag
#df_PO$`wat fosfaat verdamping [g]` <- df.ts$`wat verdamping in m3` * fosfaat_neerslag
#df_PO$`lan fosfaat verdamping [g]` <- df.ts$`lan verdamping in m3` * fosfaat_neerslag

# Uit posten PO:
# gemaal, intrek, wegzijging
df_PO$`gemaal fosfaat [g]`[1] <- df.ts$uitlaat[1]* fosfaat_initial_value
df_PO$`intrek fosfaat [g]`[1] <- NA

# Nalevering waardes: dijkstra - gemiddeld over locaties (~2012/2013)
df_PO$nalevering <- ((0.0543 * df.ts$temperatuur * 0.193) * (oppervlakte_water)) * 0.001 


df_PO$`uit fosfaat`[1] <- df_PO$`gemaal fosfaat [g]`[1]

# calculate berging sloot [g]
df_PO$`berging sloot [g]`[1] <- df.ts$`wat Oppervlak laatste waarde`[1] * fosfaat_initial_value
df_PO$`uit fosfaat`[1]

# calculate sloot [mg/l]
df_PO$`sloot [mg/l]`[1] <- fosfaat_initial_value

df_PO$`wegzijging factor` <- ifelse(df.ts$wegzijging > 0, 1, 0)
df_PO$`wegzijging factor`[is.na(df_PO$`wegzijging factor`)] <- 0

df.ts$`tot Grondwater kwel`[is.na(df.ts$wegzijging)] <- 0

# calculate delta berging sloot [g]
df_PO$`delta berging sloot [g]`[1] <- NA

for (i in 2:nrow(df.ts)) {
  df_PO$`berging sloot [g]`[i] <- df_PO$`berging sloot [g]`[i-1] + df_PO$`in fosfaat`[i-1] - df_PO$`uit fosfaat`[i-1]
  df_PO$`sloot [mg/l]`[i] <- df_PO$`berging sloot [g]`[i] / df.ts$`wat Oppervlak laatste waarde`[i]
  df_PO$`gemaal fosfaat [g]`[i] <- df.ts$`uitlaat`[i] * df_PO$`sloot [mg/l]`[i]
  df_PO$`wegzijging`[i] <- df_PO$`wegzijging factor`[i]*df.ts$wegzijging[i]* df_PO$`sloot [mg/l]`[i]
  df_PO$`intrek fosfaat [g]`[i] <- df.ts$`wat intrek`[i] * df_PO$`sloot [mg/l]`[i]
  df_PO$`uit fosfaat`[i] <- df_PO$`gemaal fosfaat [g]`[i] + df_PO$`intrek fosfaat [g]`[i] + df_PO$`wegzijging`[i]
  df_PO$`delta berging sloot [g]`[i] <- df_PO$`berging sloot [g]`[i] - df_PO$`berging sloot [g]`[i-1]
}

df_PO$`verschil` = df_PO$`in fosfaat`- df_PO$`uit fosfaat`
df_PO$`nalevering`

# Plot -----------------------------------------------------------------------
df_PO$datum <- as.Date(df_PO$datum, format = "%d-%m-%Y")

# Create a dataframe of the measured values
measured_values <- data.frame(date = as.Date(c("2018-01-16", "2018-03-22", "2018-04-26",
                                               "2018-05-30", "2018-06-27", "2018-07-19", "2018-08-27",
                                               "2018-09-27", "2018-10-25", "2018-11-26")),
                              PO_mg_l = c(0.350, 0.235,0.218,0.343,0.126,0.176,0.146,0.180,0.334,
                                          0.112))

# Plot sloot using ggplot0,350

plot_PO <- ggplot(df_PO, aes(x = datum)) +
  geom_line(aes(y = `sloot [mg/l]`, color = "Calculated concentration")) +
  geom_point(data = measured_values, aes(x = date, y = PO_mg_l, color = "Measured concentration")) +
  labs(x = "Datum", y = "Concentratie fosfaat") +
  theme_minimal() +
  theme(axis.line = element_line(size = 0.8), 
        axis.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5)) +
  theme_bw() +
  ggtitle("PO concentration in the Waardassacker in 2018") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_color_manual(name = "Legend",
                     values = c("Calculated concentration" = "darkgreen", "Measured concentration" = "darkorange"))
plot_PO



# Install the openxlsx package if not already installed
install.packages("openxlsx")

# Load the openxlsx package
library(openxlsx)

# Specify the file path where you want to save the Excel file
file_path <- "C:/Users/Kok30/OneDrive - Waternet Amsterdam/df_PO_norm_minderinlaat.xlsx"

# Create a new workbook
wb <- createWorkbook()

# Add the dataframe to the workbook
addWorksheet(wb, "Sheet1")
writeData(wb, "Sheet1", df_PO)

# Save the workbook as an Excel file
saveWorkbook(wb, file_path)



