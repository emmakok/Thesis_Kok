### Emma Kok
### Dit is een opzet van een fosfaat balans

# Clear environment and open the waterbalans
rm(list = ls())
source("C:/Users/Kok30/OneDrive - Waternet Amsterdam/Documents/Rstudio/waterbalans_versie_november2022.R")
rm(list=setdiff(ls(), "df.ts"))

df.ts <- df.ts[,c("datum", setdiff(names(df.ts),"datum"))] # date as column

api_token <- "73938989yQWarcwr2eOnBrTzQ88xPe4S"

area_balans <- "Polder Waardassacker en Holendrecht"

server <- "engine"

df.ts$temperatuur <- c(3.0,3.6,3.3,2.8,2.9,2.4,2.6,1.1,-0.6,1.6,5.0,5.2,4.3,1.2,1.2,0.8,3.4,4.8,7.7,9.0,7.0,4.0,3.2,2.4,2.3,4.5,4.7,5.1,7.4,0.4,
                       -1.5,1.0,5.0,8.6,6.6,6.8,2.6,-4.1,-5.6,-4.4,-3.7,-4.8,-5.8,-5.2,-0.6,2.9,7.3,9.5,8.1,7.1,11.7,10.7,12.2,13.4,13.7,11.4,
                       6.5,4.3,4.0,5.5,6.3,4.5,4.2,1.9,0.7,1.4,5.4,5.1,5.7,9.4,7.1,6.9,6.7,6.6,5.6,5.0,4.4,4.6,4.3,6.1,5.7,6.5,6.7,8.2,9.7,6.9,
                       9.7,12.3,11.3,13.8,9.6,5.3,7.2,6.3,4.2,2.5,3.4,5.5,7.8,5.0,4.2,3.5,4.2,4.5,5.3,6.5,7.1,8.4,8.8,10.9,8.1,6.7,7.1,7.6,7.1,
                       7.3,9.2,10.8,7.7,7.8,7.7,6.8,9.9,9.0,7.1,5.9,6.5,10.4,18.5,15.9,13.6,11.5,12.1,11.1,10.6,11.4,11.1,11.3,10.4,12.2,13.2,
                       10.5,11.7,11.2,10.2,10.8,10.5,12.6,12.5,14.9,16.7,18.5,21.0,20.7,20.0,15.7,16.5,16.8,19.1,19.0,19.3,18.6,16.7,16.6,20.3,
                       18.3,21.6,24.0,22.4,18.1,18.6,14.0,14.5,14.6,15.0,14.8,19.8,20.8,20.3,17.3,14.0,15.0,16.6,18.4,18.1,16.7,18.0,17.2,18.1,
                       17.6,18.4,18.8,19.7,18.8,18.8,17.8,18.0,20.0,20.2,17.4,17.2,19.4,18.3,17.0,18.2,19.7,18.3,17.6,17.6,17.3,17.0,16.4,16.0,
                       15.3,14.3,16.1,18.3,18.8,17.0,16.3,16.6,16.8,17.4,19.0,17.7,17.6,19.1,15.9,14.7,16.8,16.5,17.5,18.7,17.9,18.2,17.1,16.2,
                       16.3,15.8,17.0,16.2,17.6,16.3,16.5,16.3,16.4,15.4,15.6,16.5,18.3,19.4,19.7,19.5,18.3,15.9,14.5,17.3,16.8,16.2,15.9,15.3,
                       15.4,11.9,11.8,13.2,15.8,17.9,16.9,17.7,15.4,14.0,11.8,12.3,13.5,14.4,12.9,13.5,12.7,12.3,10.5,10.7,9.8,10.2,11.8,10.1,
                       10.1,13.7,10.5,10.3,11.3,11.5,16.3,15.6,10.0,8.4,7.3,7.9,10.7,12.6,13.4,11.4,13.1,12.0,11.5,9.4,7.0,5.0,5.8,7.7,9.7,9.9,
                       8.1,8.2,8.9,9.7,7.4,10.9,9.7,6.6,5.1,8.2,10.0,11.9,10.1,8.0,3.6,6.4,6.1,5.5,4.4,3.0,2.2,4.3,8.6,8.0,2.3,3.5,4.9,4.3,3.0,
                       4.0,5.1,3.6,3.0,4.3,8.9,9.7,9.0,9.8,6.5,7.7,6.2,6.8,3.5,-2.2,-2.4,2.3,7.8,-1.0,-1.0,5.6,8.8,9.5,12.9,12.8)

# Oppervlaktes ----------------------------------------------------------------
oppervlakte_totaal <- 6027093.304 #m2
oppervlakte_water <- 494326.951  #m2
oppervlakte_land <- oppervlakte_totaal - oppervlakte_water #m2

# Grondwater ------------------------------------------------------------------
data <- read.table("C:/Users/Kok30/OneDrive - Waternet Amsterdam/groundwaterlevel.txt", header = TRUE, sep = "\t")

tot_grondwater <- df.ts$`tot Grondwaterstand` / oppervlakte_totaal
wat_grondwater <- df.ts$`wat Grondwaterstand` / oppervlakte_water
land_grondwater<- df.ts$`lan Grondwaterstand` / oppervlakte_land

data_grondwater <- head(data, - 1)

                                                                                                      
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


# initial concentrations-------------------------------------------------------
df_PO <- data.frame(datum = df.ts$datum)

fosfaat_inlaat <- c("1" = 0.2, "2" = 0.2, "3" = 0.2, "4" = 0.2,
                    "5" = 0.1, "6" = 0.1, "7" = 0.1, "8" = 0.1,
                    "9" = 0.15, "10" = 0.15, "11" = 0.2, "12" = 0.2)


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

# Nalevering
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
measured_values <- data.frame(date = as.Date(c("2021-01-14", "2021-02-18", "2021-03-11", "2021-04-15",
                                               "2021-05-10", "2021-06-07", "2021-07-12", "2021-08-11",
                                               "2021-09-09", "2021-10-13", "2021-11-11", "2021-12-03")),
                              PO_mg_l = c(0.256,0.214,0.359,0.173,0.116,0.228,0.154,0.146,
                                          0.175,0.162,0.33,0.351))

# Plot sloot using ggplot
plot_PO <- ggplot(df_PO, aes(x = datum)) +
  geom_line(aes(y = `sloot [mg/l]`, color = "Berekende concentratie")) +
  geom_point(data = measured_values, aes(x = date, y = PO_mg_l, color = "Gemeten concentratie")) +
  labs(x = "Datum", y = "Concentratie fosfaat") +
  theme_minimal() +
  theme(axis.line = element_line(size = 0.8), 
        axis.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  ggtitle("PO concentratie in de Waardassacker in 2021") +
  scale_color_manual(name = "Legend",
                     values = c("Berekende concentratie" = "darkgreen", "Gemeten concentratie" = "darkorange"))

plot_PO

# Install the openxlsx package if not already installed
install.packages("openxlsx")

# Load the openxlsx package
library(openxlsx)

# Specify the file path where you want to save the Excel file
file_path <- "C:/Users/Kok30/OneDrive - Waternet Amsterdam/df_PO_2021_minderinlaat.xlsx"

# Create a new workbook
wb <- createWorkbook()

# Add the dataframe to the workbook
addWorksheet(wb, "Sheet1")
writeData(wb, "Sheet1", df_PO)

# Save the workbook as an Excel file
saveWorkbook(wb, file_path)





