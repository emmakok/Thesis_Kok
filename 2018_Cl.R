### Emma Kok

# clear environment and open the waterbalans
rm(list = ls())
source("C:/Users/Kok30/OneDrive - Waternet Amsterdam/Documents/Rstudio/waterbalans_versie_november2022.R")
rm(list=setdiff(ls(), "df.ts"))

df.ts <- df.ts[,c("datum", setdiff(names(df.ts),"datum"))] # date as column

api_token <- "01694069JSUn7fZlaDtwJSReLiSd37PV"

area_balans <- "Polder Waardassacker en Holendrecht"

server <- "engine"

# Oppervlaktes ----------------------------------------------------------------
oppervlakte_totaal <- 6027093.304 #m2
oppervlakte_water <- 494326.951  #m2
oppervlakte_land <- oppervlakte_totaal - oppervlakte_water #m2

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




# initial concentrations-------------------------------------------------------
df_Cl <- data.frame(datum = df.ts$datum)

chloride_inlaat <- c("1" = 260, "2" = 90, "3" = 250, "4" = 400,
                     "5" = 500, "6" = 620, "7" = 580, "8" = 500,
                     "9" = 580, "10" = 540, "11" = 450, "12" = 300)


df_Cl$month <- month(as.Date(df_Cl$datum))

chloride_neerslag <- 0 #mg/l
chloride_initial_value <- 127 #mg/l
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
max_value <- 45
mid_value <- 30
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
df.ts$wegzijging <- ifelse(df.ts$`wat Grondwater kwel` > 0, 0, -df.ts$`wat Grondwater kwel`)
df.ts$`wat drainage + afspoeling` <- df.ts$`wat berging opp [m3]`- df.ts$`wat neerslag in m3` + df.ts$`wat verdamping in m3` - df.ts$wegzijging - df.ts$inlaat + df.ts$`uitlaat`
df.ts$`wat intrek` <- ifelse(df.ts$`wat drainage + afspoeling`<0, -df.ts$`wat drainage + afspoeling`,0)

df_Cl$`wat drainage en afspoeling chloride [g]` <- df_Cl$`drainage chloride`* df.ts$`wat drainage + afspoeling`
df_Cl$`in chloride` <- df_Cl$`wat chloride neerslag [g]` + df_Cl$`inlaat chloride [g]` + ifelse(is.na(df_Cl$`wat drainage en afspoeling chloride [g]`), 0, df_Cl$`wat drainage en afspoeling chloride [g]`)

# OUTPUT chloride ------------------------------------------------------------
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

# FIXED VALUE DRAINAGE -------------------------------------------------------
# Assign the scaled values to 'drainage chloride' variable in 'df_Cl' dataframe
df_Cl$`drainage chloride fixed` <- 25

df.ts$wegzijging <- ifelse(df.ts$`wat Grondwater kwel` > 0, 0, -df.ts$`wat Grondwater kwel`)
df.ts$`wat drainage + afspoeling` <- df.ts$`wat berging opp [m3]`- df.ts$`wat neerslag in m3` + df.ts$`wat verdamping in m3` - df.ts$wegzijging - df.ts$inlaat + df.ts$`uitlaat`
df.ts$`wat intrek` <- ifelse(df.ts$`wat drainage + afspoeling`<0, -df.ts$`wat drainage + afspoeling`,0)

df_Cl$drain_afs_fixed <- df_Cl$`drainage chloride fixed`* df.ts$`wat drainage + afspoeling`
df_Cl$in_fixed <- df_Cl$`wat chloride neerslag [g]` + df_Cl$`inlaat chloride [g]` + ifelse(is.na(df_Cl$drain_afs_fixed), 0, df_Cl$drain_afs_fixed)

# gemaal, intrek, wegzijging
df_Cl$gemaal_fixed [1] <- df.ts$uitlaat[1]* chloride_initial_value
df_Cl$intrek_fixed [1] <- NA

df_Cl$uit_fixed [1] <- df_Cl$gemaal_fixed [1]

# calculate berging sloot [g]
df_Cl$berging_fixed [1] <- df.ts$`wat Oppervlak laatste waarde`[1] * chloride_initial_value

# calculate sloot [mg/l]
df_Cl$sloot_fixed [1] <- chloride_initial_value

df_Cl$`wegzijging factor` <- ifelse(df.ts$wegzijging > 0, 1, 0)
df_Cl$`wegzijging factor`[is.na(df_Cl$`wegzijging factor`)] <- 0

df.ts$wegzijging[is.na(df.ts$wegzijging)] <- 0

# calculate delta berging sloot [g]
df_Cl$delta_fixed [1] <- NA
df_Cl$wegzijging_fixed [1] <- 0

for (i in 2:nrow(df_Cl)) {
  df_Cl$berging_fixed [i] <- df_Cl$berging_fixed [i-1] + df_Cl$in_fixed [i-1] - df_Cl$uit_fixed [i-1]
  df_Cl$sloot_fixed [i] <- df_Cl$berging_fixed [i] / df.ts$`wat Oppervlak laatste waarde`[i]
  df_Cl$gemaal_fixed [i] <- df.ts$`uitlaat`[i] * df_Cl$sloot_fixed [i]
  df_Cl$wegzijging_fixed [i] <- df_Cl$`wegzijging factor`[i]*df.ts$wegzijging[i]* df_Cl$sloot_fixed [i]
  df_Cl$intrek_fixed [i] <- df.ts$`wat intrek`[i] * df_Cl$sloot_fixed [i]
  df_Cl$uit_fixed [i] <- df_Cl$gemaal_fixed [i] + df_Cl$intrek_fixed [i] + df_Cl$wegzijging_fixed [i]
  df_Cl$delta_fixed [i] <- df_Cl$berging_fixed [i] - df_Cl$berging_fixed [i-1]
}

# Values other way around ---------------------------------------------------
# Input drainage en afspoeling
df.ts$Grondw_pos <- abs(df.ts$`Grondwaterstand in m`)

min_value1 <- 0
max_value1 <- 45
mid_value1 <- 30
min_gws1 <- min(df.ts$Grondw_pos)
max_gws1 <- max(df.ts$Grondw_pos)

turned_value <- min_value1 + ((max_value1 - min_value1) / (max_gws1 - min_gws1)) * (df.ts$Grondw_pos - min_gws1)
turned_value <- pmax(pmin(turned_value, max_value1), min_value1)  # Limit values to the desired range

# Assign the scaled values to 'drainage chloride' variable in 'df_Cl' dataframe
df_Cl$`drainage chloride turned` <- turned_value

df.ts$wegzijging <- ifelse(df.ts$`wat Grondwater kwel` > 0, 0, -df.ts$`wat Grondwater kwel`)
df.ts$`wat drainage + afspoeling` <- df.ts$`wat berging opp [m3]`- df.ts$`wat neerslag in m3` + df.ts$`wat verdamping in m3` - df.ts$wegzijging - df.ts$inlaat + df.ts$`uitlaat`
df.ts$`wat intrek` <- ifelse(df.ts$`wat drainage + afspoeling`<0, -df.ts$`wat drainage + afspoeling`,0)

df_Cl$drain_afs_turned <- df_Cl$`drainage chloride turned`* df.ts$`wat drainage + afspoeling`
df_Cl$in_turned <- df_Cl$`wat chloride neerslag [g]` + df_Cl$`inlaat chloride [g]` + ifelse(is.na(df_Cl$drain_afs_turned), 0, df_Cl$drain_afs_turned)

# gemaal, intrek, wegzijging
df_Cl$gemaal_turned [1] <- df.ts$uitlaat[1]* chloride_initial_value
df_Cl$intrek_turned [1] <- NA

df_Cl$uit_turned [1] <- df_Cl$gemaal_turned [1]

# calculate berging sloot [g]
df_Cl$berging_turned [1] <- df.ts$`wat Oppervlak laatste waarde`[1] * chloride_initial_value

# calculate sloot [mg/l]
df_Cl$sloot_turned [1] <- chloride_initial_value

df_Cl$`wegzijging factor` <- ifelse(df.ts$wegzijging > 0, 1, 0)
df_Cl$`wegzijging factor`[is.na(df_Cl$`wegzijging factor`)] <- 0

df.ts$wegzijging[is.na(df.ts$wegzijging)] <- 0

# calculate delta berging sloot [g]
df_Cl$delta_turned [1] <- NA
df_Cl$wegzijging_turned [1] <- 0

for (i in 2:nrow(df_Cl)) {
  df_Cl$berging_turned [i] <- df_Cl$berging_turned [i-1] + df_Cl$in_turned [i-1] - df_Cl$uit_turned [i-1]
  df_Cl$sloot_turned [i] <- df_Cl$berging_turned [i] / df.ts$`wat Oppervlak laatste waarde`[i]
  df_Cl$gemaal_turned [i] <- df.ts$`uitlaat`[i] * df_Cl$sloot_turned [i]
  df_Cl$wegzijging_turned [i] <- df_Cl$`wegzijging factor`[i]*df.ts$wegzijging[i]* df_Cl$sloot_turned [i]
  df_Cl$intrek_turned [i] <- df.ts$`wat intrek`[i] * df_Cl$sloot_turned [i]
  df_Cl$uit_turned [i] <- df_Cl$gemaal_turned [i] + df_Cl$intrek_turned [i] + df_Cl$wegzijging_turned [i]
  df_Cl$delta_turned [i] <- df_Cl$berging_turned [i] - df_Cl$berging_turned [i-1]
}


# Plot -----------------------------------------------------------------------
df_Cl$datum <- as.Date(df_Cl$datum, format = "%d-%m-%Y")

# Create a dataframe of the measured values
measured_values <- data.frame(date = as.Date(c("2018-01-16", "2018-03-22", "2018-04-25", "2018-05-30",
                                              "2018-06-27", "2018-07-19", "2018-08-27", "2018-09-27",
                                              "2018-10-25", "2018-11-26")),
                              Cl_mg_l = c(115.637, 1115.148, 192.836, 376.408, 459.592, 657.127, 312.596, 
                                          421.429, 442.188, 263.684))
  
plot_Cl <- ggplot(df_Cl, aes(x = datum)) +
  geom_point(data = measured_values, aes(x = date, y = Cl_mg_l, color = "Measured concentration")) +
  geom_line(aes(y = `sloot [mg/l]`, color = "Calculated concentration")) +
  geom_line(aes(y = sloot_fixed, color = "Fixed drainage"), linetype = "dashed") +
  geom_line(aes(y = sloot_turned, color = "Turned drainage"), linetype = "dashed") +
  labs(x = "Date", y = "Concentration chloride [mg/l]") +
  theme_minimal() +
  theme(axis.line = element_line(size = 0.8), 
        axis.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5)) +  # Set hjust to 0.5 for center alignment
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  ggtitle("Cl concentration in the Waardassacker with a higher groundwaterlevel") +
  scale_color_manual(name = "Legend",
                     values = c("Measured concentration" = "red", "Calculated concentration" = "brown", "Fixed drainage" = "darkblue", "Turned drainage" = "darkgrey")) 

plot_Cl + ylim(50, 800)


# Install the openxlsx package if not already installed
install.packages("openxlsx")

# Load the openxlsx package
library(openxlsx)

# Specify the file path where you want to save the Excel file
file_path <- "C:/Users/Kok30/OneDrive - Waternet Amsterdam/df_Cl2018_norm_minderinlaat.xlsx"

# Create a new workbook
wb <- createWorkbook()

# Add the dataframe to the workbook
addWorksheet(wb, "Sheet1")
writeData(wb, "Sheet1", df_Cl)

# Save the workbook as an Excel file
saveWorkbook(wb, file_path)


# fracties ------------------------------------------
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


remove.packages(c("ggplot2"))
install.packages('ggplot2')
