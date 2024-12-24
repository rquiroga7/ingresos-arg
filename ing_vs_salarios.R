# Cargar las bibliotecas necesarias
library(tidyverse)
library(scales)
library(ggplot2)
library(zoo)
library(lubridate)

# Leer el archivo CSV EPH
ing <- read_csv("IPCF2.csv")
sal <- read_csv("indice_salarios.csv")
sal$fecha<-as.Date(sal$periodo,format="%d/%m/%Y")
# Correct the date calculation
ing$fecha <- as.Date(paste0(ing$Year, "-", (ing$Quarter * 3) - 2, "-01"), format="%Y-%m-%d")


# Unir los dos conjuntos de datos
data <- merge(ing, sal, by = "fecha", all = TRUE) %>% select(-c(Year, Quarter, periodo))

#mutate a new IngLab_index column, which is the IngLab index divided by the first non-NA IngLab value and multiplied by 100
data <- data %>%
    mutate(IngLab_index = IngLab / first(na.omit(IngLab)) * 100)

#Now re-index the IS_total_registrado column by the 
data <- data %>% 
    mutate(IS_total_registrado_index = IS_total_registrado / first(na.omit(IS_total_registrado)) * 100)

# Pivot to long format
data_long <- data %>%
  pivot_longer(cols = -c(fecha), names_to = "name", values_to = "value")

#Now plot evolution of IngLab_index and IS_total_registrado_index
data_long %>%
  filter((name == "IngLab_index" & !is.na(value)) | name == "IS_total_registrado_index") %>%
  filter(fecha <= as.Date("2024-07-01")) %>%
  ggplot(aes(x = fecha, y = value, color = name)) +
  geom_line(size=1.2) +
  ylab("Índices de Salarios/Ingresos Laborales (IngLab)") +
  xlab("Fecha") +
  ggtitle("Índices de Salarios Registrados INDEC y de Ingresos Laborales (IngLab)") +
  theme_light(base_size=18) +
  #scale_y_continuous() +
  #use log y axis scale
  scale_y_log10() +
  #change legend title to "Leyenda" and legend labels to "Ingresos Laborales" and "Indice Salarial Total"
  labs(color = "Leyenda") +
  
  scale_x_date(breaks = seq(as.Date("2017-01-01"), max(data_long$fecha), by="3 months", limits=c(as.Date("2017-01-01"), max(data$fecha)), minor_breaks = NULL), date_labels = "%Y-%m") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        legend.position = "top",
        strip.text = element_text(size = 12, face = "bold")) +
  labs(caption = "Datos INDEC. Análisis y visualización por Rodrigo Quiroga. Ver github.com/rquiroga7/ingresos-arg")
ggsave("evolution.png", dpi = 300)

#Try to incorporate aguinaldo in IS_total_registrado_index
data <- data %>%
    mutate(IS_total_registrado_index_aguinaldo = IS_total_registrado_index + IS_total_registrado_index * 3.5/3))



#Calculate variation of IngLab_index and IS_total_registrado_index compared to -3
data_variation<- data %>%
    mutate(IngLab_index_variation = IngLab_index / lag(IngLab_index, 3),
           IS_total_registrado_index_variation = IS_total_registrado_index / lag(IS_total_registrado_index, 3)) %>%
    select(fecha, IngLab_index_variation, IS_total_registrado_index_variation) %>%
    pivot_longer(cols = -c(fecha), names_to = "name", values_to = "value")

# Plot
data_variation %>%
  filter(month(fecha) %in% c(1,4,7,10)) %>%
  filter((name == "IngLab_index_variation" & !is.na(value)) | name == "IS_total_registrado_index_variation") %>%
  ggplot(aes(x = fecha, y = value, color = name)) +
  geom_line(size=1.2) +
  ylab("Variación de índices de Salarios/Ingresos Laborales (IngLab)") +
  xlab("Fecha") +
  ggtitle("Variación de índices de Salarios Registrados INDEC y de Ingresos Laborales (IngLab)") +
  theme_light() +
  scale_y_continuous() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        legend.position = "none",
        strip.text = element_text(size = 12, face = "bold")) 



#Do long to wide transformation, data_long to data_wide
data_wide <- data_variation %>%
  pivot_wider(names_from = name, values_from = value)

data_wide <- data_wide %>%
  mutate(var = (IngLab_index_variation) / (IS_total_registrado_index_variation))

#summarize
data_wide %>%
  filter(month(fecha) %in% c(1,4,7,10)) %>%
  summarize(tot_var = mean(var, na.rm = TRUE))

#Plot var
# Create a sequence of dates for the months 1, 4, 7, and 10
date_breaks <- seq(as.Date("2017-01-01"), max(data_wide$fecha), by = "3 months")
date_breaks <- date_breaks[month(date_breaks) %in% c(1, 4, 7, 10)]

data_wide %>%
  filter(month(fecha) %in% c(1, 4, 7, 10)) %>%
  ggplot(aes(x = fecha, y = var / 1.02)) +
  geom_line(size = 1.2) +
  ylab("Variación (Ingresos / índice salarial)") +
  xlab("Trimestre") +
  ggtitle("Variación") +
  theme_light() +
  scale_y_continuous() +
  scale_x_date(breaks = date_breaks, date_labels = "%Y-%m", minor_breaks = NULL) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        legend.position = "none",
        strip.text = element_text(size = 12, face = "bold"))


filtered_data <- data_wide %>%
  filter(month(fecha) %in% c(7))

#for filtered data, fit a linear model, excluding the last data point
lm_fit <- lm(IngLab_index_variation ~ IS_total_registrado_index_variation, data = head(filtered_data, -1))

# Create the scatterplot
ggplot(filtered_data, aes(x = IS_total_registrado_index_variation, y = IngLab_index_variation, color = fecha)) +
  geom_point() +
  ylab("Variación de índices de Ingresos Laborales (IngLab)") +
  xlab("Variación de índices de Salarios Registrados INDEC") +
  ggtitle("Correlación entre variación de índices de Salarios Registrados INDEC y de Ingresos Laborales (IngLab)") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        strip.text = element_text(size = 12, face = "bold")) +
  scale_color_viridis_c(option = "plasma", begin = 0.9, end = 0.1) +
  # black line for y and x values = 1
  geom_hline(yintercept = 1, color = "black") +
  geom_vline(xintercept = 1, color = "black") +
  # red line for the linear model
  geom_abline(intercept = lm_fit$coefficients[1], slope = lm_fit$coefficients[2], color = "red") +
  geom_label(aes(label = fecha), nudge_x = 0.01, nudge_y = 0.01, size = 3) +
  labs(caption = "Datos INDEC. Análisis y visualización por Rodrigo Quiroga. Ver github.com/rquiroga7/ingresos-arg")



#Calculate variation of IngLab_index and IS_total_registrado_index compared to -6
data_variation<- data %>%
    mutate(IngLab_index_variation = IngLab_index / lag(IngLab_index, 6),
           IS_total_registrado_index_variation = IS_total_registrado_index / lag(IS_total_registrado_index, 6)) %>%
    select(fecha, IngLab_index_variation, IS_total_registrado_index_variation) %>%
    pivot_longer(cols = -c(fecha), names_to = "name", values_to = "value")

# Plot
data_variation %>%
filter(month(fecha) %in% c(1,4,7,10)) %>%
  filter((name == "IngLab_index_variation" & !is.na(value)) | name == "IS_total_registrado_index_variation") %>%
  ggplot(aes(x = fecha, y = value, color = name)) +
  geom_line(size=1.2) +
  ylab("Variación de índices de Salarios/Ingresos Laborales (IngLab)") +
  xlab("Fecha") +
  ggtitle("Variación de índices de Salarios Registrados INDEC y de Ingresos Laborales (IPCF)") +
  theme_light() +
  scale_y_continuous() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        legend.position = "top",
        strip.text = element_text(size = 12, face = "bold")) 
  