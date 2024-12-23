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
ing$fecha <- ing$fecha %m+% months(-1)

# Unir los dos conjuntos de datos
data <- merge(ing, sal, by = "fecha", all = TRUE) %>% select(-c(Year, Quarter, periodo))

#mutate a new IPCF_index column, which is the IPCF index divided by the first non-NA IPCF value and multiplied by 100
data <- data %>%
    mutate(IPCF_index = IPCF / first(na.omit(IPCF)) * 100)

#Now re-index the IS_total_registrado column by the 
data <- data %>% 
    mutate(IS_total_registrado_index = IS_total_registrado / first(na.omit(IS_total_registrado)) * 100)

# Pivot to long format
data_long <- data %>%
  pivot_longer(cols = -c(fecha), names_to = "name", values_to = "value")

#Now plot evolution of IPCF_index and IS_total_registrado_index
data_long %>%
  filter((name == "IPCF_index" & !is.na(value)) | name == "IS_total_registrado_index") %>%
  ggplot(aes(x = fecha, y = value, color = name)) +
  geom_line(size=1.2) +
  ylab("Índices de Salarios/Ingresos per capita familiar (IPCF)") +
  xlab("Fecha") +
  ggtitle("Índices de Salarios Registrados INDEC y de Ingresos per capita familiar (IPCF)") +
  theme_light() +
  scale_y_continuous() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        legend.position = "none",
        strip.text = element_text(size = 12, face = "bold")) +
  labs(caption = "Datos INDEC. Análisis y visualización por Rodrigo Quiroga. Ver github.com/rquiroga7/PIB-Argentina")

ggsave("evolution.png", dpi = 300)


#Calculate variation of IPCF_index and IS_total_registrado_index compared to -3
data_variation<- data %>%
    mutate(IPCF_index_variation = IPCF_index / lag(IPCF_index, 3),
           IS_total_registrado_index_variation = IS_total_registrado_index / lag(IS_total_registrado_index, 3)) %>%
    select(fecha, IPCF_index_variation, IS_total_registrado_index_variation) %>%
    pivot_longer(cols = -c(fecha), names_to = "name", values_to = "value")

# Plot
data_variation %>%
  filter((name == "IPCF_index_variation" & !is.na(value)) | name == "IS_total_registrado_index_variation") %>%
  ggplot(aes(x = fecha, y = value, color = name)) +
  geom_line(size=1.2) +
  geom_smooth(data = data_variation %>% filter(name == "IPCF_index_variation" & !is.na(value)), 
              span =0.2, method = "loess", se = FALSE, color = "blue") +
  ylab("Variación de índices de Salarios/Ingresos per capita familiar (IPCF)") +
  xlab("Fecha") +
  ggtitle("Variación de índices de Salarios Registrados INDEC y de Ingresos per capita familiar (IPCF)") +
  theme_light() +
  scale_y_continuous() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        legend.position = "none",
        strip.text = element_text(size = 12, face = "bold")) 
  

#Calculate variation of IPCF_index and IS_total_registrado_index compared to -6
data_variation<- data %>%
    mutate(IPCF_index_variation = IPCF_index / lag(IPCF_index, 6),
           IS_total_registrado_index_variation = IS_total_registrado_index / lag(IS_total_registrado_index, 6)) %>%
    select(fecha, IPCF_index_variation, IS_total_registrado_index_variation) %>%
    pivot_longer(cols = -c(fecha), names_to = "name", values_to = "value")

# Plot
data_variation %>%
filter(month(fecha) %in% c(1,4,7,10)) %>%
  filter((name == "IPCF_index_variation" & !is.na(value)) | name == "IS_total_registrado_index_variation") %>%
  ggplot(aes(x = fecha, y = value, color = name)) +
  geom_line(size=1.2) +
  ylab("Variación de índices de Salarios/Ingresos per capita familiar (IPCF)") +
  xlab("Fecha") +
  ggtitle("Variación de índices de Salarios Registrados INDEC y de Ingresos per capita familiar (IPCF)") +
  theme_light() +
  scale_y_continuous() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        legend.position = "top",
        strip.text = element_text(size = 12, face = "bold")) 
  