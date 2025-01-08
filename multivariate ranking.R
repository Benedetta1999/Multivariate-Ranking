library(readxl)    
library(dplyr)     
library(tidyr)     
library(ggplot2)   
library(scales)
library(plotly)

###SENZA PESI
eventi <- readxl::read_excel("C:/Users/Benedetta/OneDrive - Politecnico di Torino/Desktop/Ricerca/CUNEO/Eventi Calso/Eventi Calso - paretofront.xlsx")

eventi <- eventi %>%
  mutate(
    Severity = -Severity,   
    Intensità = -Intensità
  )


# Normalizza Durata e Severity (scaling tra 0 e 1)
eventi <- eventi %>%
  mutate(
    Intensità_norm = rescale(Intensità, to = c(0, 1)),
    Severità_norm = rescale(Severity, to = c(0, 1)),
    Durata_norm = rescale(Durata, to = c(0, 1))
  )

# Somma dei valori normalizzati di Durata e Severity per ottenere un punteggio
eventi <- eventi %>%
  mutate(
    Punteggio1 = Intensità_norm + Durata_norm,
    Punteggio2 = Intensità_norm + Severità_norm,
    Punteggio3 = Severità_norm + Durata_norm
  )

# Ordina gli eventi in base al punteggio (dal più alto al più basso) e crea il ranking
eventi <- eventi %>%
  arrange(desc(Punteggio3)) %>%
  mutate(Rank = row_number())

# Converto le date da stringa a formato Date (assumendo che siano nel formato 'YYYY-MM')
eventi$Data_di_Inizio <- as.Date(paste(eventi$Data_di_Inizio, "-01", sep=""), format="%Y-%m-%d")
eventi$Data_di_Fine <- as.Date(paste(eventi$Data_di_Fine, "-01", sep=""), format="%Y-%m-%d")

# Ora posso creare l'intervallo di date
eventi$Date_Range <- paste(format(eventi$Data_di_Inizio, "%b %Y"), 
                           "-", 
                           format(eventi$Data_di_Fine, "%b %Y"))

# Aggiungi una colonna per identificare i primi 8 eventi nel ranking
eventi$Top8 <- ifelse(eventi$Rank <= 8, "Top 8", "Other")

# top_8_eventi_durata_intensità <- eventi %>%
#   filter(Rank <= 8)

# top_8_eventi_severità_intensità <- eventi %>%
#   filter(Rank <= 8)

top_8_eventi_durata_severità <- eventi %>%
  filter(Rank <= 8)

# Crea il grafico interattivo
fig <- plot_ly(eventi, 
               x = ~Intensità, 
               y = ~Durata, 
               type = 'scatter', 
               mode = 'markers', 
               color = ~Top8, 
               colors = c("Top 8" = "red", "Other" = "gray"),
               text = ~paste("Evento: ", Evento, "<br>Data Inizio: ", Date_Range), # Testo per tooltip
               hoverinfo = 'text',  
               marker = list(size = 8)) %>%
  layout(title = "Grafico Interattivo: Intensità vs Durata degli Eventi",
         xaxis = list(title = "Intensità"),
         yaxis = list(title = "Durata"))

fig

# Salva il grafico interattivo come file HTML
htmlwidgets::saveWidget(fig, "C:/Users/Benedetta/OneDrive - Politecnico di Torino/Desktop/grafico_eventi_interattivo-durata-intensità.html")


########CON PCA
 # Imposta il percorso del file
# file_path <- "C:/Users/Benedetta/OneDrive - Politecnico di Torino/Desktop/Ricerca/CUNEO/Eventi Calso/Eventi Calso - paretofront.xlsx"
# 
# # Leggi i dati dal file Excel
# eventi <- read_excel(file_path)
# 
# # Controlla i nomi delle colonne
# head(eventi)
# 
# # Assicurati che le colonne Durata e Severity esistano e correggi i nomi se necessario
# colnames(eventi) <- c("Evento", "Data_di_Inizio", "Data_di_Fine", "Durata", "Severity", "Intensità")
# 
# # Inverti il segno della colonna Severity (poiché va massimizzata)
# eventi <- eventi %>%
#   mutate(Severity = -Severity)
# 
# # Normalizza le colonne Durata e Severity (scaling tra 0 e 1)
# eventi <- eventi %>%
#   mutate(
#     Durata_norm = rescale(Durata, to = c(0, 1)),
#     Severity_norm = rescale(Severity, to = c(0, 1))
#   )
# 
# # Combina le colonne normalizzate in un formato per la PCA
# pca_data <- eventi %>%
#   select(Durata_norm, Severity_norm)
# 
# # Applica la PCA
# pca_model <- prcomp(pca_data, scale. = FALSE)  # Scale già fatto prima
# 
# # Estrai i pesi della prima componente principale (PC1)
# pesi <- pca_model$rotation[, 1]
# peso_durata <- pesi[1]
# peso_severity <- pesi[2]
# 
# # Calcola il punteggio pesato (PC1 come somma pesata delle variabili)
# eventi <- eventi %>%
#   mutate(
#     Punteggio = peso_durata * Durata_norm + peso_severity * Severity_norm
#   )
# 
# # Classifica gli eventi in base al punteggio (Ranking)
# eventi <- eventi %>%
#   arrange(desc(Punteggio)) %>%
#   mutate(Rank = row_number())
# 
# # Mostra i risultati
# print(eventi)
# 
# # Salva il ranking in un nuovo file Excel
# write.xlsx(eventi, "C:/Users/Benedetta/OneDrive - Politecnico di Torino/Desktop/Ricerca/CUNEO/Eventi Calso/Eventi_Calso_Ranking.xlsx")
# 
# 
# # Calcolare i punteggi PC1 per ogni evento
# eventi <- eventi %>%
#   mutate(PC1 = peso_durata * Durata_norm + peso_severity * Severity_norm)
# 
# # Crea il grafico di dispersione tra Durata e -Severity
# ggplot(eventi, aes(x = Durata, y = -Severity, color = Evento)) +
#   geom_point() +  # Aggiungi i punti
#   labs(
#     title = "Diagramma di dispersione tra Durata e Severità (invertita) degli eventi",
#     x = "Durata",
#     y = "Severità (invertita)",
#     color = "Numero dell'evento"
#   ) +
#   theme_minimal()





