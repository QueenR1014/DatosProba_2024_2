#Visualización Hipótesis 5. A mayor estudio de la madre, más tardío es su primer hijo
library(ggplot2)
salud = read.csv("Salud/Salud.csv")
educacion = read.csv("Educacion/Educación.csv")

data_JOIN = merge(salud, educacion, by = c("DIRECTORIO","SECUENCIA_P","SECUENCIA_ENCUESTA"))
data = data.frame(
  primer_hijo = data_JOIN$P3335S1A1,
  max_nivel = data_JOIN$P8587
)
data = na.omit(data)

mean_values = tapply(data$primer_hijo, data$max_nivel, mean)

graph = data.frame(
  x = c("Ninguno", "Preescolar", "Básica Primaria (1º - 5º)", 
                       "Básica secundaria (6º--9º)", "Media (10º--13º)", 
                       "Técnico sin título", "Técnico con título", 
                       "Tecnológico sin título", "Tecnológico con título", 
                       "Universitario sin titulo", "Universitario con titulo", 
                       "Postgrado sin titulo", "Postgrado con titulo"),
  y = mean_values,
  order = c(1:13)
)

graph$x <- factor(graph$x, levels = graph$x[order(graph$order)])

library(ggplot2)

ggplot(graph, aes(x = x, y = y, fill = y)) +  # Map fill to 'y' (mean values)
  geom_bar(stat = "identity", color = "black") +  # Create bars with border color
  geom_text(aes(label = round(y, 2)),            # Add labels with rounded mean values
            angle = 45,                           # Rotate labels by 45 degrees
            vjust = 1,                            # Adjust vertical position
            hjust = 1,                            # Adjust horizontal position
            size = 3,                             # Text size
            color = "black") +                    # Text color
  scale_fill_gradient(low = "pink", high = "red") +  # Apply color gradient
  labs(title = "Promedio de Edad de primer hijo por Nivel de Educación", 
       x = "Nivel Educativo", 
       y = "Valor Medio") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))