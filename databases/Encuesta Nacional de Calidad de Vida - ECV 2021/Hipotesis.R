#Visualización Hipótesis 3. A mayor estudio de la madre, más tardío es su primer hijo
library(ggplot2)
salud = read.csv("Salud/Salud.csv")
educacion = read.csv("Educacion/Educación.csv")

data_JOIN = merge(salud, educacion, by = c("DIRECTORIO","SECUENCIA_P","SECUENCIA_ENCUESTA"))
data = data.frame(
  primer_hijo = data_JOIN$P3335S1A1,
  max_nivel = data_JOIN$P8587,
  hijos_nacidos = data_JOIN$P3335S1
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


#Visualización 1: Diagrama de dispersión de nivel educativo de las madres vs el número de hijos que tienen
ggplot(data, aes(x = max_nivel, y = hijos_nacidos))+
  geom_point(color = "blue", size = 1)+
  geom_smooth(method = "lm", se = TRUE,color = "red",size = 1)+
  labs(title = "Nivel de estudio vs cantidad de hijos nacidos",
       x = "Nivel Educativo",
       y = "Cantidad de Hijos nacidos")+
  theme_minimal()

#Hipótesis 2: Prueba de diferencia de proporciones (De las madres que tienen hijos estudiando, 
#son más las que también ellas estudiaron que las que no)
atencion = read.csv("atencion/Atencion integral de los niños y niñas menores de 5 años.csv", sep = ";")
#pagó matricula para el niño: P6169
hijo_educado  = merge(atencion, salud, by =c("DIRECTORIO","SECUENCIA_P","SECUENCIA_ENCUESTA"))
data_h2 = merge(hijo_educado,educacion, by = c("DIRECTORIO","SECUENCIA_P"))
hijo_educado_filter = data.frame(
  estudiando = data_h2$P6169,
  max_nivel = data_h2$P8587,
  hijos_nacidos = data_h2$P3335S1
)
hijo_educado_filter = na.omit(hijo_educado_filter)