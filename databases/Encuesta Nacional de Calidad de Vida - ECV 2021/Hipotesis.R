#Visualización Hipótesis 3. A mayor estudio de la madre, más tardío es su primer hijo

library(ggplot2)
library(dplyr)
salud = read.csv("Salud/Salud.csv")
educacion = read.csv("Educacion/Educación.csv")
caracteristicas = read.csv("Caracteristicas/Caracteristicas y composicion del hogar.csv", sep= ";")

data_JOIN = merge(salud, educacion, by = c("DIRECTORIO","SECUENCIA_P","SECUENCIA_ENCUESTA"), all = TRUE)
data_JOIN = merge(data_JOIN, caracteristicas, by = c("DIRECTORIO","SECUENCIA_P","SECUENCIA_ENCUESTA"), all = TRUE)



data = data.frame(
  DIRECTORIO = data_JOIN$DIRECTORIO,
  SECUENCIA_P = data_JOIN$SECUENCIA_P,
  SECUENCIA_ENCUESTA = data_JOIN$SECUENCIA_ENCUESTA, 
  estudia = data_JOIN$P8586,
  nivel_actual_estudio = data_JOIN$P1088,
  primer_hijo = data_JOIN$P3335S1A1,
  max_nivel = data_JOIN$P8587,
  hijos_nacidos = data_JOIN$P3335S1, 
  sexo = data_JOIN$P6020,
  edad = data_JOIN$P6040
)

data <- data %>%
  filter(edad > 10, edad < 50, sexo == 2, hijos_nacidos>=1) #Solo nos quedamos con mujeres madres entre 10 y 49 años

data <- data %>%
  mutate(educ = case_when(
    nivel_actual_estudio==1  ~ 2,        # Si old_var es menor a 10, new_var será 1
    nivel_actual_estudio==2  ~ 3,
    nivel_actual_estudio==3  ~ 4,
    nivel_actual_estudio==4  ~ 5,
    nivel_actual_estudio==5  ~ 6,
    nivel_actual_estudio==6  ~ 8,
    nivel_actual_estudio==7  ~ 10,
    nivel_actual_estudio==8  ~ 12
    
  ))

data <- data %>%
  mutate(educ_nivel = coalesce(educ, max_nivel))

# Calcular la media agrupada por nivel educativo
grouped_means <- tapply(data$primer_hijo, data$educ_nivel, mean, na.rm = TRUE)
print(grouped_means)


graph = data.frame(
  x = c("Ninguno", "Preescolar", "Básica Primaria (1º - 5º)", 
        "Básica secundaria (6º--9º)", "Media (10º--13º)", 
        "Técnico sin título", "Técnico con título", 
        "Tecnológico sin título", "Tecnológico con título", 
        "Universitario sin titulo", "Universitario con titulo", 
        "Postgrado sin titulo", "Postgrado con titulo"),
  y = grouped_means,
  order = c(1:13)
)

graph$x <- factor(graph$x, levels = graph$x[order(graph$order)])

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
ggplot(data, aes(x = educ_nivel, y = hijos_nacidos))+
  geom_point(color = "blue", size = 1)+
  geom_smooth(method = "lm", se = TRUE,color = "red",size = 1)+
  labs(title = "Nivel de estudio vs cantidad de hijos nacidos",
       x = "Nivel Educativo",
       y = "Cantidad de Hijos nacidos")+
  theme_minimal()

data$educ_nivel <- as.factor(data$educ_nivel)
reg1 <- lm(hijos_nacidos ~ educ_nivel, data = data)
summary(reg1)

#Hipótesis 2: Prueba de diferencia de proporciones (De las madres que tienen hijos estudiando, 
#son más las que también ellas estudiaron que las que no)
atencion = read.csv("atencion/Atencion integral de los niños y niñas menores de 5 años.csv", sep = ";")

#pagó matricula para el niño: P6169
hijo_educado  = merge(atencion, salud, by =c("DIRECTORIO","SECUENCIA_P","SECUENCIA_ENCUESTA"), all = TRUE)
data_h2 = merge(hijo_educado, educacion, by = c("DIRECTORIO","SECUENCIA_P","SECUENCIA_ENCUESTA"), all = TRUE)

hijo_educado_filter = data.frame(
  DIRECTORIO = data_JOIN$DIRECTORIO,
  SECUENCIA_P = data_JOIN$SECUENCIA_P,
  SECUENCIA_ENCUESTA = data_JOIN$SECUENCIA_ENCUESTA, 
  estudiando = data_h2$P6169,
  max_nivel = data_h2$P8587,
  hijos_nacidos = data_h2$P3335S1,
  sexo = data_JOIN$P6020,
  edad = data_JOIN$P6040
)

hijo_estudiando = subset(hijo_educado_filter, estudiando == 1 & sexo == 2)

estudio = subset(hijo_estudiando, max_nivel > 5)
prop_estudiadas = length(estudio$estudiando)

no_estudio = subset(hijo_estudiando, max_nivel <= 5)
prop_no_estudiadas = length(hijo_no_estudiadas$estudiando)

