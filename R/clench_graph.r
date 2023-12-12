clench_graph <- function (clench_sal){

ggplot()+

  xlim(0,clench_sal$Riqueza*3) +   #Rango de X e y
  ylim(0, clench_sal$Asintota) +

  labs(x="Esfuerzo de muestreo", y="Riqueza") + #Leyendas


  #1er elemento a graficar -> CLENCH
  geom_function(
     fun =~ (clench_sal$Parametro_a * .x) / (1 + clench_sal$Parametro_b * .x),
     aes(color = "Riq. Esperada"),
     size = 1
  )+


  #2do elemento a graficar -> Riq. OBSERVADA
  geom_point(
    data = clench_sal$S_est, #Data frame que graficará
    mapping = aes(x=acumulacion.sites, y= acumulacion.richness,
                  #Definir del DF, cuales serán las "X" y cuales las "Y"
                  color = "Riq. Observada"),
    shape = 21, fill = NA
  )+

  scale_colour_manual(values = c("orange","blue"))   +

  theme(legend.title = element_blank(), legend.position = "top")

}
