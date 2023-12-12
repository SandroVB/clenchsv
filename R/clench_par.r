clench_par <- function (matriz){

acumulacion<-specaccum(matriz, #Base de datos
                        method = "exact")

puntos_acum <- data.frame(acumulacion$sites, #Jalamos solo los datos de sitios
                          acumulacion$richness) #Y los de riqueza estimada

#Escribimos ec de clench
modelo <- acumulacion.richness~(a*acumulacion.sites)/(1+b*acumulacion.sites)

#Estimamos a y b
clench <- summary(
  nls(formula = modelo, #Le decimos la formula del modelo
            data = puntos_acum, #Los datos a ajjustar
            star = list(a=50,b=1))
)



################## SALIDAS PREVIAS: ########################################

Riqueza <- max(puntos_acum$acumulacion.richness)
Parametro_a <- clench$parameters[1,1]
Parametro_b <- clench$parameters [2,1]
Asintota <- Parametro_a/Parametro_b
Completitud <- Riqueza/Asintota*100





################## SALIDA FINAL: ##########################################

list(

  #Parámetros:
  "Riqueza" = Riqueza ,
  "Parametro_a" = Parametro_a ,
  "Parametro_b" = Parametro_b ,
  "Asintota" = Asintota ,
  "Completitud" = Completitud ,
  #Matriz de S_est:
  "S_est" = puntos_acum
  )

}
