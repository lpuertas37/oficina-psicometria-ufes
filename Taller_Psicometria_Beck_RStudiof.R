# TALLER DE PSICOMETRIA EN RSTUDIO
# Depresion e invariancia Brasil-Ecuador | N = 169

# 0 instalar paquetes
# Paquetes: install.packages(c("psych", "lavaan", "semTools"))

install.packages("psych")
install.packages("lavaan")
install.packages("semTools")

library(psych)
library(lavaan)
library(semTools)

# 1. CONOCER LA BASE
# Si beck ya esta cargada en R, omitir la siguiente linea.
beck$pais = factor(beck$pais, levels = c("BR", "EC"))
de = beck[7:27]
an =beck[28:47]

dim(beck)                     # Personas y variables
names(de)                    # Items de depresion
table(beck$pais)             # Participantes por pais
summary(beck$edad)           # Edad
hist(beck$edad)

# 2. CONFIABILIDAD
psych::alpha(de)             # Alfa de Cronbach
# Confiabilidade por duas metades
splitHalf(de)

# Estatísticas descritivas
describe(de)

# Correlações policóricas
poly = polychoric(de)
poly$rho

# Alfa baseado nas correlações policóricas
alpha(poly$rho, n.obs = 169, check.keys = FALSE)

# Ômega total, assumindo um fator
omega(poly$rho)
# Revisar la advertencia del item de5 antes de decidir cambios.


#2.- convergencia discriminancia coreelación 


puntaje_de = rowSums(de) 
puntaje_an = rowSums(an)

cor.test(puntaje_de, puntaje_an, method = "spearman", exact = FALSE)


# 3. AFC ORIGINAL: 21 ITEMS
# =~ indica que los items miden el factor.
modelo1 = '
Depresion =~ de1 + de2 + de3 + de4 + de5 + de6 + de7 +
             de8 + de9 + de10 + de11 + de12 + de13 + de14 +
             de15 + de16 + de17 + de18 + de19 + de20 + de21
             
'
fit1 = cfa(modelo1, data = beck,
           ordered = names(de), estimator = "WLSMV")

summary(fit1, fit.measures = TRUE, standardized = TRUE)
# Las cargas indican cuanto se relaciona cada item con el factor.
round(inspect(fit1, "std.all")$lambda, 2)
# Investigar de5 y de19: contenido, codificacion y cargas.

# 4. SEGUNDO AFC: 19 ITEMS
# Exclusion exploratoria de de5 y de19.
modelo2 = '
Depresion =~ de1 + de2 + de3 + de4 + de6 + de7 +
             de8 + de9 + de10 + de11 + de12 + de13 +
             de14 + de15 + de16 + de17 + de18 + de20 + de21
'
fit2 = cfa(modelo2, data = beck,
           ordered = names(de), estimator = "WLSMV")
summary(fit2, fit.measures = T, standardized = T, modindices = T)
# 5. REESPECIFICACION
# Los indices de modificacion proponen posibles mejoras.
modificationIndices(fit2, sort. = TRUE, maximum.number = 5)

# ~~ permite covarianza residual entre dos items.
# Esta decision requiere justificacion teorica.
modelo3 = '
Depresion =~ de1 + de2 + de3 + de4 + de6 + de7 +
             de8 + de9 + de10 + de11 + de12 + de13 +
             de14 + de15 + de16 + de17 + de18 + de20 + de21
             de18 ~~ de21
'
fit3 = cfa(modelo3, data = beck,
           ordered = names(de), estimator = "WLSMV")
summary(fit3, fit.measures = T, standardized = T, modindices = T)




# 6. COMPARAR LOS TRES AFC
indices = c("cfi.scaled", "tli.scaled", "rmsea.scaled", "srmr")
resultado_afc = rbind(
  Original = fitMeasures(fit1, indices),
  Sin_2_items = fitMeasures(fit2, indices),
  Reespecificado = fitMeasures(fit3, indices)
)
round(resultado_afc, 3)

# Precaucion: los indices robustos del modelo final no son favorables.
round(fitMeasures(fit3, c("cfi.robust", "rmsea.robust")), 3)

#convergencia y discriminacnia
# AVE y fiabilidad compuesta de depresión
AVE(fit3)
compRelSEM(fit3)




modelo_ansiedad = paste("Ansiedad =~", paste(names(an), collapse = " + "))
modelo_da = paste(modelo3, modelo_ansiedad, sep = "\n")

# HTMT entre depresión y ansiedad
items = setdiff(names(de), c("de5", "de19"))

htmt(modelo_da, data = beck,
     ordered = c(items, names(an)),
     htmt2 = FALSE)


# 7. INVARIANZA BRASIL-ECUADOR
# Se agrupan categorias vacias en una copia, solo para esta practica.
beck_inv = beck
beck_inv$de1[beck_inv$de1 == 3] = 2
beck_inv$de15[beck_inv$de15 == 3] = 2

# Configural: misma estructura en ambos paises.
m0 = measEq.syntax(modelo3, data = beck_inv,
                   group = "pais", ordered = items,
                   parameterization = "theta", return.fit = TRUE)

# Umbrales: equivalencia de categorias de respuesta.
m1 = measEq.syntax(modelo3, data = beck_inv,
                   group = "pais", ordered = items,
                   parameterization = "theta",
                   group.equal = "thresholds", return.fit = TRUE)

# Umbrales y cargas: equivalencia de los indicadores.
m2 = measEq.syntax(modelo3, data = beck_inv,
                   group = "pais", ordered = items,
                   parameterization = "theta",
                   group.equal = c("thresholds", "loadings"),
                   return.fit = TRUE)

# Comprobar convergencia.
lavInspect(m0, "converged")
lavInspect(m1, "converged")
lavInspect(m2, "converged")

# Comparar el ajuste y los cambios en CFI.
indices_inv = c("cfi.scaled", "rmsea.scaled", "srmr")
resultado_inv = rbind(
  Configural = fitMeasures(m0, indices_inv),
  Umbrales = fitMeasures(m1, indices_inv),
  Cargas = fitMeasures(m2, indices_inv)
)
round(resultado_inv, 3)
round(diff(resultado_inv[, "cfi.scaled"]), 3)

# CONCLUSION
# Un mejor ajuste o cambios pequenos en CFI no prueban validez.
# Revisar contenido, indices robustos, convergencia e identificacion.
