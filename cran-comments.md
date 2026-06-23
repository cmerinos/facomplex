## Resumen de la verificación del paquete `facomplex`

* R CMD check local: 0 errores, 0 warnings, 0 notas.
* R CMD check en win-builder (R-devel y R-release): 
   - Sin errores, warnings o notas significativas (solo nota sobre falta de pdflatex, que es esperada por no tener LaTeX instalado).
* Dependencias externas: Ninguna.
* Tamaño del paquete: <X> MB (dentro del límite de CRAN).

## Notas específicas

* El paquete incluye datos en `data/fullclean.rda` que requieren R >= 3.5.0 por el formato de serialización. Esto está declarado en `Depends: R (>= 3.5.0)`.

## Pruebas realizadas

* Se ejecutaron pruebas unitarias con `testthat` que verifican el comportamiento de las funciones principales (`simload`, `BSI`, `plotFacomplex`). Todas pasaron correctamente.
* Los ejemplos en la documentación se ejecutan sin errores.
* Las viñetas se compilan sin problemas.

## Observaciones adicionales

* Este es el primer envío del paquete a CRAN.
* El paquete está diseñado para métodos de análisis factorial y no tiene dependencias externas más allá de las listadas en `Imports` y `Suggests`.