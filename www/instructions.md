# Guía de Usuario Completa

## 📁 Carga de Datos
- **Formatos soportados**: 
  - CSV (valores separados por comas)
  - XLSX (archivos Excel)
  - TXT (archivos de texto)
- **Opciones de configuración**:
  - Seleccione el separador de columnas (coma, punto y coma o tabulador)
  - Especifique el tipo de decimal (punto o coma)

## 🔢 Análisis Cuantitativo
**Pruebas estadísticas disponibles**:

### Comparación de grupos:
- **t-test**: Compara medias entre 2 grupos (requiere normalidad)
- **ANOVA**: Compara medias entre 3+ grupos (análisis de varianza)
- **Wilcoxon**: Versión no paramétrica del t-test
- **Kruskal-Wallis**: ANOVA no paramétrico para 3+ grupos
- **Friedman**: Para medidas repetidas/múltiples

### Correlación:
- **Pearson**: Correlación lineal (para datos normales)
- **Spearman**: Correlación no paramétrica (usando rangos)

### Normalidad:
- **Shapiro-Wilk**: Prueba de normalidad para muestras pequeñas
- **Kolmogorov-Smirnov**: Prueba de normalidad alternativa

## 🔤 Análisis Cualitativo
**Pruebas estadísticas disponibles**:

### Asociación entre variables:
- **Chi-cuadrado (χ²)**: Para tablas de contingencia
- **Fisher**: Exacta para muestras pequeñas (tablas 2x2)
- **Coeficiente de Contingencia (C)**: Medida de asociación
- **G-test**: Prueba de razón de verosimilitud

### Proporciones:
- **Binomial**: Prueba proporciones observadas vs esperadas

### Datos pareados:
- **McNemar**: Para datos categóricos relacionados
- **Cochran-Q**: Extensión de McNemar para múltiples grupos

## 📊 Teorema del Límite Central (TLC)
- **Simulación interactiva** de distribuciones:
  - Normal
  - Exponencial
  - Uniforme
- **Visualización** de la convergencia a la normalidad
- **Personalización**:
  - Número de muestras
  - Tamaño de muestra

## 📝 Interpretación Automática
- Todos los resultados incluyen:
  - Explicación en lenguaje claro
  - Evaluación de significancia estadística
  - Tamaño del efecto (cuando aplica)
  - Recomendaciones basadas en los resultados

## 💡 Consejos de Uso
1. Verifique los supuestos de cada prueba
2. Revise los gráficos antes de interpretar resultados
3. Para pruebas paramétricas, confirme normalidad con Shapiro-Wilk
4. Use pruebas no paramétricas cuando los supuestos no se cumplan