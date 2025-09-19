# PUT-forecasting

## Descripción general

Este proyecto permite la extracción, procesamiento, modelado y visualización de datos de People Using TV (PUT) mediante una aplicación Shiny. El flujo completo abarca desde la obtención de datos crudos hasta la predicción y análisis de resultados, con módulos independientes para cada etapa.

---

## Estructura del proyecto

```
PUT-forecasting/
│
├── app.R                  # Aplicación principal Shiny
├── requirements.txt       # Paquetes R requeridos
├── manifest.json          # Manifest de dependencias (actualizar antes de push)
├── data/
│   ├── raw_data.parquet   # Datos crudos extraídos
│   └── models/            # Modelos entrenados (.rds)
│
├── scripts/
│   ├── extraction.R       # Extracción de datos crudos
│   ├── preprocess.R       # Preprocesamiento de datos
│   ├── features.R         # Agregación de features temporales y eventos
│   ├── future_ts.R        # Generación de features futuros
│   ├── pretrain_models.R  # Entrenamiento de modelos
│   ├── workflows.R        # Workflows y entrenamiento de modelos
│   ├── utils.R            # Utilidades para nombres y parsing
│   └── query_template.sql # Consulta SQL base
└── PUT-forecasting.Rproj  # Proyecto RStudio
```

---

## Explicación de módulos y funciones

### `query_template.sql`
- Consulta SQL base utilizada por el módulo de extracción para obtener los datos crudos desde la fuente.

### `extraction.R`
- Ejecuta la consulta de `query_template.sql` y extrae los datos crudos.
- Guarda los datos en formato parquet en la carpeta `data/`.
- Se corre manualmente - por ahora - para actualizar los datos.

### `preprocess.R`
- Funciones para limpiar y transformar los datos crudos.
  - `load_data(path)`: Carga datos parquet y transforma fechas y variables.
  - `preprocess_data(data, ...)`: Filtra y agrega los datos según parámetros.

### `features.R`
- Agrega features de serie temporal y eventos especiales (ej. Super Bowl).
  - `add_features(data, superbowl_input)`: Añade firmas temporales, holidays y variables de eventos.

### `future_ts.R`
- Genera datos futuros (features) para predicción con horizonte de hasta un año.
  - `generate_future_ts(data, hours, superbowl)`: Crea un tsibble con las variables necesarias para predecir en el futuro.

### `pretrain_models.R`
- Entrena modelos para cada combinación de parámetros definida.
- Guarda los modelos entrenados en la carpeta `data/models/`.
- Se corre manualmente - por ahora - para actualizar o agregar modelos.

### `workflows.R` (antes `train_arimax.R`)
- Define los workflows y funciones para entrenar modelos (ejemplo: ARIMAX).
  - `workflow_arimax(ts_data)`: Crea el workflow de modelado con receta y modelo ARIMA.
  - `train_arimax(ts_data, ...)`: Entrena el modelo y retorna el fit y los splits.
  - Puedes agregar aquí nuevos workflows para otros modelos.

### `utils.R`
- Funciones utilitarias para manipular nombres de modelos y parsing de archivos.
  - `clean_name(x)`: Limpia y normaliza nombres.
  - `get_model_key(...)`: Genera la clave única para cada modelo.
  - `parse_model_filename(file_name)`: Extrae información relevante del nombre de archivo de modelo.

---

## Guía para agregar un nuevo modelo

1. **Agregar el workflow**
   - Define el nuevo workflow en `scripts/workflows.R`.
   - Si el modelo requiere nuevas funciones de preprocesamiento o features, agrégalas en `preprocess.R` o `features.R`.

2. **Actualizar el módulo de pretrain**
   - Modifica `pretrain_models.R` para incluir el nuevo workflow en las iteraciones de entrenamiento.
   - Asegúrate de que el modelo se guarde correctamente en `data/models/`.

3. **Ejecutar el entrenamiento**
   - Corre manualmente el script `pretrain_models.R` desde RStudio o VSCode para entrenar y guardar el nuevo modelo.

4. **Actualizar dependencias**
   - Si el nuevo modelo utiliza paquetes adicionales, agrégalos a `requirements.txt`.
   - Actualiza también `manifest.json` para que los entornos reproducibles incluyan los nuevos paquetes. Ejemplo:
     ```R
     rsconnect::writeManifest()
     ```

5. **Verificar integración**
   - Asegúrate de que el modelo esté disponible en la app Shiny y que los scripts lo reconozcan correctamente.
   - Si agregaste nuevas funciones, documenta su uso en el README.

---

## Ejecución manual de scripts

- Los módulos `extraction.R` y `pretrain_models.R` se ejecutan manualmente para actualizar datos y modelos.
- El resto de los scripts son llamados desde la app principal o desde otros módulos.

---
