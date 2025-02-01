# 🚀 Modelos de concurrencia

La solución esta desarrollada en el entorno de .Net, presenta ejemplos de modelos de concurrencia de Hilos y STM (Software Transactional Memory).

## 📌 Desarrollado por
* APAZA LEON  CEGMINA CLEMINCIA 
* HUANCA SEVILLA DANNY LUIS
* KAWAIDA VILLEGAS KENJI
* LUNA MAMANI EFRAIN FERNANDO

## Lógica del Proyecto STM:

Una de las características del modelo de concurrencia STM es que puede realizar grupo de operaciones de memoria de forma atómica, cuando se comparte un recurso común. Además, ofrece la ventaja de la libertad de bloqueos e inversión de prioridades, liberando la tensión entre la granularidad de los bloques y la concurrencia.

Para este ejemplo se considera un tipo de dato dictionary que almacena los items de un inventario. La colección de items almacena por defecto la cantidad de calculadoras, el valor asociado a esta colección es actualizada concurrentemente, cambiando el valor según la petición.

| Código Item      | Valor               |Descripción  |
|---------------------|-------------------------|-------------------------|
| `calculadoras`      | 1000          | Valor inicial
| `calculadoras`      | 1300          | Nuevo Valor después de sumar la cantidad de items, operación relaizado concurrentemente
| `calculadoras`      | 850          | Nuevo Valor después de eliminar la cantidad de items,operación relaizado concurrentemente
| `calculadoras`      | 500          | Nuevo Valor ,operación relaizado concurrentemente


## Lógica del Proyecto de Hilos:

Para el proyecto relacionado con hilos, se tiene una colección de key asociado a un valor que se genera de forma aleatoria, el ejercicio realiza el cálculo del factorial del valor, cada hilo realizará una operación por separado.

| key     | Valor  |Descripción  |
|--------|---------|-------------------------|
| 1      | 852     | El hilo 1 realizará el cálculo del factorial del número 852
| 2      | 2       | El hilo 2 realizará el cálculo del factorial del número 2
| 3      | 20      | El hilo 3 realizará el cálculo del factorial del número 20
| ...    | ...     | El hilo ... realizará el cálculo del factorial del número ...


## Complejidad vs. Legibilidad:

- .NET para el manejo de `HILOS` brinda la opción de Thread y Task, siendo task más robusta que Thread.
- .NET para el uso de `STM`, en lugar de bloqueos ofrece la libertad de bloqueos que son administrados por la tecnología, lo que facilita la abstración de codificación.



## 📌 Cómo Ejecutar  
Ejecutar en la terminal:

Para ejecutar el proyecto de hilos:
```
dotnet run --project ThreadProgram
```

Para ejecutar el proyecto Java que trabaja con Hilos:
```
javac Main.java
java Main
```

Para ejecutar el proyecto de STM:
```
dotnet run --project STMProgram
```


