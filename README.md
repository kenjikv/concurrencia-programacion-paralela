# Comparación de Concurrencia y Programación Paralela

## Universidad Autónoma Gabriel René Moreno
**Facultad de Ingeniería en Ciencias de la Computación y Telecomunicaciones**  
**Doctorado en Ciencias de la Computación**  
**Curso: Concurrencia y Programación Paralela Avanzada**  

### **Actividad 2**

### **Integrantes:**
- Apaza Leon Cegmina Clemencia  
- Huanca Sevilla Danny Luis  
- Kawaida Villegas Kenji  
- Luna Mamani Efrain Fernando  

**Docente:** PhD. Luis Roberto Pérez Ríos  
**Fecha:** Enero 2025, Santa Cruz, Bolivia  

---

## **Índice**
1. Introducción
2. Modelos de concurrencia
3. Métricas
4. Discusión
   - Ventajas y Desventajas
   - Problemas Encontrados y Soluciones
5. Conclusiones

---

## **1. Introducción**
El presente estudio analiza la eficiencia de modelos de concurrencia en operaciones de cálculo intensivo, como la multiplicación de matrices y el cálculo factorial de una colección. Se comparan implementaciones en C#, Python, Haskell y Java, evaluando el impacto del manejo de memoria y concurrencia en el tiempo de ejecución y escalabilidad.

---

## **2. Modelos de Concurrencia**
Se implementaron distintos paradigmas y patrones de concurrencia:

- **Hilos (Threads)**: Usados en Java y C#.
- **Multiprocesos (Multiprocessing)**: Implementado en Python para mitigar el GIL.
- **Software Transactional Memory (STM)**: Utilizado en Haskell y simulado en Python.

Cada modelo se evaluó con diferentes escenarios, midiendo su eficiencia y facilidad de implementación.

---

## **3. Métricas**
Se realizaron pruebas en matrices de 500x500 y 1000x1000, analizando el rendimiento secuencial y paralelo:

| Método de Ejecución | Python 500x500 (s) | Python 1000x1000 (s) | Java 500x500 (s) | Java 1000x1000 (s) |
|--------------------|-------------------|--------------------|-----------------|-----------------|
| Secuencial       | 78.923             | 681.492            | 0.4440          | 36.099         |
| 2 Procesos/Hilos | 39.704             | 341.271            | 0.2316          | 18.538         |
| 4 Procesos/Hilos | 21.962             | 201.743            | 0.1126          | 0.9338         |
| 8 Procesos/Hilos | 15.777             | 128.463            | 0.0187          | 0.2089         |
| 16 Procesos/Hilos | 15.385            | 119.940            | 0.0274          | 0.2093         |

Los resultados demuestran que Java optimiza mejor la concurrencia, mientras que Python presenta una mayor sobrecarga en la gestión de procesos.

---

## **4. Discusión**

### **4.1. Ventajas y Desventajas**

**Python**:
- Ventajas: Código más sencillo, escalabilidad en múltiples núcleos.
- Desventajas: Alta sobrecarga en procesos, mayor consumo de memoria.

**Java**:
- Ventajas: Gestión eficiente de hilos, mejor rendimiento en alta concurrencia.
- Desventajas: Mayor complejidad en la administración de hilos.

**C# (.NET - Threads vs Task)**:
- Ventajas: Task maneja concurrencia de manera eficiente con ThreadPool.
- Desventajas: No recomendado para tareas en tiempo real debido a la sobrecarga inicial.

**STM (Software Transactional Memory)**:

| Experimento       | Tiempo ejecución (s) | Stock Final | Reintentos | Enfoque |
|------------------|--------------------|------------|-----------|---------|
| Python STM Sim. | 0.00298             | 100        | 0         | Locks + reintentos manuales |
| Python Locks    | 0.00200             | 100        | 0         | Bloqueo directo |
| Haskell STM     | 0.00000             | 100        | 0         | Transacciones automáticas |
| Haskell Locks   | 0.00000             | 100        | 0         | Bloqueo Manual (ModifyMVar) |

---

## **5. Conclusiones**

- Java es más eficiente en ejecución concurrente debido a la optimización del JIT Compiler y la gestión eficiente de hilos.
- Python es más fácil de programar, pero su concurrencia basada en procesos es menos eficiente que los hilos en Java.
- STM en Haskell es más seguro y eficiente en concurrencia en comparación con Python, donde debe ser simulado.
- C# con Task ofrece mejor rendimiento que el uso manual de hilos en .NET, pero tiene limitaciones en tareas en tiempo real.

Este estudio proporciona información clave para la elección de tecnologías en aplicaciones de alto rendimiento y concurrencia.

---
