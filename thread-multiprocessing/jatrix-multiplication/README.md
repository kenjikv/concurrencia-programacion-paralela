# Multiplicación de Matrices en Java

Este proyecto implementa la multiplicación de matrices en **Java**, tanto en su versión **secuencial** como **paralela** utilizando `ExecutorService` y `Callable`. Se comparan los tiempos de ejecución y la escalabilidad con diferentes niveles de concurrencia.

---

## **📌 Requisitos**
### **Java**
- JDK **8+** instalado.
- Visual Studio Code con la **extensión "Java Extension Pack"** (Opcional).
- Verificar instalación con:
  ```sh
  java -version
  javac -version
  ```

Si Java no está instalado, descárgalo desde [Oracle](https://www.oracle.com/java/technologies/javase-downloads.html) o [OpenJDK](https://jdk.java.net/).

---

## **🚀 Cómo Ejecutar**

### **🔹 Compilar el Código**
```sh
javac MatrixMultiplication.java
```

Esto genera un archivo `MatrixMultiplication.class`.

---

### **🔹 Ejecutar la Versión Secuencial y Paralela**
```sh
java MatrixMultiplication
```

Esto ejecutará el programa y mostrará los tiempos de ejecución tanto en la versión secuencial como en la paralela con diferentes números de hilos.

---

### **🔹 Modificar Tamaño de la Matriz**
En el código Java, se puede cambiar el tamaño de la matriz:
```java
int size = 1000;  // Cambia a 500, 2000, etc.
```

Para matrices grandes, se recomienda ajustar los hilos al número de núcleos del procesador.

---
## **💡 Notas**
- Java utiliza **JIT (Just-In-Time Compilation)**, lo que permite optimizar la ejecución en tiempo real.
- Se recomienda ajustar los hilos a `Runtime.getRuntime().availableProcessors()` para un mejor rendimiento.
- `ExecutorService` permite una gestión eficiente de hilos sin necesidad de crear nuevos manualmente.

---
