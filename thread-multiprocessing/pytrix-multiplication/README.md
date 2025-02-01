# Multiplicación de Matrices en Python

Este proyecto implementa la multiplicación de matrices en **Python**, tanto en su versión **secuencial** como **paralela** utilizando `multiprocessing`. Se comparan los tiempos de ejecución y la escalabilidad con diferentes niveles de concurrencia.

---

## **📌 Requisitos**
### **Python**
- Python **3.8** o superior.
- Visual Studio Code con la **extensión de Python** (Opcional).
- Instalar dependencias con:
  ```sh
  pip install -r requirements.txt
  ```

---

## **🚀 Cómo Ejecutar**

### **🔹 Verificar la Instalación de Python**
Ejecutar en la terminal:
```sh
python3 --version
```

Si no tienes Python instalado, descárgalo desde [python.org](https://www.python.org/downloads/).

---

### **🔹 Ejecutar la Versión Secuencial**
```sh
python3 multiply_matrix.py
```

Esta versión usa **tres bucles anidados** para realizar la multiplicación de matrices sin concurrencia.

---

### **🔹 Ejecutar la Versión Paralela**
```sh
python3 multiply_matrix.py
```

Esta versión divide el cálculo entre **múltiples procesos**, optimizando el rendimiento en CPUs multinúcleo.

---

### **🔹 Modificar Tamaño de la Matriz**
En el código Python, se puede cambiar el tamaño de la matriz:
```python
size = 1000  # Cambia a 500, 2000, etc.
```

Para matrices grandes, se recomienda aumentar la memoria y los núcleos del procesador.

---

## **💡 Notas**
- Python tiene overhead al usar `multiprocessing` debido a la creación de procesos y la comunicación entre ellos.
- Para mejorar el rendimiento, se recomienda usar **NumPy** o implementar `shared_memory` en `multiprocessing`.
- En sistemas con muchos núcleos, se recomienda ajustar el número de procesos a `os.cpu_count()`.

---