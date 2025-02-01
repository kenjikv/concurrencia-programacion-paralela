# Ejecutando ejercicio en Haskell vs Python

## Requisitos previos

Antes de ejecutar los ejercicios, asegúrese de tener instaladas las siguientes herramientas:

### Haskell
- GHC (Glasgow Haskell Compiler)
  ```sh
  ghc --version
  ```
- Cabal (Sistema de construcción de Haskell)
  ```sh
  cabal --version
  ```
- Extensión de Haskell para Visual Studio Code (opcional, pero recomendada)

### Python
- Python 3
  ```sh
  python --version
  ```
- Extensión de Python para Visual Studio Code (opcional, pero recomendada)

## Ejecución de los programas

Ejecute los siguientes comandos para comparar la ejecución en ambos lenguajes:

### Python
```sh
python Tran_locks_python.py
python Tran_simulado_STM_python.py
```

### Haskell
```sh
runhaskell Tran_locks_haskell.hs
runhaskell Tran_STM_haskell.hs
```

Estos comandos ejecutarán las versiones de los ejercicios en Haskell y Python para la comparación de rendimiento y comportamiento.

---
Este documento puede descargarse y utilizarse para configurar el entorno de desarrollo correctamente.
