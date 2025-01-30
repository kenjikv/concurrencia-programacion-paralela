import multiprocessing as mp
import time
import random

def generate_matrix(size):
    """Genera una matriz cuadrada con valores aleatorios."""
    return [[random.random() for _ in range(size)] for _ in range(size)]

def multiply_matrices_sequential(A, B):
    """Multiplicación de matrices de forma secuencial sin NumPy."""
    size = len(A)
    C = [[0] * size for _ in range(size)]

    for i in range(size):
        for j in range(size):
            for k in range(size):
                C[i][j] += A[i][k] * B[k][j]
    
    return C

def worker(A, B, start, end, result_queue):
    """Multiplica una parte de la matriz A con B y almacena el resultado en una cola."""
    size = len(A)
    C_part = [[0] * size for _ in range(end - start)]

    for i in range(start, end):
        for j in range(size):
            for k in range(size):
                C_part[i - start][j] += A[i][k] * B[k][j]

    result_queue.put((start, C_part))

def multiply_matrices_parallel(A, B, num_processes):
    """Multiplica matrices en paralelo dividiendo las filas entre procesos."""
    size = len(A)
    chunk_size = size // num_processes
    processes = []
    result_queue = mp.Queue()

    for i in range(num_processes):
        start = i * chunk_size
        end = (i + 1) * chunk_size if i < num_processes - 1 else size
        p = mp.Process(target=worker, args=(A, B, start, end, result_queue))
        processes.append(p)
        p.start()

    # Recoger los resultados en orden
    C = [[0] * size for _ in range(size)]
    for _ in range(num_processes):
        start, C_part = result_queue.get()
        for i, row in enumerate(C_part):
            C[start + i] = row

    for p in processes:
        p.join()

    return C

if __name__ == "__main__":
    size = 1000  # Cambia el tamaño de la matriz si deseas probar con valores más grandes
    A = generate_matrix(size)
    B = generate_matrix(size)

    # Prueba secuencial
    start_time = time.time()
    C_seq = multiply_matrices_parallel(A, B, num_processes=1)
    print(f"Tiempo secuencial: {time.time() - start_time:.4f} s")

    # Prueba con diferentes números de procesos
    for num_processes in [2, 4, 8, 16]:
        start_time = time.time()
        C_par = multiply_matrices_parallel(A, B, num_processes)
        print(f"Tiempo paralelo ({num_processes} procesos): {time.time() - start_time:.4f} s")
