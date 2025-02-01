import threading
import time

class LockBased:
    def __init__(self):
        self.data = {"stock": 1000}
        self.lock = threading.Lock()
        self.retry_count = 0

    def transaction(self, changes):
        """Transacción usando locks tradicionales"""
        while True:
            with self.lock:
                # Validar y aplicar cambios directamente
                valid = True
                for key, change in changes.items():
                    if self.data[key] + change < 0:
                        valid = False
                        break

                if valid:
                    for key, change in changes.items():
                        self.data[key] += change
                    return  # Transacción exitosa
                else:
                    self.retry_count += 1  # Contar reintento

    def get_value(self, key):
        """Obtiene el valor del stock"""
        with self.lock:
            return self.data.get(key, 0)

    def get_retries(self):
        """Devuelve el número de reintentos"""
        return self.retry_count

# Crear instancia de LockBased
lock_based = LockBased()

# Worker concurrente
def worker(changes):
    lock_based.transaction(changes)

# Medir tiempo y ejecutar workers concurrentes
start_time = time.time()
threads = [
    threading.Thread(target=worker, args=({"stock": -500},)),
    threading.Thread(target=worker, args=({"stock": 300},)),
    threading.Thread(target=worker, args=({"stock": -700},))
]

for t in threads:
    t.start()
for t in threads:
    t.join()

end_time = time.time()

# Mostrar métricas
print(f"Stock final: {lock_based.get_value('stock')}")
print(f"Tiempo total: {end_time - start_time:.5f} segundos")
print(f"Número de reintentos: {lock_based.get_retries()}")
