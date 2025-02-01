import threading
import time
from contextlib import contextmanager

# Diccionario que representa el stock
items = {"calculadoras": 1000}
lock = threading.Lock()

@contextmanager
def transaction():
    """Simula un contexto de transacción"""
    lock.acquire()
    try:
        yield
    finally:
        lock.release()

def get_value(key):
    """Obtiene el valor actual del stock"""
    return items.get(key, 0)

def set_value(key, value):
    """Actualiza el valor del stock"""
    items[key] = value

def realizar_transaction(amount):
    """Ejecuta una transacción simulada"""
    global items
    
    try:
        start_time = time.time()
        with transaction():
            current_balance = get_value("calculadoras")
            new_balance = current_balance + amount

            if new_balance < 0:
                raise Exception("Cantidad de calculadoras insuficientes")
            
            set_value("calculadoras", new_balance)
            print(f"Transacción completada: {amount}. Nuevo stock: {new_balance}")
        
        end_time = time.time()
        print(f"Tiempo transcurrido de la transacción {amount} fue: {(end_time - start_time) * 1000:.2f} ms")
    except Exception as e:
        print(f"Transacción fallida: {amount}. Error: {e}")

if __name__ == "__main__":
    print(f"Cantidad inicial de Calculadoras: {get_value('calculadoras')}")
    
    # Creando hilos para simular transacciones concurrentes
    threads = [
        threading.Thread(target=realizar_transaction, args=(-2000,)),
        threading.Thread(target=realizar_transaction, args=(-150,)),
        threading.Thread(target=realizar_transaction, args=(300,)),
        threading.Thread(target=realizar_transaction, args=(-500,))
    ]
    
    # Iniciar los hilos
    for thread in threads:
        thread.start()
    
    # Esperar a que terminen
    for thread in threads:
        thread.join()
    
    print(f"Cantidad final de Calculadoras: {get_value('calculadoras')}")
