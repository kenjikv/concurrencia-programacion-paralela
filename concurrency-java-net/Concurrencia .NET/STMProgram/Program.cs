
//se puede lograr un comportamiento transaccional utilizando "System.Transactions" 
//junto con estructuras de datos inmutables y mecanismos de aislamiento.

/*principios en . NET
Atomicidad: Cada operación dentro de una transacción se ejecuta completamente o no se ejecuta en absoluto.
Consistencia: El sistema garantiza que los datos compartidos no queden en un estado inconsistente.
Aislamiento: Las transacciones no interfieren entre sí, asegurando que los cambios sean visibles solo cuando la transacción se completa.
Durabilidad (opcional): En un contexto de memoria volátil, la durabilidad puede no aplicarse, pero en sistemas que requieren persistencia, es posible integrarla.
*/

//TransactionScope de System.Transación, maneja transacciones de memoria en forma controlada

//Ventajas de STM en .NET
//Evita bloqueos y condiciones de carrera: Los cambios ocurren en una copia privada hasta que la transacción se completa.
//Mayor legibilidad: Se evitan los complicados bloqueos (lock) y condiciones de sincronización.
//Rollback automático: Si ocurre un error dentro de la transacción, todos los cambios se revierten automáticamente.


//Limitaciones de STM en .NET
//No es totalmente nativo: Aunque System.Transactions proporciona funcionalidad transaccional, no está diseñado específicamente para STM.
//Overhead de rendimiento: Mantener copias transaccionales y realizar rollback puede ser costoso en sistemas de alta concurrencia.
//No es ideal para cálculos intensivos: Para operaciones de alto rendimiento en memoria, otras técnicas como lock-free programming pueden ser más eficientes.

using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using System.Transactions;

class STMProgram
{
    /// <summary>
    /// Cantidad de items en stock
    /// </summary>
    private static Dictionary<string, int> _items = new Dictionary<string, int>{
        { "calculadoras", 1000 }
    };

    static void Main()
    {
        Console.WriteLine($"Cantidad inicial de Calculadoras: {GetValue("calculadoras")}");

        // Simulación de transacciones concurrentes
        Parallel.Invoke(
            () => RealizarTransaction(-2000),
            () => RealizarTransaction(-150),
            () => RealizarTransaction(300),
            () => RealizarTransaction(-500)
        );

        Console.WriteLine($"Cantidad final de Calculadoras: {GetValue("calculadoras")}");
    }

    static void RealizarTransaction(int amount)
    { 
        try
        {
            using (TransactionScope scope = new TransactionScope(TransactionScopeOption.RequiresNew, 
            TransactionScopeAsyncFlowOption.Enabled))
            {
                DateTimeOffset startTime = DateTimeOffset.UtcNow;

                int currentBalance = GetValue("calculadoras");
                int newBalance = currentBalance + amount;

                if (newBalance < 0)
                {
                    throw new Exception("cantidad de calculadoras insuficientes");
                }
                SetValue("calculadoras", newBalance);
                Console.WriteLine($"Transacción completada: {amount}. Nuevo cantidades: {newBalance}");

                scope.Complete(); // confirmar transacción 

                DateTimeOffset endTime = DateTimeOffset.UtcNow;  
                Console.WriteLine($"Tiempo transcurrido de la transacción {amount} fue: {(endTime - startTime).TotalMilliseconds} ms");
            }
        }
        catch (Exception ex)
        {
            Console.WriteLine($"Transacción fallida: {amount}. Error: {ex.Message}");
        }
          
    }

    static void SetValue(string key, int value)
    {
        _items[key] = value;
    }

    static int GetValue(string key)
    {
        return _items.ContainsKey(key) ? _items[key] : 0;
    }
}
