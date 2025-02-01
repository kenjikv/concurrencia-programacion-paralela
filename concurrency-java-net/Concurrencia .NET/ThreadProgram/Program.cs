using System.Numerics;

class Program
{
    static async Task Main(string[] args)
    {
        // create an istance of random class
        Random random = new Random();

        Dictionary<string, int> dictionary = new Dictionary<string, int>
        {
            { "1", random.Next(1, 901) },
            { "2", random.Next(1, 901) },
            { "3", random.Next(1, 901) },
            { "4", random.Next(1, 901) },
            { "5", random.Next(1, 901) },
            { "6", random.Next(1, 901) },
            { "7", random.Next(1, 901) },
            { "8", random.Next(1, 901) },
            { "9", random.Next(1, 901) },
            { "10", random.Next(1, 901) }
        };
        Dictionary<string, BigInteger> dictionaryFactorial = new Dictionary<string, BigInteger>();
        Dictionary<string, BigInteger> dictionaryFactorialThread = new Dictionary<string, BigInteger>();        

        
        Console.WriteLine($"--------------------------------------");

        DateTimeOffset startTimeGeneral = DateTimeOffset.UtcNow;        
        foreach (var entry in dictionary)
        {
            DateTimeOffset startTime = DateTimeOffset.UtcNow;

            BigInteger factorial = CalculateFactorial(entry.Value);
            dictionaryFactorial.Add(entry.Key, factorial);

            DateTimeOffset endTime = DateTimeOffset.UtcNow;  
            Console.WriteLine($"Tiempo transcurrido de {entry.Key}, con el valor {entry.Value}  demora: {(endTime - startTime).TotalMilliseconds} ms");  
        }

        DateTimeOffset endTimeGeneral = DateTimeOffset.UtcNow;
        Console.WriteLine($"Tiempo transcurrido total General: {(endTimeGeneral - startTimeGeneral).TotalMilliseconds} ms");

      

        Console.WriteLine($"--------------------------------------");

        //Task list for handling concurrent computation
        List<Task> tareas = new List<Task>();
        
        DateTimeOffset startTimeGeneralThread = DateTimeOffset.UtcNow;
        foreach(var entry in dictionary)
        {
           tareas.Add(Task.Run(() =>{
                DateTimeOffset startTimeThread = DateTimeOffset.UtcNow;

                BigInteger factorial= CalculateFactorial(entry.Value);
                dictionaryFactorialThread.Add(entry.Key, factorial);

                DateTimeOffset endTimeThread = DateTimeOffset.UtcNow;
                Console.WriteLine($"Tiempo transcurrido del {entry.Key} hilo, con el valor {entry.Value}  demora: {(endTimeThread - startTimeThread).TotalMilliseconds} ms"); 
            }));
        }
        await Task.WhenAll(tareas);       
        DateTimeOffset endTimeGeneralThread = DateTimeOffset.UtcNow;
        Console.WriteLine($"Tiempo transcurrido Thread General: {(endTimeGeneralThread - startTimeGeneralThread).TotalMilliseconds} ms");
        
    }


    static BigInteger CalculateFactorial(int n)
    {
        if (n == 0 || n == 1){         
              return 1;
        }
        return n * CalculateFactorial(n - 1);
    }
}
