/******************************************************************************
 
                            Online Java Compiler.
                Code, Compile, Run and Debug java program online.
Write your code in this editor and press "Run" button to execute it.
 
*******************************************************************************/
import java.util.HashMap;
import java.util.Map;
 
class HiloFactorial extends Thread {
    private String key;
    private int numero;
    HashMap<String, Integer> itemsFactorial = new HashMap<>();
 
    public HiloFactorial(String key, int numero) {
        this.key = key;
        this.numero = numero;
    }
    @Override
    public void run() {
        long inicio = System.nanoTime();       
        
        int resultado = factorial(numero);
        itemsFactorial.put(key, resultado);
        
        long fin = System.nanoTime();
        System.out.println("Tiempo de ejecución del hilo " + key + " cuyo valor  " + numero + ": " + (fin - inicio)/ 1000000.000 + " milisegundos.");        
    }
   
    private int factorial(int n)
    {
        if (n == 0 || n == 1){         
              return 1;
        }
        return n * factorial(n - 1);
    }
}
 
public class Main
{
	public static void main(String[] args) throws InterruptedException {
	    // Crear un HashMap
        HashMap<String, Integer> items = new HashMap<>();
 
        // Agregar elementos
        items.put("1", 150);
        items.put("2", 1500);
        items.put("3", 4555);
        items.put("4", 100);
        items.put("5", 930);
        items.put("6", 628);
        items.put("7", 4251);
        items.put("8", 930);
        items.put("9", 100);
        items.put("10", 60);
        
        long inicio = System.nanoTime();

	    for (Map.Entry<String, Integer> entry : items.entrySet()) {  
            HiloFactorial hilo = new HiloFactorial(entry.getKey(), entry.getValue());
            hilo.start(); 
            hilo.join();
        }
        long fin = System.nanoTime();
        System.out.println("Tiempo de ejecución general: " + (fin - inicio) / 1000000.0 + " milisegundos.");
	}
}