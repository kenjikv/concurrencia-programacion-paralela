import java.util.Random;
import java.util.concurrent.*;

public class MatrixMultiplication {
    // Método para generar una matriz aleatoria de tamaño n x n
    public static double[][] generateMatrix(int size) {
        Random random = new Random();
        double[][] matrix = new double[size][size];
        for (int i = 0; i < size; i++) {
            for (int j = 0; j < size; j++) {
                matrix[i][j] = random.nextDouble();
            }
        }
        return matrix;
    }

    // Multiplicación secuencial de matrices
    public static double[][] multiplyMatricesSequential(double[][] A, double[][] B) {
        int size = A.length;
        double[][] C = new double[size][size];

        for (int i = 0; i < size; i++) {
            for (int j = 0; j < size; j++) {
                for (int k = 0; k < size; k++) {
                    C[i][j] += A[i][k] * B[k][j];
                }
            }
        }
        return C;
    }

    // Clase para manejar la multiplicación de una parte de la matriz en paralelo
    static class MatrixMultiplierTask implements Callable<double[][]> {
        private final double[][] A, B;
        private final int startRow, endRow;

        public MatrixMultiplierTask(double[][] A, double[][] B, int startRow, int endRow) {
            this.A = A;
            this.B = B;
            this.startRow = startRow;
            this.endRow = endRow;
        }

        @Override
        public double[][] call() {
            int size = A.length;
            double[][] C_part = new double[endRow - startRow][size];

            for (int i = startRow; i < endRow; i++) {
                for (int j = 0; j < size; j++) {
                    for (int k = 0; k < size; k++) {
                        C_part[i - startRow][j] += A[i][k] * B[k][j];
                    }
                }
            }
            return C_part;
        }
    }

    // Multiplicación paralela de matrices
    public static double[][] multiplyMatricesParallel(double[][] A, double[][] B, int numThreads) throws InterruptedException, ExecutionException {
        int size = A.length;
        double[][] C = new double[size][size];

        ExecutorService executor = Executors.newFixedThreadPool(numThreads);
        Future<double[][]>[] futures = new Future[numThreads];

        int chunkSize = size / numThreads;
        for (int i = 0; i < numThreads; i++) {
            int startRow = i * chunkSize;
            int endRow = (i == numThreads - 1) ? size : (i + 1) * chunkSize;
            futures[i] = executor.submit(new MatrixMultiplierTask(A, B, startRow, endRow));
        }

        // Recoger resultados parciales y ensamblar la matriz final
        for (int i = 0; i < numThreads; i++) {
            double[][] C_part = futures[i].get();
            int startRow = i * chunkSize;
            for (int j = 0; j < C_part.length; j++) {
                C[startRow + j] = C_part[j];
            }
        }

        executor.shutdown();
        return C;
    }

    public static void main(String[] args) throws InterruptedException, ExecutionException {
        int size = 1000; // Tamaño de la matriz
        double[][] A = generateMatrix(size);
        double[][] B = generateMatrix(size);

        System.out.println("Ejecutando multiplicación en Java...");

        // Multiplicación secuencial
        long startTime = System.nanoTime();
        multiplyMatricesSequential(A, B);
        System.out.printf("Tiempo secuencial: %.4f s%n", (System.nanoTime() - startTime) / 1e9);

        // Multiplicación con diferentes números de hilos
        for (int numThreads : new int[]{2, 4, 8, 16}) {
            startTime = System.nanoTime();
            multiplyMatricesParallel(A, B, numThreads);
            System.out.printf("Tiempo paralelo (%d hilos): %.4f s%n", numThreads, (System.nanoTime() - startTime) / 1e9);
        }
    }
}
