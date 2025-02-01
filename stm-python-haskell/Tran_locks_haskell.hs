import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import System.CPUTime
import Text.Printf

-- Modificación atómica del stock con locks
modifyStock :: MVar Int -> Int -> IO Bool
modifyStock stock delta = modifyMVar stock $ \currentStock ->
    let newStock = currentStock + delta
    in if newStock < 0
        then return (currentStock, False)  -- Restaurar stock si no es válido
        else return (newStock, True)       -- Aplicar cambio

-- Función para medir el tiempo de ejecución
measureTime :: IO a -> IO (a, Double)
measureTime action = do
    start <- getCPUTime
    result <- action
    end <- getCPUTime
    let diff = fromIntegral (end - start) / (10^12)  -- Convertir a segundos
    return (result, diff)

-- Ejecutar múltiples transacciones concurrentes y medir el rendimiento
main :: IO ()
main = do
    stock <- newMVar (1000 :: Int)  -- Stock inicial
    retryCount <- newMVar (0 :: Int)  -- Contador de reintentos

    let worker delta = do
            success <- modifyStock stock delta
            unless success $ modifyMVar_ retryCount (\x -> return (x + 1))  -- Registrar reintentos

    (_, totalTime) <- measureTime $ do
        threads <- forM [1..3] $ \i -> forkIO $
            worker (case i of
                1 -> -500  -- Reducción de stock
                2 -> 300   -- Reabastecimiento
                3 -> -700) -- Reducción de stock
        mapM_ wait threads

    finalStock <- readMVar stock
    totalRetries <- readMVar retryCount

    -- Mostrar métricas con `printf`
    printf "Stock final (Locks): %d\n" (finalStock :: Int)
    printf "Tiempo total de ejecución (Locks): %.5f segundos\n" (totalTime :: Double)
    printf "Número total de reintentos (Locks): %d\n" (totalRetries :: Int)

    where
        wait tid = killThread tid
