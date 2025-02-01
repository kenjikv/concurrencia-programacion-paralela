import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import System.CPUTime
import Text.Printf

-- Transacción que modifica el stock
modifyStock :: TVar Int -> Int -> STM Bool
modifyStock stock delta = do
    currentStock <- readTVar stock
    let newStock = currentStock + delta
    if newStock < 0
        then return False  -- No se puede realizar la transacción
        else do
            writeTVar stock newStock
            return True

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
    stock <- newTVarIO (1000 :: Int)  -- Definir stock inicial
    retryCount <- newTVarIO (0 :: Int)  -- Contador de reintentos

    -- Crear workers para simular transacciones concurrentes
    let worker delta = atomically $ do
            success <- modifyStock stock delta
            if not success
                then do
                    modifyTVar retryCount (+1)  -- Registrar intento fallido
                    retry  -- Reintentar hasta que haya suficiente stock
                else return ()  -- Aplicar transacción exitosamente

    -- Medir el tiempo total de ejecución
    (_, totalTime) <- measureTime $ do
        threads <- forM [1..3] $ \i -> forkIO $
            worker (case i of
                1 -> -500  -- Reducción de stock
                2 -> 300   -- Reabastecimiento
                3 -> -700) -- Reducción de stock
        mapM_ wait threads

    -- Leer valores finales
    finalStock <- readTVarIO stock
    totalRetries <- readTVarIO retryCount

    -- Mostrar métricas
    printf "Stock final: %d\n" (finalStock :: Int)
    printf "Tiempo total de ejecución: %.5f segundos\n" (totalTime :: Double)
    printf "Número total de reintentos: %d\n" (totalRetries :: Int)

    where
        wait tid = killThread tid
