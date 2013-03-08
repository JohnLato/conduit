import Data.Conduit
import Data.Conduit.Streams
import Data.Conduit.Binary
import qualified System.IO.Streams as Streams
import Criterion.Main
import System.IO
import qualified Data.ByteString.Lazy as L

import qualified Data.Iteratee as I
import qualified Data.Iteratee.IO as I
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Control.Exception.Lifted as E
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Control.Monad.Trans

inputFP = "input.dat"
outputFP = "output.dat"

main = do
    runResourceT $ sourceFile "/dev/urandom"
                $$ isolate (1024 * 1024)
                =$ sinkFile inputFP
    defaultMain
        [ bench "conduit+resourcet" $ whnfIO $
            runResourceT $ sourceFile inputFP $$ sinkFile outputFP
        , bench "iteratee-outh-matchsize"  $ whnfIO $
            runResourceT $ I.run =<< I.enumFile 4096 inputFP (outputIter outputFP)
        , bench "iteratee-outh-large"  $ whnfIO $
            runResourceT $ I.run =<< I.enumFile 32768 inputFP (outputIter outputFP)
        , bench "iteratee-outh-default"  $ whnfIO $
            runResourceT $ I.run =<< I.enumFile 1024 inputFP (outputIter outputFP)
        , bench "conduit+NoHandle" $ whnfIO $
            runResourceT $ sourceFileNoHandle inputFP $$ sinkFileNoHandle outputFP
        , bench "conduit+NoHandleOut" $ whnfIO $
            runResourceT $ sourceFile inputFP $$ sinkFileNoHandle outputFP
        , bench "conduit+NoHandleIn" $ whnfIO $
            runResourceT $ sourceFileNoHandle inputFP $$ sinkFile outputFP
        , bench "conduit+bracket" $ whnfIO $
            withFile inputFP ReadMode $ \i ->
            withFile outputFP WriteMode $ \o ->
            sourceHandle i $$ sinkHandle o
        , bench "conduit+iostreams" $ whnfIO $
            Streams.withFileAsInput inputFP $ \i ->
            Streams.withFileAsOutput outputFP $ \o ->
            sourceStream i $$ sinkStream o
        , bench "iostreams" $ whnfIO $
            Streams.withFileAsInput inputFP $ \i ->
            Streams.withFileAsOutput outputFP $ \o ->
            Streams.connect i o
        , bench "lazy" $ whnfIO $
            withFile inputFP ReadMode $ \i ->
            withFile outputFP WriteMode $ \o ->
            L.hGetContents i >>= L.hPut o
            
        ]

outputIter fp = do
  (key,h) <- lift $ allocate (openBinaryFile fp ReadWriteMode) (hClose)
  I.mapChunksM_ (liftIO . B.hPut h)
  release key
