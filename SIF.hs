module Main (
    main
) where

import System.Posix.IO
import System.Posix.Terminal
import Control.Monad
import Control.Concurrent
import System.IO
import qualified Data.ByteString as BS



portFile = "/dev/tty.usbserial-A6003ThW"

data1 :: BS.ByteString
data1 = BS.pack [0x7e,0,4,8,1,0x4d,0x59,0x50]


main = do
        h <- openFile portFile ReadWriteMode
        hSetBuffering h NoBuffering
        -- Configure the serial port
        fd <- openFd portFile ReadWrite Nothing flags
        ta <- getTerminalAttributes fd
        let ta' = configure ta B9600
        setTerminalAttributes fd ta' Immediately
        discardData fd BothQueues
        controlFlow fd TransmitStart
        controlFlow fd RestartOutput
        sendBreak fd 10
        closeFd fd
        return fd
        -- Open the serial port
        --h <- openFile portFile ReadWriteMode
        --hSetBuffering h NoBuffering
        -- Start a reader
        --t <- forkIO $ putStrLn "Started Reader" >> reader h
        threadDelay 10000
        putStrLn "Writing Data"
        BS.hPut h data1
        threadDelay 100000
        putStrLn "Shutting down"
        --killThread t
        --threadDelay 100000
        hClose h
        return h
    where flags = OpenFileFlags True True True True False


reader h = do
    d <- BS.hGetSome h 100
    when (BS.length d == 0) (threadDelay 1000)
    when (BS.length d > 0) $ putStrLn $ "read> " ++ (show $ BS.unpack d)
    reader h


configure :: TerminalAttributes -> BaudRate -> TerminalAttributes
configure termOpts baud =
    termOpts `withInputSpeed` baud
                 `withOutputSpeed` baud
                 `withBits` 8
                 `withoutMode` TwoStopBits
                 `withoutMode` EnableParity
                 `withoutMode` StartStopInput
                 `withoutMode` StartStopOutput
                 `withoutMode` EnableEcho
                 `withoutMode` EchoErase
                 `withoutMode` EchoKill
                 `withoutMode` ProcessInput
                 `withoutMode` ProcessOutput
                 `withoutMode` MapCRtoLF
                 `withoutMode` EchoLF
                 `withoutMode` HangupOnClose
                 `withoutMode` KeyboardInterrupts
                 `withoutMode` ExtendedFunctions
                 `withMode` LocalMode
                 `withMode` ReadEnable
                 `withTime` 1
                 `withMinInput` 0
