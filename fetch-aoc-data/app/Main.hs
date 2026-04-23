{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, try)
import Control.Monad (unless, when)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (statusCode)
import System.Directory
import System.Exit (exitFailure)
import System.FilePath (takeDirectory, (</>))
import System.IO (hFlush, stdout)

-- | Path to saved session token
tokenFile :: FilePath
tokenFile = ".aoc-token"

-- | Identifies this tool to AoC per the automation etiquette guidelines.
userAgent :: BC.ByteString
userAgent = "github.com/roy-crippen/aoc-haskell by roycrippen@gmail.com"

data DownloadResult = DLOk | DLAuthFailed | DLFailed

-- | Load token from file or prompt user and save it
getSessionToken :: IO String
getSessionToken = do
  exists <- doesFileExist tokenFile
  if exists
    then do
      token <- TIO.readFile tokenFile
      let cleaned = T.unpack $ T.strip token
      putStrLn $ "✓ Loaded session token from " <> tokenFile
      pure ("session=" <> cleaned)
    else do
      putStrLn "\nHow to get your session token:"
      putStrLn "1. Log into https://adventofcode.com in your browser"
      putStrLn "2. Press F12 → Application (or Storage) → Cookies → adventofcode.com"
      putStrLn "3. Copy the long hexadecimal value of the 'session' cookie"
      putStrLn "   (do NOT include 'session=' )"
      putStr "\nPaste session token: "
      hFlush stdout
      raw <- getLine
      let cleaned = T.unpack $ T.strip $ T.pack raw
      TIO.writeFile tokenFile (T.pack cleaned)
      putStrLn $ "\n✓ Token saved to " <> tokenFile <> " for future runs."
      pure ("session=" <> cleaned)

-- | Download a single input file
downloadInput :: Manager -> String -> String -> FilePath -> IO DownloadResult
downloadInput manager cookie url filePath = do
  request <- parseRequest url
  let req =
        request
          { requestHeaders =
              [ ("Cookie", BC.pack cookie),
                ("User-Agent", userAgent)
              ]
          }

  result <- try (httpLbs req manager) :: IO (Either IOException (Response LBS.ByteString))

  case result of
    Left ex -> do
      putStrLn $ "    Network error: " <> show ex
      pure DLFailed
    Right response ->
      let sc = statusCode (responseStatus response)
       in if sc == 200
            then do
              -- Only create the dayXX directory if we actually got a valid input
              createDirectoryIfMissing True (takeDirectory filePath)
              BS.writeFile filePath (LBS.toStrict (responseBody response))
              pure DLOk
            else do
              putStrLn $ "    HTTP error: status " <> show sc
              when (sc == 404) $
                putStrLn "         → Day not available yet"
              if sc == 400 || sc == 403
                then do
                  putStrLn "         → Invalid or expired session token"
                  pure DLAuthFailed
                else pure DLFailed

main :: IO ()
main = do
  putStrLn "=== Advent of Code Input Downloader ===\n"
  putStrLn "Make sure you are logged into https://adventofcode.com\n"

  -- Year prompt
  putStr "Enter the year (e.g. 2024): "
  hFlush stdout
  yearStr <- getLine
  let year = read yearStr :: Int
  unless (year >= 2015 && year <= 2026) $
    error "Year should be between 2015 and 2026"

  putStrLn $ "\nDownloading all inputs for year " <> show year <> "..."

  -- Get session token (from file or prompt)
  sessionCookie <- getSessionToken

  -- Create base directory (data/2024)
  let baseDir = "data" </> show year
  createDirectoryIfMissing True baseDir

  manager <- newManager tlsManagerSettings

  let downloadAll [] = pure ()
      downloadAll (day : rest) = do
        let dayPadded = if day < 10 then "0" <> show day else show day
            dayDir = baseDir </> ("day" <> dayPadded)
            inputFile = dayDir </> "input.txt"

        exists <- doesFileExist inputFile
        if exists
          then do
            putStrLn $ "  Day " <> dayPadded <> ": already exists, skipping"
            downloadAll rest
          else do
            let url = "https://adventofcode.com/" <> show year <> "/day/" <> show day <> "/input"

            putStr $ "  Day " <> dayPadded <> ": downloading... "
            hFlush stdout

            result <- downloadInput manager sessionCookie url inputFile
            case result of
              DLOk -> do
                putStrLn "✓"
                threadDelay 300000
                downloadAll rest
              DLFailed -> do
                putStrLn "✗"
                threadDelay 300000
                downloadAll rest
              DLAuthFailed -> do
                putStrLn "✗"
                tokenExists <- doesFileExist tokenFile
                when tokenExists $ removeFile tokenFile
                putStrLn $ "\nSession token rejected — removed " <> tokenFile <> ". Re-run to paste a fresh token."
                exitFailure

  downloadAll ([1 .. 25] :: [Int])
  putStrLn "\n=== All done! ==="
  putStrLn $ "Inputs saved to: " <> baseDir
  putStrLn "Tip: Never commit the data/ folder or .aoc-token to git!"