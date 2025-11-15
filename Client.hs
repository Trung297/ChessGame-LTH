module Main where

import ChessLogic
import Control.Exception (finally)
import Control.Monad (forever)
import Data.Char (toLower)
import Network.Socket
import System.IO
import System.Environment (getArgs)
import Text.Read (readMaybe)

-- Định nghĩa trạng thái game (phải giống hệt server)
data GameState = GameState Board Color String deriving (Show, Read)

-- === Các hàm hiển thị (Lấy từ Main.hs gốc của bạn) ===
-- Client cần các hàm này để vẽ bàn cờ
showCell :: Cell -> String
showCell Nothing = "."
showCell (Just (Piece t c)) =
  case c of
    White -> case t of
      King   -> "♔"
      Queen  -> "♕"
      Rook   -> "♖"
      Bishop -> "♗"
      Knight -> "♘"
      Pawn   -> "♙"
    Black -> case t of
      King   -> "♚"
      Queen  -> "♛"
      Rook   -> "♜"
      Bishop -> "♝"
      Knight -> "♞"
      Pawn   -> "♟"

printBoard :: Board -> IO ()
printBoard board = do
  let rows = zip [8,7..1] board
  mapM_ (\(num,row) -> putStrLn $ show num ++ " " ++ unwords (map showCell row)) rows
  putStrLn "  a b c d e f g h"

clearScreen :: IO ()
clearScreen = putStr "\ESC[2J\ESC[H"

flush :: IO ()
flush = hFlush stdout

label :: Color -> String
label White = "Trang"
label Black = "Den"

-- === MAIN CLIENT ===
main :: IO ()
main = withSocketsDo $ do
  
  hSetEncoding stdout utf8
  args <- getArgs
  let (host, port) = case args of
                       [h] -> (h, "3000")
                       [h, p] -> (h, p)
                       _ -> ("127.0.0.1", "3000") -- Mặc định là localhost

  addr <- resolve host port
  putStrLn $ "Dang ket noi den " ++ host ++ ":" ++ port ++ "..."
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  connect sock (addrAddress addr)
  handle <- socketToHandle sock ReadWriteMode
  hSetBuffering handle LineBuffering

  colorStr <- hGetLine handle
  let myColor = if colorStr == "White" then White else Black
  putStrLn $ "Ban la quan " ++ label myColor ++ "."
  putStrLn "Nhan [Enter] de bat dau..."
  getLine -- Chờ user nhấn enter
  
  -- Chạy vòng lặp client
  finally (clientLoop handle myColor) (hClose handle)

  where
    resolve host port = do
      let hints = defaultHints { addrSocketType = Stream }
      head <$> getAddrInfo (Just hints) (Just host) (Just port)

-- Vòng lặp chính của client
clientLoop :: Handle -> Color -> IO ()
clientLoop handle myColor = forever $ do
  -- 1. Chờ và nhận trạng thái từ server
  stateStr <- hGetLine handle
  case readMaybe stateStr :: Maybe GameState of
    Nothing -> putStrLn "Loi: Khong doc duoc trang thai tu server."
    Just (GameState board turn message) -> do
      
      -- 2. Vẽ lại màn hình
      clearScreen
      putStrLn $ "BAN LA QUAN: " ++ label myColor
      putStrLn "========================"
      printBoard board
      putStrLn "========================"
      putStrLn message -- In thông báo từ server (Vd: "Luot cua ban", "Dang cho...")

      -- 3. Nếu đến lượt mình, lấy input và gửi đi
      if turn == myColor && not (take 8 message == "Ban thua" || take 8 message == "Ban thang")
        then do
          putStr (label myColor ++ " di: ")
          flush
          moveInput <- getLine
          hPutStrLn handle moveInput -- Gửi nước đi cho server
          hFlush handle
        else
          -- Không phải lượt mình, chỉ cần chờ
          return ()