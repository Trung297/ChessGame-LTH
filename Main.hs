module Main where

import ChessLogic
import Control.Concurrent.MVar
import Data.Char (toLower)
import System.IO (hSetEncoding, hFlush, stdout, utf8)
import Text.Read (readMaybe)

-- Hiển thị quân cờ
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

-- In bàn cờ
printBoard :: Board -> IO ()
printBoard board = do
  let rows = zip [8,7..1] board
  mapM_ (\(num,row) -> putStrLn $ show num ++ " " ++ unwords (map showCell row)) rows
  putStrLn "  a b c d e f g h"

-- Parse nước đi
parseMove :: String -> Maybe (Position, Position)
parseMove input =
  case words (map (\c -> if c == '-' then ' ' else c) input) of
    [from,to] -> do
      f <- parsePos from
      t <- parsePos to
      return (f,t)
    _ -> Nothing

parsePos :: String -> Maybe Position
parsePos [col,row] =
  let col' = toLower col
      rowNum = read [row] :: Int
  in if col' >= 'a' && col' <= 'h' && rowNum >=1 && rowNum <=8
     then Just (fromEnum col' - fromEnum 'a' + 1, rowNum)
     else Nothing
parsePos _ = Nothing

-- Xóa toàn bộ màn hình
clearScreen :: IO ()
clearScreen = putStr "\ESC[2J\ESC[H"

flush :: IO ()
flush = hFlush stdout

label :: Color -> String
label White = "Trắng"
label Black = "Đen"

posToStr :: Position -> String
posToStr (col,row) = [toEnum (col + fromEnum 'a' - 1), toEnum (row + fromEnum '0')]

-- MAIN
main :: IO ()
main = do
  hSetEncoding stdout utf8
  clearScreen
  boardVar <- newMVar initBoard
  histVar  <- newMVar []      -- [(from,to,oldCell,color)]
  colorVar <- newMVar White
  putStrLn "===== CHESS GAME ====="
  printBoard initBoard
  putStrLn "Nhập nước đi (vd: a2-a4)"
  loop boardVar histVar colorVar []

-- LOOP
loop :: MVar Board -> MVar [ (Position,Position,Cell,Color) ] -> MVar Color -> [(Maybe String, Maybe String)] -> IO ()
loop boardVar histVar colorVar history = do
  board <- readMVar boardVar
  color <- readMVar colorVar
  putStr (label color ++ " đi: ")
  flush
  input <- getLine

  if null input then loop boardVar histVar colorVar history
  else if take 4 input == "undo"
    then do
      let n = case words input of
                ["undo", x] -> maybe 1 id (readMaybe x :: Maybe Int)
                _ -> 1
      undoMoves boardVar histVar colorVar n
      newBoard <- readMVar boardVar
      clearScreen
      let newHistory = dropRounds n history
      printHistory newHistory
      printBoard newBoard
      loop boardVar histVar colorVar newHistory
    else case parseMove input of
      Nothing -> do
        putStrLn "Sai định dạng, thử lại!"
        loop boardVar histVar colorVar history
      Just (from,to) -> do
        case getCell board from of
          Nothing -> do
            putStrLn "Không có quân ở ô nguồn!"
            loop boardVar histVar colorVar history
          Just (Piece _ pc) ->
            if pc /= color then do
              putStrLn "Không phải quân của bạn!"
              loop boardVar histVar colorVar history
            else if not (validMove board from to) then do
              putStrLn "Nước đi không hợp lệ!"
              loop boardVar histVar colorVar history
            else do
              let moveStr = label color ++ " đi " ++ posToStr from ++ "-" ++ posToStr to
              newBoard <- modifyMVar boardVar $ \_ -> do
                let nb = movePiece board from to
                modifyMVar_ histVar (\h -> return ((from,to,getCell board to,color):h))
                return (nb, nb)
              modifyMVar_ colorVar (\_ -> return (opponent color))

              let newHistory = updateHistory color moveStr history

              -- 🔥 XÓA TOÀN BỘ MÀN HÌNH & HIỂN THỊ LẠI
              clearScreen
              printHistory newHistory
              printBoard newBoard
              putStrLn ""
              putStrLn ("Lượt hiện tại: " ++ label (opponent color))

              if isCheckmate newBoard (opponent color)
                then putStrLn (label color ++ " chiếu hết trong sự ngỡ ngàng của " ++ label (opponent color) ++ "!")
                else loop boardVar histVar colorVar newHistory

-- In lịch sử nước đi (2 dòng / lượt)
printHistory :: [(Maybe String, Maybe String)] -> IO ()
printHistory hist = do
  putStrLn "LỊCH SỬ NƯỚC ĐI:"
  if null hist then putStrLn "Chưa có nước đi nào."
  else mapM_ (\(i,(mw,mb)) -> do
                  putStrLn $ show i ++ ". " ++ maybe "" id mw
                  case mb of
                    Just s -> putStrLn $ "   " ++ s
                    Nothing -> return ()
              ) (zip [1..] (reverse hist))
  putStrLn ""

-- Cập nhật lịch sử theo lượt
updateHistory :: Color -> String -> [(Maybe String, Maybe String)] -> [(Maybe String, Maybe String)]
updateHistory White move hist = (Just move, Nothing) : hist
updateHistory Black move ((mw, Nothing):xs) = (mw, Just move) : xs
updateHistory Black move [] = [(Nothing, Just move)]

-- Xóa bớt n lượt khi undo
dropRounds :: Int -> [(Maybe String, Maybe String)] -> [(Maybe String, Maybe String)]
dropRounds n hist = drop n hist

-- Undo n nước đi
undoMoves :: MVar Board -> MVar [ (Position,Position,Cell,Color) ] -> MVar Color -> Int -> IO ()
undoMoves boardVar histVar colorVar n = modifyMVar_ boardVar $ \board -> do
  hist <- readMVar histVar
  let (toUndo, remain) = splitAt n hist
      restored = foldl (\b (from,to,oldCell,_) -> 
                          let movingPiece = getCell b to
                              b1 = setCell b to oldCell
                              b2 = setCell b1 from movingPiece
                          in b2
                       ) board toUndo
  modifyMVar_ histVar (\_ -> return remain)
  modifyMVar_ colorVar (\c -> return $ if even n then c else opponent c)
  return restored
