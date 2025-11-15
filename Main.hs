{- HLINT ignore "Use hPrint" -}
module Main where

import ChessLogic
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Exception (finally)
import Control.Monad (forever, void)
import Data.Char (toLower)
import Network.Socket
import System.IO
import Text.Read (readMaybe)

-- Định nghĩa trạng thái game để gửi cho client
-- Thêm deriving (Read, Show) để dễ dàng gửi qua socket
data GameState = GameState Board Color String deriving (Show, Read)

data ServerGameState = ServerGameState {
    sgBoard     :: Board,                
    sgColor     :: Color,                 
    
    sgHistory   :: [(Position, Position, Cell, Color)] 
}


port :: String
port = "3000"

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

label :: Color -> String
label White = "Trang"
label Black = "Den"

posToStr :: Position -> String
posToStr (col,row) = [toEnum (col + fromEnum 'a' - 1), toEnum (row + fromEnum '0')]


main :: IO ()
main = withSocketsDo $ do
  hSetEncoding stdout utf8
  addr <- resolve
  putStrLn $ "Dang lang nghe tren port " ++ port ++ "..."
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  bind sock (addrAddress addr)
  listen sock 2 

  (conn1, _peer1) <- accept sock
  h1 <- socketToHandle conn1 ReadWriteMode
  hSetBuffering h1 LineBuffering
  hPutStrLn h1 "White" 
  putStrLn "Nguoi choi 1 (Trang) da ket noi."

  (conn2, _peer2) <- accept sock
  h2 <- socketToHandle conn2 ReadWriteMode
  hSetBuffering h2 LineBuffering
  hPutStrLn h2 "Black" 
  putStrLn "Nguoi choi 2 (Den) da ket noi. Bat dau game!"

  boardVar <- newMVar initBoard
  colorVar <- newMVar White

  let initialState = ServerGameState initBoard White []
  stateVar <- newMVar initialState
  -- Chạy vòng lặp game chính
  let initialBoard = initBoard
  let initialColor = White
  let msgCurrent = "Luot cua ban (" ++ label initialColor ++ ")."
  let msgWaiting = "Dang cho " ++ label initialColor ++ " di..."
  sendState h1 initialBoard initialColor msgCurrent -- Gửi cho White
  sendState h2 initialBoard initialColor msgWaiting -- Gửi cho Black

  
  let handles = (h1, h2)
  finally (gameLoop handles boardVar colorVar) (close sock >> hClose h1 >> hClose h2)

  where
    resolve = do
      let hints = defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Stream }
      head <$> getAddrInfo (Just hints) Nothing (Just port)

gameLoop :: (Handle, Handle) -> MVar Board -> MVar Color -> IO ()
gameLoop (hWhite, hBlack) boardVar colorVar = do
  color <- readMVar colorVar
  let (hCurrent, hWaiting) = if color == White then (hWhite, hBlack) else (hBlack, hWhite)

  input <- hGetLine hCurrent
 
  board <- readMVar boardVar 
  
  case parseMove input of
    Nothing -> do
      sendState hCurrent board color "Nuoc di khong hop le. Thu lai."
      gameLoop (hWhite, hBlack) boardVar colorVar 
    
    Just (from, to) ->
      case getCell board from of
        Nothing -> do
          sendState hCurrent board color "O nguon khong co quan. Thu lai."
          gameLoop (hWhite, hBlack) boardVar colorVar
        
        Just (Piece _ pc) ->
          if pc /= color then do
            sendState hCurrent board color "Khong phai quan cua ban. Thu lai."
            gameLoop (hWhite, hBlack) boardVar colorVar
          
          else if not (validMove board from to) then do
            sendState hCurrent board color "Nuoc di khong hop le (co the bi chieu). Thu lai."
            gameLoop (hWhite, hBlack) boardVar colorVar
          
          else do
            -- Nước đi HỢP LỆ
            let moveStr = label color ++ " di " ++ posToStr from ++ "-" ++ posToStr to
            putStrLn $ "LOG: " ++ moveStr

            -- Cập nhật trạng thái game trên server
            newBoard <- modifyMVar boardVar $ \b -> do
              let nb = movePiece b from to
              return (nb, nb)
            let newColor = opponent color
            modifyMVar_ colorVar (\_ -> return newColor)

            -- Kiểm tra chiếu hết
            if isCheckmate newBoard newColor
              then do
                putStrLn $ "LOG: Chieu het! " ++ label color ++ " thang!"
                let msgWin = "Ban thang! " ++ label color ++ " chieu het " ++ label newColor ++ "!"
                let msgLose = "Ban thua! " ++ label color ++ " chieu het " ++ label newColor ++ "!"
                -- Gửi thông báo kết thúc
                sendState hCurrent newBoard newColor msgWin
                sendState hWaiting newBoard newColor msgLose
                -- KẾT THÚC GAME: không gọi đệ quy nữa
                return ()
              else do
                -- Nước đi hợp lệ, KHÔNG chiếu hết
                -- GỬI TRẠNG THÁI MỚI CHO CẢ 2 CLIENT
                let msgCurrentNew = "Luot cua ban (" ++ label newColor ++ ")."
                let msgWaitingNew = "Dang cho " ++ label newColor ++ " di..."
                
                sendState hWaiting newBoard newColor msgCurrentNew -- Gửi cho người SẮP đi
                sendState hCurrent newBoard newColor msgWaitingNew -- Gửi cho người VỪA đi
                
                -- Lặp lại vòng lặp để chờ nước đi tiếp theo
                gameLoop (hWhite, hBlack) boardVar colorVar

-- Hàm trợ giúp gửi trạng thái cho client
sendState :: Handle -> Board -> Color -> String -> IO ()
sendState h b c msg = do
  hPutStrLn h (show (GameState b c msg))
  hFlush h