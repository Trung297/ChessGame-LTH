module ChessLogic
(
  Color(..), PieceType(..), Piece(..),
  Cell, Board, Position, Move,
  initBoard, getCell, setCell, movePiece,
  validMove, isCheckmate, opponent, possibleMoves
) where

data Color = White | Black deriving (Eq, Show, Read)
data PieceType = King | Queen | Rook | Bishop | Knight | Pawn deriving (Eq, Show, Read)
data Piece = Piece PieceType Color deriving (Eq, Show, Read)
type Cell = Maybe Piece
type Board = [[Cell]]
type Position = (Int, Int)
type Move = (Position, Position)  -- internal use nếu cần

-- Khởi tạo bàn cờ chuẩn
initBoard :: Board
initBoard =
  [ [Just (Piece Rook Black), Just (Piece Knight Black), Just (Piece Bishop Black), Just (Piece Queen Black),
     Just (Piece King Black), Just (Piece Bishop Black), Just (Piece Knight Black), Just (Piece Rook Black)]
  , replicate 8 (Just (Piece Pawn Black))
  , replicate 8 Nothing
  , replicate 8 Nothing
  , replicate 8 Nothing
  , replicate 8 Nothing
  , replicate 8 (Just (Piece Pawn White))
  , [Just (Piece Rook White), Just (Piece Knight White), Just (Piece Bishop White), Just (Piece Queen White),
     Just (Piece King White), Just (Piece Bishop White), Just (Piece Knight White), Just (Piece Rook White)]
  ]

-- Lấy ô tại vị trí (col,row) với (1,1)=a1, (8,8)=h8; board stored row 8..1
getCell :: Board -> Position -> Cell
getCell board (col,row)
  | col>=1 && col<=8 && row>=1 && row<=8 = (board !! (8-row)) !! (col-1)
  | otherwise = Nothing

setCell :: Board -> Position -> Cell -> Board
setCell board (col,row) cell =
  let rIdx = 8 - row
      (beforeRows,rowTarget:afterRows) = splitAt rIdx board
      (beforeCols,_:afterCols) = splitAt (col-1) rowTarget
      newRow = beforeCols ++ [cell] ++ afterCols
  in beforeRows ++ [newRow] ++ afterRows

opponent :: Color -> Color
opponent White = Black
opponent Black = White

-- Kiểm tra đường đi trống (Rook, Bishop, Queen)
isPathClear :: Board -> Position -> Position -> Bool
isPathClear board (x1,y1) (x2,y2) =
  let dx = signum (x2-x1)
      dy = signum (y2-y1)
      positions = takeWhile (/= (x2,y2)) $ iterate (\(x,y) -> (x+dx,y+dy)) (x1+dx,y1+dy)
  in all (\pos -> getCell board pos == Nothing) positions

-- Kiểm tra nước đi hợp lệ cơ bản (không xét chiếu)
validMoveBasic :: Board -> Position -> Position -> Bool
validMoveBasic board from to = case getCell board from of
  Nothing -> False
  Just (Piece pt color) ->
    let pathClear = isPathClear board from to
        basicValid = case pt of
          Pawn -> validPawnMove board from to color
          Rook -> validRookMove from to && pathClear
          Bishop -> validBishopMove from to && pathClear
          Queen -> (validRookMove from to || validBishopMove from to) && pathClear
          Knight -> validKnightMove from to
          King -> validKingMove from to
        targetCell = getCell board to
    in basicValid && (targetCell == Nothing || isOpponentPiece board to color)

-- validMove: không cho phép nước đi khiến chính mình vẫn bị chiếu
validMove :: Board -> Position -> Position -> Bool
validMove board from to =
  case getCell board from of
    Nothing -> False
    Just (Piece _ color) ->
      let basic = validMoveBasic board from to
          newBoard = movePiece board from to
      in basic && not (isInCheck newBoard color)

-- Pawn moves
validPawnMove :: Board -> Position -> Position -> Color -> Bool
validPawnMove board (x1,y1) (x2,y2) color =
  let dir = if color==White then 1 else -1
      startRow = if color==White then 2 else 7
      forwardOne = x1==x2 && y2==y1+dir && getCell board (x2,y2)==Nothing
      forwardTwo = x1==x2 && y1==startRow && y2==y1+2*dir && getCell board (x2,y1+dir)==Nothing && getCell board (x2,y2)==Nothing
      capture = abs (x2-x1)==1 && y2==y1+dir && isOpponentPiece board (x2,y2) color
  in forwardOne || forwardTwo || capture

validRookMove :: Position -> Position -> Bool
validRookMove (x1,y1) (x2,y2) = x1==x2 || y1==y2

validBishopMove :: Position -> Position -> Bool
validBishopMove (x1,y1) (x2,y2) = abs (x2-x1) == abs (y2-y1)

validKnightMove :: Position -> Position -> Bool
validKnightMove (x1,y1) (x2,y2) = (abs (x2-x1), abs (y2-y1)) `elem` [(1,2),(2,1)]

validKingMove :: Position -> Position -> Bool
validKingMove (x1,y1) (x2,y2) = abs (x2-x1) <= 1 && abs (y2-y1) <= 1 && (x1,y1) /= (x2,y2)

isOpponentPiece :: Board -> Position -> Color -> Bool
isOpponentPiece board pos color = case getCell board pos of
  Just (Piece _ c) -> c /= color
  Nothing -> False

-- Kiểm tra 1 ô có bị tấn công bởi attackerColor
isAttacked :: Board -> Position -> Color -> Bool
isAttacked board (x,y) attackerColor =
  any (\pos -> case getCell board pos of
         Just (Piece pt c) | c == attackerColor ->
           let pathClear = isPathClear board pos (x,y)
           in case pt of
                Pawn -> abs (x - fst pos) == 1 && (snd pos + (if c == White then 1 else -1)) == y
                Rook -> validRookMove pos (x,y) && pathClear
                Bishop -> validBishopMove pos (x,y) && pathClear
                Queen -> (validRookMove pos (x,y) || validBishopMove pos (x,y)) && pathClear
                Knight -> validKnightMove pos (x,y)
                King -> validKingMove pos (x,y)
         _ -> False
      ) [(a,b) | a <- [1..8], b <- [1..8]]

-- Tìm vị trí vua của color
findKing :: Board -> Color -> Maybe Position
findKing board color = head' [ (col,row) | row <- [1..8], col <- [1..8], getCell board (col,row) == Just (Piece King color) ]

isInCheck :: Board -> Color -> Bool
isInCheck board color = case findKing board color of
  Nothing -> True
  Just kingPos -> isAttacked board kingPos (opponent color)

-- Thực hiện nước đi (và promotion thành Queen đơn giản)
movePiece :: Board -> Position -> Position -> Board
movePiece board from to =
  let piece = getCell board from
      promoted = case piece of
                   Just (Piece Pawn c) | (snd to == 8 && c == White) || (snd to == 1 && c == Black) -> Just (Piece Queen c)
                   _ -> piece
  in setCell (setCell board from Nothing) to promoted

-- Checkmate / possible moves
possibleMoves :: Board -> Color -> [Move]
possibleMoves board color =
  [ (f,t) | f <- allPositionsOfColor board color, t <- [(x,y) | x <- [1..8], y <- [1..8]], validMove board f t ]

isCheckmate :: Board -> Color -> Bool
isCheckmate board color = isInCheck board color && null (possibleMoves board color)

allPositionsOfColor :: Board -> Color -> [Position]
allPositionsOfColor board color =
  [ (c,r) | r <- [1..8], c <- [1..8], maybe False (\(Piece _ co) -> co == color) (getCell board (c,r)) ]

head' :: [a] -> Maybe a
head' [] = Nothing
head' (x:_) = Just x
