module Checkers.Moves where

import Checkers.Types



-- master function for all things related to moves, if there are jump moves, one must make jump moves, 
-- else if there are simple moves, then one must make those, otherwise you have no moves left, sorry 
moves :: GameState -> SMorJM [Move]
moves g
  | not (null (jumpMoves g)) = JM (jumpMoves g)
  | not (null (simpleMoves g)) =  SM (simpleMoves g)
  | otherwise = EndM

-- if you want moves but don't care if they're jump or simple or non-existent
moves' :: SMorJM [Move] -> [Move]
moves' (JM moves) = moves
moves' (SM moves) = moves
moves' EndM = []

--similarly sometimes you dont want to know if its a King or Pawn , equality yay!
unPorK :: PorK Coord -> Coord
unPorK (P xy) = xy 
unPorK (K xy) = xy

--one-stop place for all the simple moves available, depending on the turn, generate appropriate moves
simpleMoves :: GameState -> [Move]
simpleMoves g = case status g of
  RedPlayer -> rsimple g
  BlackPlayer -> bsimple g
  _ -> []

--returns a list of all red simple moves - all red pawn moves and all red king moves (with no repeated moves)
rsimple :: GameState -> [Move]
rsimple g = rpawn g (redPieces g) [] ++ noRepeatList g (rking g (redKings g) [])

--returns a list of all black simple moves - all black pawn moves and all black king moves (with no repeated moves)
bsimple :: GameState -> [Move]
bsimple g = bpawn g (blackPieces g) [] ++ noRepeatList g ( bking g (blackKings g) [])

-- returns a list of all viable moves that can be made by the red kings on the board if any, does not remove any repeated states in this method
-- checks if the diagonally up left, up right, down left and down right blocks are empty and then adds those moves to the list 
-- just a lot of guards to check all the conditions are met 
rking :: GameState -> PieceState -> [Move] -> [Move]
rking g [] m = m
rking g ((x,y):xs) m | upRight g (x,y) && upLeft g (x,y) && downRight g (x,y) && downLeft g (x,y) = rking g xs ([K (x,y),K (x+1,y-1)]:[K (x,y),K (x-1,y-1)]:[K(x,y),K(x+1,y+1)]:[K(x,y),K (x-1,y+1)]:m)
                     | upRight g (x,y) && upLeft g (x,y) && downRight g (x,y)  = rking g xs ([K (x,y),K (x+1,y-1)]:[K (x,y),K (x-1,y-1)]:[K(x,y),K(x+1,y+1)]:m)
                     | upRight g (x,y) && upLeft g (x,y) && downLeft g (x,y) = rking g xs ([K (x,y),K (x+1,y-1)]:[K (x,y),K (x-1,y-1)]:[K(x,y),K (x-1,y+1)]:m)
                     | upRight g (x,y) && upLeft g (x,y) = rking g xs ([K (x,y),K (x+1,y-1)]:[K (x,y),K (x-1,y-1)]:m)
                     | upRight g (x,y) && downRight g (x,y) && downLeft g (x,y) = rking g xs ([K (x,y),K (x+1,y-1)]:[K(x,y),K(x+1,y+1)]:[K(x,y),K (x-1,y+1)]:m)
                     | upRight g (x,y) && downRight g (x,y) = rking g xs ([K (x,y),K (x+1,y-1)]:[K(x,y),K(x+1,y+1)]:m)
                     | upRight g (x,y) && downLeft g (x,y) = rking g xs ([K (x,y),K (x+1,y-1)]:[K(x,y),K(x-1,y+1)]:m)
                     | upLeft g (x,y) && downRight g (x,y) && downLeft g (x,y) = rking g xs ([K (x,y),K (x-1,y-1)]:[K(x,y),K(x+1,y+1)]:[K(x,y),K (x-1,y+1)]:m)
                     | upLeft g (x,y) && downRight g (x,y) = rking g xs ([K (x,y),K (x-1,y-1)]:[K(x,y),K(x+1,y+1)]:m)
                     | upLeft g (x,y) && downLeft g (x,y) = rking g xs ([K (x,y),K (x-1,y-1)]:[K(x,y),K(x-1,y+1)]:m)
                     | downRight g (x,y) && downLeft g (x,y) = rking g xs ([K(x,y),K(x+1,y+1)]:[K(x,y),K (x-1,y+1)]:m)
                     | upRight g (x,y) = rking g xs ([K (x,y),K (x+1,y-1)]:m)
                     | upLeft  g (x,y) = rking g xs ([K (x,y),K (x-1,y-1)]:m)
                     | downRight g (x,y) = rking g xs ([K (x,y),K (x+1,y+1)]:m)
                     | downLeft g (x,y) = rking g xs ([K (x,y),K (x-1,y+1)]:m)
                     | otherwise = rking g xs m

-- takes in the gamestate and a list of moves, then filters out the moves that cause repeated states in the game. 
-- returns all the simple king moves that don't cause repeated states
noRepeatList::  GameState  -> [Move] -> [Move]
noRepeatList g [] = []
noRepeatList g (m:moves) | noRepeat (history g) m = m:noRepeatList g moves
                         | otherwise = noRepeatList g moves

-- this method is just rking in disguise
-- returns a list of all viable moves that can be made by the black kings on the board if any, does not remove any repeated states in this method
-- checks if the diagonally up left, up right, down left and down right blocks are empty and then adds those moves to the list 
-- just a lot of guards to check all the conditions are met  
bking :: GameState -> PieceState -> [Move] -> [Move]
bking g [] m = m
bking g ((x,y):xs) m | upRight g (x,y) && upLeft g (x,y) && downRight g (x,y) && downLeft g (x,y) = bking g xs ([K (x,y),K (x+1,y-1)]:[K (x,y),K (x-1,y-1)]:[K(x,y),K(x+1,y+1)]:[K(x,y),K (x-1,y+1)]:m)
                     | upRight g (x,y) && upLeft g (x,y) && downRight g (x,y)  = bking g xs ([K (x,y),K (x+1,y-1)]:[K (x,y),K (x-1,y-1)]:[K(x,y),K(x+1,y+1)]:m)
                     | upRight g (x,y) && upLeft g (x,y) && downLeft g (x,y) = bking g xs ([K (x,y),K (x+1,y-1)]:[K (x,y),K (x-1,y-1)]:[K(x,y),K (x-1,y+1)]:m)
                     | upRight g (x,y) && upLeft g (x,y) = bking g xs ([K (x,y),K (x+1,y-1)]:[K (x,y),K (x-1,y-1)]:m)
                     | upRight g (x,y) && downRight g (x,y) && downLeft g (x,y) = bking g xs ([K (x,y),K (x+1,y-1)]:[K(x,y),K(x+1,y+1)]:[K(x,y),K (x-1,y+1)]:m)
                     | upRight g (x,y) && downRight g (x,y) = bking g xs ([K (x,y),K (x+1,y-1)]:[K(x,y),K(x+1,y+1)]:m)
                     | upRight g (x,y) && downLeft g (x,y) = bking g xs ([K (x,y),K (x+1,y-1)]:[K(x,y),K(x-1,y+1)]:m)
                     | upLeft g (x,y) && downRight g (x,y) && downLeft g (x,y) = bking g xs ([K (x,y),K (x-1,y-1)]:[K(x,y),K(x+1,y+1)]:[K(x,y),K (x-1,y+1)]:m)
                     | upLeft g (x,y) && downRight g (x,y) = bking g xs ([K (x,y),K (x-1,y-1)]:[K(x,y),K(x+1,y+1)]:m)
                     | upLeft g (x,y) && downLeft g (x,y) = bking g xs ([K (x,y),K (x-1,y-1)]:[K(x,y),K(x-1,y+1)]:m)
                     | downRight g (x,y) && downLeft g (x,y) = bking g xs ([K(x,y),K(x+1,y+1)]:[K(x,y),K (x-1,y+1)]:m)
                     | upRight g (x,y) = bking g xs ([K (x,y),K (x+1,y-1)]:m)
                     | upLeft  g (x,y) = bking g xs ([K (x,y),K (x-1,y-1)]:m)
                     | downRight g (x,y) = bking g xs ([K (x,y),K (x+1,y+1)]:m)
                     | downLeft g (x,y) = bking g xs ([K (x,y),K (x-1,y+1)]:m)
                     | otherwise = bking g xs m 


-- takes in gamestate , list of black pawns , and an empty list of moves
-- checks if the cells at the downright and downleft positions diagonally are empty and then adds in those moves to the moves list
-- just a lot of guards to check all the conditions are met 
bpawn :: GameState -> PieceState -> [Move] -> [Move]
bpawn g [] m = m
bpawn g ((x, y) : xs) m
  | downRight g (x, y) && downLeft g (x, y) = bpawn g xs ([P (x, y), P (x - 1, y + 1)] : [P (x, y), P (x + 1, y + 1)] : m)
  | downRight g (x, y) = bpawn g xs ([P (x, y), P (x + 1, y + 1)] : m)
  | downLeft g (x, y) = bpawn g xs ([P (x, y), P (x - 1, y + 1)] : m)
  | otherwise = bpawn g xs m

-- takes in gamestate , list of red pawns , and an empty list of moves
-- checks if the cells at the downright and downleft positions diagonally are empty and then adds in those moves to the moves list
-- just a lot of guards to check all the conditions are met 
rpawn :: GameState -> PieceState -> [Move] -> [Move]
rpawn g [] m = m
rpawn g ((x, y) : xs) m
  | upRight g (x, y) && upLeft g (x, y) = rpawn g xs ([P (x, y), P (x - 1, y - 1)] : [P (x, y), P (x + 1, y - 1)] : m)
  | upRight g (x, y) = rpawn g xs ([P (x, y), P (x + 1, y - 1)] : m)
  | upLeft g (x, y) = rpawn g xs ([P (x, y), P (x - 1, y - 1)]:m )
  | otherwise = rpawn g xs m



--now we come to the more challenging segment - le jump moves
-- depending on status get all the jump moves from the pawns and the kings
-- don't have to worry baout repeated states here :)
jumpMoves :: GameState -> [Move]
jumpMoves g = case status g of 
  RedPlayer -> rAllJumps g (redPieces g) [] ++ rKingAllJumps g (redKings g)
  BlackPlayer -> bAllJumps g (blackPieces g) [] ++ bKingAllJumps g (blackKings g) []


--function to get the pawn jump moves for the red side
--takes in gamestate and all the red pawn positions and an empty list of moves
-- returns the completed list of potential jump moves
--checks if there are any jumps for a piece and if not recurses on the rest of the list
--if there are jump moves then it addes them to the moves list

rAllJumps :: GameState -> PieceState -> [Move] -> [Move]
rAllJumps s [] rem = rem 
rAllJumps s ((x,y):xs) rem  | rJumps s (x,y) [] == [[P (x,y)]] = rAllJumps s xs rem
                            | otherwise = rAllJumps s xs rem ++ rJumps s (x,y) [] 

--function to get the jumps for one red pawn
--takes in gamestate and one red pawn position and an empty move
-- returns the completed list of potential jump moves for that one pawn
--uses removePieces to remove the start position of the jump and the piece that was jumped over
-- if y-2==0 that is the piece reaches the back row of the black side, convert the piece to a king and check for jumps
rJumps :: GameState -> Coord -> Move-> [Move]
rJumps g (x,y) rem   | upLeftJump g (x,y) && upRightJump g (x,y) && y-2 == 0 = rKingJumps (removePieces g (x,y) (x-2,y-2)) (x-2,y-2) (rem ++ [P (x,y)]) ++ rKingJumps (removePieces g (x,y) (x+2,y-2)) (x+2,y-2) ( rem ++ [P (x,y)])
                     | upLeftJump g (x,y) && upRightJump g (x,y) = rJumps (removePieces g (x,y) (x-2,y-2)) (x-2,y-2) (rem ++ [P(x,y)]) ++ rJumps (removePieces g (x,y) (x+2,y-2)) (x+2,y-2) (rem ++ [P(x,y)]) 
                     | upRightJump g (x,y) && (y-2==0) = rKingJumps (removePieces g (x,y) (x+2,y-2)) (x+2,y-2) (rem ++ [P (x,y)]) 
                     | upRightJump g (x,y) = rJumps (removePieces g (x,y) (x+2,y-2)) (x+2,y-2) (rem ++ [P(x,y)]) 
                     | upLeftJump g (x,y) && (y-2==0) = rKingJumps (removePieces g (x,y) (x-2,y-2)) (x-2,y-2) (rem ++ [P (x,y)]) 
                     | upLeftJump g (x,y) = rJumps (removePieces g (x,y) (x-2,y-2)) (x-2,y-2) (rem ++ [P(x,y)]) 
                     | otherwise = [rem ++ [P(x,y)]]

--function to get the king jump moves for the red side
--takes in gamestate and all the red king positions and an empty list of moves
-- returns the completed list of potential jump moves
--checks if there are any jumps for a piece and if not recurses on the rest of the list
--if there are jump moves then it addes them to the moves list
rKingAllJumps :: GameState -> PieceState -> [Move]
rKingAllJumps g [] = []
rKingAllJumps g ((x,y):xs) | rKingJumps g (x,y) [] == [[K (x,y)]] = rKingAllJumps g xs 
                           | otherwise = rKingAllJumps g xs ++ rKingJumps g (x,y) [] 

--function to get the jumps for one red king
--takes in gamestate and one red king position and an empty move
-- returns the completed list of potential jump moves for that one pawn
--uses removePieces to remove the start position of the jump and the piece that was jumped over

rKingJumps :: GameState -> Coord -> Move -> [Move]
rKingJumps g (x,y) rem | upLeftJump g (x,y) && upRightJump g (x,y) && downRightJump g (x,y) && downLeftJump g (x,y) = rKingJumps (removePieces g (x,y) (x-2,y-2)) (x-2,y-2) (rem ++ [K (x,y)]) ++ rKingJumps (removePieces g (x,y) (x+2,y-2)) (x+2,y-2) (rem ++ [K (x,y)]) ++ rKingJumps (removePieces g (x,y) (x+2,y+2)) (x+2,y+2) (rem ++ [K (x,y)]) ++ rKingJumps (removePieces g (x,y) (x-2,y+2)) (x-2,y+2) (rem ++ [K (x,y)]) 
                       | upLeftJump g (x,y) && upRightJump g (x,y) && downRightJump g (x,y) = rKingJumps (removePieces g (x,y) (x-2,y-2)) (x-2,y-2) (rem ++ [K (x,y)]) ++ rKingJumps (removePieces g (x,y) (x+2,y-2)) (x+2,y-2) (rem ++ [K (x,y)]) ++ rKingJumps (removePieces g (x,y) (x+2,y+2)) (x+2,y+2) (rem ++ [K (x,y)])
                       | upLeftJump g (x,y) && upRightJump g (x,y) && downLeftJump g (x,y) = rKingJumps (removePieces g (x,y) (x-2,y-2)) (x-2,y-2) (rem ++ [K (x,y)]) ++ rKingJumps (removePieces g (x,y) (x+2,y-2)) (x+2,y-2) (rem ++ [K (x,y)]) ++ rKingJumps (removePieces g (x,y) (x-2,y+2)) (x-2,y+2) (rem ++ [K (x,y)])
                       | upLeftJump g (x,y) && upRightJump g (x,y) = rKingJumps (removePieces g (x,y) (x-2,y-2)) (x-2,y-2) (rem ++ [K (x,y)]) ++ rKingJumps (removePieces g (x,y) (x+2,y-2)) (x+2,y-2) (rem ++ [K (x,y)]) 
                       | upLeftJump g (x,y) && downRightJump g (x,y) && downLeftJump g (x,y) = rKingJumps (removePieces g (x,y) (x-2,y-2)) (x-2,y-2) (rem ++ [K (x,y)]) ++ rKingJumps (removePieces g (x,y) (x+2,y+2)) (x+2,y+2) (rem ++ [K (x,y)]) ++ rKingJumps (removePieces g (x,y) (x-2,y+2)) (x-2,y+2) (rem ++ [K (x,y)]) 
                       | upLeftJump g (x,y) && downRightJump g (x,y) = rKingJumps (removePieces g (x,y) (x-2,y-2)) (x-2,y-2) (rem ++ [K (x,y)]) ++ rKingJumps (removePieces g (x,y) (x+2,y+2)) (x+2,y+2) (rem ++ [K (x,y)]) 
                       | upLeftJump g (x,y) && downLeftJump g (x,y) = rKingJumps (removePieces g (x,y) (x-2,y-2)) (x-2,y-2) (rem ++ [K (x,y)]) ++  rKingJumps (removePieces g (x,y) (x-2,y+2)) (x-2,y+2) (rem ++ [K (x,y)]) 
                       | upLeftJump g (x,y) = rKingJumps (removePieces g (x,y) (x-2,y-2)) (x-2,y-2) (rem ++ [K (x,y)])
                       | upRightJump g (x,y) && downRightJump g (x,y) && downLeftJump g (x,y) = rKingJumps (removePieces g (x,y) (x+2,y-2)) (x+2,y-2) (rem ++ [K (x,y)]) ++ rKingJumps (removePieces g (x,y) (x+2,y+2)) (x+2,y+2) (rem ++ [K (x,y)]) ++ rKingJumps (removePieces g (x,y) (x-2,y+2)) (x-2,y+2) (rem ++ [K (x,y)]) 
                       | upRightJump g (x,y) && downRightJump g (x,y) = rKingJumps (removePieces g (x,y) (x+2,y-2)) (x+2,y-2) (rem ++ [K (x,y)]) ++ rKingJumps (removePieces g (x,y) (x+2,y+2)) (x+2,y+2) (rem ++ [K (x,y)])
                       | upRightJump g (x,y) && downLeftJump g (x,y) = rKingJumps (removePieces g (x,y) (x+2,y-2)) (x+2,y-2) (rem ++ [K (x,y)]) ++ rKingJumps (removePieces g (x,y) (x-2,y+2)) (x-2,y+2) (rem ++ [K (x,y)])
                       | upRightJump g (x,y) = rKingJumps (removePieces g (x,y) (x+2,y-2)) (x+2,y-2) (rem ++ [K (x,y)])
                       | downRightJump g (x,y) && downLeftJump g (x,y) = rKingJumps (removePieces g (x,y) (x+2,y+2)) (x+2,y+2) (rem ++ [K (x,y)]) ++ rKingJumps (removePieces g (x,y) (x-2,y+2)) (x-2,y+2) (rem ++ [K (x,y)]) 
                       | downRightJump g (x,y) = rKingJumps (removePieces g (x,y) (x+2,y+2)) (x+2,y+2) (rem ++ [K (x,y)])
                       | downLeftJump g (x,y) = rKingJumps (removePieces g (x,y) (x-2,y+2)) (x-2,y+2) (rem ++ [K (x,y)])
                       | otherwise = [rem ++ [K(x,y)]] 


--function to get the pawn jump moves for the black side
--takes in gamestate and all the black pawn positions and an empty list of moves
-- returns the completed list of potential jump moves
--checks if there are any jumps for a piece and if not recurses on the rest of the list
--if there are jump moves then it addes them to the moves list

bAllJumps :: GameState -> PieceState -> [Move] -> [Move]
bAllJumps s [] rem = rem 
bAllJumps s ((x,y):xs) rem  | bJumps s (x,y) [] == [[P (x,y)]] = bAllJumps s xs rem
                            | otherwise = bAllJumps s xs rem ++ bJumps s (x,y) []


--function to get the jumps for one black pawn
--takes in gamestate and one black pawn position and an empty move
-- returns the completed list of potential jump moves for that one pawn
--uses removePieces to remove the start position of the jump and the piece that was jumped over

bJumps :: GameState -> Coord -> Move-> [Move]
bJumps g (x,y) rem   | downLeftJump g (x,y) && downRightJump g (x,y) && y+2 == 7 = bKingJumps (removePieces g (x,y) (x-2,y+2)) (x-2,y+2) (rem ++ [P (x,y)]) ++ bKingJumps (removePieces g (x,y) (x+2,y+2)) (x+2,y+2) ( rem ++ [P (x,y)])
                     | downLeftJump g (x,y) && downRightJump g (x,y) = bJumps (removePieces g (x,y) (x-2,y+2)) (x-2,y+2) (rem ++ [P(x,y)]) ++ rJumps (removePieces g (x,y) (x+2,y+2)) (x+2,y+2) (rem ++ [P(x,y)]) 
                     | downRightJump g (x,y) && (y+2==7) = bKingJumps (removePieces g (x,y) (x+2,y+2)) (x+2,y+2) (rem ++ [P(x,y)]) 
                     | downRightJump g (x,y) = bJumps (removePieces g (x,y) (x+2,y+2)) (x+2,y+2) (rem ++ [P(x,y)]) 
                     | downLeftJump g (x,y) && (y+2==7) = bKingJumps (removePieces g (x,y) (x-2,y+2)) (x-2,y+2) (rem ++ [P(x,y)]) 
                     | downLeftJump g (x,y) = bJumps (removePieces g (x,y) (x-2,y+2)) (x-2,y+2) (rem ++ [P(x,y)]) 
                     | otherwise = [rem ++ [P(x,y)]]


--function to get the king jump moves for the black side
--takes in gamestate and all the black king positions and an empty list of moves
-- returns the completed list of potential jump moves
--checks if there are any jumps for a piece and if not recurses on the rest of the list
--if there are jump moves then it addes them to the moves list
bKingAllJumps :: GameState -> PieceState -> [Move] -> [Move]
bKingAllJumps g [] rem = rem 
bKingAllJumps g ((x,y):xs) rem | bKingJumps g (x,y) [] == [[K (x,y)]] = bKingAllJumps g xs rem 
                               | otherwise = bKingAllJumps g xs rem ++ bKingJumps g (x,y) [] 

--function to get the jumps for one black king
--takes in gamestate and one black king position and an empty move
-- returns the completed list of potential jump moves for that one pawn
--uses removePieces to remove the start position of the jump and the piece that was jumped over
bKingJumps :: GameState -> Coord -> Move -> [Move]
bKingJumps g (x,y) rem | upLeftJump g (x,y) && upRightJump g (x,y) && downRightJump g (x,y) && downLeftJump g (x,y) = bKingJumps (removePieces g (x,y) (x-2,y-2)) (x-2,y-2) (rem ++ [K (x,y)]) ++ bKingJumps (removePieces g (x,y) (x+2,y-2)) (x+2,y-2) (rem ++ [K (x,y)]) ++ bKingJumps (removePieces g (x,y) (x+2,y+2)) (x+2,y+2) (rem ++ [K (x,y)]) ++ bKingJumps (removePieces g (x,y) (x-2,y+2)) (x-2,y+2) (rem ++ [K (x,y)]) 
                       | upLeftJump g (x,y) && upRightJump g (x,y) && downRightJump g (x,y) = bKingJumps (removePieces g (x,y) (x-2,y-2)) (x-2,y-2) (rem ++ [K (x,y)]) ++ bKingJumps (removePieces g (x,y) (x+2,y-2)) (x+2,y-2) (rem ++ [K (x,y)]) ++ bKingJumps (removePieces g (x,y) (x+2,y+2)) (x+2,y+2) (rem ++ [K (x,y)])
                       | upLeftJump g (x,y) && upRightJump g (x,y) && downLeftJump g (x,y) = bKingJumps (removePieces g (x,y) (x-2,y-2)) (x-2,y-2) (rem ++ [K (x,y)]) ++ bKingJumps (removePieces g (x,y) (x+2,y-2)) (x+2,y-2) (rem ++ [K (x,y)]) ++ bKingJumps (removePieces g (x,y) (x-2,y+2)) (x-2,y+2) (rem ++ [K (x,y)])
                       | upLeftJump g (x,y) && upRightJump g (x,y) = bKingJumps (removePieces g (x,y) (x-2,y-2)) (x-2,y-2) (rem ++ [K (x,y)]) ++ bKingJumps (removePieces g (x,y) (x+2,y-2)) (x+2,y-2) (rem ++ [K (x,y)]) 
                       | upLeftJump g (x,y) && downRightJump g (x,y) && downLeftJump g (x,y) = bKingJumps (removePieces g (x,y) (x-2,y-2)) (x-2,y-2) (rem ++ [K (x,y)]) ++ bKingJumps (removePieces g (x,y) (x+2,y+2)) (x+2,y+2) (rem ++ [K (x,y)]) ++ bKingJumps (removePieces g (x,y) (x-2,y+2)) (x-2,y+2) (rem ++ [K (x,y)]) 
                       | upLeftJump g (x,y) && downRightJump g (x,y) = bKingJumps (removePieces g (x,y) (x-2,y-2)) (x-2,y-2) (rem ++ [K (x,y)]) ++ bKingJumps (removePieces g (x,y) (x+2,y+2)) (x+2,y+2) (rem ++ [K (x,y)]) 
                       | upLeftJump g (x,y) && downLeftJump g (x,y) = bKingJumps (removePieces g (x,y) (x-2,y-2)) (x-2,y-2) (rem ++ [K (x,y)]) ++  bKingJumps (removePieces g (x,y) (x-2,y+2)) (x-2,y+2) (rem ++ [K (x,y)]) 
                       | upLeftJump g (x,y) = bKingJumps (removePieces g (x,y) (x-2,y-2)) (x-2,y-2) (rem ++ [K (x,y)])
                       | upRightJump g (x,y) && downRightJump g (x,y) && downLeftJump g (x,y) = bKingJumps (removePieces g (x,y) (x+2,y-2)) (x+2,y-2) (rem ++ [K (x,y)]) ++ bKingJumps (removePieces g (x,y) (x+2,y+2)) (x+2,y+2) (rem ++ [K (x,y)]) ++ bKingJumps (removePieces g (x,y) (x-2,y+2)) (x-2,y+2) (rem ++ [K (x,y)]) 
                       | upRightJump g (x,y) && downRightJump g (x,y) = bKingJumps (removePieces g (x,y) (x+2,y-2)) (x+2,y-2) (rem ++ [K (x,y)]) ++ bKingJumps (removePieces g (x,y) (x+2,y+2)) (x+2,y+2) (rem ++ [K (x,y)])
                       | upRightJump g (x,y) && downLeftJump g (x,y) = bKingJumps (removePieces g (x,y) (x+2,y-2)) (x+2,y-2) (rem ++ [K (x,y)]) ++ bKingJumps (removePieces g (x,y) (x-2,y+2)) (x-2,y+2) (rem ++ [K (x,y)])
                       | upRightJump g (x,y) = bKingJumps (removePieces g (x,y) (x+2,y-2)) (x+2,y-2) (rem ++ [K (x,y)])
                       | downRightJump g (x,y) && downLeftJump g (x,y) = bKingJumps (removePieces g (x,y) (x+2,y+2)) (x+2,y+2) (rem ++ [K (x,y)]) ++ bKingJumps (removePieces g (x,y) (x-2,y+2)) (x-2,y+2) (rem ++ [K (x,y)]) 
                       | downRightJump g (x,y) = bKingJumps (removePieces g (x,y) (x+2,y+2)) (x+2,y+2) (rem ++ [K (x,y)])
                       | downLeftJump g (x,y) = bKingJumps (removePieces g (x,y) (x-2,y+2)) (x-2,y+2) (rem ++ [K (x,y)])
                       | otherwise = [rem ++ [K(x,y)]] 










-- Helper functions to achieve simple diagonal movement --


--is the cell thats above the piece and to the left a viable place to move, lets find out
--if there is no piece in that cell at all, then yes, we can move there and its still on the board, dont want to fall off
upLeft :: GameState -> Coord -> Bool
upLeft g (x, y)
  | notElem (x - 1, y - 1) (redPieces g) && notElem (x - 1, y - 1) (redKings g)
      && notElem (x - 1, y - 1) (blackPieces g)
      && notElem (x - 1, y - 1) (blackKings g)
      && x - 1 >= 0
      && y - 1 >= 0 =
    True
  | otherwise = False

--similarly check the cell above and to the right
upRight :: GameState -> Coord -> Bool
upRight g (x, y)
  | notElem (x + 1, y - 1) (redPieces g) && notElem (x + 1, y - 1) (redKings g)
      && notElem (x + 1, y - 1) (blackPieces g)
      && notElem (x + 1, y - 1) (blackKings g)
      && x + 1 <= 7
      && y - 1 >= 0 =
    True
  | otherwise = False

-- and the cell below and to the left
downLeft :: GameState -> Coord -> Bool
downLeft g (x, y)
  | notElem (x - 1, y + 1) (redPieces g) && notElem (x - 1, y + 1) (redKings g)
      && notElem (x - 1, y + 1) (blackPieces g)
      && notElem (x - 1, y + 1) (blackKings g)
      && x - 1 >= 0
      && y + 1 <= 7 =
    True
  | otherwise = False

--and below and to the right
downRight :: GameState -> Coord -> Bool
downRight g (x, y)
  | notElem (x + 1, y + 1) (redPieces g) && notElem (x + 1, y + 1) (redKings g)
      && notElem (x + 1, y + 1) (blackPieces g)
      && notElem (x + 1, y + 1) (blackKings g)
      && x + 1 <= 7
      && y + 1 <= 7 =
    True
  | otherwise = False

-- end of the simple movement helper functions -- 


-- function takes in gamestate, two coordinates and returns a gamestate
-- it removes the first coordinate from the side whose turn it is
-- and removes the midpoint of the coords as in the piece jumped over from the opposing side
removePieces :: GameState -> Coord -> Coord -> GameState
removePieces g (x,y) (x1,y1) | status g == RedPlayer = g { blackPieces = remove (blackPieces g) (midCoord (x,y) (x1,y1)),
                                                           blackKings = remove (blackKings g) (midCoord (x,y) (x1,y1)),
                                                           redKings = remove (redKings g) (x,y),
                                                           redPieces = remove (redPieces g) (x,y)}
                             | status g == BlackPlayer = g {redPieces = remove (redPieces g) (midCoord (x,y) (x1,y1)),
                                                           redKings = remove (redKings g) (midCoord (x,y) (x1,y1)),
                                                           blackKings = remove (blackKings g) (x,y),
                                                           blackPieces = remove (blackPieces g) (x,y)}
                             | otherwise = g

--gets middle coordinate 
midCoord :: Coord -> Coord -> Coord
midCoord (x,y) (x1,y1) = ((x+x1) `div` 2 , (y+y1) `div` 2)

-- filters out all the pieces that are the same as the coordinate given
remove :: PieceState -> Coord -> PieceState
remove [] _= []
remove list coord = filter (`compareCoord` coord) list

--compares two coordinates
compareCoord :: Coord -> Coord -> Bool
compareCoord (x,y) (x1,y1) = not ((x==x1) && (y==y1))


-- Helper functions for the Jump moves--
-- these fucntions check the diagonally adjacent cell to see if its occupied then check the next cell to see if its unoccupied and if thats the case and the cell is still within bounds
-- then tehy return true
                                                    
upLeftJump :: GameState -> Coord -> Bool
upLeftJump g (x,y) | status g ==  RedPlayer && (x-2 >=0) && (y-2 >=0 ) && notElem (x-2,y-2) (redPieces g) && notElem (x-2,y-2) (redKings g) && notElem (x-2,y-2) (blackPieces g) && notElem (x-2,y-2) (blackKings g) && (elem (x-1,y-1) (blackPieces g) || elem (x-1,y-1) (blackKings g)) = True
                   | status g == BlackPlayer && (x-2 >=0) && (y-2 >=0 )&& notElem (x-2,y-2) (redPieces g) && notElem (x-2,y-2) (redKings g) && notElem (x-2,y-2) (blackPieces g) && notElem (x-2,y-2) (blackKings g) && (elem (x-1,y-1) (redPieces g) || elem (x-1,y-1) (redKings g)) = True
                   | otherwise = False
upRightJump :: GameState -> Coord -> Bool
upRightJump g (x,y) | status g == RedPlayer && (x+2 <=7) && (y-2 >=0 ) && notElem (x+2,y-2) (redPieces g) && notElem (x+2,y-2) (redKings g) && notElem (x+2,y-2) (blackPieces g) && notElem (x+2,y-2) (blackKings g)  && (elem (x+1,y-1) (blackPieces g) || elem (x+1,y-1) (blackKings g) )= True
                   | status g == BlackPlayer && (x+2 <=7) && (y-2 >=0 ) && notElem (x+2,y-2) (redPieces g) && notElem (x+2,y-2) (redKings g) && notElem (x+2,y-2) (blackPieces g) && notElem (x+2,y-2) (blackKings g) && ( elem (x+1,y-1) (redPieces g) || elem (x+1,y-1) (redKings g)) = True
                   | otherwise = False

downLeftJump :: GameState -> Coord -> Bool 
downLeftJump g (x,y) | status g == RedPlayer  && (x-2 >=0) && (y+2 <=7 ) && notElem (x-2,y+2) (redPieces g) && notElem (x-2,y+2) (redKings g) && notElem (x-2,y+2) (blackPieces g) && notElem (x-2,y+2) (blackKings g) &&  (elem (x-1,y+1) (blackPieces g) || elem (x-1,y+1) (blackKings g)) = True
                     | status g == BlackPlayer && (x-2 >=0) && (y+2 <=7 ) && notElem (x-2,y+2) (redPieces g) && notElem (x-2,y+2) (redKings g) && notElem (x-2,y+2) (blackPieces g) && notElem (x-2,y+2) (blackKings g) &&  (elem (x-1,y+1) (redPieces g) || elem (x-1,y+1) (redKings g) )= True
                     | otherwise = False

downRightJump :: GameState -> Coord -> Bool 
downRightJump g (x,y) | status g == RedPlayer  && (x+2 <=7) && (y+2 <=7 )  && notElem (x+2,y+2) (redPieces g) && notElem (x+2,y+2) (redKings g) && notElem (x+2,y+2) (blackPieces g) && notElem (x+2,y+2) (blackKings g) && (elem (x+1,y+1) (blackPieces g) || elem (x+1,y+1) (blackKings g) )= True
                      | status g == BlackPlayer && (x+2 <=7) && (y+2 <=7 )  && notElem (x+2,y+2) (redPieces g) && notElem (x+2,y+2) (redKings g) && notElem (x+2,y+2) (blackPieces g) && notElem (x+2,y+2) (blackKings g) && (elem (x+1,y+1) (redPieces g) || elem (x+1,y+1) (redKings g)) = True
                      | otherwise = False


--end of jump helper fucntions--

--sometimes you just want the coordinate without pattern matching and without knowing the piece
unPorKMoves:: Move -> [Coord]
unPorKMoves = map unPorK

--sometimes you just want a list of lists of coordinates without pattern matching and without knowing the piece
unPorKList :: [Move] -> [[Coord]]
unPorKList = map unPorKMoves


--the actually challenging part  of this assignment - REPEATED STATES!!!!--

-- this algorithm is taken from tutorial 10 of the winter 2021 semester and then fixed to work with the modified conditions 

-- function takes in a list of moves and a candidate move and returns a bool
-- it returns a false if history algorithm returns false
-- otherwise its true
-- we check if the first move of the history is a king move, otherwise we dont need to do anything
-- we then call historyAlgo with a list of moves where the candidate move is appended to the history except the most latest move
-- the most latest move goes in as a separate list of moves
noRepeat:: [Move] -> Move -> Bool
noRepeat [] _ = True
noRepeat (firstMove:otherMoves) candidateMove | isSimpleKing firstMove = historyAlgorithm (candidateMove:otherMoves) [firstMove]
                                              | otherwise = True

--checks if the move is a simple king move or not
isSimpleKing:: Move -> Bool
isSimpleKing [K (x1,y1), K (x2,y2)] = True
isSimpleKing _ = False

--historyAlgorithm returns false if the movement list is empty while the history is not
-- we first check if our move is a simple king move then we call updateMovement on the move and the movement list
-- after that we check for cycles in the movement list (this the fix that makes this algo work for this modified version of the requirements)
-- and recurse with historyAlgorithm
--if its not a simple king move we just return since we can no longer have repeated states
historyAlgorithm :: [Move]->[Move]->Bool
historyAlgorithm _ [] = False
historyAlgorithm [] _ = True
historyAlgorithm (m:ms) movement | isSimpleKing m = historyAlgorithm ms (checkInternalCycles (updateMovement m movement) )
                                 | otherwise = True 

-- the fucntion that checks conditions and appropriately adds the given move into movement
-- if the start of the first move and the end of the second move and start of the second move and the end of the first move are the same then its a cycle, we must discard these pairs
-- if the start of the first move and the end of the second move is the same then update the move by keeping the start of the second and adding in the end of the first
-- otherwise remove the first element of movement (save it for later) and recurse with the move on the rest of movement
-- return a list of moves, can be empty
updateMovement :: Move -> [Move] -> [Move]
updateMovement [] moves = moves
updateMovement [K (x,y), K(w,z)] [] = [[K (x,y),K (w,z)]]
updateMovement [K (x,y), K(w,z)] ([K(a,b),K(c,d)]:ms) | x==c && y==d && w==a && z==b = ms
                                                      | x==c && y==d = [K(a,b),K(w,z)]:ms 
                                                      | otherwise = [K(a,b),K(c,d)]:updateMovement [K(x,y),K(w,z)] ms

--the fixer-upper fucntion
-- takes in a list of moves and removes cycles from it internally
-- basically redoes what updateMovement does but this time the candidate move is a move from the list itself

checkInternalCycles :: [Move] -> [Move]
checkInternalCycles [] = []
checkInternalCycles [[K(a,b),K(c,d)]] = [[K(a,b),K(c,d)]]
checkInternalCycles ([K(a,b),K(c,d)]:[K(x,y),K(z,w)]:ms) | not (compareCoord (a,b) (z,w)) && not (compareCoord (c,d) (x,y))= checkInternalCycles ms
                                                         | not (compareCoord (a,b) (z,w)) = checkInternalCycles ([K(x,y), K(c,d)]:ms)
                                                         | otherwise = [K(a,b),K(c,d)]:checkInternalCycles ([K(x,y),K(z,w)]:ms)



{-Repeated History Hall of Shame 
all the different methods I tried till I reached the ultimate solution- for your perusal
Too sentimental to remove - its like an album of baby photos

setup :: GameState ->Move -> Bool
setup g candidateMove = repeatsNope (unRepeat g candidateMove ) ([head (getFirst (unRepeat g candidateMove ))],[head (getSecond (unRepeat g candidateMove)) ] )

updateMove :: Move -> [Move] -> [Move]
updateMove [] moves = moves
updateMove [K (x,y), K(w,z)] [] = [[K (x,y), K(w,z)]]
updateMove [K (x,y), K(w,z)] ([K(a,b),K(c,d)]:ms)     | x==a && y==b && w==c && z == d = [K(a,b),K(c,d)]:ms
                                                      | x==c && y==d && w==a && z==b = ms
                                                      | x==c && y==d = [K(a,b),K(w,z)]:ms 
                                                      | otherwise = [K(a,b),K(c,d)]:updateMove [K(x,y),K(w,z)] ms


repeatsNope::([Move],[Move])->([Move],[Move])->Bool
repeatsNope (red,black) ([],[]) = False
repeatsNope ([],[]) (_,_) = True
repeatsNope (r:red,b:black) (rcheck,bcheck) | isSimpleKing r && isSimpleKing b = repeatsNope (red,black) (updateMove r rcheck, updateMove b bcheck)
                                            | otherwise = True

unRepeat :: GameState -> Move -> ([Move],[Move])
unRepeat g  = splitHistory g (split ((history g)))

getFirst :: ([Move],[Move])-> [Move]
getFirst (first,second)= first

getSecond ::([Move],[Move])-> [Move]
getSecond (first,second)= second

splitHistory:: GameState -> ([Move],[Move])->Move->([Move],[Move])
splitHistory g (red, black) candidateMove | status g == RedPlayer = (red, candidateMove:black)
                                          | status g == BlackPlayer = (candidateMove:red,black)
                                          | otherwise = ([],[])
freverse :: [a] -> [a]
freverse as = shunt as []
            where shunt :: [a] -> [a] -> [a]
                  shunt [] ys = ys
                  shunt (x:xs) ys = shunt xs (x:ys)
                  
split :: [a] -> ([a], [a])
split [] = ([], [])
split [x] = ([x], [])
split (x:y:xs) = (x:xp, y:yp) where (xp, yp) = split xs

noRepeats :: [Move] -> Move -> Bool 
noRepeats moves move | null (noRepeatTester moves move) = False
                     | null (checkInternalCycles (noRepeatTester moves move) ) = False 
                     | otherwise = True

noRepeatTester:: [Move] -> Move -> [Move]
noRepeatTester (firstMove:otherMoves) candidateMove = historyTester (candidateMove:otherMoves ) [firstMove]

historyTester :: [Move]->[Move] -> [Move]
historyTester _ [] = []
historyTester [] m = m
historyTester (m:ms) movement | isSimpleKing m =  historyTester ms (update m movement)
                              | otherwise = []



updateMovement' :: Move -> [Move] -> [Move]
updateMovement' [] moves = moves
updateMovement' [K (x,y), K(w,z)] [] = [[K (x,y),K (w,z)]]
updateMovement' [K (x,y), K(w,z)] ([K(a,b),K(c,d)]:ms) | not (compareCoord (c,d) (x,y)) &&  not (compareCoord (a,b) (w,z)) = ms
                                                       | not (compareCoord (c,d) (x,y)) = [K(a,b),K(w,z)]:ms 
                                                       | otherwise = [K(a,b),K(c,d)]:updateMovement' [K(x,y),K(w,z)] ms                 


update :: Move -> [Move]-> [Move]
update move [] = []
update [K(a,b), K(c,d)] [[K(x,y), K (z,w)]] | c==x && d==y && a==z && b==w = []
                                            |otherwise = [K(x,y), K (z,w)]:[[K(a,b), K(c,d)]]
update [K(a,b), K(c,d)] ([K(x,y), K (z,w)]:[K(e,f), K (g,h)]:ms )| g==a && h==b && c==e && d==f = [K(x,y), K (z,w)]:ms
                                                                 | g==a && h==b = [K(x,y), K (z,w)]: [K(e,f), K (c,d)]:ms
                                                                 | otherwise = [K(x,y), K (z,w)]:[K(e,f), K (g,h)]: (update  [K(a,b), K(c,d)] ms)
historyCheck:: [Move]->[Move] -> [Move]
historyCheck _ [] = []
historyCheck [] m = m
historyCheck (m:ms) movement | isSimpleKing m && not (null(update m movement))=  historyCheck ms (update m movement)
                              | otherwise = movement
-}