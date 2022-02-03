module Checkers.ApplyMove where

import Checkers.Moves
import Checkers.Types

-- definitions of the first and last row values
fRow = [(0,0), (1,0),(2,0), (3,0),(4,0) ,(5,0),(6,0), (7,0)]
lRow  = [ (0,7), (1,7),(2,7),(3,7), (4,7), (5,7),(6,7), (7,7)]


--this function takes in a move from the user and the gamestate and returns the updated gamestate
-- divides the fucntionality by which players turn it is
-- at the beginning of the checks- check if gameOver
-- if not tehn check if the move is not in moves and there are jumpmoves available
-- tell the user to make jump moves
-- otherwise if the move is in the legal moves and a simple move, check what the piece being moved is and appropriately apply pawn move or king move
-- otherwise if its a jump move then apply a jump based on the color of the player
-- if everything fails, it is an invalid move, tell the user
apply_move:: Move -> GameState -> GameState
apply_move mv g = case status g of
        RedPlayer -> if gameOverCheck g 
                then g{message = "Game Over", status =GameOver}
                else if not (inMoves g mv) && not (null (jumpMoves g)) 
                then g {message = "You must make a jump move"} 
                else if inMoves g mv && elem mv (simpleMoves g)  
                then if unPorK (head mv) `elem` redPieces g
                        then applySimple g mv
                        else applyKingSimple g mv
                else if inMoves g mv && elem mv (jumpMoves g)
                then applyRJumps mv (unPorKMoves mv) g
        else g {message = "invalid move"}
        BlackPlayer -> if gameOverCheck g 
                then g{message = "Game Over", status =GameOver}
                else if not (inMoves g mv) && not (null (jumpMoves g)) 
                then g {message = "You must make a jump move"}
                else if inMoves g mv && elem mv (simpleMoves g) 
                then if unPorK (head mv) `elem` blackPieces g 
                        then applySimple g mv
                        else applyKingSimple g mv
                else if inMoves g mv && elem mv (jumpMoves g)
                then applyBJumps mv (unPorKMoves mv) g
        else g {message = "invalid move"}
        _ -> g


-- check if there are any pieces or moves availble for the player, if there are not any -> game over
gameOverCheck :: GameState -> Bool
gameOverCheck g |  null (redPieces g) &&  null (redKings g) ||  null (moves' (moves g))  = True
               |  null (blackPieces g) &&  null (blackKings g) ||  null (moves' (moves g))  = True
               | otherwise = False


--check if a move is in the list of legal moves
inMoves :: GameState -> Move -> Bool
inMoves g [] = False
inMoves g m = m `elem` moves' (moves g)

--apply a simple pawn move based on the status of the player
-- if a piece has reached the last row of the opposite side remove it from pawns and add it to kings
-- otherwise remove the start from pawns and add in the end instead
-- switch the status appropriately
-- add the applied move to history
applySimple :: GameState -> Move -> GameState
applySimple g move = case status g of
    RedPlayer -> if unPorK (head (tail move)) `elem` fRow
        then g { message = "BlackPlayer Turn",
                status = BlackPlayer,
                redPieces = remove (redPieces g) (unPorK (head move)),
                redKings = unPorK (head(tail move)): redKings g,
                history = move: history g  }
        else g {
                message = "BlackPlayer Turn",
                status = BlackPlayer,
                redPieces = unPorK (head(tail move) ): remove (redPieces  g) (unPorK (head move)),
                history = move: history g
                }
    BlackPlayer -> if unPorK (head (tail move)) `elem` lRow
        then g{message = "RedPlayer Turn",
                status = RedPlayer,
                blackPieces = remove (blackPieces g) (unPorK (head move)),
                blackKings = unPorK (head(tail move)): blackKings g,
                history = move: history g }
        else g{ message = "RedPlayer Turn",
                status = RedPlayer,
                blackPieces = unPorK (head(tail move) ): remove (blackPieces  g) (unPorK (head move)),
                history = move: history g
        }

--apply a simple king move based on the status of the player
-- remove the start from kingss and add in the end instead
-- switch the status appropriately
-- add the applied move to history

applyKingSimple:: GameState -> Move -> GameState
applyKingSimple g move = case status g of
    RedPlayer-> g {message = "BlackPlayer Turn",
                status = BlackPlayer,
                redKings = unPorK (head(tail move) ): remove (redKings g) (unPorK (head move)),
                history = move: history g
                    }
    BlackPlayer-> g{message = "RedPlayer Turn",
                    status = RedPlayer,
                    blackKings = unPorK (head(tail move) ): remove (blackKings g) (unPorK (head move)),
                   history = move: history g  
    }

-- recursively apply a red jump move
-- if its a pawn jump and the pawn doesnt reach the last row, remove the start from pawns and add the end in, also remove the piece that was jumped over from the 
-- opposite side
-- if its a king jump , remove the start from kings and add the end in, also remove the piece that was jumped over from the opposite side
-- if its a pawn jump and the pawn reaches the last row, remove the start from pawns and add the end to kings, also remove the piece that was jumped over from the 
-- opposite side
-- add the move to history
-- switch the status appropriately
-- returns when the move has been fully applied (empty)
applyRJumps:: Move ->[Coord]->GameState ->  GameState
applyRJumps m [] g = g {history = m: history g}
applyRJumps m [(x,y)] g  = g {history = m: history g}
applyRJumps moves ((x,y):(x1,y1):ms) g  | (x,y) `elem`redPieces g && notElem (x1,y1) fRow =
    applyRJumps moves ((x1,y1):ms) $ g{ message = "BlackPlayer Turn",
                                 status = BlackPlayer ,
                                 blackKings= remove (blackKings g) (midCoord (x,y) (x1,y1)),
                                 blackPieces= remove (blackPieces g) (midCoord (x,y) (x1,y1)),
                                 redPieces = (x1,y1):remove (redPieces g) (x,y)
                                 
                                 }
                                 |  (x,y) `elem`redPieces g && elem (x1,y1) fRow =
    applyRJumps moves ((x1,y1):ms) $ g{ message = "BlackPlayer Turn",
                                 status = BlackPlayer ,
                                 blackKings= remove (blackKings g) (midCoord (x,y) (x1,y1)),
                                 blackPieces= remove (blackPieces g) (midCoord (x,y) (x1,y1)),
                                 redPieces = remove (redPieces g) (x,y),
                                 redKings = (x1,y1):remove (redKings g) (x,y)
                                }
                                 |  (x,y) `elem`redKings g =
    applyRJumps moves ((x1,y1):ms) $ g{ message = "BlackPlayer Turn",
                                 status = BlackPlayer ,
                                 blackKings= remove (blackKings g) (midCoord (x,y) (x1,y1)),
                                 blackPieces= remove (blackPieces g) (midCoord (x,y) (x1,y1)),
                                 redKings = (x1,y1):remove (redKings g) (x,y)
                                 }
                                 
                                 |otherwise = applyRJumps moves [] g


-- recursively apply a black jump move
-- if its a pawn jump and the pawn doesnt reach the last row, remove the start from pawns and add the end in, also remove the piece that was jumped over from the 
-- opposite side
-- if its a king jump , remove the start from kings and add the end in, also remove the piece that was jumped over from the opposite side
-- if its a pawn jump and the pawn reaches the last row, remove the start from pawns and add the end to kings, also remove the piece that was jumped over from the 
-- opposite side
-- add the move to history
-- switch the status appropriately
-- returns when the move has been fully applied (empty)
applyBJumps:: Move ->[Coord]->GameState ->  GameState
applyBJumps m [] g = g {history = m: history g}
applyBJumps m [(x,y)] g  = g {history = m: history g}
applyBJumps moves ((x,y):(x1,y1):ms) g 
                                 | (x,y) `elem` blackPieces g && notElem (x1,y1) lRow =
    applyBJumps moves ((x1,y1):ms) $ g{ message = "RedPlayer Turn",
                                 status = RedPlayer ,
                                 redKings= remove (redKings g) (midCoord (x,y) (x1,y1)),
                                 redPieces= remove (redPieces g) (midCoord (x,y) (x1,y1)),
                                 blackPieces = (x1,y1):remove (blackPieces g) (x,y)
                                 }
                                 | (x,y) `elem`blackPieces g && elem (x1,y1) lRow =
    applyBJumps moves ((x1,y1):ms) $ g{ message = "RedPlayer Turn",
                                 status = RedPlayer ,
                                 redKings= remove (redKings g) (midCoord (x,y) (x1,y1)),
                                 redPieces= remove (redPieces g) (midCoord (x,y) (x1,y1)),
                                 blackPieces = remove (blackPieces g) (x,y),
                                 blackKings = (x1,y1):remove (blackKings g) (x,y)
                                }
                                 | (x,y) `elem` blackKings g =
    applyBJumps moves ((x1,y1):ms) $ g{ message = "RedPlayer Turn",
                                 status = RedPlayer ,
                                 redKings= remove (redKings g) (midCoord (x,y) (x1,y1)),
                                 redPieces= remove (redPieces g) (midCoord (x,y) (x1,y1)),
                                 blackKings = (x1,y1):remove (blackKings g) (x,y)
                                 }
                                 |otherwise = applyBJumps moves [] g
