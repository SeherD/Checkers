# Checkers

This is the code for the CPSC 449 Checkers assignment in Haskell, completed Oct 29, 2021.
Moves.hs consists of the all legal moves possible by any piece on the board. There is also a check fo repeated states included, such that pieces do not move in cycles.
ApplyMoves.hs consists of the code required to choose a legal move for a particular piece and apply it to the board. For example, if a red pawn jumps over a black pawn and turns into a red king, then the red pawn must be removed from the list of red pawns, and added to the red kings. Additionally, the black pawn must be removed from the list of pieces. 
