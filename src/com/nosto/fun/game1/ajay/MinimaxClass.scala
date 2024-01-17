package com.nosto.fun.game1.ajay

import com.nosto.fun.game1.Piece
/**
 * @author Ajay Naik
 */
class MinimaxClass( // Board instance is responsible for board mechanics
                    val board: Board) // Constructor
{
  var evaluationCount = 0
  // Win score should be greater than all possible board scores
  private val winScore = 100000000

  // Getter function for the winScore
  def getWinScore = winScore
  // This function is used to get the next intelligent move to make for the AI.
  def calculateNextMove(depth: Int) = { // Block the board for AI to make a decision.
    var move = new Array[Int](2)
    // Used for benchmarking purposes only.
    val startTime = System.currentTimeMillis
    // Check if any available move can finish the game to make sure the AI always
    // takes the opportunity to finish the game.
    var bestMove = searchWinningMove(this.board)
    if (bestMove != null) { // Finishing move is found.
      move(0) = bestMove(1).asInstanceOf[Integer]
      move(1) = bestMove(2).asInstanceOf[Integer]
    }
    else { // If there is no such move, search the minimax tree with specified depth.
      bestMove = minimaxSearchAB(depth, this.board, true, -1.0, getWinScore)
      if (bestMove(1) == null) move = null
      else {
        move(0) = bestMove(1).asInstanceOf[Integer]
        move(1) = bestMove(2).asInstanceOf[Integer]
      }
    }
    System.out.println("Cases calculated: " + evaluationCount + " Calculation time: " + (System.currentTimeMillis - startTime) + " ms")
    evaluationCount = 0
    move
  }


  // This function calculates the relative score of the CROSS player against the ROUND.
  // (i.e. how likely is CROSS player to win the game before the ROUND player)
  // This value will be used as the score in the Minimax algorithm.
  def evaluateBoardForCROSS(board: Board, roundTurn: Boolean):Double= {
    evaluationCount += 1
    // Get board score of both players.
    var blackScore:Double = getScore(board, true, roundTurn)
    val whiteScore:Double = getScore(board, false, roundTurn)
    if (blackScore == 0) blackScore = 1.0
    // Calculate relative score of white against black
    whiteScore / blackScore
  }

  // This function calculates the board score of the specified player.
  // (i.e. How good a player's general standing on the board by considering how many
  //  consecutive 2's, 3's, 4's it has, how many of them are blocked etc...)
  def getScore(board: Board, forRound: Boolean, roundTurn: Boolean) = { // Read the board
    val boardMatrix = board.getBoard
    // Calculate score for each of the 3 directions
    evaluateHorizontal(boardMatrix, forRound, roundTurn) + evaluateVertical(boardMatrix, forRound, roundTurn) + evaluateDiagonal(boardMatrix, forRound, roundTurn)
  }

  /*
     * alpha : Best AI Move (Max)
     * beta : Best Player Move (Min)
     * returns: {score, move[0], move[1]}
     * */
  def minimaxSearchAB(depth: Int, board: Board, max: Boolean, alpha: Double, beta: Double): Array[Any] = { // Last depth (terminal node), evaluate the current board score.
    var tempAlpha =  alpha;
    var tempBeta =  beta;
    if (depth == 0) {
      val x:Array[Any] = Array(evaluateBoardForCROSS(board, !max), null, null)
      return x
    }
    // Generate all possible moves from this node of the Minimax Tree
    /*
         *                  (Move 1)
         *	               /
         *  (Current Node) --- (Move 2)
         *				   \   ...
         *                  (Move N)
         */ val allPossibleMoves = board.generateMoves
    // If there is no possible move left, treat this node as a terminal node and return the score.
    if (allPossibleMoves.size == 0) {
      val x:Array[Any] = Array(evaluateBoardForCROSS(board, !max), null, null)
      return x
    }
    var bestMove = new Array[Any](3)
    // Generate Minimax Tree and calculate node scores.
    if (max) { // Initialize the starting best move with -infinity.
      bestMove(0) = -1.0
      // Iterate for all possible moves that can be made.
      import scala.collection.JavaConversions._
      for (move <- allPossibleMoves) { // Create a temporary board that is equivalent to the current board
        val dummyBoard = new Board(board)
        // Play the move on that temporary board without drawing anything
        dummyBoard.addPieceToBoard(move(1), move(0), false)
        // Call the minimax function for the next depth, to look for a minimum score.
        // This function recursively generates new Minimax trees branching from this node
        // (if the depth > 0) and searches for the minimum white score in each of the sub trees.
        // We will find the maximum score of this depth, among the minimum scores found in the
        // lower depth.
        val tempMove = minimaxSearchAB(depth - 1, dummyBoard, !max, tempAlpha, tempBeta)
        // Updating alpha (alpha value holds the maximum score)
        // When searching for the minimum, if the score of a node is lower than the alpha
        // (max score of uncle nodes from one upper level) the whole subtree originating
        // from that node will be discarded, since the maximizing player will choose the
        // alpha node over any node with a score lower than the alpha.
        if (tempMove(0).asInstanceOf[Double] > tempAlpha) tempAlpha = tempMove(0).asInstanceOf[Double]
        // Pruning with beta
        // Beta value holds the minimum score among the uncle nodes from one upper level.
        // We need to find a score lower than this beta score, because any score higher than
        // beta will be eliminated by the minimizing player (upper level). If the score is
        // higher than (or equal to) beta, break out of loop discarding any remaining nodes
        // and/or subtrees and return the last move.
        if (tempMove(0).asInstanceOf[Double] >= tempBeta) return tempMove
        // Find the move with the maximum score.
        if (tempMove(0).asInstanceOf[Double] > bestMove(0).asInstanceOf[Double]) {
          bestMove = tempMove
          bestMove(1) = move(0)
          bestMove(2) = move(1)
        }
      }
    }
    else { // Initialize the starting best move using the first move in the list and +infinity score.
      bestMove(0) = 100000000.0
      bestMove(1) = allPossibleMoves(0)(0)
      bestMove(2) = allPossibleMoves(0)(1)

      for (move <- allPossibleMoves) {
        val dummyBoard = new Board(board)
        dummyBoard.addPieceToBoard(move(1), move(0), true)
        // Call the minimax function for the next depth, to look for a maximum score.
        // (if the depth > 0) and searches for the maximum white score in each of the sub trees.
        // We will find the minimum score of this depth, among the maximum scores found in the
        val tempMove = minimaxSearchAB(depth - 1, dummyBoard, !max, tempAlpha, tempBeta)
        // Updating beta (beta value holds the minimum score)
        // When searching for the maximum, if the score of a node is higher than the beta
        // (min score of uncle nodes from one upper level) the whole subtree originating
        // from that node will be discarded, since the minimizing player will choose the
        // beta node over any node with a score higher than the beta.
        if (tempMove(0).asInstanceOf[Double] < tempBeta) tempBeta = tempMove(0).asInstanceOf[Double]
        // Pruning with alpha
        // Alpha value holds the maximum score among the uncle nodes from one upper level.
        // We need to find a score higher than this alpha score, because any score lower than
        // alpha will be eliminated by the maximizing player (upper level). If the score is
        // lower than (or equal to) alpha, break out of loop discarding any remaining nodes
        if (tempMove(0).asInstanceOf[Double] <= tempAlpha) return tempMove
        // Find the move with the minimum score.
        if (tempMove(0).asInstanceOf[Double] < bestMove(0).asInstanceOf[Double]) {
          bestMove = tempMove
          bestMove(1) = move(0)
          bestMove(2) = move(1)
        }
      }
    }
    // Return the best move found in this depth
    bestMove
  }

  // This function looks for a move that can instantly win the game.
  def searchWinningMove(board: Board): Array[Any] = {
    val allPossibleMoves = board.generateMoves
    val winningMove = new Array[Any](3)
    // Iterate for all possible moves
    import scala.collection.JavaConversions._
    for (move <- allPossibleMoves) {
      evaluationCount += 1
      val dummyBoard = new Board(board)
      dummyBoard.addPieceToBoard(move(1), move(0), false)
      // If the white player has a winning score in that temporary board, return the move.
      if (getScore(dummyBoard, false, false) >= winScore) {
        winningMove(1) = move(0)
        winningMove(2) = move(1)
        return winningMove
      }
    }
    null
  }

  // This function calculates the score by evaluating the stone positions in horizontal direction
  def evaluateHorizontal(boardMatrix: Array[Array[Piece]], forRound: Boolean, playersTurn: Boolean) = {
    var consecutive = 0
    // blocks variable is used to check if a consecutive stone set is blocked by the opponent or
    // the board border. If the both sides of a consecutive set is blocked, blocks variable will be 2
    // If only a single side is blocked, blocks variable will be 1, and if both sides of the consecutive
    // set is free, blocks count will be 0.
    // By default, first cell in a row is blocked by the left border of the board.
    // If the first cell is empty, block count will be decremented by 1.
    // If there is another empty cell after a consecutive stones set, block count will again be
    // decremented by 1.
    var blocks = 2
    var score = 0
    // Iterate over all rows
    for (i <- 0 until boardMatrix.length) { // Iterate over all cells in a row
      for (j <- 0 until boardMatrix(0).length) { // Check if the selected player has a stone in the current cell
        if (boardMatrix(i)(j) eq (if (forRound) Piece.ROUND
        else Piece.CROSS)) { // Increment consecutive stones count
          consecutive += 1
        }
        else { // Check if cell is empty
          if (boardMatrix(i)(j) == null) { // Check if there were any consecutive stones before this empty cell
            if (consecutive > 0) { // Consecutive set is not blocked by opponent, decrement block count
              blocks -= 1
              // Get consecutive set score
              score += getConsecutiveSetScore(consecutive, blocks, forRound == playersTurn)
              // Reset consecutive stone count
              consecutive = 0
              // Current cell is empty, next consecutive set will have at most 1 blocked side.
              blocks = 1
            }
            else { // No consecutive stones.
              blocks = 1
            }
          }
          else { // Cell is occupied by opponent
            if (consecutive > 0) {
              score += getConsecutiveSetScore(consecutive, blocks, forRound == playersTurn)
              consecutive = 0
              // Current cell is occupied by opponent, next consecutive set may have 2 blocked sides
              blocks = 2
            }
            else blocks = 2
          }
        }
      }
      // End of row, check if there were any consecutive stones before we reached right border
      if (consecutive > 0) score += getConsecutiveSetScore(consecutive, blocks, forRound == playersTurn)
      // Reset consecutive stone and blocks count
      consecutive = 0
      blocks = 2
    }
    score
  }

  // This function calculates the score by evaluating the stone positions in vertical direction
  // The procedure is the exact same of the horizontal one.
  def evaluateVertical(boardMatrix: Array[Array[Piece]], forRound: Boolean, playersTurn: Boolean) = {
    var consecutive = 0
    var blocks = 2
    var score = 0
    for (j <- 0 until boardMatrix(0).length) {
      for (i <- 0 until boardMatrix.length) {
        if (boardMatrix(i)(j) eq (if (forRound) Piece.ROUND
        else Piece.CROSS)) consecutive += 1
        else if (boardMatrix(i)(j) == null) if (consecutive > 0) {
          blocks -= 1
          score += getConsecutiveSetScore(consecutive, blocks, forRound == playersTurn)
          consecutive = 0
          blocks = 1
        }
        else blocks = 1
        else if (consecutive > 0) {
          score += getConsecutiveSetScore(consecutive, blocks, forRound == playersTurn)
          consecutive = 0
          blocks = 2
        }
        else blocks = 2
      }
      if (consecutive > 0) score += getConsecutiveSetScore(consecutive, blocks, forRound == playersTurn)
      consecutive = 0
      blocks = 2
    }
    score
  }

  // This function calculates the score by evaluating the stone positions in diagonal directions
  // The procedure is the exact same of the horizontal calculation.
  def evaluateDiagonal(boardMatrix: Array[Array[Piece]], forRound: Boolean, playersTurn: Boolean) = {
    var consecutive = 0
    var blocks = 2
    var score = 0
    // From bottom-left to top-right diagonally
    for (k <- 0 to 2 * (boardMatrix.length - 1)) {
      val iStart = Math.max(0, k - boardMatrix.length + 1)
      val iEnd = Math.min(boardMatrix.length - 1, k)
      for (i <- iStart to iEnd) {
        val j = k - i
        if (boardMatrix(i)(j) eq (if (forRound) Piece.ROUND
        else Piece.CROSS)) consecutive += 1
        else if (boardMatrix(i)(j) == null) if (consecutive > 0) {
          blocks -= 1
          score += getConsecutiveSetScore(consecutive, blocks, forRound == playersTurn)
          consecutive = 0
          blocks = 1
        }
        else blocks = 1
        else if (consecutive > 0) {
          score += getConsecutiveSetScore(consecutive, blocks, forRound == playersTurn)
          consecutive = 0
          blocks = 2
        }
        else blocks = 2
      }
      if (consecutive > 0) score += getConsecutiveSetScore(consecutive, blocks, forRound == playersTurn)
      consecutive = 0
      blocks = 2
    }
    // From top-left to bottom-right diagonally
    for (k <- 1 - boardMatrix.length until boardMatrix.length) {
      val iStart = Math.max(0, k)
      val iEnd = Math.min(boardMatrix.length + k - 1, boardMatrix.length - 1)
      for (i <- iStart to iEnd) {
        val j = i - k
        if (boardMatrix(i)(j) eq (if (forRound) Piece.ROUND
        else Piece.CROSS)) consecutive += 1
        else if (boardMatrix(i)(j) == null) if (consecutive > 0) {
          blocks -= 1
          score += getConsecutiveSetScore(consecutive, blocks, forRound == playersTurn)
          consecutive = 0
          blocks = 1
        }
        else blocks = 1
        else if (consecutive > 0) {
          score += getConsecutiveSetScore(consecutive, blocks, forRound == playersTurn)
          consecutive = 0
          blocks = 2
        }
        else blocks = 2
      }
      if (consecutive > 0) score += getConsecutiveSetScore(consecutive, blocks, forRound == playersTurn)
      consecutive = 0
      blocks = 2
    }
    score
  }

  // This function returns the score of a given consecutive stone set.
  // count: Number of consecutive stones in the set
  // blocks: Number of blocked sides of the set (2: both sides blocked, 1: single side blocked, 0: both sides free)
  def getConsecutiveSetScore(count: Int, blocks: Int, currentTurn: Boolean): Int = {
    val winGuarantee = 1000000
    // If both sides of a set is blocked, this set is worthless return 0 points.
    if (blocks == 2 && count < 5) return 0

    count match {
      case 5 =>
        // 5 consecutive wins the game
        return winScore
      case 4 =>
        // 4 consecutive stones in the user's turn guarantees a win.
        // (User can win the game by placing the 5th stone after the set)
        if (currentTurn) return winGuarantee
        else { // Opponent's turn
          // If neither side is blocked, 4 consecutive stones guarantees a win in the next turn.
          if (blocks == 0) return winGuarantee / 4
          else { // If only a single side is blocked, 4 consecutive stones limits the opponents move
            // (Opponent can only place a stone that will block the remaining side, otherwise the game is lost
            // in the next turn). So a relatively high score is given for this set.
            return 200
          }
        }
      case 3 =>
        // 3 consecutive stones
        if (blocks == 0) { // Neither side is blocked.
          // If it's the current player's turn, a win is guaranteed in the next 2 turns.
          // (User places another stone to make the set 4 consecutive, opponent can only block one side)
          // However the opponent may win the game in the next turn therefore this score is lower than win
          // guaranteed scores but still a very high score.
          if (currentTurn) return 50000
          else { // If it's the opponent's turn, this set forces opponent to block one of the sides of the set.
            // So a relatively high score is given for this set.
            return 200
          }
        }
        else { // One of the sides is blocked.
          // Playmaker scores
          if (currentTurn) return 10
          else return 5
        }
      case 2 =>
        // 2 consecutive stones
        if(blocks == 0) {
          if(currentTurn)
            return 7;
          else
            return 5;
        }
        else {
          return 3;
        }
      case 1 =>
        return 1
    }
    // More than 5 consecutive stones?
    winScore * 2
  }
}
