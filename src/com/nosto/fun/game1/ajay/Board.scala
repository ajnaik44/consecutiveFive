package com.nosto.fun.game1.ajay

import java.util

import com.nosto.fun.game1.Piece
import scala.util.control.Breaks._
import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks

/**
 * Board class for responsible possible combination of next move
 *
 * @author Ajay Naik
 *
 */
class Board {
  private var boardMatrix: Array[Array[Piece]] = null //NULL: Empty CROOS: COMPUTER ROUND: Black
  /**
   * Initialize the board matrix
   *
   * @param boardSize
   */
  def this(boardSize: Int) {
    this()
    boardMatrix =  Array.ofDim[Piece](boardSize, boardSize)
  }

  // copy constructor (only copies the boardMatrix)
  def this(board: Board) = {
    this()
    var matrixToCopy: Array[Array[Piece]] = board.getBoard
    boardMatrix = Array.ofDim[Piece](matrixToCopy.length, matrixToCopy.length)
    for (m <- 0 until matrixToCopy.length; n <- 0 until matrixToCopy.length) {
      boardMatrix(m)(n) = matrixToCopy(m)(n)
    }
  }

  def getBoardSize = boardMatrix.length

  def addPieceToBoard(posX: Int, posY: Int, round: Boolean) = boardMatrix(posY)(posX) = if (round) Piece.ROUND
  else Piece.CROSS

  def addPiece(posX: Int, posY: Int, black: Boolean): Boolean = { // Check whether the cell is empty or not
    if (boardMatrix(posY)(posX) != null) return false
    boardMatrix(posY)(posX) = if (black) Piece.ROUND
    else Piece.CROSS
    true
  }

  def generateMoves:ListBuffer[Array[Int]] = {
    val moveList: ListBuffer[Array[Int]] = new ListBuffer[Array[Int]]()
    val boardSize = boardMatrix.length
    // Look for cells that has at least one stone in an adjacent cell.



    for (i <- 0 until boardSize) {

      for (j <- 0 until boardSize) {
        var flag=false
        if (boardMatrix(i)(j) != null) {
        flag=true
        }

          //todo: continue is not supported
        if (!flag && i > 0) {
          if (!flag && j > 0) if (boardMatrix(i - 1)(j - 1) != null || boardMatrix(i)(j - 1) != null) {
            val move = Array(i, j)
            moveList+=move
            flag=true;
          }
          if (!flag && j < boardSize - 1) if (boardMatrix(i - 1)(j + 1) != null || boardMatrix(i)(j + 1) != null) {
            val move = Array(i, j)
            moveList+=move
            flag=true;
          }
          if (!flag && boardMatrix(i - 1)(j) != null) {
            val move = Array(i, j)
            moveList+=move
            flag=true;
          }
        }
        if (!flag && i < boardSize - 1) {
          if (!flag && j > 0) if (boardMatrix(i + 1)(j - 1) != null || boardMatrix(i)(j - 1) != null) {
            val move = Array(i, j)
            moveList += move
            flag=true;
          }
          if (!flag && j < boardSize - 1) if (boardMatrix(i + 1)(j + 1) != null || boardMatrix(i)(j + 1) != null) {
            val move = Array(i, j)
            moveList += move
            flag=true;
          }
          if (!flag &&  boardMatrix(i + 1)(j) != null) {
            val move = Array(i, j)
            moveList += move
            flag=true;
          }
        }



    }
    }

    moveList
  }

  def getBoard = boardMatrix
}