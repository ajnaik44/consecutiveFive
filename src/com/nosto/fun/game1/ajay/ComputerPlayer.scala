package com.nosto.fun.game1.ajay

import com.nosto.fun.game1.ArenaPosition

import com.nosto.fun.game1.MainJFrame

import com.nosto.fun.game1.Piece

import com.nosto.fun.game1.Player

//remove if not needed
import scala.collection.JavaConversions._

class ComputerPlayer(var name: String) extends Player {

	var myPiece: Piece = Piece.CROSS

	private var board: Board = new Board(MainJFrame.SIZE)

	private var gameFinished: Boolean = false

	private var minimaxDepth: Int = 3

	private var Minimax: MinimaxClass = new MinimaxClass(this.board)

	// 0: There is no winner yet, 1: AI Wins, 2: Human Wins
	private var winner: Int = 0

	private var gameStart= false
	/*
	 * 	Sets the depth of the minimax tree. (i.e. how many moves ahead should the AI calculate.)
	 */

	def setAIDepth(depth: Int): Unit = {
		this.minimaxDepth = depth
	}

	def findOptimalMove(board2: Array[Array[Piece]],
						e: ArenaPosition): ArenaPosition = {
	  gameStart=true
		val posX: Int = e.getRow
		val posY: Int = e.getColumn
// Place a black stone to that cell.
		if (!playMove(posX, posY, true)) {
			null
		}
// Check if the last move ends the game.
		winner = checkWinner()
		if (winner == 2) {
			println("Player WON!")
			gameFinished = true
			null
		}
// Make the AI instance calculate a move.
		val aiMove: Array[Int] = Minimax.calculateNextMove(minimaxDepth)
		if (aiMove == null) {
			println("No possible moves left. Game Over.")
			gameFinished = true
			null
		}
// Place a black stone to the found cell.
		playMove(aiMove(1), aiMove(0), false)
		println(
				"Black: " + Minimax.getScore(board, true, true) + " White: " +
						Minimax.getScore(board, false, true))
		winner = checkWinner()
		if (winner == 1) {
			println("AI WON!")
			gameFinished = true
			new ArenaPosition(aiMove(1), aiMove(0))
		}
		if (board.generateMoves.size == 0) {
			println("No possible moves left. Game Over.")
			gameFinished = true
			new ArenaPosition(aiMove(1), aiMove(0))
		}
		new ArenaPosition(aiMove(1), aiMove(0))
	}
// Find out which cell of the board do the clicked coordinates belong to.
// Find out which cell of the board do the clicked coordinates belong to.

	private def checkWinner(): Int = {
		if (Minimax.getScore(board, true, false) >= Minimax.getWinScore) 2
		if (Minimax.getScore(board, false, true) >= Minimax.getWinScore) 1
		0
	}

	private def playMove(posX: Int, posY: Int, black: Boolean): Boolean =
			board.addPiece(posX, posY, black)



	override def setSide(p: Piece): Unit = {
		this.myPiece = p
		
		// This will initailize object again
		if(this.gameStart){
		 
	 this.board = new Board(MainJFrame.SIZE)

	 this.gameFinished = false

	 this.minimaxDepth = 3

	  this.Minimax = new MinimaxClass(this.board)

	// 0: There is no winner yet, 1: AI Wins, 2: Human Wins
	this.winner = 0
		}
		
	}

	override def getSide(): Piece = // TODO Auto-generated method stub
			this.myPiece

	override def move(
			board: Array[Array[Piece]],
			last: ArenaPosition): ArenaPosition = // TODO Auto-generated method stub
	findOptimalMove(board, last)

	override def getName(): String = // TODO Auto-generated method stub
			this.name

	override def toString(): String = getName

}
