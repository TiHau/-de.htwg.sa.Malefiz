package de.htwg.se.malefiz.model.gameboard

class Stone

case class BlockStone() extends Stone

case class PlayerStone(startX: Int, startY: Int, x: Int, y: Int, playerColor: Int) extends Stone {
  def isOnStart: Boolean = this.x == this.startX && this.y == this.startY
}