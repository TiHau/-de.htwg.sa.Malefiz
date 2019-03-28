package de.htwg.se.malefiz.model.gameboard

case class Field(var x: Int, var y: Int, var stone: Stone) {
  var avariable = false
  def hasNoStoneOnIt: Boolean = {
    stone.sort == 'f'
  }
}