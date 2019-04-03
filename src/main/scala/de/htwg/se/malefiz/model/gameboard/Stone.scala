package de.htwg.se.malefiz.model.gameboard

class Stone

case class BlockStone() extends Stone

case class PlayerStone(startField: Field, var actualField: Field, playerColor: Int) extends Stone