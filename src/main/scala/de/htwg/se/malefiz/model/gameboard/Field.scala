package de.htwg.se.malefiz.model.gameboard

case class Field(x: Int, y: Int, var stone: Option[Stone], available: Boolean = false)
