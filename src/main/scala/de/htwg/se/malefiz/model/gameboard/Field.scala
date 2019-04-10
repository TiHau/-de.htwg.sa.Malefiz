package de.htwg.se.malefiz.model.gameboard

case class Field(var x: Int, var y: Int, var stone: Option[Stone], var avariable: Boolean = false)
