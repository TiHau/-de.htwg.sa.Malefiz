package de.htwg.se.malefiz

import de.htwg.se.malefiz.aview.{ GUI, TUI }

object Malefiz {

  def main(args: Array[String]): Unit = {
    val tui = TUI()
    new GUI()
    while (true) {
      tui.readLine()
    }
  }
}
