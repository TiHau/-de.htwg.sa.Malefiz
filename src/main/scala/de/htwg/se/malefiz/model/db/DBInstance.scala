package de.htwg.se.malefiz.model.db

import com.google.inject.{Guice, Injector}
import de.htwg.se.malefiz.MalefizModule

object DBInstance {
  val injector: Injector = Guice.createInjector(new MalefizModule)
  val db: DaoInterface = injector.getInstance(classOf[DaoInterface])

}
