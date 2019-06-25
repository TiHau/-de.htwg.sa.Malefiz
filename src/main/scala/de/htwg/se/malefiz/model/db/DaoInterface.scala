package de.htwg.se.malefiz.model.db

import play.api.libs.json.JsValue


trait DaoInterface {
  def getLatestSave:JsValue
  def insert(name:String, saveJson:JsValue): Unit
}
