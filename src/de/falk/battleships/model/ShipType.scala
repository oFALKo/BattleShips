package de.falk.battleships.model

sealed trait ShipType {
  def name : String
  def length : Int
}

case object Torpedoboat extends ShipType {
  def name = "torpedo boat"
  def length = 1
}
case object Submarine extends ShipType {
  def name = "submarine"
  def length = 2
}
case object Destroyer extends ShipType {
  def name = "destroyer"
  def length = 3
}
case object Cruiser extends ShipType {
  def name = "cruiser"
  def length = 4
}

case object Battleship extends ShipType {
  def name = "battleship"
  def length = 5
}