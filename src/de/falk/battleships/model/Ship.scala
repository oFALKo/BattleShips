package de.falk.battleships.model

import scala.collection.SortedSet

case class Ship(shipType: ShipType, coordinate: Coordinate, direction: Direction, damages: SortedSet[Int]) {
  def isDestroyed : Boolean = shipType.length == damages.size

  def doDamage(impact: Coordinate) : Ship = {
    if (isHitBy(impact)) {
      val indexDamage = findIndexOfImpactInsideDamages(impact)
      if (indexDamage == -1)
        this // return the same. should never happen
      else
        Ship(shipType, coordinate, direction, damages + indexDamage)
    }
    else {
      throw new IllegalArgumentException("The impact does not hit the ship!")
    }
  }

  def coordinates : List[Coordinate] = {
    var result = List[Coordinate]()

    result = result :+ coordinate

    var coordinateCurrent = coordinate
    var coordinateNext: Coordinate = new Coordinate(0, 0)

    for (i <- 1 until shipType.length) {
      coordinateNext = coordinateCurrent.goto(direction)
      result = result :+ coordinateNext
      coordinateCurrent = coordinateNext
    }
    result
  }

  def overlap(other: Ship) : Boolean = coordinates.intersect(other.coordinates).nonEmpty

  def isHitBy(impact: Coordinate) : Boolean = coordinates.intersect(List(impact)).nonEmpty

  def willSunk(impact: Coordinate) : Boolean = coordinates.intersect(List(impact)).nonEmpty && shipType.length == damages.size + 1

  def findIndexOfShipInsideList(ships: List[Ship]) : Int = {
    var result = -1
    for (i <- ships.indices) {
      if (this == ships(i))
        result = i
    }
    result
  }

  private def findIndexOfImpactInsideDamages(impact: Coordinate) : Int = {
    var result = -1
    val coordinatesThis = coordinates

    for (i <- 0 until shipType.length) {
      if (coordinatesThis(i) == impact)
        result = i
    }
    result
  }

}

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


sealed trait Direction

case object North extends Direction
case object East extends Direction
case object South extends Direction
case object West extends Direction

object Direction {
  def fromIntTo(i: Int) : Direction = i match {
    case 0 => East
    case 1 => South
    case 2 => West
    case _ => North
  }
}

