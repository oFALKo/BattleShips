package de.falk.battleships.configuration

import de.falk.battleships.model.{Coordinate, ShipType}

sealed trait ShotStrategy

case object OnlyRandomOfNotShot extends ShotStrategy

case object RandomOfNotShotOrAroundPartialHit extends ShotStrategy

object ShotStrategy {
   def onlyRandomOfNotShot(informationForShot: (List[Coordinate], List[(Coordinate, ShipType)])) : Coordinate = {
    scala.util.Random.shuffle(informationForShot._1).head
  }
}