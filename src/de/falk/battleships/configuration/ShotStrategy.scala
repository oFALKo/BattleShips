package de.falk.battleships.configuration

import de.falk.battleships.model.{Coordinate, InformationForShot}

sealed trait ShotStrategy

case object OnlyRandomOfNotShot extends ShotStrategy

case object RandomOfNotShotOrAroundPartialHit extends ShotStrategy

object ShotStrategy {
   def onlyRandomOfNotShot(informationForShot: InformationForShot) : Coordinate = {
    scala.util.Random.shuffle(informationForShot.notShots).head
  }
}