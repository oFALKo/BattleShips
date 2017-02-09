package de.falk.battleships.model

import de.falk.battleships.configuration.{OnlyRandomOfNotShot, ShotStrategy}

class Player(val name: String, val ownShipsAndImpacts: OwnMap, val otherShipsAndImpacts: OtherMap, val shotStrategy: ShotStrategy) {

  def doShot() : Coordinate = shotStrategy match {
    case OnlyRandomOfNotShot => ShotStrategy.onlyRandomOfNotShot(otherShipsAndImpacts.coordinatesNotShotAndPartialHits)
  }

  def getShot(impact: Coordinate) : (Player, UpdatedValue) = {
    val result = ownShipsAndImpacts.getShot(impact)
    (new Player(name, result._1, otherShipsAndImpacts, shotStrategy), result._2)
  }

  def insertShotResultOtherMap(shotCoordinate: Coordinate, shotResult: UpdatedValue, otherPlayerShips: List[Ship]) : Player =
    new Player(name, ownShipsAndImpacts, otherShipsAndImpacts.insertShotResult(shotCoordinate, shotResult, otherPlayerShips), shotStrategy)

}

