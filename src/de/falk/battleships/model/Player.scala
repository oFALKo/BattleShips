package de.falk.battleships.model

import de.falk.battleships.configuration.{OnlyRandomOfNotShot, ShotStrategy}

class Player(val name: String, val board: Board, val shotStrategy: ShotStrategy) {

  def doShot() : Coordinate = shotStrategy match {
    case OnlyRandomOfNotShot => ShotStrategy.onlyRandomOfNotShot(board.otherShipsAndImpacts.coordinatesNotShotAndPartialHits)
  }

  def getShot(impact: Coordinate) : (Player, ShotResult) = {
    val result = board.getShot(impact)
    (new Player(name, result._1, shotStrategy), result._2)
  }

  def insertShotResultOtherMap(shotCoordinate: Coordinate, shotResult: ShotResult, otherPlayerShips: List[Ship]) : Player =
    new Player(name, board.insertShotResultOtherMap(shotCoordinate, shotResult, otherPlayerShips), shotStrategy)

}

