package de.falk.battleships.model

class Board(val ownShipsAndImpacts: OwnMap, val otherShipsAndImpacts: OtherMap) {

  def getShot(impact: Coordinate) : (Board, ShotResult) = {
    val result = ownShipsAndImpacts.getShot(impact)
    (new Board(result._1, otherShipsAndImpacts), result._2)
  }

  def insertShotResultOtherMap(shotCoordinate: Coordinate, shotResult: ShotResult, otherPlayerShips: List[Ship]) : Board =
    new Board(ownShipsAndImpacts, otherShipsAndImpacts.insertShotResult(shotCoordinate, shotResult, otherPlayerShips))

}
