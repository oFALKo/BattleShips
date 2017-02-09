package de.falk.battleships.model


class Game(val playerOne: Player, val playerTwo: Player, val currentRound: Int) {

  def doShotPlayerOne() : Coordinate = playerOne.doShot()

  def doShotPlayerTwo() : Coordinate = playerTwo.doShot()

  def getShotPlayerOne(impact: Coordinate) : (Game, UpdatedValue) = {
    val result = playerOne.getShot(impact)
    (new Game(result._1, playerTwo, currentRound), result._2)
  }

  def getShotPlayerTwo(impact: Coordinate) : (Game, UpdatedValue) = {
    val result = playerTwo.getShot(impact)
    (new Game(playerOne, result._1, currentRound), result._2)
  }

  def insertShotResultOtherMapPlayerOne(shotCoordinate: Coordinate, shotResult: UpdatedValue, otherPlayerShips: List[Ship]) : Game =
    new Game(playerOne.insertShotResultOtherMap(shotCoordinate, shotResult, otherPlayerShips), playerTwo, currentRound)

  def insertShotResultOtherMapPlayerTwo(shotCoordinate: Coordinate, shotResult: UpdatedValue, otherPlayerShips: List[Ship]) : Game =
    new Game(playerOne, playerTwo.insertShotResultOtherMap(shotCoordinate, shotResult, otherPlayerShips), currentRound)

}
