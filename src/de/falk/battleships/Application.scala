package de.falk.battleships

import de.falk.battleships.configuration.OnlyRandomOfNotShot
import de.falk.battleships.generators.Random
import de.falk.battleships.model._

import scala.collection.SortedSet

object Application extends App {
  val lettersDimension = 8
  val digitsDimension = 8
  val standardShipTypes: List[ShipType] = createStandardShipTypes
  var game = initializeGame


  var continue = true
  do {
    val shotCoordinate = game.doShotPlayerOne()
    val gameAndShotResult = game.getShotPlayerTwo(shotCoordinate)
    val shotResult = gameAndShotResult._2
    val gameUpdatedOnce = gameAndShotResult._1
    val gameUpdatedTwice = gameUpdatedOnce.insertShotResultOtherMapPlayerOne(shotCoordinate, shotResult, game.playerTwo.board.ownShipsAndImpacts.ships)
    showGame(gameUpdatedTwice)
    //    StdIn.readLine()
    if (gameUpdatedTwice.playerTwo.board.ownShipsAndImpacts.allShipsSunk) {
      println("Player: '" + gameUpdatedTwice.playerOne.name + "' won!")
      continue = false
    }
    else {
      val shotCoordinateTwo = gameUpdatedTwice.doShotPlayerTwo()
      val gameAndShotResultTwo = gameUpdatedTwice.getShotPlayerOne(shotCoordinateTwo)
      val shotResultTwo = gameAndShotResultTwo._2
      val gameUpdatedThrice = gameAndShotResultTwo._1
      val gameUpdatedForth = gameUpdatedThrice.insertShotResultOtherMapPlayerTwo(shotCoordinateTwo, shotResultTwo, game.playerOne.board.ownShipsAndImpacts.ships)
      showGame(gameUpdatedForth)
      if (gameUpdatedForth.playerOne.board.ownShipsAndImpacts.allShipsSunk) {
        println("Player: '" + gameUpdatedTwice.playerTwo.name + "' won!")
        continue = false
      }
      //    StdIn.readLine()
      game = new Game(gameUpdatedForth.playerOne, gameUpdatedForth.playerTwo, gameUpdatedForth.currentRound + 1)
    }

    if (game.currentRound == lettersDimension * digitsDimension + 1) {
      continue = false
    }

  } while (continue)

  def showGame(game: Game) : Unit = {
    println("")
    println("Round: " + game.currentRound)
    showPlayer(game.playerOne)
    showPlayer(game.playerTwo)
  }

  def showPlayer(player: Player) : Unit = {
    println("")
    println("Name player: " + player.name)

    showBoard(player.board)
  }

  def showBoard(board: Board) : Unit = {
    for (dimensionDigits <- 0 until digitsDimension) {
      showOwnMapLine(board.ownShipsAndImpacts, dimensionDigits)
      print("    ")
      showOtherMapLine(board.otherShipsAndImpacts, dimensionDigits)
      println("")
    }
  }

  def showOwnMapLine(ownMap: OwnMap, dimensionDigits: Int) : Unit = {
    for (dimensionLetters <- 0 until lettersDimension) {
      print(ownMap.impacts(dimensionDigits * lettersDimension + dimensionLetters).sign + " ")
    }
  }

  def showOtherMapLine(otherMap: OtherMap, dimensionDigits: Int) : Unit = {
    for (dimensionLetters <- 0 until lettersDimension) {
      print(otherMap.impacts(dimensionDigits * lettersDimension + dimensionLetters).sign + " ")
    }
  }

  def print(toOutput: String) : Unit = {
    System.out.print(toOutput)
  }

  def println(toOutput: String) : Unit = {
    System.out.println(toOutput)
  }

  def placeAllShips(shipTypes: List[ShipType]) : OwnMap = {
    def placeAllShipsRec(shipTypes:List[ShipType], ownMap: OwnMap) : OwnMap = {
      if (shipTypes.size == 1)
        findCoordinatesShip(shipTypes.head, ownMap)
      else {
        placeAllShipsRec(shipTypes.tail, findCoordinatesShip(shipTypes.head, ownMap))
      }

    }

    placeAllShipsRec(shipTypes, new OwnMap(lettersDimension, digitsDimension, List[Ship](), initializeOwnMap(lettersDimension, digitsDimension)))
  }

  def findCoordinatesShip(shipType: ShipType, map: OwnMap) : OwnMap = {
    if (map.moreThanHalfMapOccupied) {
      throw new IllegalArgumentException("Map is occupied more than half with ships!")
    }
    var a = 0
    var o = 0
    var ship = Ship(shipType, new Coordinate(0, 0), Direction.fromIntTo(0), SortedSet[Int]())
    do {
      a = Random.nextInt(0, lettersDimension)
      o = Random.nextInt(0, digitsDimension)
      ship = Ship(shipType, new Coordinate(a, o), Direction.fromIntTo(Random.nextInt(0, 4)), SortedSet[Int]())
    } while (map.shipOutside(ship) || map.shipCollide(ship))

    var impactsChanged = map.impacts
    for (coordinate <- ship.coordinates) {
      impactsChanged = impactsChanged.updated(Coordinate.toIndex(coordinate, map.dimensionLetters), ComplexInitializedValue(NoHit, ship.shipType))
    }
    new OwnMap(map.dimensionLetters, map.dimensionDigits, map.ships :+ ship, impactsChanged)
  }

  def initializeOwnMap(dimensionLetters: Int, dimensionDigits: Int) : List[OwnMapValue] = {
    var result = List[OwnMapValue]()
    for (i <- 0 until dimensionLetters * dimensionDigits) result = result :+ Empty
    result
  }

  def initializeOtherMap(dimensionLetters: Int, dimensionDigits: Int) : List[OtherMapValue] = {
    var result = List[OtherMapValue]()
    for (i <- 0 until dimensionLetters * dimensionDigits) result = result :+ NotShot
    result
  }

  def createStandardShipTypes : List[ShipType] = {
    List(Torpedoboat, Torpedoboat, Submarine, Destroyer, Cruiser, Battleship)
  }

  def initializeGame : Game = {
    val map1Player1 = placeAllShips(standardShipTypes)
    val map2Player1 = new OtherMap(lettersDimension, digitsDimension, initializeOtherMap(lettersDimension, digitsDimension))
    val boardPlayer1 = new Board(map1Player1, map2Player1)
    val player1 = new Player("player one", boardPlayer1, OnlyRandomOfNotShot)

    val map1Player2 = placeAllShips(standardShipTypes)
    val map2Player2 = new OtherMap(lettersDimension, digitsDimension, initializeOtherMap(lettersDimension, digitsDimension))
    val boardPlayer2 = new Board(map1Player2, map2Player2)
    val player2 = new Player("player two", boardPlayer2, OnlyRandomOfNotShot)

    new Game(player1, player2, 1)
  }

}
