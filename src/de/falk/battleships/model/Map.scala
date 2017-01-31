package de.falk.battleships.model


abstract class Map(val dimensionLetters: Int, val dimensionDigits: Int, val impacts: List[FieldValue]) {

}

class OwnMap(override val dimensionLetters: Int, override val dimensionDigits: Int, val ships: List[Ship],override  val impacts: List[OwnMapValue]) extends Map(dimensionLetters, dimensionDigits, impacts) {
  import de.falk.battleships.model.{ComplexUpdatedValue => ShotResultComplex, Miss => ShotResultMiss}

  def getShot(impactCoordinate: Coordinate) : (OwnMap, ShotResult) = {
    def evaluateImpact : ShotResultValue = {
      ships.find(ship => ship.isHitBy(impactCoordinate)) match {
        case None => ShotResultMiss
        case Some(hitShip) =>
          if (hitShip.willSunk(impactCoordinate))
            ShotResultComplex(FullHit, hitShip.shipType)
          else
            ShotResultComplex(PartialHit, hitShip.shipType)
      }
    }

    def shipsChanged(shotResult: ShotResultValue) : List[Ship] = {
      ships.find(ship => ship.isHitBy(impactCoordinate)) match {
        case None => ships // return the same
        case Some(hitShip) => {
          val index = hitShip.findIndexOfShipInsideList(ships)
          if (index == -1)
            ships // should never happen
          else
            shotResult match {
              case ShotResultMiss => ships // return the same
              case complex: ShotResultComplex => ships.updated(index, hitShip.doDamage(impactCoordinate))
            }
        }
      }
    }

    def impactChanged : List[OwnMapValue] = {
      var result = List[OwnMapValue]()
      ships.find(ship => ship.isHitBy(impactCoordinate)) match {
        case None => impacts.updated(Coordinate.toIndex(impactCoordinate, dimensionLetters), Shot)
        case Some(hitShip) => {
          if (hitShip.willSunk(impactCoordinate)) {
            result = impacts.updated(Coordinate.toIndex(hitShip.coordinate, dimensionLetters), ComplexUpdatedValue(FullHit, hitShip.shipType))
            for (coordinateToUpdate <- hitShip.coordinates)
              result = result.updated(Coordinate.toIndex(coordinateToUpdate, dimensionLetters), ComplexUpdatedValue(FullHit, hitShip.shipType))
          }
          else
            result = impacts.updated(Coordinate.toIndex(impactCoordinate, dimensionLetters), ComplexUpdatedValue(PartialHit, hitShip.shipType))
          result
        }
      }

    }

    val shotResultValue = evaluateImpact
    (new OwnMap(dimensionLetters, dimensionDigits, shipsChanged(shotResultValue), impactChanged), ShotResult(shotResultValue))
  }

  def moreThanHalfMapOccupied : Boolean = ships.map(s => s.shipType.length).sum >= dimensionLetters * dimensionDigits / 2

  def allShipsSunk : Boolean = ships.filter(s => ! s.isDestroyed).isEmpty

  def shipCollide(ship: Ship) : Boolean = ships.filter(s => s.overlap(ship)).nonEmpty

  def shipOutside(ship: Ship) : Boolean = ship.coordinates.filter(c => c.dimensionLetters < 0 || c.dimensionDigits < 0 || c.dimensionLetters >= dimensionLetters || c.dimensionDigits >= dimensionDigits).nonEmpty

}

class OtherMap(override val dimensionLetters: Int, override val dimensionDigits: Int,override  val impacts: List[OtherMapValue]) extends Map(dimensionLetters, dimensionDigits, impacts) {

//  private var impacts: List[OtherMapValue] = fillMapWith(NotShot).asInstanceOf[List[OtherMapValue]]

  def coordinatesNotShotAndPartialHits : InformationForShot = {
    var coordinatesNotShot = List[Coordinate]()
    var coordinatesPartialHit = List[(Coordinate, ShipType)]()

    for (dimLetters <- 0 until dimensionLetters) {
      for (dimDigits <- 0 until dimensionDigits) {
        val index = dimensionLetters * dimDigits + dimLetters
        coordinatesNotShot = if (NotShot == impacts(index)) coordinatesNotShot :+ new Coordinate(dimLetters, dimDigits) else coordinatesNotShot
        coordinatesPartialHit =
          if (impacts(index).isInstanceOf[ComplexUpdatedValue] && impacts(index).asInstanceOf[ComplexUpdatedValue].hitType == PartialHit)
            coordinatesPartialHit :+ (new Coordinate(dimLetters, dimDigits), impacts(index).asInstanceOf[ComplexUpdatedValue].shipType)
          else
            coordinatesPartialHit
      }
    }
    new InformationForShot(coordinatesNotShot, coordinatesPartialHit)
  }

  def insertShotResult(shotCoordinate: Coordinate, shotResult: ShotResult, otherPlayerShips: List[Ship]) : OtherMap = {
    var resultImpacts:List[OtherMapValue] = impacts

    def determineSunkShip : Ship = {
      otherPlayerShips.find(ship => ship.isHitBy(shotCoordinate)) match {
        case None => otherPlayerShips.head // should never happen
        case Some(hitShip) => hitShip
      }
    }

    if (shotResult.simpleOrComplex == Miss) {
      resultImpacts = resultImpacts.updated(Coordinate.toIndex(shotCoordinate, dimensionLetters), Miss)
    }
    else {
      if (shotResult.simpleOrComplex.asInstanceOf[ComplexUpdatedValue].hitType == FullHit) {
        val sunkShip = determineSunkShip
        for (coordinate <- sunkShip.coordinates) {
          resultImpacts = resultImpacts.updated(Coordinate.toIndex(coordinate, dimensionLetters), shotResult.simpleOrComplex.asInstanceOf[OtherMapValue])
        }
      }
      else {
        resultImpacts = resultImpacts.updated(Coordinate.toIndex(shotCoordinate, dimensionLetters), shotResult.simpleOrComplex.asInstanceOf[OtherMapValue])
      }
    }
    new OtherMap(dimensionLetters, dimensionDigits, resultImpacts)
  }

  def initializeMap() : List[InitializedSimpleValue] = {
    var result = List[InitializedSimpleValue]()
    for (i <- 0 until dimensionLetters * dimensionDigits) result = result :+ NotShot
    result
  }

}
