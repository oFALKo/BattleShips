package de.falk.battleships.model

class OwnMap(val dimensionLetters: Int, val dimensionDigits: Int, val ships: List[Ship], val impacts: List[FieldValue])  {

  def getShot(impactCoordinate: Coordinate) : (OwnMap, UpdatedValue) = {
    def evaluateImpact : UpdatedValue = {
      ships.find(ship => ship.isHitBy(impactCoordinate)) match {
        case None => UpdatedShot
        case Some(hitShip) =>
          if (hitShip.willSunk(impactCoordinate))
            UpdatedShip(FullHit, hitShip.shipType)
          else
            UpdatedShip(PartialHit, hitShip.shipType)
      }
    }

    def shipsChanged(shotResult: UpdatedValue) : List[Ship] = {
      ships.find(ship => ship.isHitBy(impactCoordinate)) match {
        case None => ships // return the same
        case Some(hitShip) => {
          val index = hitShip.findIndexOfShipInsideList(ships)
          if (index == -1)
            ships // should never happen
          else
            shotResult match {
              case UpdatedShot => ships // return the same
              case updatedShip: UpdatedShip => ships.updated(index, hitShip.doDamage(impactCoordinate))
            }
        }
      }
    }

    def impactChanged : List[FieldValue] = {
      var result = List[FieldValue]()
      ships.find(ship => ship.isHitBy(impactCoordinate)) match {
        case None => impacts.updated(Coordinate.toIndex(impactCoordinate, dimensionLetters), UpdatedShot)
        case Some(hitShip) => {
          if (hitShip.willSunk(impactCoordinate)) {
            result = impacts.updated(Coordinate.toIndex(hitShip.coordinate, dimensionLetters), UpdatedShip(FullHit, hitShip.shipType))
            for (coordinateToUpdate <- hitShip.coordinates)
              result = result.updated(Coordinate.toIndex(coordinateToUpdate, dimensionLetters), UpdatedShip(FullHit, hitShip.shipType))
          }
          else
            result = impacts.updated(Coordinate.toIndex(impactCoordinate, dimensionLetters), UpdatedShip(PartialHit, hitShip.shipType))
          result
        }
      }

    }

    val shotResult = evaluateImpact
    (new OwnMap(dimensionLetters, dimensionDigits, shipsChanged(shotResult), impactChanged), shotResult)
  }

  def moreThanHalfMapOccupied : Boolean = ships.map(s => s.shipType.length).sum >= dimensionLetters * dimensionDigits / 2

  def allShipsSunk : Boolean = ships.filter(s => ! s.isDestroyed).isEmpty

  def shipCollide(ship: Ship) : Boolean = ships.filter(s => s.overlap(ship)).nonEmpty

  def shipOutside(ship: Ship) : Boolean = ship.coordinates.filter(c => c.dimensionLetters < 0 || c.dimensionDigits < 0 || c.dimensionLetters >= dimensionLetters || c.dimensionDigits >= dimensionDigits).nonEmpty

}

class OtherMap(val dimensionLetters: Int, val dimensionDigits: Int, val impacts: List[FieldValue]) {

  def coordinatesNotShotAndPartialHits : (List[Coordinate], List[(Coordinate, ShipType)]) = {
    var coordinatesNotShot = List[Coordinate]()
    var coordinatesPartialHit = List[(Coordinate, ShipType)]()

    for (dimLetters <- 0 until dimensionLetters) {
      for (dimDigits <- 0 until dimensionDigits) {
        val index = dimensionLetters * dimDigits + dimLetters
        coordinatesNotShot = if (InitializedNotShot == impacts(index)) coordinatesNotShot :+ new Coordinate(dimLetters, dimDigits) else coordinatesNotShot
        coordinatesPartialHit =
          if (impacts(index).isInstanceOf[UpdatedShip] && impacts(index).asInstanceOf[UpdatedShip].hitType == PartialHit)
            coordinatesPartialHit :+ (new Coordinate(dimLetters, dimDigits), impacts(index).asInstanceOf[UpdatedShip].shipType)
          else
            coordinatesPartialHit
      }
    }
    (coordinatesNotShot, coordinatesPartialHit)
  }

  def insertShotResult(shotCoordinate: Coordinate, shotResult: UpdatedValue, otherPlayerShips: List[Ship]) : OtherMap = {
    var resultImpacts:List[FieldValue] = impacts

    def determineSunkShip : Ship = {
      otherPlayerShips.find(ship => ship.isHitBy(shotCoordinate)) match {
        case None => otherPlayerShips.head // should never happen
        case Some(hitShip) => hitShip
      }
    }

    if (shotResult == UpdatedShot) {
      resultImpacts = resultImpacts.updated(Coordinate.toIndex(shotCoordinate, dimensionLetters), UpdatedShot)
    }
    else {
      if (shotResult.asInstanceOf[UpdatedShip].hitType == FullHit) {
        val sunkShip = determineSunkShip
        for (coordinate <- sunkShip.coordinates) {
          resultImpacts = resultImpacts.updated(Coordinate.toIndex(coordinate, dimensionLetters), shotResult)
        }
      }
      else {
        resultImpacts = resultImpacts.updated(Coordinate.toIndex(shotCoordinate, dimensionLetters), shotResult)
      }
    }
    new OtherMap(dimensionLetters, dimensionDigits, resultImpacts)
  }

  def initializeMap() : List[FieldValue] = {
    var result = List[FieldValue]()
    for (i <- 0 until dimensionLetters * dimensionDigits) result = result :+ InitializedNotShot
    result
  }

}
