package de.falk.battleships.model

case class Coordinate(dimensionLetters: Int, dimensionDigits: Int) {

  def goto(direction: Direction) : Coordinate = {
    direction match {
      case East => new Coordinate(dimensionLetters + 1, dimensionDigits)
      case South => new Coordinate(dimensionLetters, dimensionDigits + 1)
      case West => new Coordinate(dimensionLetters - 1, dimensionDigits)
      case North => new Coordinate(dimensionLetters, dimensionDigits - 1)
    }
  }

}

object Coordinate {

  def toIndex(coordinate: Coordinate, dimLetters: Int) : Int = dimLetters * coordinate.dimensionDigits + coordinate.dimensionLetters

  def fromIndex(index: Int, dimLetters: Int) : Coordinate = {
    def lineToCoordinateRecursive(indexCurrent: Int, dimensionDigitsCounter: Int) : Coordinate = {
      if (indexCurrent < dimLetters)
        new Coordinate(indexCurrent, dimensionDigitsCounter)
      else
        lineToCoordinateRecursive(indexCurrent - dimLetters, dimensionDigitsCounter + 1)
    }
    lineToCoordinateRecursive(indexCurrent = index, dimensionDigitsCounter = 0)
  }


}
