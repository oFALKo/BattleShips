# Changes of BattleShips versions

# v0.2.3
    - remove abstract class Map
    - remove Types.scala
    - simplified FieldValue.scala

# v0.2.2 some simplifications:
    delete Board.scala, InformationForShot + ShotResult -> Types.scala, ShipType + Direction -> Ship.scala, HitType -> FieldValue.scala

# v0.2.1 some simplifications:
    delete Board.scala, InformationForShot + ShotResult -> Types.scala, ShipType + Direction -> Ship.scala, HitType -> FieldValue.scala

# v0.2 modifications to 'Coordinate.scala'

- simplify method 'Coordinate.fromIndex' from recursive approach to a more concise mathematical way

      def fromIndex(index: Int, dimLetters: Int) : Coordinate = {
        def lineToCoordinateRecursive(indexCurrent: Int, dimensionDigitsCounter: Int) : Coordinate = {
          if (indexCurrent < dimLetters)
            new Coordinate(indexCurrent, dimensionDigitsCounter)
          else
            lineToCoordinateRecursive(indexCurrent - dimLetters, dimensionDigitsCounter + 1)
        }
        lineToCoordinateRecursive(indexCurrent = index, dimensionDigitsCounter = 0)
      }

      ===>

      def fromIndex(index: Int, dimLetters: Int) = Coordinate(index / dimLetters, index % dimLetters)
