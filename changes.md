# Changes of BattleShips versions



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