package de.falk.battleships.generators

import java.util.Random

object Random {
  val random = new Random(System.currentTimeMillis())

  def nextInt(lowerLimitInclusive: Int, upperLimitExclusive: Int) : Int = {
    if (lowerLimitInclusive >= upperLimitExclusive) {
      throw new IllegalArgumentException("The parameter 'lowerLimitInclusive' is greater or equal than the parameter 'upperLimitInclusive' of the method 'RandomInteger.next'.")
    }

    lowerLimitInclusive + random.nextInt(upperLimitExclusive - lowerLimitInclusive)
  }
}
