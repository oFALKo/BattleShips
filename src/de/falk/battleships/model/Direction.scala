package de.falk.battleships.model

sealed trait Direction

case object North extends Direction
case object East extends Direction
case object South extends Direction
case object West extends Direction

object Direction {
  def fromIntTo(i: Int) : Direction = i match {
    case 0 => East
    case 1 => South
    case 2 => West
    case _ => North
  }
}

