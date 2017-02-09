package de.falk.battleships.model

sealed trait FieldValue {
  def sign : String
}

sealed trait InitalizedValue extends FieldValue
sealed trait UpdatedValue extends FieldValue

case object InitializedNotShot extends InitalizedValue {
  def sign = "-"
}

case class InitializedShip(hitType: HitType, shipType: ShipType) extends InitalizedValue {
  def sign = "#"
}

case object UpdatedShot extends UpdatedValue {
  def sign = "+"
}

case class UpdatedShip(hitType: HitType, shipType: ShipType) extends UpdatedValue {
  def sign = hitType match {
    case NoHit => "+"
    case PartialHit => "*"
    case FullHit => "X"
  }
}

sealed trait HitType {
  def displayName: String
}

case object NoHit extends HitType {
  def displayName = "no hit"
}

case object PartialHit extends HitType {
  def displayName = "partial hit"
}

case object FullHit extends HitType {
  def displayName = "full hit"
}

