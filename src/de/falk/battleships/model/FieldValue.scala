package de.falk.battleships.model

sealed trait FieldValue {
  def sign : String
  def updated : Boolean
  def complex : Boolean
}

sealed trait InitializedValue extends FieldValue {
  def updated = false
}

sealed trait UpdatedValue extends FieldValue {
  def updated = true
}

sealed trait ShotResultValue extends FieldValue

sealed trait SimpleValue extends FieldValue {
  def complex = false
}
sealed trait ComplexValue extends FieldValue {
  def complex = true
  def hitType: HitType
  def shipType: ShipType
}

sealed trait InitializedSimpleValue extends InitializedValue with SimpleValue

sealed trait OwnMapValue extends FieldValue
sealed trait OtherMapValue extends FieldValue

case object Empty extends InitializedSimpleValue with OwnMapValue {
  def sign = "-"
}

case object NotShot extends InitializedSimpleValue with OtherMapValue {
  def sign = "-"
}

case class ComplexInitializedValue(hitType: HitType, shipType: ShipType) extends InitializedValue with ComplexValue with OwnMapValue {
  def sign = "#"
}

case object Miss extends UpdatedValue with SimpleValue with OtherMapValue with ShotResultValue {
  def sign = "+"
}

case object Shot extends UpdatedValue with SimpleValue with OwnMapValue {
  def sign = "+"
}

case class ComplexUpdatedValue(hitType: HitType, shipType: ShipType) extends UpdatedValue with ComplexValue with OwnMapValue with OtherMapValue with ShotResultValue {
  def sign = hitType match {
    case PartialHit => "*"
    case FullHit => "X"
  }
}

