package de.falk.battleships.model

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