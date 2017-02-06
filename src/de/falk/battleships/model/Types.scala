package de.falk.battleships.model

case class InformationForShot(notShots: List[Coordinate], partialHits: List[(Coordinate, ShipType)])

case class ShotResult(simpleOrComplex: ShotResultValue)
