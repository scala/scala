package scala.tools.nsc;

abstract class PositionConfiguration {
  type PositionType;
  def coercePosToInt(pos : PositionType) : Int;
  def coerceIntToPos(pos : Int) : PositionType;
  val NoPos : PositionType;
  val FirstPos : PositionType;
}
