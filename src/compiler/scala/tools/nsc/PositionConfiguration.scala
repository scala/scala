package scala.tools.nsc;
import scala.tools.nsc.reporters._
import scala.tools.nsc.util.Position;

trait PositionConfiguration {
  type PositionType;
  def coercePosToInt(pos : PositionType) : Int;
  def coerceIntToPos(pos : Int) : PositionType;
  val NoPos : PositionType;
  val FirstPos : PositionType;
}
