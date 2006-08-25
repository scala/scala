package scala.tools.nsc;
import scala.tools.nsc.reporters._
import scala.tools.nsc.util.Position;

class StdGlobal(settings: Settings, reporter: Reporter) extends Global(settings, reporter) {
  type PositionType = Int;
  implicit def coercePosToInt(pos : PositionType) : Int = pos;
  def coerceIntToPos(pos : Int) : PositionType = pos;
  val NoPos = Position.NOPOS;
  val FirstPos = Position.FIRSTPOS;
}
