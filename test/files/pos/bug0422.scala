import scala.util.regexp.WordExp;
import scala.util.automata.WordBerrySethi;

object BoolWordExp extends WordExp {
  type _labelT = MyLabels;
  type _regexpT = RegExp;
  abstract class MyLabels extends Label ;
  case class MyLabel(c:Char) extends MyLabels;
}

object MyTranslator extends WordBerrySethi {
  override val lang = BoolWordExp;
  import lang._;
  override protected def seenLabel( r:RegExp, i:Int, label: _labelT ): Unit = {
    super.seenLabel(r,i,label)
  }
}
