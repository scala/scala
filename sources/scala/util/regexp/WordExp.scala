// $Id$

package scala.util.regexp ;

/** regular word expressions. use them with an alphabet: <pre>
abstract class IntLabels extends Alphabet;
object IntWordExp extends WordExp[IntLabels] {
  type regexp = RegExp;
};
 * </pre>
 */
trait WordExp extends Base {

  trait Label;

  type _regexpT <: RegExp ;
  type _labelT <: Label;

  case class Letter(a: _labelT)    extends RegExp {
    final val isNullable = false;
  }

}
