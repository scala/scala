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

  type T_label <: Label;
  type regexp <: RegExp ;

  case class Letter(a: T_label)    extends RegExp {
    final val isNullable = false;
  }

}
