// $Id$

package scala.util.regexp ;

import scala.util.alphabet.Alphabet ;

/** regular word expressions. use them with an alphabet: <pre>
abstract class IntLabels extends Alphabet;
object IntWordExp extends WordExp[IntLabels] {
  type regexp = RegExp;
};
 * </pre>
 */
trait WordExp[ A <: Alphabet ] extends Base {

  case class Letter(a: A)    extends RegExp {
    final val isNullable = false;
  }

}
