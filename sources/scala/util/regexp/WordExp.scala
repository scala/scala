// $Id$

package scala.util.regexp ;

/** regular word expressions. use them with an alphabet: <pre>
abstract class IntLabels extends Alphabet;
object IntWordExp extends WordExp[IntLabels] {
  type regexp = RegExp;
};
 * </pre>
 */

trait WordExp[ A <: Alphabet ] extends Base {

  type label = A;

  case class Letter(a: label)    extends RegExp;

}
