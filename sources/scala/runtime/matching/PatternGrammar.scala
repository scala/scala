package scala.runtime.matching ;

import scala.util.alphabet.IntAlphabet ;
import scala.collection.{ immutable, mutable, Map, Set };

/** runtime representation of patterns. This class augments
 *  scala.util.grammar.TreeHedgeGrammar, with an abstract representation
 *  of variable bindings. Variables are simply consecutive integers,
 *  following pre-order of occurrence in pattern
 *  @caseVars an array, field i holding the number of variables in case i
 */
abstract class PatternGrammar extends scala.util.grammar.ImmutableTreeHedgeGrammar[IntAlphabet] {

  val vars:Array[Int];

  def test(test:Int, inp:Any): Boolean;
}
