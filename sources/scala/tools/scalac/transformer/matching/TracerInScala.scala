import scalac.ast.Tree;
import scalac.symtab.Symbol;
import scalac.symtab.Type;
//import scalac.transformer.matching.CodeFactory ;
import scalac.transformer.matching.DetWordAutom ;

package scala.tools.scalac.transformer.matching {
/** @todo: factor common things of LeftTracerInScala and RightTracerInScala
 */
class TracerInScala(dfa: DetWordAutom, elementType: Type, owner: Symbol, cf: CodeFactory )
extends Autom2Scala(dfa, elementType, owner, cf) {

  override var optimize = true;

  final def collectVars(pat: Tree) = {
    val c = new CollectVariableTraverser();
    c.collectVars(pat)
  }

  final def containsBinding(pat: Tree) = {
    val c = new CollectVariableTraverser();
    c.containsBinding(pat);
  }

}
}
