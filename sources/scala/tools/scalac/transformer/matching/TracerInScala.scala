import scalac.ast.Tree;
import scalac.symtab.Symbol;
import scalac.symtab.Type;

package scala.tools.scalac.transformer.matching {
/** @todo: factor common things of LeftTracerInScala and RightTracerInScala
 */
class TracerInScala(dfa: DetWordAutom, elementType: Type, owner: Symbol, cf: CodeFactory )
extends Autom2Scala(dfa, elementType, owner, cf) {

  override var optimize = true;

}
}
