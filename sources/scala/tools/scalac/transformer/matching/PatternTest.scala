import scalac.ast.Tree ;
import scalac.atree.AConstant ;
import scalac.symtab.Type ;
import scala.util.alphabet.Alphabet;

package scala.tools.scalac.transformer.matching {

  abstract class PatternTest extends Alphabet ;

  case class  Constructor( tpe:Type ) extends PatternTest ;
  case object WildCard                extends PatternTest;

  abstract class LeafTest extends PatternTest ;

  case class EqualsConstant( const:AConstant ) extends LeafTest ;
  case class EqualsValue( value:Tree )         extends LeafTest ;
  case class TypeTest( tpe:Type )        extends LeafTest;

}
