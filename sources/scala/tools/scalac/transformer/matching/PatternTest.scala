import scalac.ast.Tree ;
import scalac.atree.AConstant ;
import scalac.symtab.Type ;
import scala.util.alphabet.{ AlphabetPlusWildcard, WildcardLabel };

package scala.tools.scalac.transformer.matching {

  abstract class PatternTest extends AlphabetPlusWildcard ;

  case class  Constructor( tpe:Type ) extends PatternTest ;
  case class EqualsValue( value:Tree )         extends PatternTest;
  case class TypeTest( tpe:Type )              extends PatternTest;

  case object WildcardTest extends PatternTest with WildcardLabel;


}
