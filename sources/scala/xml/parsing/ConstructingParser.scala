package scala.xml.parsing ;


/** an xml parser. parses XML and invokes callback methods of a MarkupHandler
 */
abstract class ConstructingParser extends MarkupParser[Node] {

  val handle = new ConstructingHandler();

  //val enableEmbeddedExpressions: Boolean;

  /** report a syntax error */
  def reportSyntaxError(str: String): Unit = throw FatalError(str);

  /** this method assign the next character to ch and advances in input */
  def nextch: Unit;

  /** this method should assign the first character of the input to ch */
  def init: Unit;


}
