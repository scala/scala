package scala.tools.servlet.engine.config ;

import tools.util.SourceReader ;

import scala.xml.parsing.MarkupParser;

import scala.collection.mutable;

case class ConfigReaderError(msg: String) extends Exception(msg);

class ConfigReader(inp: String) extends MarkupParser[Config] {

  /** sets up reader for source files */
  val inputIt: Iterator[Char] = {
    val charset = java.nio.charset.Charset.forName("ISO-8859-1"); // @todo
    val decoder = charset.newDecoder();
    val reader = new SourceReader(decoder);
    val file:Seq[Char] = reader.read( inp );
    file.elements
  }

  override var ch = inputIt.next;

  def nextch = {
    if( inputIt.hasNext )
      ch = inputIt.next;
  }

  val handle = new ConfigHandler();

  //val enableEmbeddedExpressions: Boolean;

  /** report a syntax error */
  def reportSyntaxError(str: String): Unit = throw ConfigReaderError(str);

  def init = nextch;
  val preserveWS = false;

}
