package scala.tools.nsc
package util

import java.io.{ StreamTokenizer, FileNotFoundException }
import scala.tools.nsc.io.File

/**
 * Expands all arguments starting with @ to the contents of the
 * file named like each argument.
 */
object ArgumentsExpander {
  def expandArg(arg: String): List[String] = {
    require(arg.nonEmpty && arg.head == '@')
    expandFromFile(arg drop 1)
  }

  /*
   * Extracts all the arguments in a specified file.
   * Throws FileNotFoundException if the file does not exist.
   */
  def expandFromFile(fileName: String): List[String] = {
    val file = File(fileName)
    if (!file.exists)
      throw new FileNotFoundException("argument file %s could not be found" format fileName)

    file applyReader { in =>
      val tokenizer = new StreamTokenizer( in )
      tokenizer.resetSyntax
      tokenizer.wordChars(' ', 255)
      tokenizer.whitespaceChars(0, ' ')
      tokenizer.commentChar('#')
      tokenizer.quoteChar('"')
      tokenizer.quoteChar('\'')

      def getToken = if (tokenizer.nextToken == StreamTokenizer.TT_EOF) None else Some(tokenizer.sval)
      Iterator continually getToken takeWhile (_.isDefined) map (_.get) toList
    }
  }
}
