package scala.tools.nsc
package util

import java.io.{FileReader, BufferedReader, StreamTokenizer, FileNotFoundException}
import scala.tools.nsc.io.AbstractFile
import scala.collection.mutable.ListBuffer

/**
 * Expands all arguments starting with @ to the contents of the
 * file named like each argument.
 */
object ArgumentsExpander {

  def expandArg(arg: String): List[String] =
    expandFromFile(arg.substring(1))

  /*
   * Extracts all the arguments in a specified file.
   * Throws FileNotFoundException if the file does not exist.
   */
  private def expandFromFile(fileName: String): List[String]  = {
    val f = AbstractFile.getFile(fileName)
    if (f eq null) throw new FileNotFoundException(
      "argument file "+ fileName +" could not be found")

    val in = new BufferedReader(new FileReader(f.file))

    val tokenizer = new StreamTokenizer( in )
    tokenizer.resetSyntax
    tokenizer.wordChars(' ', 255)
    tokenizer.whitespaceChars(0, ' ')
    tokenizer.commentChar('#')
    tokenizer.quoteChar('"')
    tokenizer.quoteChar('\'')

    val ts = new ListBuffer[String]
    while (tokenizer.nextToken() != StreamTokenizer.TT_EOF) {
      ts += tokenizer.sval
    }
    in.close()
    ts.toList
  }
}
