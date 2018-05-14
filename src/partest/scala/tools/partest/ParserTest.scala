/* NSC -- new Scala compiler
 * Copyright 2005-2014 LAMP/EPFL
 */

package scala.tools.partest

/** A class for testing parser output.
 *  Just supply the `code` and update the check file.
 */
abstract class ParserTest extends DirectTest {

  override def extraSettings: String = "-usejavacp -Ystop-after:parser -Xprint:parser"

  override def show(): Unit = {
    // redirect err to out, for logging
    val prevErr = System.err
    System.setErr(System.out)
    compile()
    System.setErr(prevErr)
  }
}
