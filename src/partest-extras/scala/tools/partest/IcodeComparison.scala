/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.partest

import scala.tools.partest.nest.FileManager.compareContents

/** A class for testing icode.  All you need is this in a
 *  partest source file --
 *  {{{
 *    object Test extends IcodeComparison
 *  }}}
 *  -- and the generated output will be the icode for everything
 *  in that file.  See scaladoc for possible customizations.
 *  TODO promote me to partest
 */
abstract class IcodeComparison extends DirectTest {
  /** The phase after which icode is printed.
   *  Override to check icode at a different point,
   *  but you can't print at a phase that is not enabled
   *  in this compiler run. Defaults to "icode".
   */
  def printIcodeAfterPhase = "icode"

  /** When comparing the output of two phases, this is
   *  the other phase of interest, normally the preceding
   *  phase.  Defaults to "icode" for tests of optimizer phases.
   */
  def printSuboptimalIcodeAfterPhase = "icode"

  /** The source code to compile defaults to the test file.
   *  I.e., the test file compiles itself. For a comparison,
   *  the test file will be compiled three times.
   */
  def code = testPath.slurp()

  /** By default, the test code is compiled with -usejavacp. */
  override def extraSettings: String = "-usejavacp"

  /** Compile the test code and return the contents of all
   *  (sorted) .icode files, which are immediately deleted.
   *  @param arg0 at least one arg is required
   *  @param args must include -Xprint-icode:phase
   */
  def collectIcode(arg0: String, args: String*): List[String] = {
    compile("-d" :: testOutput.path :: arg0 :: args.toList : _*)
    val icodeFiles = testOutput.files.toList filter (_ hasExtension "icode")

    // Some methods in scala.reflect.io.File leak an InputStream, leaving the underlying file open.
    // Windows won't delete an open file, but we must ensure the files get deleted, since the logic
    // here depends on it (collectIcode will be called multiple times, and we can't allow crosstalk
    // between calls).  So we are careful to use `slurp` which does call `close`, and careful to
    // check that `delete` returns true indicating successful deletion.
    try     icodeFiles sortBy (_.name) flatMap (f => f.slurp().lines.toList)
    finally icodeFiles foreach (f => require(f.delete()))
  }

  /** Collect icode at the default phase, `printIcodeAfterPhase`. */
  def collectIcode(): List[String] = collectIcode(s"-Xprint-icode:$printIcodeAfterPhase")

  /** Default show is showComparison. May be overridden for showIcode or similar. */
  def show() = showComparison()

  /** Compile the test code with and without optimization, and
   *  then print the diff of the icode.
   */
  def showComparison() = {
    val lines1 = collectIcode(s"-Xprint-icode:$printSuboptimalIcodeAfterPhase")
    val lines2 = collectIcode("-optimise", s"-Xprint-icode:$printIcodeAfterPhase")

    println(compareContents(lines1, lines2))
  }

  /** Print icode at the default phase, `printIcodeAfterPhase`. */
  def showIcode() = println(collectIcode() mkString EOL)
}
