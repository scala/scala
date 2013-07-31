/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.partest

import scala.tools.partest.nest.FileUtil.compareContents

/** A trait for testing icode.  All you need is this in a
 *  partest source file:
 *  {{{
 *    object Test extends IcodeTest
 *  }}}
 *  And then the generated output will be the icode for everything
 *  in that file.  See source for possible customizations.
 */
abstract class IcodeTest extends DirectTest {
  // override to check icode at a different point.
  // but you can't print at a phase that is not configured.
  def printIcodeAfterPhase = "icode"
  def printSuboptimalIcodeAfterPhase = "icode"
  // override to use source code other than the file being tested.
  def code = testPath.slurp()

  override def extraSettings: String = "-usejavacp"

  /** Compile the test and return the contents of all (sorted) .icode
   *  files, which are immediately deleted. 
   *  @param args must include -Xprint-icode:phase
   */
  def collectIcode(arg0: String, args: String*): List[String] = {
    compile("-d" :: testOutput.path :: arg0 :: args.toList : _*)
    val icodeFiles = testOutput.files.toList filter (_ hasExtension "icode")

    try     icodeFiles sortBy (_.name) flatMap (f => f.lines.toList)
    finally icodeFiles foreach (f => f.delete())
  }
  /** Use the default phase, `printIcodeAfterPhase`. */
  def collectIcode(): List[String] =
    collectIcode(s"-Xprint-icode:$printIcodeAfterPhase")

  // Default show() compiles the code with and without optimization and
  // outputs the diff.
  def show() {
    val lines1 = collectIcode(s"-Xprint-icode:$printSuboptimalIcodeAfterPhase")
    val lines2 = collectIcode("-optimise", s"-Xprint-icode:$printIcodeAfterPhase")

    println(compareContents(lines1, lines2))
  }
  def showIcode() = println(collectIcode() mkString "\n")
}
