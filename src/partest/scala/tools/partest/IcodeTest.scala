/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.partest

import scala.tools.nsc._
import nest.FileUtil._
import io.Directory

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
  def printIcodeAfterPhase = "icode"
  // override to use source code other than the file being tested.
  def code = testPath.slurp()

  override def extraSettings: String = "-usejavacp -Xprint-icode:" + printIcodeAfterPhase

  // Compile, read in all the *.icode files, delete them, and return their contents
  def collectIcode(args: String*): List[String] = {
    compile("-d" :: testOutput.path :: args.toList : _*)
    val icodeFiles = testOutput.files.toList filter (_ hasExtension "icode")

    try     icodeFiles sortBy (_.name) flatMap (f => f.lines.toList)
    finally icodeFiles foreach (f => f.delete())
  }

  // Default show() compiles the code with and without optimization and
  // outputs the diff.
  def show() {
    val lines1 = collectIcode("")
    val lines2 = collectIcode("-optimise")

    println(compareContents(lines1, lines2))
  }
}
