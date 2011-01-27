package scala.tools.nsc.interactive
package tests

import scala.tools.nsc.Settings
import scala.tools.nsc.reporters.StoreReporter
import scala.tools.nsc.util.{BatchSourceFile, SourceFile, Position}
import scala.tools.nsc.io._

/** A base class for writing interactive compiler tests.
 *
 * @author Iulian Dragos
 *
 */
abstract class InteractiveTest {

  val settings = new Settings
  val reporter= new StoreReporter

  settings.YpresentationDebug.value = true
  lazy val compiler: CompilerControl = new Global(settings, reporter)

  def sources(filename: String*): Seq[SourceFile] =
    filename map source

  def source(filename: String) = new BatchSourceFile(AbstractFile.getFile(filename))

  def pos(filename: String, line: Int, col: Int): Position =
    source(filename).position(line, col)

  def runTest: Unit

  def main(args: Array[String]) {
    runTest
  }
}

