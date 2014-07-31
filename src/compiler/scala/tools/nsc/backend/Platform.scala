/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package backend

import util.ClassPath
import io.AbstractFile
import scala.tools.nsc.classpath.FlatClassPath

/** The platform dependent pieces of Global.
 */
trait Platform {
  val symbolTable: symtab.SymbolTable
  import symbolTable._

  /** Old implementation of compiler's classpath. */
  def classPath: ClassPath[AbstractFile]

  // TODO finally this one should replace old implementation and be renamed to classPath
  /** New implementation of compiler's classpath. */
  def flatClassPath: FlatClassPath

  /** Any platform-specific phases. */
  def platformPhases: List[SubComponent]

  /** Symbol for a method which compares two objects. */
  def externalEquals: Symbol

  /** The various ways a boxed primitive might materialize at runtime. */
  def isMaybeBoxed(sym: Symbol): Boolean

  /**
   * Tells whether a class with both a binary and a source representation
   * (found in classpath and in sourcepath) should be re-compiled. Behaves
   * on the JVM similar to javac, i.e. if the source file is newer than the classfile,
   * a re-compile is triggered. On .NET by contrast classfiles always take precedence.
   */
  def needCompile(bin: AbstractFile, src: AbstractFile): Boolean
}

