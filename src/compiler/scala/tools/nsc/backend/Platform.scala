/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package backend

import util.ClassPath
import io.AbstractFile

/** The platform dependent pieces of Global.
 */
trait Platform {
  val global: Global
  import global._

  /** The binary classfile representation type */
  type BinaryRepr

  /** The compiler classpath. */
  def classPath: ClassPath[BinaryRepr]

  /** The root symbol loader. */
  def rootLoader: LazyType

  /** Update classpath with a substitution that maps entries to entries */
  def updateClassPath(subst: Map[ClassPath[BinaryRepr], ClassPath[BinaryRepr]])

  /** Any platform-specific phases. */
  def platformPhases: List[SubComponent]

  /** Symbol for a method which compares two objects. */
  def externalEquals: Symbol

  /** The various ways a boxed primitive might materialize at runtime. */
  def isMaybeBoxed(sym: Symbol): Boolean

  /** Create a new class loader to load class file `bin` */
  def newClassLoader(bin: BinaryRepr): loaders.SymbolLoader

  /**
   * Tells whether a class should be loaded and entered into the package
   * scope. On .NET, this method returns `false` for all synthetic classes
   * (anonymous classes, implementation classes, module classes), their
   * symtab is encoded in the pickle of another class.
   */
  def doLoad(cls: ClassPath[BinaryRepr]#ClassRep): Boolean

  /**
   * Tells whether a class with both a binary and a source representation
   * (found in classpath and in sourcepath) should be re-compiled. Behaves
   * on the JVM similar to javac, i.e. if the source file is newer than the classfile,
   * a re-compile is triggered. On .NET by contrast classfiles always take precedence.
   */
  def needCompile(bin: BinaryRepr, src: AbstractFile): Boolean
}

