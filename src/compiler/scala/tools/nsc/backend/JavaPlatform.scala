/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package backend

import io.AbstractFile
import util.{ClassPath,JavaClassPath}
import util.ClassPath.{ JavaContext, DefaultJavaContext }
import scala.tools.util.PathResolver

trait JavaPlatform extends Platform {
  import global._
  import definitions._

  type BinaryRepr = AbstractFile

  lazy val classPath  = new PathResolver(settings).result
  def rootLoader = new loaders.PackageLoader(classPath.asInstanceOf[ClassPath[platform.BinaryRepr]])
    // [Martin] Why do we need a cast here?
    // The problem is that we cannot specify at this point that global.platform should be of type JavaPlatform.
    // So we cannot infer that global.platform.BinaryRepr is AbstractFile.
    // Ideally, we should be able to write at the top of the JavaPlatform trait:
    //   val global: Global { val platform: JavaPlatform }
    //   import global._
    // Right now, this does nothing because the concrete definition of platform in Global
    // replaces the tighter abstract definition here. If we had DOT typing rules, the two
    // types would be conjoined and everything would work out. Yet another reason to push for DOT.

  private def depAnalysisPhase =
    if (settings.make.isDefault) Nil
    else List(dependencyAnalysis)

  private def classEmitPhase =
    if (settings.target.value == "jvm-1.5") genJVM
    else genASM

  def platformPhases = List(
    flatten,        // get rid of inner classes
    classEmitPhase  // generate .class files
  ) ++ depAnalysisPhase

  lazy val externalEquals          = getDecl(BoxesRunTimeClass, nme.equals_)
  lazy val externalEqualsNumNum    = getDecl(BoxesRunTimeClass, nme.equalsNumNum)
  lazy val externalEqualsNumChar   = getDecl(BoxesRunTimeClass, nme.equalsNumChar)
  lazy val externalEqualsNumObject = getDecl(BoxesRunTimeClass, nme.equalsNumObject)

  /** We could get away with excluding BoxedBooleanClass for the
   *  purpose of equality testing since it need not compare equal
   *  to anything but other booleans, but it should be present in
   *  case this is put to other uses.
   */
  def isMaybeBoxed(sym: Symbol) = {
    (sym == ObjectClass) ||
    (sym == JavaSerializableClass) ||
    (sym == ComparableClass) ||
    (sym isNonBottomSubClass BoxedNumberClass) ||
    (sym isNonBottomSubClass BoxedCharacterClass) ||
    (sym isNonBottomSubClass BoxedBooleanClass)
  }

  def newClassLoader(bin: AbstractFile): loaders.SymbolLoader =
    new loaders.ClassfileLoader(bin)

  def doLoad(cls: ClassPath[BinaryRepr]#ClassRep): Boolean = true

  def needCompile(bin: AbstractFile, src: AbstractFile) =
    src.lastModified >= bin.lastModified
}
