/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package backend

import io.AbstractFile
import scala.tools.nsc.classpath.{AggregateFlatClassPath, FlatClassPath}
import scala.tools.nsc.settings.ClassPathRepresentationType
import scala.tools.nsc.util.ClassFileLookup
import scala.tools.util.FlatClassPathResolver
import scala.tools.util.PathResolver

trait JavaPlatform extends Platform {
  val global: Global
  override val symbolTable: global.type = global
  import global._
  import definitions._

  private[nsc] var currentFlatClassPath: Option[FlatClassPath] = None

  private[nsc] def flatClassPath: FlatClassPath = {
    if (currentFlatClassPath.isEmpty) currentFlatClassPath = Some(new FlatClassPathResolver(settings).result)
    currentFlatClassPath.get
  }

  /** Update classpath with a substituted subentry */
  def updateClassPath(subst: Map[FlatClassPath, FlatClassPath]): Unit = global.classPath match {
    case AggregateFlatClassPath(entries) =>
      currentFlatClassPath = Some(AggregateFlatClassPath(entries map (e => subst.getOrElse(e, e))))

    case cp: FlatClassPath =>
      currentFlatClassPath = Some(subst.getOrElse(cp, cp))
  }

  def platformPhases = List(
    flatten,  // get rid of inner classes
    genBCode  // generate .class files
  )

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

  def needCompile(bin: AbstractFile, src: AbstractFile) =
    src.lastModified >= bin.lastModified
}
