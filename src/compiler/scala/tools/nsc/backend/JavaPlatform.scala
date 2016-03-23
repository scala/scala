/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package backend

import io.AbstractFile
import scala.tools.nsc.classpath.{AggregateFlatClassPath, FlatClassPath}
import scala.tools.nsc.settings.ClassPathRepresentationType
import scala.tools.nsc.util.{ClassFileLookup, ClassPath, MergedClassPath}
import scala.tools.util.FlatClassPathResolver
import scala.tools.util.PathResolver

trait JavaPlatform extends Platform {
  val global: Global
  override val symbolTable: global.type = global
  import global._
  import definitions._

  private[nsc] var currentClassPath: Option[MergedClassPath[AbstractFile]] = None

  def classPath: ClassPath[AbstractFile] = {
    assert(settings.YclasspathImpl.value == ClassPathRepresentationType.Recursive,
      "To use recursive classpath representation you must enable it with -YclasspathImpl:recursive compiler option.")

    if (currentClassPath.isEmpty) currentClassPath = Some(new PathResolver(settings).result)
    currentClassPath.get
  }

  private[nsc] var currentFlatClassPath: Option[FlatClassPath] = None

  private[nsc] def flatClassPath: FlatClassPath = {
    assert(settings.YclasspathImpl.value == ClassPathRepresentationType.Flat,
      "To use flat classpath representation you must enable it with -YclasspathImpl:flat compiler option.")

    if (currentFlatClassPath.isEmpty) currentFlatClassPath = Some(new FlatClassPathResolver(settings).result)
    currentFlatClassPath.get
  }

  /** Update classpath with a substituted subentry */
  def updateClassPath(subst: Map[ClassFileLookup[AbstractFile], ClassFileLookup[AbstractFile]]) = global.classPath match {
    case cp: ClassPath[AbstractFile] =>
      val s = subst.asInstanceOf[Map[ClassPath[AbstractFile], ClassPath[AbstractFile]]]
      currentClassPath = Some(new MergedClassPath(cp.entries map (e => s.getOrElse(e, e)), cp.context))

    case AggregateFlatClassPath(entries) =>
      val s = subst.asInstanceOf[Map[FlatClassPath, FlatClassPath]]
      currentFlatClassPath = Some(AggregateFlatClassPath(entries map (e => s.getOrElse(e, e))))

    case cp: FlatClassPath =>
      currentFlatClassPath = Some(subst.getOrElse(cp, cp).asInstanceOf[FlatClassPath])
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
