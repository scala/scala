/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Paul Phillips
 *
 * Copyright (c) 2014 Contributor. All rights reserved.
 */

package scala.tools.nsc
package backend

import io.AbstractFile
import util.ClassPath
import scala.tools.util.PathResolver
import scala.tools.nsc.classpath.FlatClassPath
import scala.tools.nsc.classpath.DefaultFlatClassPathManager
import scala.tools.nsc.settings.ClassPathImplementationType

trait JavaPlatform extends Platform {
  val global: Global
  override val symbolTable: global.type = global
  import global._
  import definitions._

  private var currentClassPath: Option[ClassPath[AbstractFile]] = None

  def classPath: ClassPath[AbstractFile] = {
    assert(settings.YclasspathImpl.value == ClassPathImplementationType.Recursive)
    if (currentClassPath.isEmpty) currentClassPath = Some(new PathResolver(settings/*, flatClassPath*/).result) // TODO commented out incremental compiler hack
    currentClassPath.get
  }

  lazy val flatClassPath: FlatClassPath = {
    assert(settings.YclasspathImpl.value == ClassPathImplementationType.Flat)
    DefaultFlatClassPathManager.createClassPath(settings)
  }

  private def classEmitPhase =
    if (settings.isBCodeActive) genBCode
    else genASM

  def platformPhases = List(
    flatten,        // get rid of inner classes
    classEmitPhase  // generate .class files
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
