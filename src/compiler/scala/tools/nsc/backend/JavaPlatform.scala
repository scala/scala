/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc
package backend

import io.AbstractFile
import scala.tools.nsc.classpath.AggregateClassPath
import scala.tools.util.PathResolver
import scala.tools.nsc.util.ClassPath

trait JavaPlatform extends Platform {
  val global: Global
  override val symbolTable: global.type = global
  import global._
  import definitions._

  private[nsc] var currentClassPath: Option[ClassPath] = None

  protected[nsc] def classPath: ClassPath = {
    if (currentClassPath.isEmpty) currentClassPath = Some(new PathResolver(settings, global.closeableRegistry).result)
    currentClassPath.get
  }

  /** Update classpath with a substituted subentry */
  def updateClassPath(subst: Map[ClassPath, ClassPath]): Unit = global.classPath match {
    case AggregateClassPath(entries) =>
      currentClassPath = Some(AggregateClassPath(entries map (e => subst.getOrElse(e, e))))

    case cp: ClassPath =>
      currentClassPath = Some(subst.getOrElse(cp, cp))
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
    (sym == SerializableClass) ||
    (sym == ComparableClass) ||
    (sym isNonBottomSubClass BoxedNumberClass) ||
    (sym isNonBottomSubClass BoxedCharacterClass) ||
    (sym isNonBottomSubClass BoxedBooleanClass)
  }

  def needCompile(bin: AbstractFile, src: AbstractFile) =
    src.lastModified >= bin.lastModified
}
