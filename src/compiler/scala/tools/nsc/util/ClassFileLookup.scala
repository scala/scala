/*
 * Copyright (c) 2014 Contributor. All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Scala License which accompanies this distribution, and
 * is available at http://www.scala-lang.org/license.html
 */
package scala.tools.nsc.util

import scala.tools.nsc.io.AbstractFile
import java.net.URL

/**
 * Simple interface that allows us to abstract over how class file lookup is performed
 * in different classpath implementations.
 */
// TODO at the end, after removal of old classpath implementation, this class shouldn't be generic
// T should be just changed to AbstractFile
trait ClassFileLookup[T] {
  def findClassFile(name: String): Option[AbstractFile]

  def findClass(name: String): Option[ClassRepresentation[T]]

  /**
   * A sequence of URLs representing this classpath.
   */
  def asURLs: Seq[URL]
}

/**
 * Represents classes which can be loaded with a ClassfileLoader and/or SourcefileLoader.
 */
// TODO at the end, after removal of old classpath implementation, this class shouldn't be generic
// T should be just changed to AbstractFile
trait ClassRepresentation[T] {
  val binary: Option[T]
  val source: Option[AbstractFile]

  def name: String
}

object ClassRepresentation {
  def unapply[T](classRep: ClassRepresentation[T]): Option[(Option[T], Option[AbstractFile])] =
    Some((classRep.binary, classRep.source))
}
