/*
 * Copyright (c) 2014 Contributor. All rights reserved.
 */
package scala.tools.nsc.util

import scala.tools.nsc.io.AbstractFile
import java.net.URL

/**
 * Simple interface that allows us to abstract over how class file lookup is performed
 * in different classpath representations.
 */
// TODO at the end, after the possible removal of the old classpath representation, this class shouldn't be generic
// T should be just changed to AbstractFile
trait ClassFileLookup[T] {
  def findClassFile(name: String): Option[AbstractFile]

  /**
   * It returns both classes from class file and source files (as our base ClassRepresentation).
   * So note that it's not so strictly related to findClassFile.
   */
  def findClass(name: String): Option[ClassRepresentation[T]]

  /**
   * A sequence of URLs representing this classpath.
   */
  def asURLs: Seq[URL]

  /** The whole classpath in the form of one String.
    */
  def asClassPathString: String

  // for compatibility purposes
  @deprecated("Use asClassPathString instead of this one", "2.11.5")
  def asClasspathString: String = asClassPathString

  /** The whole sourcepath in the form of one String.
    */
  def asSourcePathString: String
}

/**
 * Represents classes which can be loaded with a ClassfileLoader and/or SourcefileLoader.
 */
// TODO at the end, after the possible removal of the old classpath implementation, this class shouldn't be generic
// T should be just changed to AbstractFile
trait ClassRepresentation[T] {
  def binary: Option[T]
  def source: Option[AbstractFile]

  def name: String
}

object ClassRepresentation {
  def unapply[T](classRep: ClassRepresentation[T]): Option[(Option[T], Option[AbstractFile])] =
    Some((classRep.binary, classRep.source))
}
