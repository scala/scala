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

package scala
package reflect
package io

import java.io.InputStream

/** A distinguished object so you can avoid both null
 *  and Option.
 *
 *  ''Note:  This library is considered experimental and should not be used unless you know what you are doing.''
 */
object NoAbstractFile extends AbstractFile {
  def absolute: AbstractFile = this
  def container: AbstractFile = this
  def create(): Unit = ???
  def delete(): Unit = ???
  def file: java.io.File = null
  def input: InputStream = null
  def isDirectory: Boolean = false
  override def isVirtual: Boolean = true
  def iterator: Iterator[AbstractFile] = Iterator.empty
  def lastModified: Long = 0L
  def lookupName(name: String, directory: Boolean): AbstractFile = null
  def lookupNameUnchecked(name: String, directory: Boolean): AbstractFile = null
  def name: String = ""
  def output: java.io.OutputStream = null
  def path: String = ""
  override def toByteArray = Array[Byte]()
  override def toString = "<no file>"
}
