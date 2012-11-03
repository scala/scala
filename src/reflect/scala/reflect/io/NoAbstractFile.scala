/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.reflect
package io

import java.io.InputStream
import java.io.{ File => JFile }

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
  def file: JFile = null
  def input: InputStream = null
  def isDirectory: Boolean = false
  def iterator: Iterator[AbstractFile] = Iterator.empty
  def lastModified: Long = 0L
  def lookupName(name: String, directory: Boolean): AbstractFile = null
  def lookupNameUnchecked(name: String, directory: Boolean): AbstractFile = null
  def name: String = ""
  def output: java.io.OutputStream = null
  def path: String = ""
  override def toByteArray = Array[Byte]()
}
