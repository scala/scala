/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 */

package scala.tools.nsc
package interpreter

import scala.tools.nsc.io.AbstractFile
import util.ScalaClassLoader

/**
 * A class loader that loads files from a {@link scala.tools.nsc.io.AbstractFile}.
 *
 * @author Lex Spoon
 */
class AbstractFileClassLoader(root: AbstractFile, parent: ClassLoader)
    extends ClassLoader(parent)
    with ScalaClassLoader
{
  private def findBytes(name: String, onError: => Array[Byte]): Array[Byte] = {
    var file: AbstractFile = root
    val pathParts          = name.split("[./]").toList

    for (dirPart <- pathParts.init) {
      file = file.lookupName(dirPart, true)
      if (file == null)
        return onError
    }

    file.lookupName(pathParts.last+".class", false) match {
      case null   => onError
      case file   => file.toByteArray
    }
  }

  override def findBytesForClassName(name: String): Array[Byte] =
    findBytes(name, super.findBytesForClassName(name))

  override def findClass(name: String): JClass = {
    val bytes = findBytes(name, throw new ClassNotFoundException(name))
    defineClass(name, bytes, 0, bytes.length)
  }
}

