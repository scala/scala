/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 */
// $Id$

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
  override def findClass(name: String): Class[_] = {
    def onull[T](x: T): T = if (x == null) throw new ClassNotFoundException(name) else x
    var file: AbstractFile = root
    val pathParts = name.split("[./]").toList

    for (dirPart <- pathParts.init)
      file = onull(file.lookupName(dirPart, true))

    file = onull(file.lookupName(pathParts.last+".class", false))
    val bytes = file.toByteArray
    defineClass(name, bytes, 0, bytes.length)
  }
}
