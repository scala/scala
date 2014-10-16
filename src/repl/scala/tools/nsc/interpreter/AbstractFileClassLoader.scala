/* NSC -- new Scala compiler
 * Copyright 2013-2014 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package interpreter

import scala.reflect.io.AbstractFile

@deprecated("Use `scala.tools.nsc.util.AbstractFileClassLoader`", "2.11.0")
class AbstractFileClassLoader(root: AbstractFile, parent: ClassLoader) extends util.AbstractFileClassLoader(root, parent)
