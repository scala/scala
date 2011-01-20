/* NSC -- new Scala compiler
 * Copyright 2007-2011 LAMP/EPFL
 * @author Lex Spoon
 */

package scala.tools.nsc
package plugins

/** ...
 *
 * @author Lex Spoon
 * @version 1.0, 2007-5-21
 */
class PluginLoadException(filename: String, cause: Exception)
extends Exception(cause)
