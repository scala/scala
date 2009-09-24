/*     ___ ____ ___   __   ___   ___
**    / _// __// _ | / /  / _ | / _ \  Scala classfile decoder
**  __\ \/ /__/ __ |/ /__/ __ |/ ___/  (c) 2003-2007, LAMP/EPFL
** /____/\___/_/ |_/____/_/ |_/_/      http://scala-lang.org/
**
*/

// $Id$

package scala.tools.scalap

/** Loads decoder.properties from the jar. */
object Properties extends scala.util.PropertiesTrait
{
  protected def propCategory    = "decoder"
  protected def pickJarBasedOn  = classOf[Classfile]
  val cmdName                   = scala.tools.nsc.Properties.cmdName
}
