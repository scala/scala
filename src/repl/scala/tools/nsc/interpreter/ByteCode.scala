/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc
package interpreter

import java.lang.reflect
import util.ScalaClassLoader
import ScalaClassLoader.appLoader
import scala.reflect.NameTransformer._

object ByteCode {
  /** Until I figure out why I can't get scalap onto the classpath such
   *  that the compiler will bootstrap, we have to use reflection.
   */
  private lazy val DECODER: Option[AnyRef] =
    for (clazz <- appLoader.tryToLoadClass[AnyRef]("scala.tools.scalap.Decode$")) yield
      clazz.getField(MODULE_INSTANCE_NAME).get(null)

  private def decoderMethod(name: String, args: JClass*): Option[reflect.Method] = {
    for (decoder <- DECODER ; m <- Option(decoder.getClass.getMethod(name, args: _*))) yield m
  }

  private lazy val aliasMap = {
    for (module <- DECODER ; method <- decoderMethod("typeAliases", classOf[String])) yield
      method.invoke(module, _: String).asInstanceOf[Option[Map[String, String]]]
  }

  def aliasesForPackage(pkg: String) = aliasMap flatMap (_(pkg))
}
