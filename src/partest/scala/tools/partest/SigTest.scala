/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.partest

import scala.tools.nsc.Settings
import scala.tools.nsc.interpreter.ILoop
import java.lang.reflect.{ Method => JMethod, Field => JField }

/** Support code for testing signatures.
 */
trait SigTest {
  def mstr(m: JMethod) = "  (m) %s%s".format(
    m.toGenericString,
    if (m.isBridge) " (bridge)" else ""
  )
  def fstr(f: JField) = "  (f) %s".format(f.toGenericString)

  def isObjectMethodName(name: String) = classOf[Object].getMethods exists (_.getName == name)

  def fields[T: ClassManifest](p: JField => Boolean) = {
    val cl = classManifest[T].erasure
    val fs = (cl.getFields ++ cl.getDeclaredFields).distinct sortBy (_.getName)

    fs filter p
  }
  def methods[T: ClassManifest](p: JMethod => Boolean) = {
    val cl = classManifest[T].erasure
    val ms = (cl.getMethods ++ cl.getDeclaredMethods).distinct sortBy (x => (x.getName, x.isBridge))

    ms filter p
  }
  def allFields[T: ClassManifest]()                = fields[T](_ => true)
  def allMethods[T: ClassManifest]()               = methods[T](m => !isObjectMethodName(m.getName))
  def fieldsNamed[T: ClassManifest](name: String)  = fields[T](_.getName == name)
  def methodsNamed[T: ClassManifest](name: String) = methods[T](_.getName == name)

  def allGenericStrings[T: ClassManifest]() =
    (allMethods[T]() map mstr) ++ (allFields[T]() map fstr)

  def genericStrings[T: ClassManifest](name: String) =
    (methodsNamed[T](name) map mstr) ++ (fieldsNamed[T](name) map fstr)

  def show[T: ClassManifest](name: String = "") = {
    println(classManifest[T].erasure.getName)
    if (name == "") allGenericStrings[T]() foreach println
    else genericStrings[T](name) foreach println
  }
}
