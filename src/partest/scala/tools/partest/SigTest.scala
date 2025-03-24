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

package scala.tools.partest

import java.lang.reflect.{ Method => JMethod, Field => JField }
import scala.reflect.{ClassTag, classTag}

/** Support code for testing signatures.
 */
trait SigTest {
  def mstr(m: JMethod) = "  (m) %s%s".format(
    m.toGenericString,
    if (m.isBridge) " (bridge)" else ""
  )
  def fstr(f: JField) = "  (f) %s".format(f.toGenericString)

  def isObjectMethodName(name: String) = classOf[Object].getMethods exists (_.getName == name)

  def fields[T: ClassTag](p: JField => Boolean) = {
    val cl = classTag[T].runtimeClass
    val fs = (cl.getFields ++ cl.getDeclaredFields).distinct sortBy (_.getName)

    fs filter p
  }
  def methods[T: ClassTag](p: JMethod => Boolean) = {
    val cl = classTag[T].runtimeClass
    val ms = (cl.getMethods ++ cl.getDeclaredMethods).distinct sortBy (x => (x.getName, x.isBridge))

    ms filter p
  }
  def allFields[T: ClassTag]()                = fields[T](_ => true)
  def allMethods[T: ClassTag]()               = methods[T](m => !isObjectMethodName(m.getName))
  def fieldsNamed[T: ClassTag](name: String)  = fields[T](_.getName == name)
  def methodsNamed[T: ClassTag](name: String) = methods[T](_.getName == name)

  def allGenericStrings[T: ClassTag]() =
    allMethods[T]().map(mstr _) ++ allFields[T]().map(fstr _)

  def genericStrings[T: ClassTag](name: String) =
    methodsNamed[T](name).map(mstr _) ++ fieldsNamed[T](name).map(fstr _)

  def show[T: ClassTag](name: String = "") = {
    println(classTag[T].runtimeClass.getName)
    if (name == "") allGenericStrings[T]() foreach println
    else genericStrings[T](name) foreach println
  }
}
