/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

import scala.tools.partest.ReplTest

object Test extends ReplTest {
  override def extraSettings = "-feature -language:_"

  def code = """
:type 42
val x: 23 = 23
:type x
val y = x
:type y
final val z = x
:type z
def xx: 23 = 23
:type xx
final val yy = xx
:type yy
  """.trim
}