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


import scala.tools.nsc.transform.async.user.{async, autoawait}

object Test extends App { assert(test == "foobar")
  @async
  def test: String = {
    @autoawait def id(a: String) = a

    id("foo") + id("bar")
  }
}
