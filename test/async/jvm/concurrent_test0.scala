//> using options -Xasync

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


import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.tools.testkit.async.Async._
import scala.concurrent.duration.Duration

object Test extends App { assert(test == "foobar")
  def test: String = Await.result(async {
    def id(a: String) = async { a }

    await(id("foo")) + await(id("bar"))
  }, Duration.Inf)
}

