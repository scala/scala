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

package scala

/** A marker trait that enables dynamic invocations. Instances `x` of this
 *  trait allow method invocations `x.meth(args)` for arbitrary method
 *  names `meth` and argument lists `args` as well as field accesses
 *  `x.field` for arbitrary field names `field`.
 *
 *  If a call is not natively supported by `x` (i.e. if type checking
 *  fails), it is rewritten according to the following rules:
 *
 *  {{{
 *  foo.method("blah")      ~~> foo.applyDynamic("method")("blah")
 *  foo.method(x = "blah")  ~~> foo.applyDynamicNamed("method")(("x", "blah"))
 *  foo.method(x = 1, 2)    ~~> foo.applyDynamicNamed("method")(("x", 1), ("", 2))
 *  foo.field           ~~> foo.selectDynamic("field")
 *  foo.varia = 10      ~~> foo.updateDynamic("varia")(10)
 *  foo.arr(10) = 13    ~~> foo.selectDynamic("arr").update(10, 13)
 *  foo.arr(10)         ~~> foo.applyDynamic("arr")(10)
 *  }}}
 *
 *  Defining direct or indirect subclasses of this trait
 *  is only possible if the language feature `dynamics` is enabled.
 */
trait Dynamic extends Any


