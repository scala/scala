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

/** Classes and objects (but note, not traits) inheriting the `DelayedInit`
 *  marker trait will have their initialization code rewritten as follows:
 *  `code` becomes `delayedInit(code)`.
 *
 *  Initialization code comprises all statements and all value definitions
 *  that are executed during initialization.
 *
 *  Example:
 *  {{{
 *    trait Helper extends DelayedInit {
 *      def delayedInit(body: => Unit) = {
 *        println("dummy text, printed before initialization of C")
 *        body // evaluates the initialization code of C
 *      }
 *    }
 *
 *    class C extends Helper {
 *      println("this is the initialization code of C")
 *    }
 *
 *    object Test extends App {
 *      val c = new C
 *    }
 *  }}}
 *
 *  Should result in the following being printed:
 *  {{{
 *    dummy text, printed before initialization of C
 *    this is the initialization code of C
 *  }}}
 *
 *  @see "Delayed Initialization" subsection of the Scala Language Specification (section 5.1)
 *
 */
@deprecated("DelayedInit semantics can be surprising. Support for `App` will continue. See the release notes for more details: https://github.com/scala/scala/releases/tag/v2.11.0", "2.11.0")
trait DelayedInit {
  def delayedInit(x: => Unit): Unit
}
