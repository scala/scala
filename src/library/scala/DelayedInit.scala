/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2010-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

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
 *  @author  Martin Odersky
 */
@deprecated("DelayedInit semantics can be surprising. Support for `App` will continue. See the release notes for more details: https://github.com/scala/scala/releases/tag/v2.11.0-RC1", "2.11.0")
trait DelayedInit {
  def delayedInit(x: => Unit): Unit
}
