package scala.reflect

import scala.reflect.api.{Universe => ApiUniverse}

/** The Scala reflection API (located at scala-reflect.jar).
 *
 *  Using Scala reflection requires understanding of a couple of basic concepts like Symbols, Types, Mirror and Universes.
 *  @see [[http://docs.scala-lang.org/overviews/reflection/overview.html]].
 *
 *  In Scala 2.10.0, reflection API and its implementation have experimental status. This means that the API and the docs are not complete and can be changed
 *  in binary- and source-incompatible manner in 2.10.1. This also means that the implementation has known issues
 */
package object api {

  // anchors for materialization macros emitted during tag materialization in Implicits.scala
  // implementation is hardwired into `scala.reflect.reify.Taggers`
  // using the mechanism implemented in `scala.tools.reflect.FastTrack`
  // todo. once we have implicit macros for tag generation, we can remove these anchors
  private[scala] def materializeWeakTypeTag[T](u: ApiUniverse): u.WeakTypeTag[T] = ??? // macro
  private[scala] def materializeTypeTag[T](u: ApiUniverse): u.TypeTag[T] = ??? // macro
}