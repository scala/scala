package scala.reflect

import scala.reflect.api.{Universe => ApiUniverse}

/** The Scala reflection API (located at scala-reflect.jar).
 *
 *  Using Scala reflection requires understanding of a couple of basic concepts like [[Symbols Symbols]], [[Types Types]], [[Mirror Mirrors]] and [[Universe Universes]].
 *  @see [[http://docs.scala-lang.org/overviews/reflection/overview.html]].
 *
 *  In Scala 2.10.0, reflection API and its implementation have experimental status. This means that the API and the docs are not complete and can be changed
 *  in binary- and source-incompatible manner in 2.10.1. This also means that the implementation has known issues
 *
 *  @groupprio API        9
 *  @groupprio Extractors 10
 *  @groupprio Tags       11
 *  @groupdesc API        The methods available for each reflection entity, without the implementation. Since the
 *                        reflection entities are abstract types that are later overridden, their API counterparts
 *                        guarantee a minimum set of methods implemented in both runtime reflection and macros.
 *  @groupdesc Extractors Extractors provide the machinery necessary to allow pattern matching and construction of
 *                        reflection entities that is similar to case classes, although the entities are only abstract
 *                        types that are later overridden.
 *  @groupdesc Tags       Implicit values that provide [[scala.reflect.ClassTag `ClassTags`]] for the reflection
 *                        classes. These are abstract in the interface but are later filled in to provide ClassTags
 *                        for the either the runtime reflection or macros entities, depending on the use.
 */
package object api {

  // anchors for materialization macros emitted during tag materialization in Implicits.scala
  // implementation is hardwired into `scala.reflect.reify.Taggers`
  // using the mechanism implemented in `scala.tools.reflect.FastTrack`
  // todo. once we have implicit macros for tag generation, we can remove these anchors
  private[scala] def materializeWeakTypeTag[T](u: ApiUniverse): u.WeakTypeTag[T] = ??? // macro
  private[scala] def materializeTypeTag[T](u: ApiUniverse): u.TypeTag[T] = ??? // macro
}