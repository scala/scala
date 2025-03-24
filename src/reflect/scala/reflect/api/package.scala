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
package reflect

import scala.reflect.api.{Universe => ApiUniverse}

/**
 * <span class="badge badge-red" style="float: right;">EXPERIMENTAL</span>
 *
 * The Scala Reflection API (located in scala-reflect.jar).
 *
 * In Scala 2.10.0, the Scala Reflection API and its implementation have an "experimental" status.
 * This means that the API and the docs are not complete and can be changed in binary- and source-incompatible
 * manner in 2.10.1. This also means that the implementation has some known issues.
 *
 * The following types are the backbone of the Scala Reflection API, and serve as a good starting point
 * for information about Scala Reflection:
 *
 *  - [[scala.reflect.api.Symbols]]
 *  - [[scala.reflect.api.Types]]
 *  - [[scala.reflect.api.Mirrors]]
 *  - [[scala.reflect.api.Universe]]
 *
 *  For more information about Scala Reflection, see the
 * [[https://docs.scala-lang.org/overviews/reflection/overview.html Reflection Guide]]
 *
 *  @groupname ReflectionAPI Scala Reflection API
 *  @groupprio API        9
 *  @groupprio Extractors 10
 *  @groupprio Tags       11
 *  @groupdesc API        The methods available for each reflection entity, without the implementation. Since the
 *                        reflection entities are later overridden by runtime reflection and macros, their API
 *                        counterparts guarantee a minimum set of methods that are implemented.
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
  private[scala] def materializeWeakTypeTag[T](u: ApiUniverse): u.WeakTypeTag[T] = macro ???
  private[scala] def materializeTypeTag[T](u: ApiUniverse): u.TypeTag[T] = macro ???
}
