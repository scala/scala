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


/** `Singleton` is used by the compiler as a supertype for singleton types. This includes literal types,
  * as they are also singleton types.
  *
  * {{{
  * scala> object A { val x = 42 }
  * defined object A
  *
  * scala> implicitly[A.type <:< Singleton]
  * res12: A.type <:< Singleton = generalized constraint
  *
  * scala> implicitly[A.x.type <:< Singleton]
  * res13: A.x.type <:< Singleton = generalized constraint
  *
  * scala> implicitly[42 <:< Singleton]
  * res14: 42 <:< Singleton = generalized constraint
  *
  * scala> implicitly[Int <:< Singleton]
  * ^
  * error: Cannot prove that Int <:< Singleton.
  * }}}
  *
  * `Singleton` has a special meaning when it appears as an upper bound on a formal type
  * parameter. Normally, type inference in Scala widens singleton types to the underlying
  * non-singleton type. When a type parameter has an explicit upper bound of `Singleton`,
  * the compiler infers a singleton type.
  *
  * {{{
  * scala> def check42[T](x: T)(implicit ev: T =:= 42): T = x
  * check42: [T](x: T)(implicit ev: T =:= 42)T
  *
  * scala> val x1 = check42(42)
  * ^
  * error: Cannot prove that Int =:= 42.
  *
  * scala> def singleCheck42[T <: Singleton](x: T)(implicit ev: T =:= 42): T = x
  * singleCheck42: [T <: Singleton](x: T)(implicit ev: T =:= 42)T
  *
  * scala> val x2 = singleCheck42(42)
  * x2: Int = 42
  * }}}
  *
  * See also [[https://docs.scala-lang.org/sips/42.type.html SIP-23 about Literal-based Singleton Types]].
  */
final trait Singleton extends Any
