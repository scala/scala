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

package scala.collection.immutable

import scala.collection.{IterableFactory, IterableFactoryDefaults}

/** A trait for collections that are guaranteed immutable.
  *
  * @tparam A the element type of the collection
  *
  * @define coll immutable collection
  * @define Coll `immutable.Iterable`
  */
trait Iterable[+A] extends collection.Iterable[A]
                      with collection.IterableOps[A, Iterable, Iterable[A]]
                      with IterableFactoryDefaults[A, Iterable] {

  override def iterableFactory: IterableFactory[Iterable] = Iterable
}

@SerialVersionUID(3L)
object Iterable extends IterableFactory.Delegate[Iterable](List) {
  override def from[E](it: IterableOnce[E]): Iterable[E] = it match {
    case iterable: Iterable[E] => iterable
    case _ => super.from(it)
  }
}
