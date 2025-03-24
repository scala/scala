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

package scala.collection.mutable

import scala.collection.{IterableFactory, IterableFactoryDefaults}

trait Iterable[A]
  extends collection.Iterable[A]
    with collection.IterableOps[A, Iterable, Iterable[A]]
    with IterableFactoryDefaults[A, Iterable] {

  override def iterableFactory: IterableFactory[Iterable] = Iterable
}

/**
  * $factoryInfo
  * @define coll mutable collection
  * @define Coll `mutable.Iterable`
  */
@SerialVersionUID(3L)
object Iterable extends IterableFactory.Delegate[Iterable](ArrayBuffer)

/** Explicit instantiation of the `Iterable` trait to reduce class file size in subclasses. */
abstract class AbstractIterable[A] extends scala.collection.AbstractIterable[A] with Iterable[A]
