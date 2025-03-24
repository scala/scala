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

package scala.tools.nsc.transform.async

import scala.collection.mutable
import scala.reflect.internal.util.FreshNameCreator

/**
  * A per-global cache of names needed by the Async macro.
  */
final class AsyncNames[U <: reflect.internal.Names with Singleton](val u: U) {
  self =>
  import u._

  abstract class NameCache[N <: U#Name](val base: String) {
    val prefix: String = base + "$"
    private final val CacheLimit = 128
    val cached = new java.util.ArrayList[N](CacheLimit)
    cached.addAll(java.util.Collections.nCopies(CacheLimit, null.asInstanceOf[N]))
    protected def newName(s: String): N
    def apply(nameFactory: FreshNameCreator#NameFactory): N = {
      val i = nameFactory.index()
      if (i < CacheLimit.toLong) {
        cached.get(i.toInt) match {
          case null =>
            val result = newName(nameFactory.newNameAtIndex(i))
            cached.set(i.toInt, result)
            result
          case name =>
            name
        }
      } else {
        newName(nameFactory.newNameAtIndex(i))
      }
    }
  }

  final class TermNameCache(base: String) extends NameCache[U#TermName](base) {
    override protected def newName(s: String): U#TermName = TermName(s)
  }
  private val matchRes: TermNameCache = new TermNameCache("match")
  private val ifRes: TermNameCache = new TermNameCache("if")
  private val await: TermNameCache = new TermNameCache("await")

  final class NameSource[N <: U#Name](cache: NameCache[N], freshNameCreator: FreshNameCreator) {
    private val factory = freshNameCreator.newNameFactory(cache.prefix)
    def apply(): N = {
      cache(factory)
    }
  }

  class AsyncName(freshNameCreator: FreshNameCreator) {
    final val matchRes = new NameSource[U#TermName](self.matchRes, freshNameCreator)
    final val ifRes = new NameSource[U#TermName](self.ifRes, freshNameCreator)
    final val await = new NameSource[U#TermName](self.await, freshNameCreator)

    private val seenPrefixes = mutable.HashSet[Name]()

    final def freshenIfNeeded(name: TermName): TermName = {
      if (seenPrefixes.contains(name)) {
        TermName(freshNameCreator.newName(name.toStringWithSuffix("$")))
      } else {
        seenPrefixes.addOne(name)
        name
      }
    }
    final def freshenIfNeeded(name: TypeName): TypeName = {
      if (seenPrefixes.contains(name)) {
        TypeName(freshNameCreator.newName(name.toStringWithSuffix("$")))
      } else {
        seenPrefixes.addOne(name)
        name
      }
    }
    final def freshen(name: TermName): TermName = {
      TermName(freshNameCreator.newName(name.toStringWithSuffix("$")))
    }
    final def freshen(name: TypeName): TypeName = {
      TypeName(freshNameCreator.newName(name.toStringWithSuffix("$")))
    }
  }
}
