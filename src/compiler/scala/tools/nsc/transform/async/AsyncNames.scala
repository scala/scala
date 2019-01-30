/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc.transform.async

import java.util.concurrent.atomic.AtomicInteger

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.reflect.api.Names

/**
  * A per-global cache of names needed by the Async macro.
  */
final class AsyncNames[U <: Names with Singleton](val u: U) {
  self =>
  import u._

  abstract class NameCache[N <: U#Name](base: String) {
    val cached = new ArrayBuffer[N]()
    protected def newName(s: String): N
    def apply(i: Int): N = {
      if (cached.isDefinedAt(i)) cached(i)
      else {
        assert(cached.length == i)
        val name = newName(freshenString(base, i))
        cached += name
        name
      }
    }
  }

  final class TermNameCache(base: String) extends NameCache[U#TermName](base) {
    override protected def newName(s: String): U#TermName = TermName(s)
  }
  final class TypeNameCache(base: String) extends NameCache[U#TypeName](base) {
    override protected def newName(s: String): U#TypeName = TypeName(s)
  }
  private val matchRes: TermNameCache = new TermNameCache("match")
  private val ifRes: TermNameCache = new TermNameCache("if")
  private val await: TermNameCache = new TermNameCache("await")


  final class NameSource[N <: U#Name](cache: NameCache[N]) {
    private val count = new AtomicInteger(0)
    def apply(): N = cache(count.getAndIncrement())
  }

  class AsyncName {
    final val matchRes = new NameSource[U#TermName](self.matchRes)
    final val ifRes = new NameSource[U#TermName](self.ifRes)
    final val await = new NameSource[U#TermName](self.await)

    private val seenPrefixes = mutable.AnyRefMap[Name, AtomicInteger]()
    private val freshened = mutable.HashSet[Name]()

    final def freshenIfNeeded(name: TermName): TermName = {
      seenPrefixes.getOrNull(name) match {
        case null =>
          seenPrefixes.put(name, new AtomicInteger())
          name
        case counter =>
          freshen(name, counter)
      }
    }
    final def freshenIfNeeded(name: TypeName): TypeName = {
      seenPrefixes.getOrNull(name) match {
        case null =>
          seenPrefixes.put(name, new AtomicInteger())
          name
        case counter =>
          freshen(name, counter)
      }
    }
    final def freshen(name: TermName): TermName = {
      val counter = seenPrefixes.getOrElseUpdate(name, new AtomicInteger())
      freshen(name, counter)
    }
    final def freshen(name: TypeName): TypeName = {
      val counter = seenPrefixes.getOrElseUpdate(name, new AtomicInteger())
      freshen(name, counter)
    }
    private def freshen(name: TermName, counter: AtomicInteger): TermName = {
      if (freshened.contains(name)) name
      else TermName(freshenString(name.toString, counter.incrementAndGet()))
    }
    private def freshen(name: TypeName, counter: AtomicInteger): TypeName = {
      if (freshened.contains(name)) name
      else TypeName(freshenString(name.toString, counter.incrementAndGet()))
    }
  }

  private def freshenString(name: String, counter: Int): String = name.toString + "$async$" + counter
}
