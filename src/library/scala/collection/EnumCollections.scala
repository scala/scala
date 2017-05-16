package scala.collection

import java.{ lang => jl, util => ju }
import scala.collection.JavaConversions._
import scala.collection.{ mutable => scm }

/**
 * Provides efficient Scala Sets for Java enum types (via a wrapped java.util.EnumSet).
 *
 * {{{
 *   val allValues = EnumSet.all[Thread.State]
 *   val valueRange = EnumSet.range(Thread.State.RUNNABLE, Thread.State.TERMINATED)
 *   val emptySet = EnumSet[Thread.State]
 *   val fromCollection = EnumSet(Seq(Thread.State.RUNNABLE, Thread.State.BLOCKED))
 *   val fromTraversable = EnumSet(Seq(Thread.State.BLOCKED, Thread.State.NEW).iterator)
 *   val fromVarargs = EnumSet(Thread.State.TIMED_WAITING, Thread.State.TERMINATED)
 * }}}
 */
object EnumSet {
  def all[T <: jl.Enum[T]: Manifest]: scm.Set[T] =
    ju.EnumSet.allOf(manifest[T].runtimeClass.asInstanceOf[Class[T]])

  def range[T <: jl.Enum[T]](begin: T, end: T): scm.Set[T] =
    ju.EnumSet.range(begin, end)

  def apply[T <: jl.Enum[T]: Manifest](): scm.Set[T] =
    ju.EnumSet.noneOf(manifest[T].runtimeClass.asInstanceOf[Class[T]])

  def apply[T <: jl.Enum[T]: Manifest](c: Iterable[T]): scm.Set[T] =
    if (c.isEmpty) apply[T] else ju.EnumSet.copyOf(c)

  def apply[T <: jl.Enum[T]: Manifest](c: TraversableOnce[T]): scm.Set[T] =
    apply[T] ++= c

  def apply[T <: jl.Enum[T]: Manifest](c: T*): scm.Set[T] =
    apply(c.asInstanceOf[Iterable[T]])
}

/**
 * Provides efficient Scala Maps for Java enum types (via a wrapped java.util.EnumMap).
 *
 * {{{
 *   val emptyMap = EnumMap[Thread.State, Foo]
 *   val fromMap = EnumMap(Map(Thread.State.NEW -> "new"))
 *   val fromTupledCollection = EnumMap(Seq(Thread.State.NEW -> "new", Thread.State.BLOCKED -> "io"))
 *   val fromTupleVarargs = EnumMap(Thread.State.TERMINATED -> "dead", Thread.State.WAITING -> "slow")
 * }}}
 */
object EnumMap {
  def apply[T <: jl.Enum[T]: Manifest, V](): scm.Map[T, V] =
    new ju.EnumMap[T, V](manifest[T].runtimeClass.asInstanceOf[Class[T]])

  def apply[T <: jl.Enum[T]: Manifest, V](in: Map[T, V]): scm.Map[T, V] =
    if (in.isEmpty) apply[T, V] else new ju.EnumMap(in)

  def apply[T <: jl.Enum[T]: Manifest, V](in: TraversableOnce[(T, V)]): scm.Map[T, V] =
    apply[T, V] ++= in

  def apply[T <: jl.Enum[T]: Manifest, V](in: (T, V)*): scm.Map[T, V] =
    apply(in.asInstanceOf[Iterable[(T, V)]])
}
