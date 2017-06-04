package strawman.collection

import strawman.collection.immutable.List

import scala.{Array, Char, Int, AnyVal}
import scala.Predef.String
import strawman.collection.mutable.StringBuilder

import scala.reflect.ClassTag

class StringOps(val s: String)
  extends AnyVal
     with SeqOps[Char, Seq, String]
     with Buildable[Char, String]
     with ArrayLike[Char] {

  protected def coll = new StringView(s)

  protected[this] def fromSpecificIterable(coll: Iterable[Char]): String = {
    val sb = new StringBuilder
    for (ch <- coll) sb += ch
    sb.result
  }

  def iterableFactory = List

  protected[this] def newBuilder = new StringBuilder

  def length = s.length
  def apply(i: Int) = s.charAt(i)

  override def knownSize = s.length

  override def className = "String"

  def iterator(): Iterator[Char] = coll.iterator()

  /** Overloaded version of `map` that gives back a string, where the inherited
    *  version gives back a sequence.
    */
  def map(f: Char => Char): String = {
    val sb = new StringBuilder
    for (ch <- s) sb += f(ch)
    sb.result
  }

  /** Overloaded version of `flatMap` that gives back a string, where the inherited
    *  version gives back a sequence.
    */
  def flatMap(f: Char => String): String = {
    val sb = new StringBuilder
    for (ch <- s) sb ++= f(ch)
    sb.result
  }

  /** Overloaded version of `++` that gives back a string, where the inherited
    *  version gives back a sequence.
    */
  def ++(xs: IterableOnce[Char]): String = {
    val sb = new StringBuilder() ++= s
    for (ch <- xs.iterator()) sb += ch
    sb.result
  }

  /** Another overloaded version of `++`. */
  def ++(xs: String): String = s + xs
}

case class StringView(s: String) extends IndexedView[Char] {
  def length = s.length
  def apply(n: Int) = s.charAt(n)
  override def className = "StringView"
}

