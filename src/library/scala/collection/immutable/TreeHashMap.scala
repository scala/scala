/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2008-2009, David MacIver         **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: $

package scala.collection.immutable

// A dummy to fool ant until reintegration.
class TreeHashMap

/* TODO: Reintegrate

object TreeHashMap {
  private[TreeHashMap] val _empty = new TreeHashMap[Any, Nothing](IntMap.empty[Nothing]);
  def empty[Key, Value] : TreeHashMap[Key, Value] = _empty.asInstanceOf[TreeHashMap[Key, Value]];

  def apply[Key, Value](elems : (Key, Value)*) : TreeHashMap[Key, Value] = fromIterable(elems)

  def fromIterable[Key, Value](elems : Iterable[(Key, Value)]) : TreeHashMap[Key, Value] =
    elems.foldLeft(TreeHashMap.empty[Key, Value])((m, entry) => m.update(entry._1, entry._2))

  private def apply[Key, Value](underlying : IntMap[AssocMap[Key, Value]]) = new TreeHashMap(underlying);
}

/**
 * An immutable Map which is based on an IntMap mapping hash codes to lists of (Key, Value) pairs with the
 * same hashCode.
 *
 * Unlike the usual immutable.HashMap it is truly immutable behind the scenes. Consequently it is entirely
 * safe to share between multiple threads and should achieve a much greater degree of sharing between instances.
 *
 * Performancewise, get and update seem to be somewhat slower than with a traditional hash map, but bulk operations
 * such as filter, foreach and transform appear to get something of a speedup.
 *
 * @author David MacIver
 */
class TreeHashMap[Key, +Value] private (private val underlying : IntMap[AssocMap[Key, Value]]) extends scala.collection.immutable.Map[Key, Value]{
  def iterator : Iterator[(Key, Value)] = new Iterator[(Key, Value)]{
    val assocIt = new AssocMapIterator(AssocMap.empty[Key, Value]);
    val intIterator = underlying.values;

    def hasNext = assocIt.hasNext || intIterator.hasNext;
    def next = {
      if (!assocIt.hasNext) assocIt.it = intIterator.next;
      assocIt.next
    }
  }

  def empty[V] = TreeHashMap.empty[Key, V]

  private def hash(key : Key) = {
    var h = key.hashCode;
    h ^= ((h >>> 20) ^ (h >>> 12));
    h ^ (h >>> 7) ^ (h >>> 4);
  }

  override def stringPrefix = "TreeHashMap"

  override lazy val size = underlying.values.foldLeft(0)((x, y) => x + y.size);

  override def foreach[U](f : ((Key, Value)) =>  U) = underlying.foreachValue(_.foreach(f));

  override def toList : List[(Key, Value)] = {
    val buffer = new scala.collection.mutable.ListBuffer[(Key, Value)];
    foreach(buffer += _);
    buffer.toList;
  }

  override def apply(key : Key) =
    underlying.get(hash(key)) match {
      case None => default(key);
      case Some(table) => table.getOrElse(key, default(key));
  }

  override def isEmpty = underlying.isEmpty;

  override def filter(f : ((Key, Value)) => Boolean) : TreeHashMap[Key, Value]= {
    val newunderlying = underlying.modifyOrRemove(
      (key, x) => {
        val newtable = x.filter(f);
        if (newtable.isEmpty) None;
        else Some(newtable);
      }
    )
    if (newunderlying eq underlying) this;
    else TreeHashMap(newunderlying);
  }

  override def transform[C](f : (Key, Value) => C) = {
    val newunderlying = underlying.transform(
      (key, value) => value.transform(f)
    )
    if (underlying eq newunderlying) this.asInstanceOf[TreeHashMap[Key, C]];
    else TreeHashMap(newunderlying);
  }

  def get(key : Key) = underlying.get(hash(key)).flatMap(_.get(key));
  def update[S >: Value](key : Key, value : S) : TreeHashMap[Key, S] = TreeHashMap(
    underlying.updateWith[AssocMap[Key, S]](hash(key), AssocMap.singleton[Key, S](key, value), (x, y) => y.merge(x))
  )

  def -(key : Key) : TreeHashMap[Key, Value] = {
    val h = hash(key);
    underlying.get(h) match {
      case None => this;
      case Some(table) => {
        val newtable = table - key;
        if (table eq newtable) this;
        else if (newtable.isEmpty) TreeHashMap(underlying - h)
        else TreeHashMap(underlying.update(h, newtable));
      }
    }
  }

  override def ++[V >: Value](that : Iterable[(Key, V)]) : TreeHashMap[Key, V] = that match {
    case (that : TreeHashMap[_, _]) => this ++ TreeHashMap.fromIterable(that.asInstanceOf[TreeHashMap[Key, V]])
    case that => that.foldLeft(this : TreeHashMap[Key, V])((m, e) => m.update(e._1, e._2));
  }

  def ++[V >: Value](that : TreeHashMap[Key, V]) : TreeHashMap[Key, V] =
    TreeHashMap(this.underlying.unionWith[AssocMap[Key, V]](that.underlying, (key, x, y) => x ++ y));
}


private [collection] object AssocMap {
  val _empty = Nil[Any]
  def empty[Key, Value] : AssocMap[Key, Value] = _empty.asInstanceOf[AssocMap[Key, Value]]
  def singleton[Key, Value](key : Key, value : Value) = Cons(key, value, empty);
  def apply[Key, Value](maps : (Key, Value)*) =
    maps.foldLeft(empty[Key, Value])((x, y) => x.update(y._1, y._2));

  private[collection] case class Nil[Key]() extends AssocMap[Key, Nothing]
  private[collection] case class Cons[S, +T](key: S, value: T, tail: AssocMap[S, T]) extends AssocMap[S, T]
}

import AssocMap._

// AssocMap is very similar to ListMap. I don't want to patch ListMap right
// now, so I've got a separate implementation here to make tweaks to. Long
// term any of these changes should be merged into ListMap
// Short term it doesn't really matter because there are almost no viable
// use cases for ListMap compared to one of the alternatives.
private[collection] sealed abstract class AssocMap[Key, +Value] extends immutable.Map[Key, Value]{
  def empty[V] = AssocMap.empty[Key, V]

  final def get(key : Key) : Option[Value] = this match {
    case Nil() => None;
    case Cons(key2, value, tail) => if (key == key2) Some(value)
                                    else tail.get(key);
  }

  override final def getOrElse[V >: Value](key : Key, default : =>V) : V = this match {
    case Nil() => default;
    case Cons(key2, value, tail) => if (key == key2) value
                                    else tail.getOrElse(key, default);
  }

  override final def apply(key : Key) : Value = getOrElse(key, default(key));

  override def filter(f : ((Key, Value)) => Boolean) : AssocMap[Key, Value] = this match {
    case Cons(key, value, tail) => {
      val filteredtail = tail.filter(f);

      if (f((key, value))) {
        if (tail eq filteredtail) this;
        else Cons(key, value, filteredtail);
      } else filteredtail;
    }
    case Nil() => AssocMap.empty;
  }

  def iterator : Iterator[(Key, Value)] = new AssocMapIterator(this)

  @deprecated("use `iterator' instead") def elements = iterator

  override final def foreach[U](f : ((Key, Value)) =>  U) = this match {
    case Cons(key, value, tail) => { f((key, value)); tail.foreach(f); }
    case Nil() => {}
  }

  def size = this match {
    case Nil() => 0;
    case Cons(_, _, tail) => 1 + tail.size;
  }

  override def isEmpty = this match {
    case Nil() => true;
    case _ => false;
  }

  private def removeKeyValue(key : Any, value : Any) : Option[AssocMap[Key, Value]] = this match {
    case Nil() => None;
    case Cons(key2, value2, tail) =>
      if (key == key2){
        if (value == value2) Some(tail);
        else None;
      } else tail.removeKeyValue(key, value) match {
        case None => None;
        case Some(x) => Some(Cons(key2, value2, x));
      }
  }

  private def sameMappings(that : AssocMap[_, _]) : Boolean = (this, that) match {
    case (Nil(), Nil()) => true;
    case (Nil(), _) => false;
    case (_, Nil()) => false;
    case (Cons(key, value, tail), that) =>  that.removeKeyValue(key, value) match {
        case None => false;
        case Some(x) => tail.sameMappings(x);
      }
  }

  final def merge[S >: Value](that : AssocMap[Key, S]) : AssocMap[Key, S] = (this, that) match {
    case (Nil(), that) => that;
    case (_, Nil()) => this;
    case (Cons(key, value, tail), that) =>
      tail.merge[S](that).update(key, value);
  }

  def update[S >: Value](key : Key, value : S) : AssocMap[Key, S] = this match {
    case Nil() => Cons(key, value, empty);
    case Cons(key2, value2, tail) =>
      if (key2 == key) Cons(key, value, tail)
      else Cons(key2, value2, tail.update(key, value));
  }

  override def transform[C](f: (Key, Value) => C): AssocMap[Key, C] = this match {
    case Nil() =>
      AssocMap.empty[Key, C]
    case Cons(key, value, tail) =>
      val newtail = tail.transform(f)
      val newval = f(key, value)
      if ((tail eq newtail) && (value.asInstanceOf[AnyRef] eq newval.asInstanceOf[AnyRef])) this.asInstanceOf[AssocMap[Key, C]];
      else Cons(key, newval, newtail);
  }

  def -(key : Key) : AssocMap[Key, Value]= this match {
    case Nil() => this;
    case Cons(key2, value, tail) =>
      if (key == key2) tail;
      else {
        val newtail = tail - key;
        if (tail eq newtail) this;
        else Cons(key2, value, newtail);
      }
  }

  final def ++[V >: Value](that : AssocMap[Key, V]) : AssocMap[Key, V] = (this, that) match {
    case (Nil(), that) => that;
    case (_, Nil()) => this;
    case (t1, Cons(key, value, tail)) => t1.update(key, value.asInstanceOf[Value] /* evil hack to work around bug 1140 */) ++ tail;
  }
}

private[collection] class AssocMapIterator[Key, Value](var it : AssocMap[Key, Value]) extends Iterator[(Key, Value)]{
  def hasNext = it match {
    case Nil() => false;
    case _ => true;
  }

  def next = {
    val Cons(key, value, next) = it;
    it = next;
    (key, value);
  }
}

*/
