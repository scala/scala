/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.mutable


/** <p>
 *    This is a simple wrapper class for <a href="Map.html"
 *    target="contentFrame"><code>scala.collection.mutable.Map</code></a>.
 *  </p>
 *  <p>
 *    It is most useful for assembling customized map abstractions
 *    dynamically using object composition and forwarding.
 *  </p>
 *
 *  @author  Matthias Zenger, Martin Odersky
 *  @version 2.0, 31/12/2006
 */
trait MapProxy[A, B] extends Map[A, B] with collection.MapProxy[A, B] {

  def self: Map[A, B]

  override def update(key: A, value: B): Unit = self.update(key, value)
  override def += (kv: (A, B)) = self += kv
  override def += (kv1: (A, B), kv2: (A, B), kvs: (A, B)*) = self.+=(kv1, kv2, kvs: _*)
  override def ++= (kvs: Iterable[(A, B)]) = self ++= kvs
  override def ++= (kvs: Iterator[(A, B)]) = self ++= kvs
  override def + (kv: (A, B)): Map[A, B] = self + kv
  override def + (kv1: (A, B), kv2: (A, B), kvs: (A, B)*): Map[A, B] = self.+(kv1, kv2, kvs: _*)
  override def ++ (kvs: Iterable[(A, B)]): Map[A, B] = self ++ kvs
  override def ++ (kvs: Iterator[(A, B)]): Map[A, B] = self ++ kvs
  override def -= (key: A) = self -= key
  override def -= (key1: A, key2: A, keys: A*) = self.-=(key1, key2, keys: _*)
  override def --= (keys: Iterable[A]) = self --= keys
  override def --= (keys: Iterator[A]) = self --= keys
  override def - (key: A): Map[A, B] = self - key
  override def - (key1: A, key2: A, keys: A*): Map[A, B] = self.-(key1, key2, keys: _*)
  override def -- (keys: Iterable[A]): Map[A, B] = self -- keys
  override def -- (keys: Iterator[A]): Map[A, B] = self -- keys
  override def clear(): Unit = self.clear
  override def transform(f: (A, B) => B) = self transform f
  override def retain(p: (A, B) => Boolean): Unit = self retain p
  override def <<(cmd: Message[(A, B)]): Unit = self << cmd
  override def clone(): Map[A, B] = self.clone()
  @deprecated
  override def incl(mappings: (A, B)*): Unit = self.incl(mappings: _*)
  @deprecated
  override def excl(keys: A*): Unit = self.excl(keys: _*)
}
