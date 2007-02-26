package scala.collection.immutable;

trait SortedMap[A,+B] extends Map[A,B] with collection.SortedMap[A,B] {
  override def rangeImpl(from : Option[A], until : Option[A]) : SortedMap[A,B];
  override def from(from: A) = rangeImpl(Some(from), None);
  override def until(until: A) = rangeImpl(None, Some(until));
  override def range(from: A, until: A) = rangeImpl(Some(from),Some(until));
  override def empty[C]: SortedMap[A, C]
  override def update [B1 >: B] (key: A, value: B1): SortedMap[A, B1]
  override def + [B1 >: B] (kv: Pair[A, B1]): SortedMap[A, B1] = update(kv._1, kv._2)

  override def + [B1 >: B] (kv1: Pair[A, B1], kv2: Pair[A, B1], kvs: Pair[A, B1]*): SortedMap[A, B1] =
    this + kv1 + kv2 ++ kvs
  override def ++ [B1 >: B] (kvs: Iterable[Pair[A, B1]]): SortedMap[A, B1] =
    ((this: SortedMap[A, B1]) /: kvs) ((m, kv) => m + kv)
  override def ++ [B1 >: B] (kvs: Iterator[Pair[A, B1]]): SortedMap[A, B1] =
    ((this: SortedMap[A, B1]) /: kvs) ((m, kv) => m + kv)
  override def - (key: A): SortedMap[A, B]
  override def - (key1: A, key2: A, keys: A*): SortedMap[A, B] =
    this - key1 - key2 -- keys
  override def -- (keys: Iterable[A]): SortedMap[A, B] = this -- keys.elements

  override def -- (keys: Iterator[A]): SortedMap[A, B] =
    (this /: keys) ((m, key) => m - key)

  override def transform[C](f: (A, B) => C): SortedMap[A, C] = {
    var res = empty[C]
    foreach { case Pair(key, value) => res = res.update(key, f(key, value)) }
    res
  }
  override def filter(p: Pair[A, B] => Boolean): SortedMap[A, B] = {
    var res = this
    foreach {
      case kv @ Pair(key, _) => if (!p(kv)) { res = res - key }
    }
    res
  }
}