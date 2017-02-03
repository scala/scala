package xsbt

object JavaUtils {
  implicit class JavaForEach[T](val iterable: java.lang.Iterable[T]) extends AnyVal {

    @inline
    def foreach(op: T => Unit): Unit = {
      val iterator = iterable.iterator()
      while (iterator.hasNext) op(iterator.next())
    }
  }

  implicit class JavaMapForEach[K, V](val map: java.util.Map[K, V]) extends AnyVal {

    @inline
    def foreach(op: (K, V) => Unit): Unit = {
      val iterator = map.keySet().iterator()
      while (iterator.hasNext) {
        val key = iterator.next()
        op(key, map.get(key))
      }
    }
  }
}
