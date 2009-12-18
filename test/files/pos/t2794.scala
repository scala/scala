class Key[T]

class Entry[T](val k: Key[T], val v: T)

object Entry {

    def makeDefault[T >: Null <: AnyRef] = new Entry[T](new Key[T], null: T)

}
