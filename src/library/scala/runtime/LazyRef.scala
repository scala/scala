package scala.runtime

/* Classes used as holders for local lazy vals */

class LazyRef[T] {
  var value: T = _
  @volatile var initialized: Boolean = false
}

class LazyBoolean {
  var value: Boolean = _
  @volatile var initialized: Boolean = false
}

class LazyByte {
  var value: Byte = _
  @volatile var initialized: Boolean = false
}

class LazyChar {
  var value: Char = _
  @volatile var initialized: Boolean = false
}

class LazyShort {
  var value: Short = _
  @volatile var initialized: Boolean = false
}

class LazyInt {
  var value: Int = _
  @volatile var initialized: Boolean = false
}

class LazyLong {
  var value: Long = _
  @volatile var initialized: Boolean = false
}

class LazyFloat {
  var value: Float = _
  @volatile var initialized: Boolean = false
}

class LazyDouble {
  var value: Double = _
  @volatile var initialized: Boolean = false
}

class LazyUnit {
  @volatile var initialized: Boolean = false
}
