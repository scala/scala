/*                     __                                                      *\
**     ________ ___   / /  ___     Scala API                                   **
**    / __/ __// _ | / /  / _ |    (c) 2002-2016, LAMP/EPFL and Lightbend, Inc **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/                      **
** /____/\___/_/ |_/____/_/ | |                                                **
**                          |/                                                 **
\*                                                                             */

package scala.runtime

/** Classes used as holders for lazy vals defined in methods. */

class LazyRef[T] {
  @volatile private[this] var _initialized: Boolean = _
  def initialized = _initialized

  private[this] var _value: T = _
  def value: T = _value
  def initialize(value: T): T = {
    _value = value
    _initialized = true
    value
  }

  override def toString = s"LazyRef ${if (_initialized) s"of: ${_value}" else "thunk"}"
}

class LazyBoolean {
  @volatile private[this] var _initialized: Boolean = _
  def initialized = _initialized

  private[this] var _value: Boolean = _
  def value: Boolean = _value
  def initialize(value: Boolean): Boolean = {
    _value = value
    _initialized = true
    value
  }

  override def toString = s"LazyBoolean ${if (_initialized) s"of: ${_value}" else "thunk"}"
}

class LazyByte {
  @volatile private[this] var _initialized: Boolean = _
  def initialized = _initialized

  private[this] var _value: Byte = _

  def value: Byte = _value

  def initialize(value: Byte): Byte = {
    _value = value
    _initialized = true
    value
  }

  override def toString = s"LazyByte ${if (_initialized) s"of: ${_value}" else "thunk"}"
}

class LazyChar {
  @volatile private[this] var _initialized: Boolean = _
  def initialized = _initialized

  private[this] var _value: Char = _
  def value: Char = _value
  def initialize(value: Char): Char = {
    _value = value
    _initialized = true
    value
  }

  override def toString = s"LazyChar ${if (_initialized) s"of: ${_value}" else "thunk"}"
}

class LazyShort {
  @volatile private[this] var _initialized: Boolean = _
  def initialized = _initialized

  private[this] var _value: Short = _
  def value: Short = _value
  def initialize(value: Short): Short = {
    _value = value
    _initialized = true
    value
  }

  override def toString = s"LazyShort ${if (_initialized) s"of: ${_value}" else "thunk"}"
}

class LazyInt {
  @volatile private[this] var _initialized: Boolean = _
  def initialized = _initialized

  private[this] var _value: Int = _
  def value: Int = _value
  def initialize(value: Int): Int = {
    _value = value
    _initialized = true
    value
  }

  override def toString = s"LazyInt ${if (_initialized) s"of: ${_value}" else "thunk"}"
}

class LazyLong {
  @volatile private[this] var _initialized: Boolean = _
  def initialized = _initialized

  private[this] var _value: Long = _
  def value: Long = _value
  def initialize(value: Long): Long = {
    _value = value
    _initialized = true
    value
  }

  override def toString = s"LazyLong ${if (_initialized) s"of: ${_value}" else "thunk"}"
}

class LazyFloat {
  @volatile private[this] var _initialized: Boolean = _
  def initialized = _initialized

  private[this] var _value: Float = _
  def value: Float = _value
  def initialize(value: Float): Float = {
    _value = value
    _initialized = true
    value
  }

  override def toString = s"LazyFloat ${if (_initialized) s"of: ${_value}" else "thunk"}"
}

class LazyDouble {
  @volatile private[this] var _initialized: Boolean = _
  def initialized = _initialized

  private[this] var _value: Double = _
  def value: Double = _value
  def initialize(value: Double): Double = {
    _value = value
    _initialized = true
    value
  }

  override def toString = s"LazyDouble ${if (_initialized) s"of: ${_value}" else "thunk"}"
}

class LazyUnit {
  @volatile private[this] var _initialized: Boolean = _
  def initialized = _initialized

  def initialize(): Unit = _initialized = true

  override def toString = s"LazyUnit${if (_initialized) "" else " thunk"}"
}
