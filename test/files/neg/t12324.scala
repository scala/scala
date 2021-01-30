
@throws[Exception] class ScalaClass[T](someList: List[T]) {
  throw new IllegalArgumentException("Boom!")
}

object X {
  // might be useful to annotate accessors of lazy vals
  @throws[Exception] object Y { ??? }
}

trait T {
  def f[A <: AnyRef](a: A) = a: AnyRef @throws[Exception]

  def g(): Unit = (): @throws[Exception]

  def n(i: Int) = i match { case 42 => 27: @throws[Exception] }  // not all cruft reaches refchecks
}
