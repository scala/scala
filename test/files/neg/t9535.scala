class C[E <: Exception] {
  // cannot be expressed in Scala (it's allowed in Java)
  // https://issues.scala-lang.org/browse/SI-7066
  @throws[E1] def f[E1 <: Exception] = 1

  @throws(classOf[E]) def g: E = ??? // neg test: classOf requires class type
}
