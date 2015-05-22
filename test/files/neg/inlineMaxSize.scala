// not a JUnit test because of https://github.com/scala-opt/scala/issues/23
class C {
  @inline final def f = 0
  @inline final def g = f + f + f + f + f + f + f + f + f + f
  @inline final def h = g + g + g + g + g + g + g + g + g + g
  @inline final def i = h + h + h + h + h + h + h + h + h + h
  @inline final def j = i + i + i
}
