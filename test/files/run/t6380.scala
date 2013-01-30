object Test extends App {
  classOf[Foo].getDeclaredMethods().sortBy(_.getName).map(_.getExceptionTypes.sortBy(_.getName).toList).toList.foreach(println)
}

class Foo {
  @throws[Exception]
  def bar1 = ???
  @throws[Throwable]("always")
  def bar2 = ???
  @throws(classOf[RuntimeException])
  def bar3 = ???
  @throws[IllegalArgumentException] @throws[NoSuchElementException]
  def bar4 = ???
  @throws(classOf[IndexOutOfBoundsException]) @throws(classOf[IndexOutOfBoundsException])
  def bar5 = ???
  @throws[IllegalStateException]("Cause") @throws[IllegalStateException]
  def bar6 = ???
  @throws[NullPointerException]("Cause A") @throws[NullPointerException]("Cause B")
  def bar7 = ???
}