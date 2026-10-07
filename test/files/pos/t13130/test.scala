//> using options -Werror -Wunused:privates

class C {
  private final val k = "hello, world"

  @Ann(value = "i13130", produces = k)
  def f(): Unit = ()
}
