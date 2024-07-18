
//> using options -Werror -Xlint -Xsource:3

class C[@specialized A]

class D {
  def f[@specialized A](a: A): A = a

  @annotation.elidable(42)
  def g() = println("hello, world")
}
