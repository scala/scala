//> using options -deprecation -Xfatal-warnings
//
class A {
  val `enum`: Int = 1
  println(enum)
  val `export`: Int = 1
  val `given`: Int = 1
  def foo(`given`: Int) = given
  def bla[`export` <: Int] = {
    class `enum`
    new enum
  }
}
class B {
  val enum: Int = 1 // error
  val export: Int = 1 // error
  val given: Int = 1 // error
  def foo(given: Int) = {} // error
  def bla[export <: Int] = {} // error
}
class enum // error
