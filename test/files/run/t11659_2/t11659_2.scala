import test.foo._

// scalac: -Yshow-trees-stringified -Vprint:constructors
object Test extends App {


  def Foo[S <: AnyRef](name: S) = new Foo[S](name)

  def Foo2[S <: AnyRef](name: S) = new Foo[name.type](name)


  val foo = Foo("x")

  val foo2 = Foo2("x")


  println(foo.toString != null)

  println(foo2.toString != null)


}
