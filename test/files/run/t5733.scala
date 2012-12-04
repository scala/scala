import scala.language.dynamics

object A extends Dynamic {
  var a = "a"

  def selectDynamic(method:String): String = a

  def updateDynamic(method:String)(v:String) { a = v }
}

class B extends Dynamic {
  var b = "b"

  def selectDynamic(method:String): String = b

  def updateDynamic(method:String)(v:String) { b = v }
}

object Test extends App {
  assert( A.foo == "a" )
  assert( A.bar == "a" )
  A.aaa = "aaa"
  assert( A.bar == "aaa" )

  val b = new B
  assert( b.foo == "b" )
  assert( b.bar == "b" )
  b.bbb = "bbb"
  assert( b.bar == "bbb" )

  {
    println("Running ABTest asserts")
    A.a = "a"
    (new ABTest).test()
  }

  println("Done")
}

class ABTest {
  def test() {
    assert( A.foo == "a" )
    assert( A.bar == "a" )
    A.aaa = "aaa"
    assert( A.bar == "aaa" )

    val b = new B
    assert( b.foo == "b" )
    assert( b.bar == "b" )
    b.bbb = "bbb"
    assert( b.bar == "bbb" )
  }
}
