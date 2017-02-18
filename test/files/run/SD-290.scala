object p1 {
  class B
  object B

  class C extends java.io.Serializable
  object C

  type D = DD
  object D
}
package object p2 {
  class B
  object B

  class C extends java.io.Serializable
  object C

  type D = DD
  object D

}
class DD extends java.io.Serializable

object Test {
  def main(args: Array[String]): Unit = {

    // This is the behaviour that was intended and was unchanged by this commmit.
    assert(!(p1.B : Object).isInstanceOf[scala.Serializable])
    assert(p1.C.isInstanceOf[scala.Serializable])
    assert(!(p1.D: Object).isInstanceOf[scala.Serializable])

    assert(!(p2.B : Object).isInstanceOf[scala.Serializable])
    assert(p2.C.isInstanceOf[scala.Serializable])

    // this behaviour was different in 2.12.1 and earlier due to a bug
    // in companionSymbolOf
    assert(!(p2.D: Object).isInstanceOf[scala.Serializable])
  }
}
