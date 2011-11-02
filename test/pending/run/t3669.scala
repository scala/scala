trait MyTrait[T <: { var id: U }, U] {
  def test(t: T): T = { 
    val v: U = t.id
    t.id = v
    t
  }
}

class C (var id: String){
  // uncommenting this fixes it
  // def id_=(x: AnyRef) { id = x.asInstanceOf[String] }
}

class Test extends MyTrait[C, String]

object Test {
  def main(args: Array[String]): Unit = {
    val t = new Test()
    val c1 = new C("a")
    val c2 = t.test(c1)
  }
}
