// Soundness bug, at #963 and dup at #2079.
trait A {
  type T
  var v : T
}

object B {
  def f(x : { val y : A }) { x.y.v = x.y.v }

  var a : A = _
  var b : Boolean = false
  def y : A = {
    if(b) {
      a = new A { type T = Int; var v = 1 }
      a
    } else {
      a = new A { type T = String; var v = "" }
      b = true
      a
    }
  }
}

object Test extends App {
  B.f(B)
}
