trait A {
  type T <: { type S[-U] }
  val x : T
  def y : x.S[AnyRef]
  def z : x.S[String] = y
}

object B extends A {
 type T = { type S[U] = U }
 val x : T = null
 def y : x.S[AnyRef] = new AnyRef
 def t : String = z
}

// println(B.t)