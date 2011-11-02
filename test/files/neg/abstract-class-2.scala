class P {
  trait S1
  val p = new P
  
  trait S2 {
    def f(x: p.S1): Int
  }
}

class P2 extends P {
  object O2 extends S2 {
    def f(x: S1) = 5
  }   
}
