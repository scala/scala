object pmbugbounds {
  trait Bar
  class Foo[t <: Bar] {}
            
  (new Foo[Bar]) match {
    case _ : Foo[x] => null
  }
}
