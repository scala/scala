trait Foo[CC[_]] { }

class Bip {
  val x = new Foo[Array] { } 
}