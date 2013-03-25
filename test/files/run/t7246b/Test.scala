object Test extends App {

  val so = new SubOuter
  val si = new so.SubInner
  println(si.baseInner)
  println(si.subInner)
}

class SubOuter extends Outer {
  val subOuter = "sub"
  class SubInner extends Inner {
    def subInner = subOuter
  }
}
