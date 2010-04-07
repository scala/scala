object forbug {
  val l1 = List(List(ValDef(new A)), List(ValDef(new A)))
  for ((e1s, e2s) <- l1.zip(l1);
       (e1, e2) <- e1s.zip(e2s)) {
    e1.a.doSome(20)
//    ()
  }
}


class A {
  def doSome(a: Int): this.type = {
    println(a)
    this
  }
}

case class ValDef(a: A)

