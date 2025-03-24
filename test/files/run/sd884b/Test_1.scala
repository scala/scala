import scala.tools.partest.ReplTest

object Test extends ReplTest {
  override def code =
    """class B {
      |  @ann(x = 11) def m1 = 1
      |  @ann(y = 22) def m2 = 1
      |
      |  @kon(x = 11) def k1 = 1
      |  @kon(y = 22) def k2 = 1
      |}
      |:power
      |def t(tp: Type) = {
      |  val ms = tp.members.toList.filter(_.name.startsWith("m")).sortBy(_.name)
      |  for (m <- ms) {
      |    val i = m.annotations.head
      |    println(i)
      |    println(i.args.map(_.tpe))
      |    println(i.args.map(i.argIsDefault))
      |  }
      |  val ks = tp.members.toList.filter(_.name.startsWith("k")).sortBy(_.name)
      |  ks.foreach(k => println(k.annotations.head))
      |  ks.foreach(k => println(k.annotations.head.assocsWithDefaults))
      |}
      |t(typeOf[A])
      |t(typeOf[B])
      |""".stripMargin
}
