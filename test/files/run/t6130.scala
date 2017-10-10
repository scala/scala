import scala.tools.partest._

object Test extends StoreReporterDirectTest {
  override def extraSettings: String = "-usejavacp -Xprint:patmat -Ystop-after:patmat"

  override def code =
    """trait T { type T ; val t: T }
      |object tInt extends T { type T = Int; val t = 1 }
      |
      |trait TU { type U }
      |
      |object XT {
      |  def unapply(x: T): Option[(x.T, x.T)] = Some(((x.t, x.t)))
      |}
      |
      |object XTU {
      |  def unapply(t: TU): Option[t.U] = ???
      |}
      |
      |object XA {
      |  def unapply(x: AnyRef): Option[x.type] = Some(x)
      |}
      |
      |
      |// TODO: show that `<unapply-selector>` is gone from the following lines (after patmat)
      |class Test {
      |  //  <synthetic> <triedcooking> val o9: scala.this.Option[scala.this.Tuple2[<unapply-selector>.T,<unapply-selector>.T]] = XT.unapply(p2);
      |  // <triedcooking> val a: <unapply-selector>.T = o9.get._1;
      | def t: Int = Some(tInt) match { case Some(XT(a, _ )) => a }
      |
      | def tu = (null: Any) match {
      |   // <synthetic> <triedcooking> val o8: scala.this.Option[<unapply-selector>.U] = XTU.unapply(x2);
      |   case XTU(otherExRep) =>
      |     // <triedcooking> val otherExRep: <unapply-selector>.U = o8.get;
      |     println(otherExRep)
      | }
      |
      | def anyref(z: AnyRef) = {
      |   z match {
      |     // <synthetic> <triedcooking> val o8: scala.this.Option[<unapply-selector>.type] = XA.unapply(x1);
      |     case XA(x) => x
      |     case _ => ()
      |   }
      | }
      |}
      |
      |
    """.stripMargin

  def show(): Unit = {
    val baos = new java.io.ByteArrayOutputStream()
    Console.withOut(baos)(Console.withErr(baos)(compile()))
    val out = baos.toString("UTF-8")

    val unapplySelectorDummies = out.lines.filter(_.contains("<unapply-selector>")).map(_.trim).toList
    assert(unapplySelectorDummies.isEmpty, unapplySelectorDummies)
  }
}
