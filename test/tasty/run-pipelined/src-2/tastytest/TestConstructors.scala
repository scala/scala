package tastytest

import lib.Constructors

object TestConstructors extends scala.App {

  val cgi = new Constructors.CanonicalGeneric[Int]()
  val c = new Constructors.Canonical()

  val sgi = new Constructors.SingleGeneric[Int](42)
  val s = new Constructors.Single(42)

  val mgi_1 = new Constructors.MultipleGeneric[Int](42, 43)
  val mgi_2 = new Constructors.MultipleGeneric[Int](44)

  val m_1 = new Constructors.Multiple(42, 43)
  val m_2 = new Constructors.Multiple(44)

  assert(Seq[Any](cgi, c, sgi, s, mgi_1, mgi_2, m_1, m_2).length > 0)

}
