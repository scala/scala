trait CaseClass
trait ProdCaseClass extends CaseClass { def x: Int }
trait SeqCaseClass extends CaseClass { def xs: Seq[Int] }

case class CaseClass1() extends CaseClass
case class CaseClass2(xs: Int*) extends SeqCaseClass
case class CaseClass3(x: Int) extends ProdCaseClass
case class CaseClass4(x: Int, xs: Int*) extends ProdCaseClass with SeqCaseClass

object Extractor1 { def unapply(x: CaseClass): Boolean = false }
object Extractor2 { def unapplySeq(x: SeqCaseClass): Option[Seq[Int]] = Some(x.xs) }
object Extractor3 { def unapply(x: ProdCaseClass): Option[Int] = Some(x.x) }
object Extractor4 { def unapplySeq(x: ProdCaseClass with SeqCaseClass): Option[(Int, Seq[Int])] = Some(x.x, x.xs) }

class A {
  def f1(x: Any) = x match {
    case CaseClass1()           => -1
    case CaseClass2(xs @ _*)    => xs.sum
    case CaseClass3(x)          => x
    case CaseClass4(x, xs @ _*) => x + xs.sum
    case Extractor4(x, xs @ _*) => 1000 + x + xs.sum
    case Extractor3(x)          => 1000 + x
    case Extractor2(xs @ _*)    => 1000 + xs.sum
    case Extractor1()           => -3
    case _                      => -2
  }
  def f2(x: Any) = x match {
    case Extractor4(x, xs @ _*) => 1000 + x + xs.sum
    case Extractor3(x)          => 1000 + x
    case Extractor2(xs @ _*)    => 1000 + xs.sum
    case Extractor1()           => -3
    case CaseClass1()           => -1
    case CaseClass2(xs @ _*)    => xs.sum
    case CaseClass3(x)          => x
    case CaseClass4(x, xs @ _*) => x + xs.sum
    case _                      => -2
  }
  def run() {
    List(
      f1(CaseClass1()),
      f1(CaseClass2(1, 2, 3)),
      f1(CaseClass3(4)),
      f1(CaseClass4(5, 6, 7)),
      f2(CaseClass1()),
      f2(CaseClass2(1, 2, 3)),
      f2(CaseClass3(4)),
      f2(CaseClass4(5, 6, 7))
    ) foreach println
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    (new A).run
  }
}


class B {
  case class CaseClass0()
  case class CaseClass0v(xs: Int*)

  case class CaseClass(x: Int, y: Int)
  object Extractor { def unapply(x: Any): Option[(Int, Int)] = Some((1, 1)) }

  case class CaseSeq(x: Char, y: Double, zs: Int*)
  object ExtractorSeq { def unapplySeq(x: Any): Option[(Int, Int, Seq[Int])] = Some((1, 1, List(1))) }

  def f1(x: CaseClass) = x match { case CaseClass(y, z) => y }
  def f2(x: Any) = x match { case Extractor(y, z) => y }

  def f3(x: CaseSeq) = x match {
    case CaseSeq(x, y)    => y
    case CaseSeq(x, y, z) => z
  }
  def f4(x: CaseSeq) = x match {
    case CaseSeq(x, y, z)      => z :: Nil
    case CaseSeq(x, y, z @ _*) => z
  }

  def f5(x: Any) = x match { case ExtractorSeq(x, y, z) => z }
  def f6(x: Any) = x match { case ExtractorSeq(x, y, z @ _*) => z }

  def g1(x: CaseClass0) = x match {
    case CaseClass0() => true
  }
  def g2(x: CaseClass0v) = x match {
    case CaseClass0v()        => true
    case CaseClass0v(5)       => true
    case CaseClass0v(x)       => true
    case CaseClass0v(xs @ _*) => false
  }
}

package p1 {
  trait _X {
    case class _Foo();
    object _Bar {
      def unapply(foo: _Foo): Boolean = true;
    }
  }

  object Y extends _X {
    val foo = _Foo()
    foo match {
      case _Bar() =>
      case _ => assert(false)
    }
  }
}
