import scala.annotation.unchecked.uncheckedVariance

class Outer {
  // A final class gets no outer ref, so we expect to see warnings where an outer ref check should be performed
  final case class Inner(val s: String) // unchecked warning

  def belongs(a: Any): Unit = a match {
    case Inner(s) => // unchecked warning
    case _ =>
  }

  def belongsStaticSameOuter(a: Inner): Unit = a match {
    case Inner(s) => // no need for outer check
    // match is exhaustive, no default case needed
  }

  def belongsOtherOuter(a: Outer#Inner): Unit = a match {
    case Inner(s) => // unchecked warning
    case O.Inner(s) => // unchecked warning
    case _ =>
  }
}

object O extends Outer {
  def belongsStaticSameOuter2(a: Inner): Unit = a match {
    case Inner(s) => // no need for outer check
    // match is exhaustive, no default case needed
  }

  def belongsStaticSameOuter3(a: Inner): Unit = a match {
    case _: Inner => // no need for outer check
    // match is exhaustive, no default case needed
  }

  def belongsStaticSameOuter4(a: Inner): Unit = a match {
    case _: (Inner @uncheckedVariance) => // no need for outer check
    // match is exhaustive, no default case needed
  }

  def belongsOtherOuter2(a: Outer#Inner): Unit = a match {
    case Inner(s) => // unchecked warning
    case _ =>
  }

  def belongsOtherOuter3(a: Outer#Inner): Unit = a match {
    case _: Inner => // unchecked warning
    case _ =>
  }

  def belongsOtherOuter4(a: Outer#Inner): Unit = a match {
    case _: (Inner @unchecked) => // warning supressed
    case _ =>
  }

  def belongsOtherOuter5(a: Outer#Inner): Unit = a match {
    case _: (Inner @uncheckedVariance) => // unchecked warning
    case _ =>
  }

  def nested: Unit = {
    final case class I(s: String)

    def check1(a: Any): Unit = a match {
      case I(s) => // no need for outer check
      case _ =>
    }

    def check2(a: I): Unit = a match {
      case I(s) => // no need for outer check
      // match is exhaustive, no default case needed
    }
  }
}

class O2 {
  def nested: Unit = {
    final case class I(s: String)

    def check1(a: Any): Unit = a match {
      case I(s) => // no need for outer check (is this correct?)
      case _ =>
    }

    def check2(a: I): Unit = a match {
      case I(s) => // no need for outer check (is this correct?)
      // match is exhaustive, no default case needed
    }
  }
}

package p {
  object T {
    case class C(x: Int)
  }
}

object U {
  val T = p.T
}

class Test {
  def m(a: Any) = a match {
    case U.T.C(1) => 1  // used to warn
    case _ => 1
  }
}
