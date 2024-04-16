//> using options -Werror -Xlint:strict-unsealed-patmat
object Bug {
  object Perhaps {
    def unapply[A](oa: Option[A]): Some[Option[A]] = Some(oa)
  }

  def single(opt: Option[String])  = opt match { // missing None case
    case Perhaps(Some(s)) => s
  }
  def nested(opt: Option[String])  = opt match { // missing None case
    case Perhaps(Perhaps(Some(s))) => s
  }
  def nested2(opt: Option[String]) = opt match { // missing None case
    case Perhaps(Perhaps(Perhaps(Some(s)))) => s
  }

  class Foo(val str: Option[String])
  object Foo {
    def unapply(foo: Foo): Some[Option[String]] = Some(foo.str)
  }

  def foo(foo: Foo) = foo match { // missing None case
    case Foo(Some(s)) => s
  }


  class Bar(val str: Option[String], val ing: Option[String])
  object Bar {
    def unapply(bar: Bar): Some[(Option[String], Option[String])] = Some((bar.str, bar.ing))
  }

  def bar(bar: Bar) = bar match { // missing None case
    case Bar(Some(s), _) => s
  }


  def list(list: List[Option[String]]) = list match {
    case Perhaps(Some(s)) :: _ => s
    case Perhaps(None   ) :: _ => "<none>"
    case Nil                   => "<nil>"
  } // was: warning: match may not be exhaustive.
    //      It would fail on the following input: List(_)


  object Length {
    def unapply(str: String): Some[Int] = Some(str.length)
  }

  def length(str: String) = str match { // missing non-0 case
    case Length(0) => "empty!"
  }


  object ToStr {
    def unapply(any: Any): Some[String] = Some(any.toString)
  }

  object ToInt {
    def unapply(str: String): Option[Int] = str.toIntOption
  }

  def nestedUnderIrrefutable(any: Any) = any match { // missing non-int case
    case ToStr(ToInt(n)) => n
  }


  sealed trait May[+A]
  final case class Just[+A](value: A) extends May[A]
  final case class Nada()             extends May[Nothing]

  object Possibly {
    def unapply[A](may: May[A]): Some[May[A]] = Some(may)
  }

  def usingMay[A](may: May[A]) = may match { // missing Nada case
    case Possibly(Just(a)) => a
  }
}
