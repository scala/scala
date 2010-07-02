class ParseResult[+T]
case class Success[+T](t: T) extends ParseResult[T]

abstract class Nonterminal[Output] {

  type SubNonterminal = Nonterminal[T] forSome { type T <: Output }

  def parse: ParseResult[Output]

  def parse1(nts: List[SubNonterminal]): ParseResult[Output] =
    nts match {
      case nt::nts => nt.parse match { case Success(so) => Success(so) }
      case Nil => throw new Error
    }
}
