package scala.util.parsing.combinator1

// p ~ q ~ r ^^ {case a ~ b ~ c => }
case class ~[+a, +b](_1: a, _2: b) {
  override def toString = "("+ _1 +" ~ "+ _2 +")"
}

// shortcut for scala.util.parsing.combinator.~(_, _) -- just ~(_, _) resolves to unary_~
object mkTilde { def apply[a, b](_1: a, _2: b) = scala.util.parsing.combinator.~(_1, _2) }


  //def flatten[t, s <: (~[t, s] \/ t)](p: ~[t, s]): List[t] = p match {
  //  case hd ~ tl => hd :: flatten(tl)
  //  case a ~ b => List(a, b)
  //}
  //def flatten[t, s <: ~[t, s]](p: ~[t, s]): List[t] = p._1 :: flatten(p)
  //def flatten[t](p: ~[t, t]): List[t] = List(p._1, p._2)

