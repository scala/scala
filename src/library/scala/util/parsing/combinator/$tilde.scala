package scala.util.parsing.combinatorold;

// p ~ q ~ r ^^ {case a ~ b ~ c => }
case class ~[+a, +b](_1: a, _2: b)  // extends Pair[a, b]

// shortcut for scala.util.parsing.combinatorold.~(_, _) -- just ~(_, _) resolves to unary_~
object mkTilde { def apply[a, b](_1: a, _2: b) = scala.util.parsing.combinatorold.~(_1, _2) }


  //def flatten[t, s <: (~[t, s] \/ t)](p: ~[t, s]): List[t] = p match {
  //  case hd ~ tl => hd :: flatten(tl)
  //  case a ~ b => List(a, b)
  //}
  //def flatten[t, s <: ~[t, s]](p: ~[t, s]): List[t] = p._1 :: flatten(p)
  //def flatten[t](p: ~[t, t]): List[t] = List(p._1, p._2)

