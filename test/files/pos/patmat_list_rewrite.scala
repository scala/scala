//> using options -Werror
//
class C {
  def m(xs: List[String]) = xs match {
    case List() => "z"
    case y :: _ => y
  }
  // was: patmat_list_rewrite.scala:4: warning: match may not be exhaustive.
  // It would fail on the following input: Nil
}
