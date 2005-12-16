abstract class P[+a, +b] { // SLS, Example 4.4.2
  def fst: a;
  def snd: b
}

trait Vector[+a] { // SLS, Example 4.4.3 b)
  def append[b >: a](x: Vector[b]): Vector[b]
}
