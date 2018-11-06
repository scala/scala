abstract class Dist[@specialized(AnyRef) A, @specialized(Int) B] {
  def apply(a: A): A
  def iterateUntil(): Dist[A, B] = new Dist[A, B] {
    def loop(a: A): A = a
    def apply(a: A): A = loop(a)
  }
}
