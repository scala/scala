// Sorted first, so that it completes the signatures of the other units before their imports are typed.
class Ref {
  def b = new B().m
  def c = new C().m
  def d = new D().m
}
