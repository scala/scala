object Test {
  val bippy    = new Symbol("bippy")
  val imposter = new Symbol("bippy")
  val notBippy = new Symbol("not-bippy")
  val syms = List(bippy, imposter, notBippy)

  // the equals method should only be used for case `bippy`,
  // for the singleton type pattern, case _: bippy.type, the spec mandates `bippy eq _` as the test
  class Symbol(val name: String) {
    override def equals(other: Any) = other match {
      case x: Symbol  => name == x.name
      case _          => false
    }
    override def toString = name
  }

  // TODO: test bytecode equality for f and fDirect (and g and gDirect),
  // for now the optimizer doesn't quite get from `f` to `fDirect`
  def f(s: Symbol) = s match {
    case _: bippy.type  => true
    case _              => false
  }
  def fDirect(s: Symbol) = bippy eq s

  def g(s: Symbol) = s match {
    case _: bippy.type => 1
    case `bippy`       => 2
    case _             => 3
  }
  def gDirect(s: Symbol) = if (bippy eq s) 1 else if (bippy == s) 2 else 3
  
  def main(args: Array[String]): Unit = {
    // `syms map f` should be: true false false
    assert(syms forall (s => f(s) == fDirect(s)))
    // `syms map g` should be: 1 2 3
    assert(syms forall (s => g(s) == gDirect(s)))
  }
}