abstract class Test0 {
  def visit(f: Int => Unit): Boolean
  def visit(f: Int => String): Boolean
}
trait Test {
  type TypeA <: TraitA;
  type TypeB <: TypeA with TraitB;

  def f(node : TypeA) : Unit;
  def f(brac : TypeB) : Unit;

  trait TraitA;
  trait TraitB;

}
