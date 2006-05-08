abstract class Test0 {
  def visit(f: int => unit): boolean
  def visit(f: int => String): boolean
}
trait Test {
  type TypeA <: TraitA;
  type TypeB <: TypeA with TraitB;

  def f(node : TypeA) : Unit;
  def f(brac : TypeB) : Unit;

  trait TraitA;
  trait TraitB;

}
