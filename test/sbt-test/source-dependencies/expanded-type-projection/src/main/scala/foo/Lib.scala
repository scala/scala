package foo

trait Factory {
  type Product
}

trait Type extends Factory {
  type Prepend[P <: Factory] <: Type
}

trait Types[A <: Factory, B <: Type] extends Type {
  override type Prepend[P <: Factory] = Types[P, Types[A, B]]
  type Product = A#Product with B#Product
}

object Types {
  type ::[H <: Factory, T <: Type] = T#Prepend[H]
}

trait Nil extends Type {
  type Prepend[P <: Factory] = Types[P, Nil]
}

trait FactoryA extends Type {
  override type Product = A
}

trait A

trait FactoryB extends Factory {
  override type Product = B
}

trait B {
  def foo = 1
}