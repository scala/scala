/** This a consequence of the current type checking algorithm, where bounds are checked only after variables are instantiated.
 * I believe this will change once we go to constraint-based type inference.
 * Alternatively, we can pursue a more extensive fix to SI-6169
 *
 * The below code shows a compiler flaw in that the wildcard "_" as value for a bounded type parameter either
 * breaks the boundary - as it result in Any - or doesn't evaluate to the boundary (as I'd hoped it to be).
*/

class SomeClass(val intValue:Int)
class MyClass[T <: SomeClass](val myValue:T)
class Flooz[A >: Null <: SomeClass, T >: Null <: A](var value: T)

class A {
  def f1(i:MyClass[_])                       = i.myValue.intValue
  def f2(i:MyClass[_ <: SomeClass])          = i.myValue.intValue
  // def f3[T](i: MyClass[T])                   = i.myValue.intValue
  def f4[T <: SomeClass](i: MyClass[T])      = i.myValue.intValue
  // def f5[T >: Null](i: MyClass[T])           = i.myValue.intValue
  // def f6[T >: Null <: String](i: MyClass[T]) = i.myValue.intValue + i.myValue.charAt(0)

  // def g1[A, T](x: Flooz[A, T]) = { x.value = null ; x.value.intValue }
  def g2(x: Flooz[_, _]) = { x.value = null ; x.value.intValue }

  class MyClass2(x: MyClass[_]) { val p = x.myValue.intValue }
  // class MyClass3[T <: String](x: MyClass[T]) { val p = x.myValue.intValue + x.myValue.length }
  // class MyClass4[T >: Null](x: MyClass[T]) { val p = x.myValue.intValue }
}
