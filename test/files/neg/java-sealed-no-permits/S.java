// no class in this file is final, abstract, private or protected
public sealed interface S {
  record A() implements S {}
  record B() implements S {}
  enum E implements S { X }
  non-sealed class C implements S {}
}
