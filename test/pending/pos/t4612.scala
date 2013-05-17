class CyclicReferenceCompilerBug {
  trait Trait[A] {
    def foo: A
  }

  class Class extends Trait[Class] {
    def foo = new Class

    trait OtherTrait extends Trait[OtherTrait] {
      self: Class =>

      def foo = new Class
    }
  }
}
