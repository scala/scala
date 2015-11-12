// that would be:
// SI-5079 "Scaladoc can't link to an object (only a class or trait)"
// SI-4497 "Links in Scaladoc - Spec and implementation unsufficient"
// SI-4224 "Wiki-links should support method targets"
// SI-3695 "support non-fully-qualified type links in scaladoc comments"
// SI-6487 "Scaladoc can't link to inner classes"
// SI-6495 "Scaladoc won't pick up group name, priority and description from owner chain"
// SI-6501 "Scaladoc won't link to a @template type T as a template but as a member"
package scala.test.scaladoc.links {
  import language.higherKinds
  class C

  trait Target {
    type T
    type S = String
    class C
    def foo(i: Int) = 2
    def foo(s: String) = 3
    def foo[A[_]](x: A[String]) = 5
    def foo[A[_[_]]](x: A[List]) = 6
    val bar: Boolean
    def baz(c: scala.test.scaladoc.links.C) = 7
  }

  object Target {
    type T = Int => Int
    type S = Int
    type ::[X] = scala.collection.immutable.::[X]
    class C
    def foo(i: Int) = 2
    def foo(z: String) = 3
    def foo[A[_]](x: A[String]) = 5
    def foo[A[_[_]]](x: A[List]) = 6
    val bar: Boolean = false
    val onlyInObject = 1
    def baz(c: scala.test.scaladoc.links.C) = 7
  }

  /**
   *  Links to the trait:
   *  - [[scala.test.scaladoc.links.Target$                object Test]]
   *  - [[scala.test                                       package scala.test]]
   *  - [[scala.test.scaladoc.links.Target!.T              trait Target -> type T]]
   *  - [[test.scaladoc.links.Target!.S                    trait Target -> type S]]
   *  - [[scaladoc.links.Target!.foo(i:Int)*               trait Target -> def foo]]
   *  - [[links.Target!.bar                                trait Target -> def bar]]
   *  - [[[[Target!.foo[A[_[_]]]*                          trait Target -> def foo with 3 nested tparams]]]] (should exercise nested parens)
   *  - [[Target$.T                                        object Target -> type T]]
   *  - [[Target$.S                                        object Target -> type S]]
   *  - [[Target$.::                                       object Target -> type ::]]
   *  - [[Target$.foo(z:Str*                               object Target -> def foo]]
   *  - [[Target$.bar                                      object Target -> def bar]]
   *  - [[[[Target$.foo[A[_[_]]]*                          trait Target -> def foo with 3 nested tparams]]]] (should exercise nested parens)
   *  - [[Target.onlyInObject                              object Target -> def foo]] (should find the object)
   *  - [[Target$.C                                        object Target -> class C]] (should link directly to C, not as a member)
   *  - [[Target!.C                                        trait Target -> class C]] (should link directly to C, not as a member)
   *  - [[Target$.baz(c:scala\.test\.scaladoc\.links\.C)*  object Target -> def baz]] (should use dots in prefix)
   *  - [[Target!.baz(c:scala\.test\.scaladoc\.links\.C)*  trait Target -> def baz]] (should use dots in prefix)
   *  - [[localMethod                                      object TEST -> localMethod]] (should use the current template to resolve link instead of inTpl, that's the package)
   *  - [[#localMethod                                     object TEST -> localMethod]] (should exercise Java-style links to empty members)
   *  - [[ImOutside                                        class ImOutside (check correct lookup in EmptyPackage)]]
   *  - [[ImOutside.Inner#foo                              class ImOutside#class Inner#method foo (check correct lookup in EmptyPackage)]]
   *  - [[ImOutside.T                                      class ImOutside#type T (check correct linking to templates)]]
   *  - [[ImOutside.T#foo                                  class ImOutside#type T#method foo (check correct interaction between @template and links)]]
   */
  object TEST {
    def localMethod = 3
  }
}
trait ImOutside {
  /** @template */
  type T <: Inner
  class Inner {
    def foo: Any
  }
}
