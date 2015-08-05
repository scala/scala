class Outer {
  def assertNoFields(c: Class[_]) {
    assert(c.getDeclaredFields.isEmpty)
  }
  def assertHasOuter(c: Class[_]) {
    assert(c.getDeclaredFields.exists(_.getName.contains("outer")))
  }
  class Member
  final class FinalMember

  def test {
    assertHasOuter(classOf[Member])
    assertNoFields(classOf[FinalMember])
    final class C
    assertNoFields(classOf[C])
    class D
    assertNoFields(classOf[D])
    (() => {class E; assertNoFields(classOf[E])}).apply()

    // The outer reference elision currently runs on a class-by-class basis. If it cannot rule out that a class has
    // subclasses, it will not remove the outer reference. A smarter analysis here could detect if no members of
    // a sealed (or effectively sealed) hierarchy use the outer reference, the optimization could be performed.
    class Parent
    class Child extends Parent
    assertHasOuter(classOf[Parent])

    // Note: outer references (if they haven't been elided) are used in pattern matching as follows.
    // This isn't relevant to term-owned classes, as you can't refer to them with a prefix that includes
    // the outer class.
    val outer1 = new Outer
    val outer2 = new Outer
    (new outer1.Member: Any) match {
      case _: outer2.Member => sys.error("wrong match!")
      case _: outer1.Member => // okay
    }

    // ... continuing on that theme, note that `Member` isn't considered as a local class, it is owned by a the class
    // `LocalOuter`, which itself happens to be term-owned. So we expect that it has an outer reference, and that this
    // is respected in type tests.
    class LocalOuter {
      class Member
      final class FinalMember
    }
    assertNoFields(classOf[LocalOuter])
    assertHasOuter(classOf[LocalOuter#Member])
    val localOuter1 = new LocalOuter
    val localOuter2 = new LocalOuter
    (new localOuter1.Member: Any) match {
      case _: localOuter2.Member => sys.error("wrong match!")
      case _: localOuter1.Member => // okay
    }
    // Final member classes still lose the outer reference.
    assertNoFields(classOf[LocalOuter#FinalMember])
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    new Outer().test
  }
}
