import org.scalacheck._, Prop._, Gen._, Arbitrary._
import scala.reflect.runtime.universe._, Flag._

object RuntimeErrorProps extends QuasiquoteProperties("errors") {
  def testFails[T](block: =>T) = test {
    assertThrows[IllegalArgumentException] {
      block
    }
  }

  property("default param anon function") = testFails {
    val param = q"val x: Int = 1"
    q"{ $param => x + 1 }"
  }

  property("non-casedef case") = testFails {
    val x = q"x"
    q"foo match { case $x }"
  }

  property("non-new annotation") = testFails {
    val annot = q"foo"
    q"@$annot def foo"
  }

  property("non-valdef param") = testFails {
    val param = q"foo"
    q"def foo($param)"
  }

  property("non-valdef class param") = testFails {
    val param = q"foo"
    q"class Foo($param)"
  }

  property("non-typedef type param") = testFails {
    val tparam = tq"T"
    q"class C[$tparam]"
  }

  property("non-definition refine stat") = testFails {
    val stat = q"foo"
    tq"Foo { $stat }"
  }

  property("non-definition early def") = testFails {
    val stat = q"foo"
    q"class Foo extends { $stat } with Bar"
  }

  property("type apply for definition") = testFails {
    val defn = q"def foo"
    q"$defn[foo]"
  }

  property("non-val selftype") = testFails {
    val foo = q"foo"
    q"class Foo { $foo => }"
  }

  property("for empty enums") = testFails {
    val enums = List.empty[Tree]
    q"for(..$enums) 0"
  }

  property("for starts with non-from enum") = testFails {
    val enums = fq"foo = bar" :: Nil
    q"for(..$enums) 0"
  }

  property("for invalid enum") = testFails {
    val enums = q"foo" :: Nil
    q"for(..$enums) 0"
  }
}
