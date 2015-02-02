import scala.tools.nsc.interactive.tests.core.IdempotencyTest

// At the time of writing this test, removing any part of `enterExistingSym` 
// leads to a failure.
object Test {
  def main(args: Array[String]) {
    test("""
      object Foo {
        def term {
          def foo(c: String = "") = c
          class MagicInterruptionMarker
          foo()/*?*/
        }
      }
    """)

    test("""
      object Foo {
        def term {
          def foo = 42
          class MagicInterruptionMarker
          foo/*?*/
        }
      }
    """)

    test("""
      object Foo {
        def term {
          lazy val foo = 42
          class MagicInterruptionMarker
          foo/*?*/
        }
      }
    """)

    test("""
      object Foo {
        implicit class C(val a: String) extends AnyVal
        class MagicInterruptionMarker
        ""/*?*/
      }
    """)
  }

  def test(code0: String) {
    val t = new IdempotencyTest {
      def code = code0
    }
    t.show()
  }
}

