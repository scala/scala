import scala.tools.partest.{DirectTest, fileSeparator, pathSeparator}

package p {
  package u {
    package v {
      class C {
        def m = 1
        def m(x: Int) = 1
        def :: = 1
        class :: {
          def m = 1
        }
        class D {
          def m = 1
        }
        object D {
          def m = 1
        }
      }
      object C {
        def m = 1
      }
      object O {
        def #### = 1
        class ####
        def ### = 1
      }
    }
    class K {
      def m = 1
    }
    object K {
      def m = 1
    }
    package w {
      object O {
        def m = 1
      }
    }
  }
}

object Test extends DirectTest {
  override def extraSettings: String = s"-usejavacp -deprecation -cp ${testOutput.path} -Yexternal-annotation-files ${testPath.path}${fileSeparator}annots.txt"

  def code =
    """class A {
      |  import p.u.v._
      |  val c = new C // dis
      |  c.m           // dis
      |  c.m(1)        // dis
      |  c.::          // dis
      |  (new c.::).m  // dat
      |  (new c.D).m   // -
      |  c.D.m         // dis
      |  C.m           // dat
      |  O.####        // dis
      |  new O.####    // dat
      |  O.###         // -
      |  (new p.u.K).m // whu
      |  p.u.K.m       // whu
      |  p.u.w.O.m     // -
      |}
      |""".stripMargin

  def show(): Unit = {
    compile()
  }
}
