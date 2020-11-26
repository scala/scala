
object Test extends scala.tools.partest.ReplTest {
  def code = """:paste -java <| EOF
    |package p;
    |public class C {
    |    public int c() {
    |        return 42;
    |    }
    |    public String toString() {
    |        return "hi, C";
    |    }
    |}
EOF
new p.C
class D extends p.C
new D().c()
  """
}
