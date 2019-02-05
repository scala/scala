import tools.partest.DirectTest
import reflect.internal.util._

object Test extends DirectTest {

  override def extraSettings: String = s"-usejavacp -cp ${testOutput.path} -opt:l:inline -opt-inline-from:** -Yopt-log-inline _ -d ${testOutput.path}"

  override def code = """object Main {
  @noinline def t1a(a: A_1) = a.a(): @inline
  @noinline def t1b(a: A_1) = (a.a(): @inline).apply(a, "")

  @noinline def t2a(a: A_1) = a.b(): @inline
  @noinline def t2b(a: A_1) = (a.b(): @inline).apply("")

  @noinline def t3a(a: A_1) = a.c(): @inline
  @noinline def t3b(a: A_1) = (a.c(): @inline).apply("")

  @noinline def t4a(a: A_1) = a.d(""): @inline
  @noinline def t4b(a: A_1) = (a.d(""): @inline).apply(a, "")

  @noinline def t5a(a: A_1) = a.e(""): @inline
  @noinline def t5b(a: A_1) = (a.e(""): @inline).apply("")

  @noinline def t6a(a: A_1) = a.f(""): @inline
  @noinline def t6b(a: A_1) = (a.f(""): @inline).apply("")

  def main(args: Array[String]): Unit = {
    val a = new A_1("")

    println(t1a(a).apply(a, ""))
    println(t1b(a))

    println(t2a(a).apply(""))
    println(t2b(a))

    println(t3a(a).apply("").m1(""))
    println(t3b(a).m1(""))

    println(t4a(a).apply(a, ""))
    println(t4b(a))

    println(t5a(a).apply(""))
    println(t5b(a))

    println(t6a(a).apply("").m1(""))
    println(t6b(a).m1(""))
  }
}"""

  override def show(): Unit = {
    compile()
    ScalaClassLoader(getClass.getClassLoader) run ("Main", Nil)

  }
}
