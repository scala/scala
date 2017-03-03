trait TFinal { final val bla: Int = 123 }

// bla should be final in C
class CFinal extends TFinal


trait TConst { final val C = "S" }
// there should be a C method in `T$class`!
class CConst extends TConst {  }


object Test {
  def main(args: Array[String]): Unit = {
    val f1 = classOf[CFinal].getDeclaredMethod("bla")
    import java.lang.reflect.Modifier._
    assert(isFinal(f1.getModifiers), f1)

    classOf[CConst].getMethod("C")

    import language.reflectiveCalls
    assert(new CConst().asInstanceOf[{def C: String}].C == "S")
  }
}
