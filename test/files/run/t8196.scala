object Test extends App {
 
  trait FormTrait {
    import scala.reflect.runtime.{ universe => ru }

    val runtimeMirror = ru.runtimeMirror(this.getClass.getClassLoader)
    val instanceMirror = runtimeMirror.reflect(this)
    val members = instanceMirror.symbol.typeSignature.members
    def fields = members.filter(_.typeSignature <:< ru.typeOf[Int])
  }
 
  val f = () => {
 
    class Form1 extends FormTrait {
      val f1 = 5
    }
    val form1 = new Form1
 
    println(form1.fields)
 
    val form2 = new FormTrait {
      val g1 = new Form1
    }
 
    form2.g1 // comment this line in order to make the test pass
    ()
  }

  f()  
}
