import scala.reflect.runtime.{ universe => ru }

object Test extends App {
 
  trait FormTrait {

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

  val g = () => {
    // Reported as SI-8195, same root cause
    trait Form {
 
      private val runtimeMirror = ru.runtimeMirror(this.getClass.getClassLoader)
      private val instanceMirror = runtimeMirror.reflect(this)
      private val members = instanceMirror.symbol.typeSignature.members
 
    }
 
    val f1 = new Form {
      val a = 1
    }
 
    val f2 = new Form {
      val b = f1.a
    }
  }

  f() 
  g() 
}
