import scala.reflect.runtime.universe._
import scala.reflect.ClassTag

case class R(
  sales : Int,
  name : String
)

class Foo{
 import Test._
 def foo = {
  val expectedType = implicitly[TypeTag[R]]
  val classTag = implicitly[ClassTag[R]]
  val cl = classTag.runtimeClass.getClassLoader
  val cm = runtimeMirror(cl)
  val constructor = expectedType.tpe.member( termNames.CONSTRUCTOR ).asMethod
  val sig = constructor.info
  val sym = cm.classSymbol( classTag.runtimeClass )
  try {
    val cls = cm.reflect( this ).reflectClass( sym )
    cls.reflectConstructor( constructor )( 5,"test" ).asInstanceOf[R]
    println("this indicates a failure")
  } catch {
    case ex: Throwable =>
      println(ex.getMessage)
  }
 }
}

object Test extends App{
  val foo = new Foo
  println( foo.foo )
}