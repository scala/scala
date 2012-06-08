import scala.reflect.runtime.universe._
import scala.reflect.ClassTag

class Foo{
  case class R(
    sales : Int,
    name : String
  )

 def foo = {
  val expectedType = implicitly[TypeTag[R]]
  val classTag = implicitly[ClassTag[R]]
  val cl = classTag.runtimeClass.getClassLoader
  val cm = runtimeMirror(cl)
  val constructor = expectedType.tpe.member( nme.CONSTRUCTOR ).asMethodSymbol
  val sig = constructor.typeSignature
  val sym = cm.classSymbol( classTag.runtimeClass )
  try {
    val cls = cm.reflectClass( sym )
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