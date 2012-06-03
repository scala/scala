import scala.reflect._

class Car{ override def toString = "I am a car" }
object x{
  def nullary = "nullary"
  def noargs() = "noargs"
  def - = "symbolic"
  def $(s:String) = "symbolic with args"
  val value = "value"
  def overloaded(i:Int) = "overloaded with primitive"
  def overloaded(s:String) = s
  def default( a:Int, b:Int = 2 ) = "default: "+(a+b)
  def named( a:Int, b:Int, c:Int ) = "named: "+(a+b+c)
  def typeArgs[T]( v:T ) = "typeArgs: "+v
  var mutable = "before mutation"
  def multiArgLists( a:String )( b:String ) = "multiArgList " + a + b
  def bar( s:String )(implicit car:Car) = s + car.toString
}

object Test extends App{
  val d = new DynamicProxy{ val dynamicProxyTarget = x }

  println( d.noargs )
  println( d.noargs() )
  println( d.nullary )
  println( d.value )
  println( d.- )
  println( d.$("x") )

  try{
    println( d.test )
  } catch {
    case _ => println("non-existent method")
  }

  println( d.mutable )
  
  println("mutation 1")
  d.mutable_=("after mutation 1")
  println( d.mutable )

  println("mutation 2")
  d.mutable = "after mutation 2"
  println( d.mutable )

  println( d.overloaded("overloaded with object") )
  println( d.overloaded(1) )

  // test some non-constant arguments
  def s = "overloaded with object in var"
  println( d.overloaded(s) )
  println( d.overloaded(s + " 2") )
  
  val car = new Car
  println( d.typeArgs(car) ) // inferred
  // println( d.typeArgs[Car](car) ) // explicit not working (yet) 
  
  println( d.default( 1,3 ) )
  println( d.default( 1 ) )

  println( d.named(1,c=3,b=2) ) // applyDynamicNamed seems to be broken
  
  // println( d.multiArgLists("a")("b") ) // not working yet

  /*
  // may never work
  // testing implicit parameters (first test works when moving x into TestDynamicReflect)
  implicit val car2 = new Car
  println( d.bar( "Yeah, ") );  // FAILS: could not find implicit value for parameter car
  {println( d.bar( "Yeah, ") )} // FAILS: could not find implicit value for parameter car
  */
}
