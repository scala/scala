
import language.postfixOps
import reflect.runtime._
import universe._

object Test {

  class Foo private () {
    override def toString = "privately constructed"
  }

  def main(args: Array[String]): Unit = {

    //val foo = new Foo  // no access
    val klass = currentMirror reflectClass typeOf[Foo].typeSymbol.asClass
    val init  = typeOf[Foo].members find { case m: MethodSymbol => m.isConstructor case _ => false } get
    val ctor  = klass reflectConstructor init.asMethod
    val foo   = ctor()   // no access?
    Console println foo
  }
}

