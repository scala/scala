import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.ToolBox

object Test extends App {
  def funNullary() = {
    def z = 3
    reify{z}
  }

  def fun0Args() = {
    def z() = 3
    reify{z}
  }
  
  def fun1Args() = {
    def z(x: Int) = x
    reify{z(3)}
  }

  def fun2Args() = {
    def z(x: Int, y: Int) = x + y
    reify{z(1,2)}
  }

  def funVarargs() = {
    def z(x: Int, y: Int*) = x + y.sum 
    reify{z(1,1,1)}
  }

  def funCurried() = {
    def z(x: Int)(y: Int) = x + y 
    reify{z(1)(2)}
  }


  val toolbox = cm.mkToolBox()
  val dyn0 = toolbox.eval(funNullary().tree)
  val foo0 = dyn0.asInstanceOf[Int]

  val dyn1 = toolbox.eval(fun0Args().tree)
  val foo1 = dyn1.asInstanceOf[Int]

  val dyn2 = toolbox.eval(fun1Args().tree)
  val foo2 = dyn2.asInstanceOf[Int]

  val dyn3 = toolbox.eval(fun2Args().tree)
  val foo3 = dyn3.asInstanceOf[Int]

  val dyn4 = toolbox.eval(funVarargs().tree)
  val foo4 = dyn4.asInstanceOf[Int]

  val dyn5 = toolbox.eval(funCurried().tree)
  val foo5 = dyn5.asInstanceOf[Int]

  println(foo0)
  println(foo1)
  println(foo2)
  println(foo3)
  println(foo4)
  println(foo5)
}
