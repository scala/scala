import scala.swing._
import scala.swing.event._
import subscript.Predef._
import subscript.swing.SimpleSubscriptApplication
import subscript.swing.Scripts._
import subscript._
import subscript.DSL._
import subscript.Predef._
import subscript.vm._

// Subscript sample application: A..B
//
// Note: the main part of this source file has been manually compiled from Subscript code into plain Scala

object ABC  {
  
  
// script..
//  a(p0: String),b(p1: String) = {println("a,b communicate: "+p0+p1)}
//  main(args: Array[String]) = a & b
//
// would be translated to 2 methods:   
// script method, to be called from bridge method or from other scripts
// bridge method. Normally returns a ScriptExecuter; 
// only a "main" method with the proper parameter type has return type Unit, to serve as a program entry point 

  val __a = _communicator('a)
  val __b = _communicator('b)
  
  val __a_b = _communication{(_comm: N_communication) => 
    val _p0 = _comm._getParameter[String]('p0)
    val _p1 = _comm._getParameter[String]('p1)
    _normal0{println("a,b communicate: "+_p0.value+_p1.value)}}
  
  _relate(__a_b, __a~'p0, __b~'p1)
    
  def _a(_p0: FormalInputParameter[String]) = _comscript(this, __a, _p0~'p0)
  def _b(_p1: FormalInputParameter[String]) = _comscript(this, __b, _p1~'p1)
  
  def _main(_args: FormalInputParameter[Array[String]]) = _script(this, 'main, _args~'args) {_par(_call{_a("Hello ")}, _call{_b("world!")})}

  def  main( args: Array[String]): Unit = _execute(_main(args))
}
