//package subscript.examples

import subscript._
import subscript.DSL._
import subscript.Predef._
import subscript.vm._

import scala.swing._
import subscript.swing.SimpleSubscriptApplication

// Subscript sample application: "Hello world!", printed using a sequence of 2 code fragments

object HelloWorld extends HelloWorldApp
class  HelloWorldApp 
{
  // bridge method:
  def main( args: Array[String]): Unit = println("Success: "+_execute(_main(args)).hasSuccess)
  
 def script..
   main(args: Array[String]) = print("Hello ") println("world!")

// The compiler translates this internally to a method:
//
//  def _main(_args: FormalInputParameter[Array[String]]) = _script(this, 'main, _args~'args) {_seq({print("Hello ")}, {println("world!") })}
}