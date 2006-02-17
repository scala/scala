// scala.tools.nsc.Main.scala
package test;

/** The main class for NSC, a compiler for the programming
 *  language Scala.
 */
object Test {
/*
  def process(): AnyRef = {
    class Compiler;
    var compiler$module: Compiler = new Compiler;
    def compiler() = compiler$module;
    class Generator {
      val c : Compiler = compiler()
    }
    var generator$module: Generator = new Generator;
    def generator() = generator$module;
    generator()
  }
*/
  def process1(): AnyRef = {
    object generator {
      val c = compiler
    }
    object compiler;
    generator
  }


}
