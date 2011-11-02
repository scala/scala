package test;

object Main extends App {
  class Global {
    case class Template(x : Int, y : Int) {
      Console.println("outer: " + Global.this);
    }
  }
  trait Contexts { self: Analyzer =>
    val xxx : global.Template = {
      assert(globalInit0 != null);
      globalInit0.Template(10, 20);
    }
  }
  abstract class Analyzer extends Contexts {
    type Global <: Main.Global;
    final val global : Global = globalInit;
    def globalInit : Global;
    final def globalInit0 = globalInit.asInstanceOf[global.type];
  }
  
  object global0 extends Global {
    object analyzer extends Analyzer {
      type Global = global0.type;
      override def globalInit = global0;
    }
  }
  Console.println(global0.analyzer.xxx);
}
