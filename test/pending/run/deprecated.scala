object Test {
  abstract class AbstractStuff {
    def dostuff: Unit
  }
  
  [postabstract]
  class BlueStuff extends AbstractStuff {
    [deprecated] def dostuff = Console.println("blue")
    def five = "five"
  }
  
  class LightBlueStuff extends BlueStuff {
    [deprecated] override def dostuff = {Console.println("light blue")}
    
    // warning: deprecated method overrides a concrete method
    [deprecated] override def five = "light five"
  }

  // warning: not marked as postabstract
  class RedStuff extends AbstractStuff {
    [deprecated] def dostuff = Console.println("red")
  }

  
  def main(args: Array[String]) {
    //  warning: BlueStuff will be abstract after deprecated methods are removed
    val blue = new BlueStuff  

    //  warning: RedStuff will be abstract after deprecated methods are removed
    val red = new RedStuff  
    
    //  warning: dostuff is deprecated
    blue.dostuff 
  } 
}
