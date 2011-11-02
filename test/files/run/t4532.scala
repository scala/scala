import scala.tools.partest.ReplTest
import scala.tools.util.Javap

object Test extends ReplTest {
  
  // ugh, windows
  def expectedOutput =
"""Type in expressions to have them evaluated.
Type :help for more information.

scala> 

scala> object Bippy { class Dingus ; object Bop }
defined module Bippy

scala> :javap Bippy.Dingus
Compiled from "<console>"public class Bippy$Dingus extends java.lang.Object implements scala.ScalaObject{    public Bippy$Dingus();}
scala> :javap Bippy.Bop
Compiled from "<console>"public final class Bippy$Bop$ extends java.lang.Object implements scala.ScalaObject{    public static final Bippy$Bop$ MODULE$;    public static {};    public Bippy$Bop$();}
scala> 

scala> 
"""

  override def eval() = 
    if (Javap.isAvailable()) super.eval()
    else expectedOutput.lines

  def code = """
    |object Bippy { class Dingus ; object Bop }
    |:javap Bippy.Dingus
    |:javap Bippy.Bop
  """.stripMargin
}
