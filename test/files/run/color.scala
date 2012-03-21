import scala.tools.util.color._

object Test {
  // The ones which are somewhat widely supported.
  def effects = List(Bright, Underline, Inverse)
  
  def demo(text: String) = {
    def to_s(esc: Ansi): String = esc.atoms map {
      case x: AnsiBackground => "" + x
      case x                 => "%-10s" format x
    } mkString " "
      
    def show(esc: Ansi) = println("%s  %s".format(text in esc, to_s(esc)))
    
    println("\n1 color")
    for (c <- Ansi.colors) show(c)
    println("\n1 effect")
    for (e <- Ansi.effects) show(e)
    println("\n1 color 1 effect")
    for (c <- Ansi.colors; e <- effects) show(c / e)
    println("\n2 colors 0 effects")
    for (c1 <- Ansi.colors ; c2 <- Ansi.colors) show(c2 on c1)
    println("\n2 colors 1 effect")
    for (c1 <- Ansi.colors ; c2 <- Ansi.colors ; e1 <- effects) show((c2 on c1) / e1)
    println("\n2 colors 2 effects")
    for (c1 <- Ansi.colors ; c2 <- Ansi.colors ; e1 <- effects ; e2 <- effects ; if e1 != e2) show((c2 on c1) / e1 / e2)
  }
  
  def main(args: Array[String]): Unit = {
    val str = if (args.size > 1) args mkString " " else "the quick brown fox"
    demo(str)
  }
}
