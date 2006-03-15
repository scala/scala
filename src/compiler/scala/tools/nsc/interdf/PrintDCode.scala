package scala.tools.nsc.interdf
import scala.tools.nsc.reporters.ConsoleReporter
import scala.collection.mutable.HashMap

/** An example use of DCode: compile the requested files and then
  * print out their DCode
  */
object PrintDCode {
  def printDCode(dcode: DCode, files: List[String]) = {
    import dcode._

    val blockNames = new HashMap[BasicBlock,String]
    def blockName(blk: BasicBlock): String = {
      if(blockNames.contains(blk))
        blockNames(blk)
      else {
        val name = "blk" + blockNames.size
        blockNames(blk) = name
        name
      }
    }

    val run = new compiler.Run
    run.compile(files)  // XXX this actually compiles the files; really, it should not
                        // produce the class files....

    for(val unit  <- run.units)
        Console.println("Unit: " + unit)

     for(val Pair(nam, cls) <- compiler.icodes.classes.toList) {
       Console.println("Name: " + nam + " class: " + cls)
       for(val meth <- cls.methods) {
         Console.println("  method: " + meth)
         val converted = icodeToDCode(meth.code)
         converted.blocks.map(blockName) // name the blocks in order
         for(val blk <- converted.blocks) {
           Console.println("     " + blockName(blk) + ":")
           for(val ins <- blk.instrs)
             Console.println("        " + ins)
           Console.println("     blocks after " +
                           blockName(blk) + ": " + blk.next.map(blockName))
           Console.println
         }
       }
     }
  }

  def main(args: Array[String]): Unit = {
    val reporter = new ConsoleReporter()
    def error(msg: String): Unit =
      reporter.error(null,
          msg + "\n  " + "HACK" + " -help  gives more information")

    val command = new CompilerCommand(List.fromArray(args), error, false)
    object compiler extends Global(command.settings, reporter)
    val comp = compiler
    val dcode = new DCode{val compiler = comp}
    printDCode(dcode, command.files)
  }
}
