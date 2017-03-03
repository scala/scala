class Global { class Name }

trait CommonPrintUtils {
  val global: Global 
  
  lazy val precedence: global.Name => Int = ???
}

trait CompilerProvider {  val global: Global = ??? }
    
class AbstractPrinter extends CommonPrintUtils with CompilerProvider 