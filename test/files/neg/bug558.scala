package scala.tools.nsc.models;
import scala.tools.nsc.io.AbstractFile;

abstract class NewModel {
  abstract class SymbolURL {
    val top   : RootURL;
    val name  : String;
    val source  : AbstractFile;
  }
  abstract class NodeURL extends SymbolURL {
    val parent : SymbolURL;
    final val top = parent.top;
    final val source = top.file;
    
  }
  abstract class RootURL extends SymbolURL {
    final val top   : RootURL = this;
  }
}
