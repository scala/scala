import scala.tools.nsc.interactive.tests.InteractiveTest
import scala.reflect.internal.util.SourceFile
import scala.tools.nsc.interactive.Response

object Test extends InteractiveTest {
    
  override def execute(): Unit = {
    val sf = sourceFiles.find(_.file.name == "A.scala").head
    uniqueParseTree_t1001326(sf)
    unattributedParseTree_t1001326(sf)
    neverModifyParseTree_t1001326(sf)
    shouldAlwaysReturnParseTree_t1001326(sf)
  }
  
  /**
   * Asking twice for a parseTree on the same source should always return a new tree
   */
   private def uniqueParseTree_t1001326(sf: SourceFile) {
    val parseTree1 = compiler.parseTree(sf)
    val parseTree2 = compiler.parseTree(sf)
    if (parseTree1 != parseTree2) {
      reporter.println("Unique OK")
    } else {
      reporter.println("Unique FAILED")
    }
  }
  
  /**
   * A parseTree should never contain any symbols or types
   */
  private def unattributedParseTree_t1001326(sf: SourceFile) {
    if (noSymbolsOrTypes(compiler.parseTree(sf))) {
      reporter.println("Unattributed OK")
    } else {
      reporter.println("Unattributed FAILED")
    }
  }
  
  /**
   * Once you have obtained a parseTree it should never change
   */  
  private def neverModifyParseTree_t1001326(sf: SourceFile) {
    val parsedTree = compiler.parseTree(sf)
    loadSourceAndWaitUntilTypechecked(sf)
    if (noSymbolsOrTypes(parsedTree)) {
      reporter.println("NeverModify OK")
    } else {
      reporter.println("NeverModify FAILED")
    }
  }
  
  /**
   * Should always return a parse tree
   */
   private def shouldAlwaysReturnParseTree_t1001326(sf: SourceFile) {
     loadSourceAndWaitUntilTypechecked(sf)
     if (noSymbolsOrTypes(compiler.parseTree(sf))) {
       reporter.println("AlwaysParseTree OK")
     } else {
       reporter.println("AlwaysParseTree FAILED")
     }
   }
  
  /**
   * Load a source and block while it is type-checking.
   */
  private def loadSourceAndWaitUntilTypechecked(sf: SourceFile): Unit = {
    compiler.askToDoFirst(sf)
    val res = new Response[Unit]
    compiler.askReload(List(sf), res)
    res.get
    askLoadedTyped(sf).get
  }
  
  /**
   * Traverses a tree and makes sure that there are no types or symbols present in the tree with
   * the exception of the symbol for the package 'scala'. This is because that symbol will be
   * present in some of the nodes that the compiler generates.
   */
  private def noSymbolsOrTypes(tree: compiler.Tree): Boolean = {
    tree.forAll { t =>
      (t.symbol == null || 
       t.symbol == compiler.NoSymbol || 
       t.symbol == compiler.definitions.ScalaPackage // ignore the symbol for the scala package for now
      ) && (
       t.tpe == null || 
       t.tpe == compiler.NoType)
    }
  }
  
}