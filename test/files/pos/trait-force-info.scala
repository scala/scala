/** This does NOT crash unless it's in the interactive package.
 */

package scala.tools.nsc
package interactive

trait MyContextTrees {
  val self: Global
  val NoContext = self.analyzer.NoContext
}
// 
// error: java.lang.AssertionError: assertion failed: trait Contexts.NoContext$ linkedModule: <none>List()
//  at scala.Predef$.assert(Predef.scala:160)
//  at scala.tools.nsc.symtab.classfile.ClassfileParser$innerClasses$.innerSymbol$1(ClassfileParser.scala:1211)
//  at scala.tools.nsc.symtab.classfile.ClassfileParser$innerClasses$.classSymbol(ClassfileParser.scala:1223)
//  at scala.tools.nsc.symtab.classfile.ClassfileParser.classNameToSymbol(ClassfileParser.scala:489)
//  at scala.tools.nsc.symtab.classfile.ClassfileParser.sig2type$1(ClassfileParser.scala:757)
//  at scala.tools.nsc.symtab.classfile.ClassfileParser.sig2type$1(ClassfileParser.scala:789)
