/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */



import scalac.CompilationUnit;
//import scalac.ast.TreeGen;
//import scalac.util.*;
//import scalac.symtab.*;
   import scalac.util.Name ;
   import scalac.ast.Tree ;
   import scalac.symtab.Symbol ;


import scalac.ApplicationError ;
package scala.tools.scalac.transformer.matching {
/** this class takes care of tedious stuff which has nothing to do with
 *  matching
 */
 abstract class PatternTool(unit: CompilationUnit)  {

   def containsBinding(pat: Tree): Boolean = {
     var generatedVars = false;

     def handleVariableSymbol(sym: Symbol): Unit  =
       if( sym.name.toString().indexOf("$") == -1 ) {
         generatedVars = true; // .add( sym );
       }

     def isVariableName(name: Name): Boolean =
       ( name.isVariable() ) && ( name != Name.fromString("_") ) ;

     def isVariableSymbol(sym: Symbol): Boolean =
       ( sym != null )&&( !sym.isPrimaryConstructor() );

     def traverse1(trees: Array[Tree]): Unit = {
       var i = 0; while(i < trees.length) {
         traverse(trees(i));
         i = i + 1
       }
     }
     def traverse(tree: Tree): Unit = {

       import Tree._ ;

       tree.match {
         case x @ Ident(name)=>
           if(x.symbol() != unit.global.definitions.PATTERN_WILDCARD)
             throw new ApplicationError("shouldn't happen?!");

         case Bind(name, subtree) =>
           var sym: Symbol = _;

         if( isVariableName( name )
            && isVariableSymbol( {sym = tree.symbol(); tree.symbol()} ))
           handleVariableSymbol( sym );

         traverse( subtree );

         case Select(_,_) => ;

         // congruence
         case Apply(fun, args) =>
           traverse1(args);
         case Sequence(trees) =>
           traverse1(trees);
         case Typed(expr, tpe) => // needed??
           traverse(expr);
         case _ : Alternative | _ : Select | _ : Literal =>  ; // no variables

         case _ =>
           throw new ApplicationError("unknown pattern node:"+tree+" = "+tree.getClass());
       }
     }
     traverse( pat );
     generatedVars;
   }

   final def fresh = unit.fresh;

   final def gen = unit.global.treeGen;

   final def defs = unit.global.definitions;

 } // class PatternTool
}
