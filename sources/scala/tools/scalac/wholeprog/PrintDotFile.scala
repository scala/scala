import scalac.{CompilationUnit => scalac_CompilationUnit}
import scalac.ast._;
import scalac.util.Name;

package scala.tools.scalac.wholeprog {

/**
 * Print the AST into a dot file, which can be used by
 * the graphviz "dot" tool to build a graph image. Useful for
 * understanding the Abstract Syntax Tree.
 */
class PrintDotFile(_units: Array[scalac_CompilationUnit]) {
  private val units = _units;
  private var writer: java.io.Writer = null;

  def makeDotFile(output: String): unit = {
      val outputFile = new java.io.File(output);
      writer = new java.io.FileWriter(outputFile);

      writer.write("digraph tree {\nnode [style=filled, color=cadetblue2];\n");

      units.foreach( (u:scalac_CompilationUnit) =>
	u.body.foreach ( (t: Tree) => walk(t, null) ) );

      writer.write("}\n");
      writer.close();
  }


  def makeDotFile(output: String, tree: Tree): unit = {
      writer = new java.io.FileWriter(output);

      writer.write("digraph tree {\nnode [style=filled, color=cadetblue2];\n");

      walk(tree, null);

      writer.write("}\n");
      writer.close();
  }

    def printNode(t: Object, label: String): unit = {
      writer.write(t.hashCode().toString() + " [label = \"" + label + "\"];\n");
    }


    def printEdge(t1: Object, t2: Object, label: String): unit = {
      if (t1 != null)
	writer.write(t1.hashCode().toString() + " -> " + t2.hashCode().toString() +
		     "[label= \"" + label + "\"];\n");
    }

    def printEdge(t1: Object, t2: Object): unit = printEdge(t1, t2, "");

  def walk(t: Tree, parent: Tree, l: String): unit = {
    t match {

//      case Tree$Empty => {
//	printNode(t, "Empty");
//	printEdge(parent, t);
//      }

      case Tree$DocDef(comment, definition) => {
	printNode(t, "DocDef");
	printEdge(parent, t);

	walk(definition, t);
      }

      case Tree$ClassDef(mods, name, tparams, vparams, tpe, template) => {
	printNode(t, "ClassDef: " + name.toString());
	printEdge(parent, t);

	tparams.foreach( (p: Tree) => {
	  walk(p, t);
	});

	vparams.foreach( (arr) => {
	  (p: Tree) =>  walk(p, t);
	});

	walk(tpe, t);

	walk(template, t);
      }

      case Tree$PackageDef(packaged, impl) => {
	printNode(t, "PackageDef");
	printEdge(parent, t);

	walk(packaged, t);
	walk(impl, t);
      }

      case Tree$ModuleDef(_, name, tpe, impl) => {
	printNode(t, "ModuleDef: " + name.toString());
	printEdge(parent, t);

	walk(tpe, t);
	walk(impl, t);
      }

      case Tree$ValDef(_, name, tpe, rhs) => {
	printNode(t, "ValDef: " + name.toString());
	printEdge(parent, t);

	walk(tpe, t);
	walk(rhs, t);
      }

      case Tree$PatDef(_, pat, rhs) => {
	printNode(t, "PatDef");
	printEdge(parent, t);

	walk(pat, t);
	walk(rhs, t);
      }

      case Tree$DefDef(_, name, tparams, vparams, tpe, rhs) => {
	printNode(t, "DefDef: " + name.toString());
	printEdge(parent, t);

	tparams.foreach( (tt) =>  walk(tt, t) );

	vparams.foreach( (at) => at.foreach( (tt) => walk(tt, t) ) );
	walk(tpe, t);
	walk(rhs, t);
      }

      case Tree$AbsTypeDef(_, name, rhs, lobound) => {
	printNode(t, "AbsTypeDef: " + name.toString());
	printEdge(parent, t);

	walk(rhs, t);
	walk(lobound, t);
      }

      case Tree$AliasTypeDef(_, name, tparams, rhs) => {
	printNode(t, "AliasTypeDef: " + name.toString());
	printEdge(parent, t);

	tparams.foreach( (tt) => walk(tt, t) );
	walk(rhs, t);
      }

      case Tree$Import(expr, selectors) => {
	printNode(t, "Import");
	printEdge(parent, t);

	walk(expr, t);
	selectors.foreach( (n: Name) => {
	  printNode(n, "Name: " + n);
	  printEdge(t, n);
	});
      }

      case Tree$CaseDef(pat, guard, body) => {
	printNode(t, "CaseDef");
	printEdge(parent, t);

	walk(pat, t);
	walk(guard, t);
	walk(body, t);
      }

      case Tree$Template(parents, body) => {
	printNode(t, "Template");
	printEdge(parent, t);

	parents.foreach( (tt) => {
	  walk(tt, t, "parent");
	});

	body.foreach( (tt) => {
	  walk(tt, t, "body");
	});
      }

      case Tree$LabelDef(name, params, rhs) => {
	printNode(t, "LabelDef: " + name.toString());
	printEdge(parent, t);

	params.foreach( (tt) => walk(tt, t) );
	walk(rhs, t);
      }

      case Tree$Block(stats, expr) => {
	printNode(t, "Block");
	printEdge(parent, t);

	stats.foreach( (tt) => walk(tt, t) );
	walk(expr, t);
      }

      case Tree$Sequence(trees) => {
	printNode(t, "Sequence");
	printEdge(parent, t);

	trees.foreach( (tt) => walk(tt, t) );
      }

      case Tree$Alternative(trees) => {
	printNode(t, "Alternative");
	printEdge(parent, t);

	trees.foreach( (tt) => walk(tt, t) );
      }

      case Tree$Bind(name, rhs) => {
	printNode(t, "Bind: " + name.toString());
	printEdge(parent, t);

	walk(rhs, t);
      }

      case Tree$Visitor(cases) => {
	printNode(t, "Visitor");
	printEdge(parent, t);

	cases.foreach( (tt) => walk(tt, t) );
      }

      case Tree$Function(vparams, body) => {
	printNode(t, "Function");
	printEdge(parent, t);

	vparams.foreach( (tt) => walk(tt, t) );
	walk(body, t);
      }

      case Tree$Assign(lhs, rhs) => {
	printNode(t, "Assign");
	printEdge(parent, t);

	walk(lhs, t);
	walk(rhs, t);
      }

      case Tree$If(cond, thenp, elsep) => {
	printNode(t, "If");
	printEdge(parent, t);

	walk(cond, t);
	walk(thenp, t);
	walk(elsep, t);
      }

      // we ignore the "tags" array...
      case Tree$Switch(test, _, bodies, otherwise) => {
	printNode(t, "Switch");
	printEdge(parent, t);

	walk(test, t);
	bodies.foreach( (tt) => walk(tt, t) );
	walk(otherwise, t);
      }

      case Tree$Return(expr) => {
	printNode(t, "Return");
	printEdge(parent, t);

	walk(expr, t);
      }

      case Tree$Throw(expr) => {
	printNode(t, "Throw");
	printEdge(parent, t);

	walk(expr, t);
      }

      case Tree$New(template) => {
	printNode(t, "New");
	printEdge(parent, t);

	walk(template, t);
      }

      case Tree$Create(qualifier, targs) => {
	printNode(t, "Create");
	printEdge(parent, t);

	walk(qualifier, t);
	targs.foreach( (a) => walk(a, t) );
      }

      case Tree$Typed(expr, tpe) => {
	printNode(t, "Typed");
	printEdge(parent, t);

	walk(expr, t);
	walk(tpe, t);
      }

      case Tree$TypeApply(fun, args) => {
	printNode(t, "TypeApply");
	printEdge(parent, t, l);

	walk(fun, t);

	args.foreach( (tt) => walk(tt, t) );
      }

      case Tree$Apply(fun, args) => {
	printNode(t, "Apply");
	printEdge(parent, t, l);

	walk(fun, t, "fun");

	args.foreach( (tt) => walk(tt, t, "arg") );
      }

      case Tree$Super(qualifier, mixin) => {
	printNode(t, "Super");
	printEdge(parent, t);

	printNode(qualifier, "Qualifier: " + qualifier.toString());
	printNode(mixin, "Mixin: " + mixin.toString());

	printEdge(t, qualifier);
	printEdge(t, mixin);
      }

      case Tree$This(qualifier) => {
	printNode(t, "This");
	printEdge(parent, t);

	printNode(qualifier, "Qualifier: " + qualifier);
	printEdge(t, qualifier);
      }

      case Tree$Select(qualifier, selector) => {
	printNode(t, "Select");
	printEdge(parent, t, l);

//	printNode(qualifier, "Qualifier: " + qualifier.toString());
//	printEdge(t, qualifier);
//	printNode(qualifier, "Qualifier");
//	Console.println("--qualifier: " + qualifier + " : " + selector);
	walk(qualifier, t);

	printNode(selector, "Selector: " + selector.toString());
	printEdge(t, selector);
      }

      case Tree$Ident(name) => {
	printNode(t, "Ident");
	printEdge(parent, t);

	printNode(name, "Name: " + name);
	printEdge(t, name);
      }

      case Tree$Literal(value) => {
	printNode(t, "Literal");
	printEdge(parent, t);

	printNode(value, "Value: " + value);
	printEdge(t, value);
      }

      case Tree$TypeTerm() => {
	printNode(t, "TypeTerm: " + t.getType());
	printEdge(parent, t);
      }

      case Tree$SingletonType(ref) => {
	printNode(t, "SingletonType");
	printEdge(parent, t);

	walk(ref, t);
      }

      case Tree$SelectFromType(qual, selector) => {
	printNode(t, "SelectFromType");
	printEdge(parent, t);

	walk(qual, t);
	printNode(selector, "Selector: " + selector);
	printEdge(t, selector);
      }

      case Tree$FunType(argtypes, restpe) => {
	printNode(t, "FunType");
	printEdge(parent, t);

	argtypes.foreach( (tt) => walk(tt, t) );
	walk(restpe, t);
      }

      case Tree$CompoundType(parents, refinements) => {
	printNode(t, "CompoundType");
	printEdge(parent, t);

	parents.foreach( (tt) => walk(tt, t) );
	refinements.foreach( (tt) => walk(tt, t) );
      }

      case Tree$AppliedType(tpe, args) => {
	printNode(t, "AppliedType");
	printEdge(parent, t);

	walk(tpe, t);
	args.foreach( (tt) => walk(tt, t) );
      }

      case Tree$Try(block, catcher, finalizer) => {
	printNode(t, "Try");
	printEdge(parent, t);

	walk(block, t);
	walk(catcher, t);
	walk(finalizer, t);
      }

      case _ => {
//	Console.println(t.toString());
//	assert(false, t.toString());
//	printNode(t.symbol().name.toString(), "Unknown");
//	if (parent != null)
//	  printEdge(parent.symbol().name.toString(), t.symbol().name.toString());
      }
    }
  }

  def walk(t: Tree, parent: Tree): unit = walk(t, parent, "");

} // class PrintDotFile

} // package scala.tools.scalac.wholeprog
