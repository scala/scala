// $Id$

import scalac.{Global => scalac_Global}
import scalac.{CompilationUnit => scalac_CompilationUnit}
import scalac.atree._;
import scalac.symtab._;
import scalac.util.Name;
import scalac.util._;
import scala.collection.mutable._;
import scalac.ast._;

package scala.tools.scalac.wholeprog {



/**
 * This class builds the set of classes included in the Application (whole
 * program), starting with a root class, given from the outside (usually
 * the user).
 *
 */
class ApplicationBuilder(globall: scalac_Global) {

  private val global = globall;
  var rootClassName: Name = Name.fromString("");
  //var map: SymbolMap = null;

  // algorithm data
  var worklist: Set[Symbol] = new HashSet[Symbol];
  var app: Set[Symbol]      = new HashSet[Symbol];

  def finalClasses(app: Set[Symbol], units: Array[scalac_CompilationUnit]): Unit = {
    val m = new MonomorphicCallSites(global, app);
    m.buildClassHierarchy;

    m.hierarchy.visitDFS( (n) => {
      if (m.hierarchy.inEdges(n.id).length == 0) {
//	Console.println("Final class: " + n.info.name.toString());
	n.info.flags = n.info.flags | Modifiers.FINAL;
      }
    });

//     val transf: GenTransformer = new GenTransformer(global);

//     units.foreach( (u) => {
//       u.body.foreach( (b) => transf.transform(b) );
//     });

    m.monomorphicCallsites;
  }


  /** find the whole application that is referenced by the root class  */
  def buildApplication(root: String, units: Array[scalac_CompilationUnit]): unit = {
    rootClassName = Name.fromString(root);

    var rootClass: Symbol = null;
    try {
      rootClass = global.definitions.getModule(root).moduleClass();
    } catch {
      case _ => global.error(root + " is not the name of an object");
    }

    if (rootClass == Symbol.NONE)
      global.error(root + " is not an object");

    // build the map from Symbol's to AST nodes
    buildCodeMap(units);
//    Console.println(SymbolMap);

    worklist += rootClass;

    while (!worklist.isEmpty)
      visitClass(worklist.toList.head);

    dumpApplication;

    // now we call from here the final-classes analysis,
    // but we should really move it to a more sensible place, like a
    // whole-program anlysis controller, or "plan", the same way the
    // compiler phases are structured
    finalClasses(app, units);
  }
  // where
    def buildCodeMap(units: Array[scalac_CompilationUnit]): Unit = {
      //val map = new SymbolMap();

      def mapTree(t: Tree): unit = {
	t match {
	  case Tree$PackageDef(pack, impl) => {
	    mapTree(pack);
	    mapTree(impl);
	  }

	  case Tree$ClassDef(mods, name, tparams, vparams, tpe, impl) => {
	    SymbolMap.addSymbol(t.symbol(), t);
	    mapTree(impl);
	  }

	  case Tree$Template(parents, body) => {
	    body.foreach( (b) => mapTree(b) );
	  }

	  case _ => ;
	};
      }

      units.foreach( (u: scalac_CompilationUnit) =>
	u.body.foreach( (b) => mapTree(b) ));
    }

    def dumpApplication: Unit = {
      val outputFile = new java.io.File(global.args.XappFile.value);
      val writer: java.io.Writer = new java.io.FileWriter(outputFile);

      app.foreach( (s: Symbol) => {
	writer.write(s.name.toString() + "\n");
      });
      // if we don't flush, nothing gets written?!!
      writer.flush();
    }

  def visitClass(clsSym: Symbol): unit = {
    if (clsSym.isExternal())
      visitExternalClass(clsSym);
    else
      visitInternalClass(clsSym);
  }

  def visitExternalClass(clsSym: Symbol): unit = {
    assert(clsSym.isClass(), "Not a class symbol? " + clsSym);

    app += clsSym;
    worklist -= clsSym;

    // parents
    addTypesToWorklist(List.fromArray(clsSym.parents(), 0, clsSym.parents().length));

//    updateClassHierarchy(clsSym);

    // members
    clsSym.members().elements().foreach( (s: Symbol) => {
      if (s.isClass())
	visitClass(s);
      else if (s.isValue() || s.isVariable()) {
	// a field
      } else if (s.isMethod()) {
	// a method
      }
    });
  }

  /**
   * Visit a class declaration.. Walk all its methods and add to the Application set
   * all classes referenced by a method call, instantiation or assignment. We have to
   * have a source represenation of this class (it is not Java code/or external class).
   *
   * What we include for each class:
   *   - superclasses
   *   - field types (actually their classes)
   *   - return and argument types for methods (actually their classes)
   *   - receiver type for method calls (actually its class)
   *   - receiver and field type for foreign field accesses (same obs as before)
   */
  def visitInternalClass(clsSym: Symbol): unit = {
    var cls: Tree = null;

    app += clsSym;
    worklist -= clsSym;

    SymbolMap.getDefinition(clsSym) match {
      case Some(c) => cls = c;
      case None => {
	Console.println("Could not find symbol " + clsSym + " in code map");
	return;
      }
    }

    val parents = clsSym.parents();

//    updateClassHierarchy(clsSym);

    // superclasses
    addTypesToWorklist(List.fromArray[Type](parents, 0, parents.length));

    addReferences(cls);
  }
  // where
/*    def updateClassHierarchy(cls: Symbol): Unit = {
      val parents = cls.parents();
      var i = 1;
      val subclass: Node = chGraph.getNode(cls);

      def typeToSymbol(t: Type): Symbol = {
	t match {
	  case Type$TypeRef(_, s, _) => s;
	  case _ => { global.error("Cannot handle base type " + t + " for " + cls ); null }
	}
      }


      if (parents.length > 0) {
	chGraph.addBaseClass(subclass, chGraph.getNode(typeToSymbol(parents(0))));
        while (i < parents.length) {
	  chGraph.addMixin(subclass, chGraph.getNode(typeToSymbol(parents(i))));
	  i = i + 1;
	}
      }
    }

*/
    /**
     * Walk the tree and add to the worklist any class symbol that is
     * referenced from this tree.
     */
    def addReferences(t: Tree): unit = {
      t match {
	case Tree$ClassDef(mods, name, tparams, vparams, tpe, impl) => {
	  tparams.foreach( (tpar) => addReferences(tpar) );
	  vparams.foreach( (varray) => varray.foreach( (v) => addReferences(v) ) );

	  addReferences(tpe);
	  addReferences(impl);
	}

	case Tree$PackageDef(packaged, impl) => {
	  addReferences(packaged);
	  addReferences(impl);
	}

	case Tree$ModuleDef(mods, name, tpe, impl) => {
	  addReferences(tpe);
	  addReferences(impl);
	}

	case Tree$ValDef(mods, name, tpe, rhs) => {
	  assert(t.hasSymbol());

	  val s = t.symbol().getType();
	  addTypesToWorklist(s :: Nil);

	  addReferences(tpe);
	  addReferences(rhs);
	}

	case Tree$PatDef(mods, pat, rhs) => {
	  assert(t.hasSymbol());

	  val s = t.symbol().getType();
	  addTypesToWorklist(s :: Nil);

	  addReferences(pat);
	  addReferences(rhs);
	}

	case Tree$DefDef(mods, name, tparams, vparams, tpe, impl) => {
	  assert(t.hasSymbol());

	  val s = t.symbol().getType();
	  addTypesToWorklist(s :: Nil);

	  tparams.foreach( (tpar) => addReferences(tpar) );
	  vparams.foreach( (varray) => varray.foreach( (v) => addReferences(v) ) );

	  addReferences(tpe);
	  addReferences(impl);
	}

	case Tree$AbsTypeDef(mods, name, rhs, lobound) => {
	  addReferences(rhs);
	  addReferences(lobound);
	  if (t.hasSymbol())
	    addTypesToWorklist(t.symbol().getType() :: Nil);
	}

	case Tree$AliasTypeDef(mods, name, tparams, rhs) => {
	  tparams.foreach( (tt) => addReferences(tt) );
	  addReferences(rhs);
	}

	case Tree$Import(expr, selectors) => ;

	case Tree$CaseDef(pat, guard, body) => {
	  addReferences(pat);
	  addReferences(guard);
	  addReferences(body);
	}

	case Tree$Template(parents, body) => {
	  body.foreach( (b) => addReferences(b) );
	}

	case Tree$LabelDef(name, params, rhs) => {
	  params.foreach( (p) => addReferences(p) );
	  addReferences(rhs);
	}

	case Tree$Block(stats, expr) => {
	  stats.foreach( (stat) => addReferences(stat) );
	  addReferences(expr);
	}

	case Tree$Sequence(trees) => {
	  trees.foreach( (t) => addReferences(t) );
	}

	case Tree$Function(vparams, body) => {
	  vparams.foreach( (vpar) => addReferences(vpar) );
	  addReferences(body);
	}

	case Tree$Assign(lhs, rhs) => {
	  addReferences(lhs);
	  addReferences(rhs);
	}

	case Tree$If(cond, thenp, elsep) => {
	  addReferences(cond);
	  addReferences(thenp);
	  addReferences(elsep);
	}

	case Tree$Switch(test, tags, bodies, otherwise) => {
	  addReferences(test);
	  bodies.foreach( (b) => addReferences(b) );
	  addReferences(otherwise);
	}

	case Tree$Return(expr) => addReferences(expr);

	case Tree$Throw(expr) => addReferences(expr);

	// the only place where new references can be added
	case Tree$New(template) => {
	  addReferences(template);

//	  Console.println("<new> template with type! " + template.getType());
	  addTypesToWorklist(template.getType() :: Nil);
	}

	case Tree$Typed(expr, tpe) => {
	  addReferences(expr);
	  addReferences(tpe);
	}

	case Tree$TypeApply(fun, args) => {
	  addReferences(fun);
	  args.foreach( (a) => addReferences(a) );
	}

	case Tree$Apply(fun, args) => {
	  addReferences(fun);
	  addTypesToWorklist(fun.getType() :: Nil);

	  args.foreach( (a) => addReferences(a) );
	}

	case Tree$Super(qualifier, mixin) => ;

	case Tree$This(qualifier) => ;

	case Tree$Select(qualifier, selector) => {
	  addTypesToWorklist(qualifier.getType() :: Nil);

	  addReferences(qualifier);
	}

	case Tree$Ident(name) => ;

	case Tree$Literal(value) => ;

	case Tree$TypeTerm() => {
	  addTypesToWorklist(t.getType() :: Nil);
	}

	case Tree$SingletonType(ref) => {
	  addReferences(ref);
	}

	case Tree$SelectFromType(qualifier, selector) => addReferences(qualifier);

	case Tree$FunType(argTypes, restpe) => {
	  argTypes.foreach( (a) => addReferences(a) );
	  addReferences(restpe);
	}

	case Tree$CompoundType(parents, refinements) => {
	  parents.foreach( (p) => addReferences(p) );
	  refinements.foreach( (r) => addReferences(r) );
	}

	case Tree$AppliedType(tpe, args) => {
	  addReferences(tpe);
	  args.foreach( (a) => addReferences(a) );
	}

	case Tree$Try(block, catcher, finalizer) => {
	  addReferences(block);
	  addReferences(catcher);
	  addReferences(finalizer);
	}

	case _ => ;
      }
    }

    def addReferencesList(trees: List[Tree]) = {
      trees.foreach( (t) => addReferences(t));
    }

    def typeToSymbol(t: Type): Symbol = {
      t match {
	case Type$TypeRef(_, s, _) => if (s.isAbstractType()) typeToSymbol(t.bound()) else s;
	case Type$SingleType(pre, sym) => typeToSymbol(sym.getType());
	case Type$ThisType(sym) => sym;
	case Type$PolyType(tparams, result) => typeToSymbol(result); // attention!
	case Type$ConstantType(base, value) => typeToSymbol(base);
	case Type$MethodType(vparams, result) => typeToSymbol(result);
	case Type$CompoundType(_, _) => t.symbol();
	case _ => { global.error("Cannot handle base type " + t); null }
      }
    }

  def addTypesToWorklist(types: List[Type]): unit = {
    types.foreach( (t) => addSymbolToWorklist(typeToSymbol(t)) );
  }

/*
   // where
      def addTypesToWorklist2(types: List[Type]): unit = {
	// types have to be TypeRef really, actually class types so that we
	// can identify the corresponding class and add it to our processing list
	types.foreach( (t: Type) => {
	  t.match {
	    case Type$TypeRef(pre, s, args) => {
	      addTypesToWorklist(List.fromArray[Type](args, 0, args.length));
	      addSymbolToWorklist(s);
	    }

	    case Type$CompoundType(parts, members) => Console.println("CompoundType " + t);

	    case Type$ThisType(sym) => Console.println("ThisType " + t);

	    case Type$SingleType(pre, sym) => Console.println("SingleType " + t);

	    case Type$ConstantType(base, value) => Console.println("ConstantType " + t);

	    case Type$MethodType(vparams, result) => {
	      val xs = List.fromArray[Symbol](vparams, 0, vparams.length);
	      addTypesToWorklist(xs map { (s) => s.getType() });
	      addTypesToWorklist(result :: Nil);
	    }

	    case Type$PolyType(tparams, result) => {
	      val xs = List.fromArray[Symbol](tparams, 0, tparams.length);
	      addTypesToWorklist(xs map { (s) => s.getType() });
	      addTypesToWorklist(result :: Nil);
	    }

	    case Type$OverloadedType(alts, alttypes) => Console.println("OverloadedType " + t);

	    case Type$LazyType() => Console.println("LazyType " + t);

	    case Type$TypeVar(origin, constr) => Console.println("TypeVar " + t);

	    case Type$UnboxedType(tag) => Console.println("UnboxedType " + t);

	    case Type$UnboxedArrayType(elempt) => Console.println("UnboxedArrayType " + t);

	    case _ => Console.println("[worklist] Could not handle type " + t.toString());
	  }
	});
      }
*/
      /** the specified symbol has to be a class symbol. It adds its corresponding
        *  Class symbol to the worklist if the necessary conditions are met          */
      def addSymbolToWorklist(sym: Symbol): Unit = {
	if (sym != null && sym.isClass())
	  if (!app.contains(sym) && !worklist.contains(sym)) {
	    global.log("Adding type " + sym + " to worklist");
	    worklist += sym;
	  }
      }
}

/**
 * A map from Symbol values to AClass values. It is used by the ApplicationBuilder
 * in order to resolve class name symbols (to find their definition)
 */
object SymbolMap {
  private var map: Map[Symbol, Tree] = new HashMap[Symbol, Tree]();

  def addSymbol(s: Symbol, definition: Tree): unit =
    map.update(s, definition);

  /** Return the atree for the class definition of this symbol, if
    * there is one, or None otherwise                              */
  def getDefinition(s: Symbol): Option[Tree] = {
    map.get(s);
  }

  def getDefinition1(s: Symbol): Tree = {
    getDefinition(s) match {
      case Some(t) => t;
      case _ => null;
    }
  }

  def elements = map.elements;

  override def toString(): String = {
    var str: String = "";

    elements.foreach( (tuple: Tuple2[Symbol, Tree]) => {
      str = str.concat(SymbolPrinter.fullName(tuple._1));
      str = str.concat(" -> ");
      //str = str.concat(tuple._2.toString());
      str = str.concat("\n");
    });
    str;
  }
}

} // package scala.tools.scalac.globalanalysis

