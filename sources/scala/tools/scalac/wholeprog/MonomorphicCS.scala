/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2004, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

/*
**       The Whole Program analysis phase.
**
**       Identify monomorphic call sites in classes (where the receiver
**       can always be statically determined).
**
** [iuli]   2.05.2004                                                   */

import scalac.{Global => scalac_Global}
import scalac.{Unit => scalac_Unit}
import scalac.symtab._;
import scalac.util._;
import scala.collection.mutable._;
import scalac.ast._;
import scala.tools.scalac.wholeprog.graph.{Node => GNode};
import scala.tools.scalac.wholeprog.graph._;

package scala.tools.scalac.wholeprog {

class MonomorphicCallSites(globall: scalac_Global, application: Set[Symbol]) {
  type CallGraph = Graph[Symbol, MethodNode, CallEdge];
  type CallGraphNode = GNode[Symbol, MethodNode];
  type CallGraphEdge = Edge[Symbol, CallEdge];

  private val global = globall;
  val hierarchy = new Graph[Symbol, Symbol, String];
  val callGraph = new CallGraph;

  var instantiatedClasses = new HashSet[Symbol];

  var inlinable: List[Tuple3[CallGraphNode, CallGraphNode, CallSite]] = Nil;


  /** create the class inheritance hierarchy */
  def buildClassHierarchy: Unit = {
    application.foreach( (sym) => {
      hierarchy.addNode(new GNode(sym, sym));
    });

    application.foreach( (sym) => {
      val parents = sym.parents();

      // the first "parent" is always the superclass? seems to be like that
      val superclass = if (parents.length > 0) parents(0) else null;

      parents.foreach( (p) => {
	val e = new Edge[Symbol, String](sym, typeToSymbol(p));

	e.info = if (p == superclass) "extends" else  "with";
	hierarchy.addEdge(e);
      });
    });

    if (!global.args.XdotFile.value.equals("$")) {
      val file: java.io.Writer = new java.io.FileWriter("ch.dot");
      file.write(hierarchy.toDotString);
      file.flush();
    }
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
      case _ => { global.error("* Cannot handle base type " + t); null }
    }
  }

  /** Identify and print monomoprhic callsites */
  def monomorphicCallsites: Unit = {
    val cg = callGraph;
    var nr: int = 0;
    var views: Int = 0;
    var closures: Int = 0;

    def logStatistics(e: CallGraphEdge): Unit = {
      if (SymbolPrinter.fullName(cg.getNode(e.to).info.classz).startsWith("scala.Function"))
	closures = closures + 1;
      if (cg.getNode(e.to).info.method.name.toString().equals("view"))
	views = views + 1;
    }

    def inlineCallSite(e: CallGraphEdge): Unit = {
      val caller = cg.getNode(e.from);
      val callee = cg.getNode(e.to);

      if (canInline(caller, callee))
	inlinable = new Tuple3(caller, callee, e.info.site) :: inlinable;
      else
	Logger.log("[monomorphiccs] skipped monomorphic call site " +
		   SymbolPrinter.fullName(caller.info.method) + " -> " +
		   SymbolPrinter.fullName(callee.info.method));
    }

    def canInline(caller: CallGraphNode, callee: CallGraphNode): boolean =
      !callee.info.method.isInitializer() &&
      caller.info.code != null &&
      callee.info.code != null &&
      !callee.info.method.isDeferred() &&
      application.contains(caller.info.classz) &&
      application.contains(callee.info.classz);

    Logger.setFile("inlining.log");

    Console.println("[start build callgraph]");
    buildCallGraph;
    Console.println("[end build callgraph]");

    if (global.args.Xrta.value) {
      // perform additional pruning
      Console.println("[start RTA]");
      rapidTypeAnalysis(instantiatedClasses);
      Console.println("[end RTA]");
    }

    if (!global.args.XdotFile.value.equals("$"))
      dumpCallGraph;

    Console.println("[start Monomorphic call site identification]");
    cg.nodes.foreach( (id, n) => {
      n.info.callSites.foreach( (site) => {
	val mcs = cg.getOutEdges(id).filter( e => e.info.site == site );

	if (mcs.length == 1) {
	  inlineCallSite(mcs.head);
// 	  Console.println("Monomorphic call-site: " +
// 			  SymbolPrinter.fullName(mcs.head.from) + " " +
// 			  SymbolPrinter.fullName(mcs.head.to));
	  logStatistics(mcs.head);
	  nr = nr + 1;
	}
      });
    });

    Console.println("[end Monomorphic call site identification]");

    Console.println("We identified " + nr + " monomorphic call-sites. ("
		    + inlinable.length + " inlinable).");
    Console.println("[closures = " + closures + ", views = " + views + "]");

    if (global.args.Xinline.value) {
      Console.println("[start inlining]");

//      inlinable.foreach( (t) => Console.println("inline callsite " + t._3.t));

      doInline(inlinable);
      Console.println("[end inlining]");
    }
  }

  /** Perform a (buggy) form of rapid type analysis, as described in Sundaresan 99
      The idea is that the call graph can be signifficanly pruned if all edges that go
      into a method of a class that was never instantiated in the program are removed.

      The assumption is that all "new" statements are known, and there is no other way to
      instantiate classes. While the first one may be true for whole-program analysis
      (and the -separate:no flag for scalac), the second might not hold: object classes
      (singletons) are created "magically", and no "new" statement is parsed.
  */
  def rapidTypeAnalysis(instances: Set[Symbol]): Unit = {

    /** instantiated classes include singleton classes */
    def isInstatiated(s: Symbol): boolean =
      instances.contains(s) ||
      s.getType().isSubType(global.definitions.ANYVAL_TYPE()) ||
      s.isModuleClass();

    Console.println("Printing instantiated classes");
    Logger.log("[RTA] Instantiated classes: ");
    instantiatedClasses.foreach( s => Logger.log("[RTA] " + SymbolPrinter.fullName(s)));

//     callGraph.visitDFS( (n: CallGraphNode) => {
//       if (n.info.classz != null &&
// 	  !isInstatiated(n.info.classz)) {
// 	callGraph.removeEdges(callGraph.getInEdges(n.id));
// 	Logger.log("[RTA] Removed inedges for " + SymbolPrinter.fullName(n.id));
//       }
//     });

    Console.println("[Visiting call graph]");
    callGraph.nodes.foreach( (id, n) => {
      n.info.callSites.foreach( (site) => {
	val targets = callGraph.getOutEdges(id).filter( e => e.info.site == site );

	if (targets.length > 1) {
	  // test for instantiation
	  targets.foreach( (t) => if ( !isInstatiated(callGraph.getNode(t.to).info.classz) ) {
	    callGraph.removeEdge(t);
	    Logger.log("[RTA] Removed edge " +
		       SymbolPrinter.fullName(t.from) + " -> " +
		       SymbolPrinter.fullName(t.to));

	  });
	}
      });
    });
  }

  /** perform inlines */
  def doInline(sites: List[Tuple3[CallGraphNode, CallGraphNode, CallSite]]): Unit = {
    val transformer: Transformer = new InlineMethods(sites, global);

    global.units.foreach( (u) => {
      u.body = transformer.transform(u.body);
    });

    Console.println("We inlined " + transformer.asInstanceOf[InlineMethods].inlines + " callsites");
    Logger.flush;
  }

  def buildCallGraph: Unit = {
    createNodes(callGraph);
    createEdges(callGraph);

    // print call graph size
    var nodes = 0;
    var edges = 0;
    var callsites = 0;

    callGraph.nodes.foreach( (id, n) => {
      nodes = nodes + 1;
      callsites = callsites + n.info.callSites.length;
      edges = edges + callGraph.getOutEdges(id).length;
    });

    Console.println("Call graph: " + nodes + " nodes, " +
		    edges + " edges, [" /* + callGraph.edges.length */ + "]" +
		    callsites + " callsites.");
  }

  def dumpCallGraph: Unit = {
    val file: java.io.Writer = new java.io.FileWriter("callGraph.dot");
    file.write(callGraph.toDotString);
    file.flush();
  }

  def createNodes(cg: CallGraph): Unit = {
    val trav: Traverser = new MethodNodeCreator(cg);

    global.units.foreach( (u) => trav.traverse(u.body) );
  }

  def createEdges(cg: CallGraph): Unit = {
    val trav: Traverser = new CallGraphEdgeTraverser(cg);

    global.units.foreach( (u) => trav.traverse(u.body) );
  }

  /** Walk the nodes in the AST tree and creates nodes in the callgraph
      corresponding to each method  */
  class MethodNodeCreator(cg: CallGraph) extends Traverser {

    override def traverse(tree: Tree): Unit = {
      tree match {
	case Tree$DefDef(_, name, _, _, _, _) => {
	  val methSym = tree.symbol();

	  cg.addNode(new CallGraphNode(methSym, new MethodNode(methSym, methSym.enclClass(), tree)));
	  Logger.log("[callgraph] Created node " + SymbolPrinter.fullName(methSym));
	}

	case _ => ;
      }

      super.traverse(tree);
    }
  }


  /** Walk all source code and create the call graph edges. */
  class CallGraphEdgeTraverser(cg: CallGraph) extends Traverser {
    var enclMethod: Symbol = null;

    override def traverse(tree: Tree): Unit = {
      var oldMethod: Symbol = enclMethod;

      tree match {
	case Tree$DefDef(_, name, _, _, _, _) => {
	  oldMethod  = enclMethod;
	  enclMethod = tree.symbol();

	  super.traverse(tree);
	}

	case Tree$Create(qualifier, targs) => {
//	  Console.println("Create: " + tree.symbol());
	  assert(tree.symbol().isClass());
	  instantiatedClasses += tree.symbol();

	  traverse(qualifier);
	}

	case Tree$Apply(fun, args) => {

	  if (enclMethod != null) {

	    var targetMeth = fun.symbol();
	    var callsiteTree = tree;

	    if (targetMeth == null)
	      fun match {
		case Tree$TypeApply(innerFun, args) => {
		  targetMeth = innerFun.symbol();
		  callsiteTree = innerFun;
		  //Console.println("Hopla, a type apply for " + innerFun);
		}
		case _ => ;
	      }

	    if (targetMeth == null)
	      Console.println("Null symbol for method " + tree);

	    //assert(targetMeth != null, "Null target method for " + tree);
	    if (targetMeth != null)
	      createEdges(targetMeth, callsiteTree);
//	    else
//	      Console.println("Null symbol: " + tree);
// 	    fun match {
// 	      case Tree$Select(qualifier, selector) => {
// 		val target = typeToSymbol(qualifier.getType());

// 		if (target != null)
// 		  createEdges(target, qualifier, selector, tree);
// 	      }
// 	      case _ => ;
// 	    } /* else
// 	      Console.println("No f***ing enclosing method - " + fun); */
	  }

	  traverse(fun);
	  args.foreach( a => traverse(a));
	};

	case _ => super.traverse(tree);
      }

      enclMethod = oldMethod;
    }

    /** Add edges between the callsite and all possible targets. Possible
      * targets are methods in the target class (or "nearest" superclass)
      * or subclasses that override the specific method   */
    def createEdges(targetMeth: Symbol, t: Tree): Unit = {
      val site: CallSite = new CallSite(t);

      def createEdgesForSubclasses(cls: Symbol): Unit = {
	// walk all subclasses
	hierarchy.getInEdges(cls).foreach( (e) => {
	  val c = hierarchy.nodes(e.from).info;
	  val it = c.members().iterator(true);

	  while (it.hasNext()) {
	    val m = it.next();
	    if (m.overrides(targetMeth)) {
	      if (cg.getNode(m) == null)
		cg.addNode(new CallGraphNode(m, new MethodNode(m, c, null)));

	      cg.addEdge(enclMethod, m).info = new CallEdge(t, site);
	    }
	  }

//	  else Console.println("Found a subclass that is not a subtype: " +
//			       SymbolPrinter.fullName(c) + "[" + c.getType() + "] not <: " +
//			       targetCls + "[" + refType + "]");

	  createEdgesForSubclasses(c);
	});

      }

      // add callsite to node
      if (cg.getNode(enclMethod) == null)
	cg.addNode(new CallGraphNode(enclMethod, new MethodNode(enclMethod, enclMethod.enclClass(), null)));

      cg.getNode(enclMethod).info.callSites = site :: cg.getNode(enclMethod).info.callSites;

      if (targetMeth != Symbol.NONE) {
	if (cg.getNode(targetMeth) == null)
	  cg.addNode(new CallGraphNode(targetMeth, new MethodNode(targetMeth, targetMeth.enclClass(), null)));

	cg.addEdge(enclMethod, targetMeth).info = new CallEdge(t, site);
      }

      createEdgesForSubclasses(targetMeth.enclClass());
    }
  }

//  def makeNodeId(meth: Symbol): String = SymbolPrinter.fullName(meth);
}

/** Class that maintains information about nodes in the callgraph */
case class MethodNode(method: Symbol, classz: Symbol, code: Tree) {
  var callSites: List[CallSite] = Nil;

  override def toString(): String = SymbolPrinter.fullName(method);
}

case class CallSite(t: Tree) {
}

class CallEdge(code: Tree, s: CallSite) {
  val site = s;
  override def toString() = "\"" + s.hashCode() + "\"";
}


object SymbolPrinter {
  def fullName(s: Symbol): String = {

    def fullName2(post: String, s: Symbol): String =
      if (s.owner().isRoot())
	s.name.toString() + "." + post
      else
	fullName2(s.name.toString() + "." + post, s.owner());

    fullName2(s.name.toString(), s.owner())
  }

}


} // package wholeprog
