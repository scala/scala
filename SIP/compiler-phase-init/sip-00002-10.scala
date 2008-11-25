/* Package information */
package depgraph


/* Simple import of data structures and IO */
import scala.collection.mutable.{HashSet,HashMap}
import java.io.{BufferedWriter,FileWriter}



/* Class made to model the internal compiler phases
 *
 * All internal phases inherits from SubComponent
 */
abstract class SubComponent {
  val phaseName: String
  val runsAfter: List[String]
  val runsRightAfter: Option[String]
  val internal: Boolean = true
}

/*
 *
 */
class Global extends Plugins with PhaseAssembly {

  /* Simple option value to hold the compiler phase chain */
  private var phasesCache : Option[List[SubComponent]] = None

  /* The set of phase objects that is the basis for the compiler phase chain */
  protected val phasesSet : HashSet[SubComponent] = new HashSet[SubComponent]

  /* All the internal phase objects
   *
   */
  object parser extends {
    val phaseName = "nsc::parser"
    val runsAfter = List[String]()
    val runsRightAfter = None
  } with SubComponent

  object typer extends {
    val phaseName = "nsc::typer"
    val runsAfter = List[String]("nsc::parser")
    val runsRightAfter = None
  } with SubComponent

  object pickler extends {
    val phaseName = "nsc::pickler"
    val runsAfter = List[String]("nsc::typer")
    val runsRightAfter = None
  } with SubComponent

  object liftcode extends {
    val phaseName = "nsc::liftcode"
    val runsAfter = List[String]("nsc::pickler")
    val runsRightAfter = None
  } with SubComponent

  object tailcalls extends {
    val phaseName = "nsc::tailcalls"
    val runsAfter = List[String]("nsc::pickler","nsc::liftcode")
    val runsRightAfter = None
  } with SubComponent

  object erasure extends {
    val phaseName = "nsc::erasure"
    val runsAfter = List[String]()
    val runsRightAfter = Some("nsc::tailcalls")
  } with SubComponent

  object cleanup extends {
    val phaseName = "nsc::cleanup"
    val runsAfter = List[String]("nsc::erasure")
    val runsRightAfter = None
  } with SubComponent

  object jvm extends {
    val phaseName = "nsc::jvm"
    val runsAfter = List[String]("nsc::cleanup")
    val runsRightAfter = None
  } with SubComponent

  object terminal extends {
    val phaseName = "nsc::terminal"
    val runsAfter = List[String]("nsc::jvm","nsc::msil")
    val runsRightAfter = None
  } with SubComponent

  /* Helper method
   */
  private def computePhaseDescriptors: List[SubComponent] = {
    computeInternalPhases()            // Global.scala
    computePluginPhases()              // plugins/Plugins.scala
    buildCompilerFromPhasesSet()       // PhaseAssembly.scala
  }

  /* Will add the internal compiler phases to the phases set
   */
  protected def computeInternalPhases() {
    phasesSet += parser
    phasesSet += typer
    phasesSet += pickler
    phasesSet += liftcode
    phasesSet += tailcalls
    phasesSet += erasure
    phasesSet += cleanup
    phasesSet += jvm
    phasesSet += terminal
  }

  /* Getter method for the compiler phases chain
   */
  def phaseDescriptors : List[SubComponent] = {
    if (phasesCache.isEmpty) {
      phasesCache = Some(computePhaseDescriptors)
    }
    phasesCache.get
  }

}


/* Class make to model the Plug-in supplied phases
 *
 * All external phases inherits from PluginComponent
 */
abstract class PluginComponent extends SubComponent {
  override val internal = false
  val runsRightAfter: Option[String] = None
}

/* Trait made to model the behavior of the plugins
 *
 */
trait Plugins { self: Global =>

  /* Example plugin phases
   *
   */
  object plugin1 extends {
    val phaseName = "plug1::optimization1"
    val runsAfter = List[String]("nsc::typer")
  } with PluginComponent

  object plugin2 extends {
    val phaseName = "plug2::optimization1"
    val runsAfter = List[String]("nsc::liftcode")
  } with PluginComponent

  object plugin3 extends {
    val phaseName = "plug2::optimization2"
    val runsAfter = List[String]("plug2::optimization1","nsc::cleanup")
  } with PluginComponent

  /* Add plug-in supplied phase objects to the phases set
   */
  def computePluginPhases() {
    phasesSet += plugin1
    phasesSet += plugin2
    phasesSet += plugin3
  }

}


/* Trait made to seperate the constraint solving from the rest of the compiler
 *
 */
trait PhaseAssembly { self: Global =>

  /* Aux datastructure for solving the constraint system
   * Simple edge with to and from refs
   */
  class Edge(f: Node, t: Node, h: Boolean) {
    var frm = f
    var to = t
    var hard = h
  }

  /* Aux datastructure for solving the constraint system
   * Simple node with name and object ref for the phase object,
   * also sets of in and out going dependencies
   */
  class Node(phs:SubComponent, name:String) {
    var phasename: String = name
    var phaseobj: SubComponent = phs
    var after: HashSet[Edge] = new HashSet[Edge]()
    var deps: HashSet[Edge] = new HashSet[Edge]()
  }

  /* Aux datastructure for solving the constraint system
   * The depency graph container with helper methods for node and edge creation
   */
  class DependencyGraph {

    val nodes = new HashMap[String,Node]()
    val edges = new HashSet[Edge]()

    /* Given a phase object, get the node for this phase object. If the
     * node object does not exist, then create it.
     */
    def getNodeByPhase(phs : SubComponent) : Node = {
      var node : Node = getNodeByPhase(phs.phaseName)
      if (node.phaseobj == null) {
	node.phaseobj = phs
      }
      node
    }

    /* Given the name of a phase object, get the node for that name. If the
     * node object does not exits, then create it.
     */
    def getNodeByPhase(name : String) : Node = {
      var node : Node = null
      this.nodes.get(name) match {
	case None =>
          node = new Node(null,name)
          nodes += (name->node)
	case Some(n) =>
          node = n
      }
      node
    }

    /* Connect the frm and to nodes with an edge and make it soft.
     * Also add the edge object to the set of edges, and to the dependency
     * list of the nodes
     */
    def softConnectNodes(frm: Node, to: Node) {
      var e = new Edge(frm, to, false)
      this.edges += e

      frm.after += e
      to.deps += e
    }

    /* Connect the frm and to nodes with an edge and make it hard.
     * Also add the edge object to the set of edges, and to the dependency
     * list of the nodes
     */
    def hardConnectNodes(frm: Node, to: Node) {
      var e = new Edge(frm, to, true)
      this.edges += e

      frm.after += e
      to.deps += e
    }
  }

  /* This method will simplify the graph, by removing unneeded edges and turning the graph into
   * a tree.
   */
  private def simplifyGraphFromNode(node : Node, graph : DependencyGraph) : Unit = {
    var removal : List[Edge] = Nil
    for(edge <- node.deps) {
      if (edge.frm.after.size > 1)
	removal = edge :: removal
    }
    for(edge <- removal) {
	println("[removing edge: " + edge.frm.phasename + " => " + edge.to.phasename + "]")
	node.deps -= edge
	edge.frm.after -= edge
	graph.edges -= edge
      }

    var nodes = dependencyOrder(node.deps)
    for(nd <- nodes) {
      simplifyGraphFromNode(nd, graph)
    }
  }

  /* This is a simple method that tests for cycles in the graph. If a cycle is found, a fatal error
   * will be produced.
   */
  private def testForCycles(node : Node, names : HashSet[String]) : Unit = {

    if (names.contains( node.phasename ) ) {
      println("There is a cycle in this graph! The algorithm was able to reach the node " + node.phasename + " twice!")
      System.exit(1)
    }

    names += node.phasename

    var nodes = dependencyOrder(node.deps)
    for(nd  <- nodes) {
      testForCycles(nd, names)
    }

    names -= node.phasename
  }

  /* Given the dependency list of a node, return a list so that first come the reversed
   * external phases sorted alphabetically, followed by the internal phase
   */
  private def dependencyOrder(deps : HashSet[Edge]) : List[Node] = {
    var external = deps.filter(e => (! e.frm.phaseobj.internal))
    var internal = deps.filter(e => e.frm.phaseobj.internal)

    var extnodes = (Nil ++ external.map(e => e.frm)).sort((n1,n2) => (n1.phasename compareTo n2.phasename) < 0)
    extnodes = extnodes.reverse

    var nodes = Nil ++ internal.map(e => e.frm) ++ extnodes
    nodes = nodes.reverse
    return nodes
  }

  /* Find all edges in the given graph that are hard links. For each hard link we
   * need to check that its the only dependency. If not, then we will promote the
   * other dependencies down
   */
  private def enforceHardlinks(graph : DependencyGraph) : Unit = {
    var rerun = true
    while(rerun) {
      rerun = false
      var hardlinks = graph.edges.filter(e => e.hard)
      for(hl <- hardlinks) {
	var sanity = Nil ++ hl.to.deps.filter(e => e.hard)
	if (sanity.length == 0) {

	  println("This is not supposed to happen!")
	  System.exit(1)
          // throw new FataError()
	} else if (sanity.length > 1) {

	  println("Multiple phases want to run right after the same phase")
	  println("Phases:")
	  for (edge <- sanity) {
	    println(" - " + edge.frm.phasename)
	  }
	  System.exit(1)

	} else {

	  var promote = hl.to.deps.filter(e => (!e.hard))
	  hl.to.deps.clear
	  sanity foreach (edge => hl.to.deps += edge)
	  for (edge <- promote) {
	    rerun = true
	    println("[promote the dependency of " + edge.frm.phasename + ": "  + edge.to.phasename + " => " + hl.frm.phasename + "]")
	    edge.to = hl.frm
	    hl.frm.deps += edge
	  }
	}
      }
    }
  }

  /* Remove all nodes in the given graph, that have no phase object
   * Make sure to clean up all edges when removing the node object
   */
  private def removeDanglingNodes(graph : DependencyGraph) : Unit = {
    var dnodes = graph.nodes.values.filter(n => (n.phaseobj == null))
    for(node <- dnodes) {
      println("[dropping depend on node with no phase: " + node.phasename + "]")
      graph.nodes -= node.phasename
      for(edge <- node.deps) {
	graph.edges -= edge
	edge.frm.after -= edge
      }
    }
  }

  /* This is a helper method, that given a dependency graph will generate a graphviz dot
   * file showing its structure.
   * Plug-in supplied phases are marked as green nodes and hard links are marked as blue edges.
   */
  private def graphToDotFile(graph : DependencyGraph, filename : String) : Unit = {
    var sbuf = new StringBuffer()
    var extnodes = new HashSet[Node]()
    sbuf.append("digraph G {\n")
    for(edge <- graph.edges) {
      sbuf.append("\"" + edge.frm.phasename + "\"->\"" + edge.to.phasename + "\"")
      if (! edge.frm.phaseobj.internal) {
       	extnodes += edge.frm
      }
      if (edge.hard) {
	sbuf.append(" [color=\"#0000ff\"]\n")
      } else {
	sbuf.append(" [color=\"#000000\"]\n")
      }
    }
    for(node <- extnodes) {
      sbuf.append("\"" + node.phasename + "\" [color=\"#00ff00\"]\n")
    }
    sbuf.append("}\n")
    var out = new BufferedWriter(new FileWriter(filename))
    out.write(sbuf.toString)
    out.flush()
    out.close()
  }


  /* Given the phases set, will build a dependency graph from the phases set
   * Using the aux. method of the DependencyGraph to create nodes and egdes
   */
  private def phasesSetToDepGraph(phsSet : HashSet[SubComponent]) : DependencyGraph = {
    val graph = new DependencyGraph()

    for(phs <- phsSet) {

      var fromnode = graph.getNodeByPhase(phs)

      phs.runsRightAfter match {
	case None =>
	  for(phsname <- phs.runsAfter) {
	    if (! (phsname equals "terminal")) {
	      var tonode = graph.getNodeByPhase(phsname)
	      graph.softConnectNodes(fromnode, tonode)
	    } else {
	      println("[depends on terminal not allowed, dropping depend: " + fromnode.phasename + " => "+ phsname +"]")
	    }
	  }
	case Some(phsname) =>
	  if (! (phsname equals "terminal")) {
	    var tonode = graph.getNodeByPhase(phsname)
            graph.hardConnectNodes(fromnode, tonode)
	  } else {
	    println("[depends on terminal not allowed, dropping depend: " + fromnode.phasename + " => "+ phsname +"]")
	  }
      }

    }

    return graph
  }


  /* Simple transformation function, that given a dependency graph transforms it into a dependency tree
   * Will return the root of the tree
   */
  private def depGraphToDepTree(graph : DependencyGraph) : Node = {

    graphToDotFile(graph, "depgraph1.dot")

    // Remove nodes without phaseobj
    removeDanglingNodes(graph)

    graphToDotFile(graph, "depgraph2.dot")

    // Enforce hardlinks / runsRightAfter and promote nodes down the tree
    enforceHardlinks(graph)

    graphToDotFile(graph, "depgraph3.dot")

    var root = graph.getNodeByPhase("nsc::parser")

    // Test for cycles in graph, if found will generate fatal error
    testForCycles(root, new HashSet[String]())

    // Simplify graph by removing edges starting from the root
    simplifyGraphFromNode(root, graph)

    graphToDotFile(graph, "depgraph4.dot")

    root
  }


  /* Given a node and a list of phases, it will traverse the dependencies of the node object and
   * call itself recursively
   *
   */
  private def depTree2CompilerPhaseList(node : Node, pchain : List[SubComponent]) : List[SubComponent] = {
    var chain : List[SubComponent] = pchain
    chain = chain ::: List(node.phaseobj)
    if (node.deps.size == 0)
      return chain

    else if (node.deps.size == 1) {
      for(edge <- node.deps)
	chain = depTree2CompilerPhaseList(edge.frm, chain)
      return chain

    } else {

      var nodes = dependencyOrder(node.deps)
      for(nd <- nodes) {
	chain = depTree2CompilerPhaseList(nd, chain)
      }
      return chain
    }
  }


  /* Method called from computePhaseDescriptors in class Global
   * This method will call three aux. methods to convert the phases set into a dependency graph
   * Convert the dependency graph into a dependency tree and return the root of the tree
   * Assemble the compiler phase chain from the root of the dependency tree
   */
  def buildCompilerFromPhasesSet() : List[SubComponent] = {

    // Add all phases in the set to the graph
    val graph = phasesSetToDepGraph(phasesSet)

    val root = depGraphToDepTree(graph)

    return depTree2CompilerPhaseList(root, Nil)
  }

}


/**
 * Test object that will create a new object from the Global class
 * and call the method phaseDescriptors to get the list of phase objects
 * and print the phase names to stdout
 *
 */
object DepGraphTest extends Application {

  val global = new Global()

  var compilerchain = global.phaseDescriptors

  for(phase <- compilerchain) {
    println(" - " + phase.phaseName)
  }

}

