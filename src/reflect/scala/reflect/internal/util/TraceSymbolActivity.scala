package scala.reflect.internal
package util

import scala.collection.{ mutable, immutable }
import language.postfixOps

trait TraceSymbolActivity {
  val global: SymbolTable
  import global._

  if (traceSymbolActivity && global.isCompilerUniverse)
    scala.sys addShutdownHook showAllSymbols()

  private type Set[T] = scala.collection.immutable.Set[T]
  private val Set = scala.collection.immutable.Set

  val allSymbols  = mutable.Map[Int, Symbol]()
  val allChildren = mutable.Map[Int, List[Int]]() withDefaultValue Nil
  val prevOwners  = mutable.Map[Int, List[(Int, Phase)]]() withDefaultValue Nil
  val symsCaused  = mutable.Map[Int, Int]() withDefaultValue 0
  val allTrees    = mutable.Set[Tree]()

  def recordSymbolsInTree(tree: Tree) {
    allTrees += tree
  }

  def recordNewSymbol(sym: Symbol) {
    if (sym.id > 1) {
      allSymbols(sym.id) = sym
      allChildren(sym.owner.id) ::= sym.id
    }
  }
  def recordNewSymbolOwner(sym: Symbol, newOwner: Symbol) {
    val sid = sym.id
    val oid = sym.owner.id
    val nid = newOwner.id

    prevOwners(sid) ::= (oid -> phase)
    allChildren(oid) = allChildren(oid) filterNot (_ == sid)
    allChildren(nid) ::= sid
  }

  /** TODO.
   */
  private def reachableDirectlyFromSymbol(sym: Symbol): List[Symbol] = (
       List(sym.owner, sym.alias, sym.thisSym)
    ++ sym.children
    ++ sym.info.parents.map(_.typeSymbol)
    ++ sym.typeParams
    ++ sym.paramss.flatten
  )
  private def reachable[T](inputs: Traversable[T], mkSymbol: T => Symbol): Set[Symbol] = {
    def loop(seen: Set[Symbol], remaining: List[Symbol]): Set[Symbol] = {
      remaining match {
        case Nil          => seen
        case head :: rest =>
          if ((head eq null) || (head eq NoSymbol) || seen(head)) loop(seen, rest)
          else loop(seen + head, rest ++ reachableDirectlyFromSymbol(head).filterNot(seen))
      }
    }
    loop(immutable.Set(), inputs.toList map mkSymbol filterNot (_ eq null) distinct)
  }
  private def treeList(t: Tree) = {
    val buf = mutable.ListBuffer[Tree]()
    t foreach (buf += _)
    buf.toList
  }

  private def reachableFromSymbol(root: Symbol): Set[Symbol] =
    reachable[Symbol](List(root, root.info.typeSymbol), x => x)

  private def reachableFromTree(tree: Tree): Set[Symbol] =
    reachable[Tree](treeList(tree), _.symbol)

  private def signature(id: Int) = runBeforeErasure(allSymbols(id).defString)

  private def dashes(s: Any): String = ("" + s) map (_ => '-')
  private def show(s1: Any, ss: Any*) {
    println("%-12s".format(s1) +: ss mkString " ")
  }
  private def showHeader(s1: Any, ss: Any*) {
    show(s1, ss: _*)
    show(dashes(s1), ss map dashes: _*)
  }
  private def showSym(sym: Symbol) {
    def prefix = ("  " * (sym.ownerChain.length - 1)) + sym.id
    try println("%s#%s %s".format(prefix, sym.accurateKindString, sym.name.decode))
    catch {
      case x => println(prefix + " failed: " + x)
    }
    allChildren(sym.id).sorted foreach showIdAndRemove
  }
  private def showIdAndRemove(id: Int) {
    allSymbols remove id foreach showSym
  }
  private def symbolStr(id: Int): String = {
    if (id == 1) "NoSymbol" else {
      val sym = allSymbols(id)
      sym.accurateKindString + " " + sym.name.decode
    }
  }
  private def ownerStr(id: Int): String = {
    val sym = allSymbols(id)
    sym.name.decode + "#" + sym.id
  }

  private def freq[T, U](xs: collection.Traversable[T])(fn: T => U): List[(U, Int)] = {
    val ys = xs groupBy fn mapValues (_.size)
    ys.toList sortBy (-_._2)
  }

  private def showMapFreq[T](xs: collection.Map[T, Traversable[_]])(showFn: T => String) {
    xs.mapValues(_.size).toList.sortBy(-_._2) take 100 foreach { case (k, size) =>
      show(size, showFn(k))
    }
    println("\n")
  }
  private def showFreq[T, U](xs: Traversable[T])(groupFn: T => U, showFn: U => String = (x: U) => "" + x) = {
    showMapFreq(xs.toList groupBy groupFn)(showFn)
  }
  private lazy val findErasurePhase: Phase = {
    var ph = phase
    while (ph != NoPhase && ph.name != "erasure") {
      ph = ph.prev
    }
    ph
  }
  private def runBeforeErasure[T](body: => T): T = atPhase(findErasurePhase)(body)

  def showAllSymbols() {
    if (!traceSymbolActivity) return
    allSymbols(1) = NoSymbol

    println("" + allSymbols.size + " symbols created.")
    println("")

    showHeader("descendants", "symbol")
    showFreq(allSymbols.values flatMap (_.ownerChain drop 1))(_.id, symbolStr)

    showHeader("children", "symbol")
    showMapFreq(allChildren)(symbolStr)

    if (prevOwners.nonEmpty) {
      showHeader("prev owners", "symbol")
      showMapFreq(prevOwners) { k =>
        val owners = (((allSymbols(k).owner.id, NoPhase)) :: prevOwners(k)) map {
          case (oid, NoPhase) => "-> owned by " + ownerStr(oid)
          case (oid, ph)      => "-> owned by %s (until %s)".format(ownerStr(oid), ph)
        }
        signature(k) :: owners mkString "\n                "
      }
    }

    val nameFreq = allSymbols.values.toList groupBy (_.name)
    showHeader("frequency", "%-15s".format("name"), "owners")
    showMapFreq(nameFreq) { name =>
      "%-15s %s".format(name.decode, {
        val owners = freq(nameFreq(name))(_.owner)

        "%4s owners (%s)".format(
          owners.size,
          owners.take(3).map({ case (k, v) => v + "/" + k }).mkString(", ") + ", ..."
        )
      })
    }

    allSymbols.keys.toList.sorted foreach showIdAndRemove
  }
}
