package scala
package reflect.internal
package util

import scala.collection.mutable

trait TraceSymbolActivity {
  val global: SymbolTable
  import global._

  private[this] var enabled = traceSymbolActivity
  if (enabled && global.isCompilerUniverse)
    scala.sys addShutdownHook showAllSymbols()

  val allSymbols  = mutable.Map[Int, Symbol]()
  val allChildren = mutable.Map[Int, List[Int]]() withDefaultValue Nil
  val prevOwners  = mutable.Map[Int, List[(Int, Phase)]]() withDefaultValue Nil
  val allTrees    = mutable.Set[Tree]()

  def recordSymbolsInTree(tree: Tree) {
    if (enabled)
      allTrees += tree
  }

  def recordNewSymbol(sym: Symbol) {
    if (enabled && sym.id > 1) {
      allSymbols(sym.id) = sym
      allChildren(sym.owner.id) ::= sym.id
    }
  }
  def recordNewSymbolOwner(sym: Symbol, newOwner: Symbol) {
    if (enabled) {
      val sid = sym.id
      val oid = sym.owner.id
      val nid = newOwner.id

      prevOwners(sid) ::= (oid -> phase)
      allChildren(oid) = allChildren(oid) filterNot (_ == sid)
      allChildren(nid) ::= sid
    }
  }

  private lazy val erasurePhase = findPhaseWithName("erasure")
  private def signature(id: Int) = enteringPhase(erasurePhase)(allSymbols(id).defString)

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
      case x: Throwable => println(prefix + " failed: " + x)
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

  private def freq[T, U](xs: scala.collection.Traversable[T])(fn: T => U): List[(U, Int)] = {
    val ys = xs groupBy fn mapValues (_.size)
    ys.toList sortBy (-_._2)
  }

  private def showMapFreq[T](xs: scala.collection.Map[T, Traversable[_]])(showFn: T => String) {
    xs.mapValues(_.size).toList.sortBy(-_._2) take 100 foreach { case (k, size) =>
      show(size, showFn(k))
    }
    println("\n")
  }
  private def showFreq[T, U](xs: Traversable[T])(groupFn: T => U, showFn: U => String) = {
    showMapFreq(xs.toList groupBy groupFn)(showFn)
  }

  def showAllSymbols() {
    if (!enabled) return
    enabled = false
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
