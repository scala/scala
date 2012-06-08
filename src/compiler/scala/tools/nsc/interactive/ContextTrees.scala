/* NSC -- new Scala compiler
 * Copyright 2009-2011 Scala Solutions and LAMP/EPFL
 * @author Martin Odersky
 */
package scala.tools.nsc
package interactive

import collection.mutable.ArrayBuffer
import scala.reflect.internal.util.Position

trait ContextTrees { self: Global =>

  type Context = analyzer.Context
  lazy val NoContext = analyzer.NoContext
  type Contexts = ArrayBuffer[ContextTree]

  /** A context tree contains contexts that are indexed by positions.
   *  It satisfies the following properties:
   *  1. All context come from compiling the same unit.
   *  2. Child contexts have parent contexts in their outer chain.
   *  3. The `pos` field of a context is the same as `context.tree.pos`, unless that
   *     position is transparent. In that case, `pos` equals the position of
   *     one of the solid descendants of `context.tree`.
   *  4. Children of a context have non-overlapping increasing positions.
   *  5. No context in the tree has a transparent position.
   */
  class ContextTree(val pos: Position, val context: Context, val children: ArrayBuffer[ContextTree]) {
    def this(pos: Position, context: Context) = this(pos, context, new ArrayBuffer[ContextTree])
    override def toString = "ContextTree("+pos+", "+children+")"
  }

  /** Optionally returns the smallest context that contains given `pos`, or None if none exists.
   */
  def locateContext(contexts: Contexts, pos: Position): Option[Context] = synchronized {
    def locateNearestContextTree(contexts: Contexts, pos: Position, recent: Array[ContextTree]): Option[ContextTree] = {
      locateContextTree(contexts, pos) match {
        case Some(x) =>
          recent(0) = x
          locateNearestContextTree(x.children, pos, recent)
        case None => recent(0) match {
            case null => None
            case x => Some(x)
          }
      }
    }
    locateNearestContextTree(contexts, pos, new Array[ContextTree](1)) map (_.context)
  }

  def locateContextTree(contexts: Contexts, pos: Position): Option[ContextTree] = {
    if (contexts.isEmpty) None
    else {
      val hi = contexts.length - 1
      if ((contexts(hi).pos properlyPrecedes pos) || (pos properlyPrecedes contexts(0).pos)) None
      else {
        def loop(lo: Int, hi: Int): Option[ContextTree] = {
          val mid = (lo + hi) / 2
          val midpos = contexts(mid).pos
          if ((pos precedes midpos) && (mid < hi))
            loop(lo, mid)
          else if ((midpos precedes pos) && (lo < mid))
            loop(mid, hi)
          else if (midpos includes pos)
            Some(contexts(mid))
          else if (contexts(mid+1).pos includes pos)
            Some(contexts(mid+1))
          else None
        }
        loop(0, hi)
      }
    }
  }

  /** Insert a context at correct position into a buffer of context trees.
   *  If the `context` has a transparent position, add it multiple times
   *  at the positions of all its solid descendant trees.
   */
  def addContext(contexts: Contexts, context: Context): Unit = {
    val cpos = context.tree.pos
    if (cpos.isTransparent)
      for (t <- context.tree.children flatMap solidDescendants)
        addContext(contexts, context, t.pos)
    else
      addContext(contexts, context, cpos)
  }

  /** Insert a context with non-transparent position `cpos`
   *  at correct position into a buffer of context trees.
   */
  def addContext(contexts: Contexts, context: Context, cpos: Position): Unit = synchronized {
    try {
      if (!cpos.isRange) {}
      else if (contexts.isEmpty) contexts += new ContextTree(cpos, context)
      else {
        val hi = contexts.length - 1
        if (contexts(hi).pos precedes cpos)
          contexts += new ContextTree(cpos, context)
        else if (contexts(hi).pos properlyIncludes cpos) // fast path w/o search
          addContext(contexts(hi).children, context, cpos)
        else if (cpos precedes contexts(0).pos)
          new ContextTree(cpos, context) +=: contexts
        else {
          def insertAt(idx: Int): Boolean = {
            val oldpos = contexts(idx).pos
            if (oldpos sameRange cpos) {
              contexts(idx) = new ContextTree(cpos, context, contexts(idx).children)
              true
            } else if (oldpos includes cpos) {
              addContext(contexts(idx).children, context, cpos)
              true
            } else if (cpos includes oldpos) {
              val start = contexts.indexWhere(cpos includes _.pos)
              val last = contexts.lastIndexWhere(cpos includes _.pos)
              contexts(start) = new ContextTree(cpos, context, contexts.slice(start, last + 1))
              contexts.remove(start + 1, last - start)
              true
            } else false
          }
          def loop(lo: Int, hi: Int) {
            if (hi - lo > 1) {
              val mid = (lo + hi) / 2
              val midpos = contexts(mid).pos
              if (cpos precedes midpos)
                loop(lo, mid)
              else if (midpos precedes cpos)
                loop(mid, hi)
              else
                addContext(contexts(mid).children, context, cpos)
            } else if (!insertAt(lo) && !insertAt(hi)) {
              val lopos = contexts(lo).pos
              val hipos = contexts(hi).pos
              if ((lopos precedes cpos) && (cpos precedes hipos))
                contexts.insert(hi, new ContextTree(cpos, context))
              else
                inform("internal error? skewed positions: "+lopos+" !< "+cpos+" !< "+hipos)
            }
          }
          loop(0, hi)
        }
      }
    } catch {
      case ex: Throwable =>
        println(ex)
        ex.printStackTrace()
        println("failure inserting "+cpos+" into "+contexts+"/"+contexts(contexts.length - 1).pos+"/"+
                (contexts(contexts.length - 1).pos includes cpos))
        throw ex
    }
  }
}

