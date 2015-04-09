/* NSC -- new Scala compiler
 * Copyright 2009-2013 Typesafe/Scala Solutions and LAMP/EPFL
 * @author Martin Odersky
 */
package scala.tools.nsc
package interactive

import scala.collection.mutable.ArrayBuffer
import scala.annotation.tailrec

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

  /** Returns the most precise context possible for the given `pos`.
   *
   *  It looks for the finest ContextTree containing `pos`, and then look inside
   *  this ContextTree for a child ContextTree located immediately before `pos`.
   *  If such a child exists, returns its context, otherwise returns the context of
   *  the parent ContextTree.
   *
   *  This is required to always return a context which contains the all the imports
   *  declared up to `pos` (see SI-7280 for a test case).
   *
   *  Can return None if `pos` is before any valid Scala code.
   */
  def locateContext(contexts: Contexts, pos: Position): Option[Context] = synchronized {
    @tailrec
    def locateFinestContextTree(context: ContextTree): ContextTree = {
      if (context.pos includes pos) {
        locateContextTree(context.children, pos) match {
          case Some(x) =>
            locateFinestContextTree(x)
          case None =>
            context
        }
      } else {
        context
      }
    }
    def sanitizeContext(c: Context): Context = {
      c.retyping = false
      c
    }
    locateContextTree(contexts, pos) map locateFinestContextTree map (ct => sanitizeContext(ct.context))
  }

  /** Returns the ContextTree containing `pos`, or the ContextTree positioned just before `pos`,
   *  or None if `pos` is located before all ContextTrees.
   */ 
  def locateContextTree(contexts: Contexts, pos: Position): Option[ContextTree] = {
    if (contexts.isEmpty) None
    else {
      // binary search on contexts, loop invar: lo <= hi, recursion metric: `hi - lo`
      @tailrec
      def loop(lo: Int, hi: Int, previousSibling: Option[ContextTree]): Option[ContextTree] = {
        // [SI-8239] enforce loop invariant & ensure recursion metric decreases monotonically on every recursion
        if (lo > hi) previousSibling
        else if (pos properlyPrecedes contexts(lo).pos)
          previousSibling
        else if (contexts(hi).pos properlyPrecedes pos)
          Some(contexts(hi))
        else {
          val mid = (lo + hi) / 2
          val midpos = contexts(mid).pos
          if (midpos includes pos)
            Some(contexts(mid))
          else if (midpos properlyPrecedes pos)
            // recursion metric: (hi - ((lo + hi)/2 + 1)) < (hi - lo)
            // since (hi - ((lo + hi)/2 + 1)) - (hi - lo) = lo - ((lo + hi)/2 + 1) < 0
            // since 2*lo - lo - hi - 2 = lo - hi - 2 < 0
            // since lo < hi + 2
            // can violate lo <= hi, hence the lo > hi check at the top [SI-8239]
            loop(mid + 1, hi, Some(contexts(mid)))
          else if (lo != hi) // avoid looping forever (lo == hi violates the recursion metric) [SI-8239]
            // recursion metric: ((lo + hi)/2) - lo < (hi - lo)
            // since ((lo + hi)/2) - lo - (hi - lo) = ((lo + hi)/2) - hi < 0
            // since 2 * (((lo + hi)/2) - hi) = lo - hi < 0 since lo < hi
            loop(lo, mid, previousSibling)
          else previousSibling
        }
      }
      loop(0, contexts.length - 1, None)
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

