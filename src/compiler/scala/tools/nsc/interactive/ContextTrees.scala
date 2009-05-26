package scala.tools.nsc.interactive

import collection.mutable.ArrayBuffer
import nsc.util.Position

trait ContextTrees { self: Global =>

  type Context = analyzer.Context
  type Contexts = ArrayBuffer[ContextTree]

  class ContextTree(val context: Context, val children: ArrayBuffer[ContextTree]) {
    def this(context: Context) = this(context, new ArrayBuffer[ContextTree])
    def pos: Position = context.tree.pos
    override def toString = "ContextTree("+pos+", "+children+")"
  }

  def locateContext(contexts: Contexts, pos: Position): Option[Context] = {
    if (contexts.isEmpty) None
    else {
      val hi = contexts.length - 1
      if ((contexts(hi).pos precedes pos) || (pos precedes contexts(0).pos)) None
      else {
        def loop(lo: Int, hi: Int): Option[Context] = {
          assert(lo <= hi)
          val mid = (lo + hi) / 2
          val midpos = contexts(mid).pos
          if (pos precedes midpos)
            loop(lo, mid - 1)
          else if (midpos precedes pos)
            loop(mid + 1, hi)
          else if (midpos includes pos)
            locateContext(contexts(mid).children, pos) orElse Some(contexts(mid).context)
          else
            None
        }
        loop(0, hi)
      }
    }
  }

  def addContext(contexts: Contexts, context: Context) {
    val cpos = context.tree.pos
    try {
    if (!cpos.isDefined || cpos.isSynthetic) {}
    else if (contexts.isEmpty) contexts += new ContextTree(context)
    else {
      val hi = contexts.length - 1
      if (contexts(hi).pos properlyPrecedes cpos)
        contexts += new ContextTree(context)
      else if (contexts(hi).pos properlyIncludes cpos) // fast path w/o search
        addContext(contexts(hi).children, context)
      else if (cpos properlyPrecedes contexts(0).pos)
        new ContextTree(context) +: contexts
      else {
        def insertAt(idx: Int): Boolean = {
          val oldpos = contexts(idx).pos
          if (oldpos sameRange cpos) {
            contexts(idx) = new ContextTree(context, contexts(idx).children)
            true
          } else if (oldpos includes cpos) {
            addContext(contexts(idx).children, context)
            true
          } else if (cpos includes oldpos) {
            val start = contexts.indexWhere(cpos includes _.pos)
            val last = contexts.lastIndexWhere(cpos includes _.pos)
            contexts(start) = new ContextTree(context, contexts.slice(start, last + 1))
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
          } else if (!insertAt(lo) && !insertAt(hi)) {
            val lopos = contexts(lo).pos
            val hipos = contexts(hi).pos
            if ((lopos precedes cpos) && (cpos precedes hipos))
              contexts.insert(hi, new ContextTree(context))
            else
              inform("internal error? skewed positions: "+lopos+" !< "+cpos+" !< "+hipos)
          }
        }
        loop(0, hi)
      }
    }
  } catch {
    case ex: Throwable =>
      println("failure inserting "+context.tree.pos+" into "+contexts+"/"+contexts(contexts.length - 1).pos+"/"+
              (contexts(contexts.length - 1).pos includes cpos))
      throw ex
  }}
}

