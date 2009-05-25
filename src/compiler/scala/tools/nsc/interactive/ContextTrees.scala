package scala.tools.nsc.interactive

import collection.mutable.ArrayBuffer
import nsc.util.Position

trait ContextTrees { self: Global =>

  type Context = analyzer.Context
  type Contexts = ArrayBuffer[ContextTree]

  class ContextTree(val context: Context, val children: ArrayBuffer[ContextTree]) {
    def this(context: Context) = this(context, new ArrayBuffer[ContextTree])
    def pos: Position = context.tree.pos
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
    if (contexts.isEmpty) contexts += new ContextTree(context)
    else {
      val hi = contexts.length - 1
      if (contexts(hi).pos precedes cpos)
        contexts += new ContextTree(context)
      else if (contexts(hi).pos includes cpos) // fast path w/o search
        addContext(contexts(hi).children, context)
      else if (cpos precedes contexts(0).pos)
        new ContextTree(context) +: contexts
      else {
        def loop(lo: Int, hi: Int) {
          assert(lo <= hi)
          val mid = (lo + hi) / 2
          val midpos = contexts(mid).pos
          if (cpos precedes midpos)
            loop(lo, mid - 1)
          else if (midpos precedes cpos)
            loop(mid + 1, hi)
          else if (midpos sameRange cpos)
            contexts(mid) = new ContextTree(context, contexts(mid).children)
          else if ((midpos includes cpos) && !(cpos includes midpos))
            addContext(contexts(mid).children, context)
          else if (cpos includes midpos)
            contexts(mid) = new ContextTree(context, ArrayBuffer(contexts(mid)))
          else {
            inform("internal error? skewed positions: "+midpos+"/"+cpos)
            contexts(mid) = new ContextTree(context)
          }
        }
        loop(0, hi)
      }
    }
  }
}

