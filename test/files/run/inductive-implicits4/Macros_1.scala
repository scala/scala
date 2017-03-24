import scala.language.experimental.macros
import scala.reflect.macros.whitebox

package object shapeless {
  def cachedImplicit[T]: T = macro CachedImplicitMacros.cachedImplicitImpl[T]
}

package shapeless {
  class CachedImplicitMacros(val c: whitebox.Context) {
    import c.universe._

    def cachedImplicitImpl[T](implicit tTag: WeakTypeTag[T]): Tree = {
      val casted = c.asInstanceOf[reflect.macros.runtime.Context]
      val typer = casted.callsiteTyper
      val global: casted.universe.type = casted.universe
      val analyzer: global.analyzer.type = global.analyzer
      val tCtx = typer.context
      val owner = tCtx.owner
      if(!owner.isVal && !owner.isLazy)
        c.abort(c.enclosingPosition, "cachedImplicit should only be used to initialize vals and lazy vals")
      val tTpe = weakTypeOf[T]
      val application = casted.macroApplication
      val tpe = {
        val tpe0 =
          if (tTpe.typeSymbol.isParameter) owner.tpe.asInstanceOf[Type]
          else tTpe
        tpe0.finalResultType
      }.asInstanceOf[global.Type]

      // Run our own custom implicit search that isn't allowed to find
      // the thing we are enclosed in
      val sCtx = tCtx.makeImplicit(false)
      val is = new analyzer.ImplicitSearch(
        tree = application,
        pt = tpe,
        isView = false,
        context0 = sCtx,
        pos0 = c.enclosingPosition.asInstanceOf[global.Position]
      ) {
        override def searchImplicit(
          implicitInfoss: List[List[analyzer.ImplicitInfo]],
          isLocalToCallsite: Boolean
        ): analyzer.SearchResult = {
          val filteredInput = implicitInfoss.map { infos =>
            infos.filter { info =>
              val sym = if(info.sym.isLazy) info.sym else info.sym.accessedOrSelf
              sym.owner != owner.owner || (!sym.isVal && !sym.isLazy)
            }
          }
          super.searchImplicit(filteredInput, isLocalToCallsite)
        }
      }
      val best = is.bestImplicit
      if (best.isFailure) {
        val errorMsg = tpe.typeSymbolDirect match {
          case analyzer.ImplicitNotFoundMsg(msg) =>
            msg.format(TermName("evidence").asInstanceOf[global.TermName], tpe)
          case _ =>
            s"Could not find an implicit value of type $tpe to cache"
        }
        c.abort(c.enclosingPosition, errorMsg)
      } else {
        best.tree.asInstanceOf[Tree]
      }
    }
  }
}
