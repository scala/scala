import scala.tools.partest._

object Test extends DirectTest {
  override def code = ""
  lazy val global = newCompiler("-usejavacp")
  import global._, definitions._

  override def show() {
    new global.Run()
    // Once we plug all of the view gaps, the output should be empty!
    checkViews()
  }

  def isExempt(sym: Symbol) = {
    val exempt = Set("view", "repr", "sliceWithKnownDelta", "sliceWithKnownBound", "transform", "filterImpl")
    (exempt contains sym.name.decoded)
  }

  def checkView(viewType: Type, viewLikeType: Type) {
    val sep = "=" * 70
    println(s"\n$sep\nChecking ${viewType.typeSymbol.fullName}\n$sep")
    val termMembers = viewType.nonPrivateMembers.toList filter (_.isTerm) map fullyInitializeSymbol
    val inheritedFromGenericCollection
      = termMembers filterNot (_.owner.name.decoded contains "ViewLike") filterNot (_.owner == viewType.typeSymbol)
    def returnsView(sym: Symbol) = viewType.memberType(sym).finalResultType contains viewType.typeSymbol
    val needOverride = inheritedFromGenericCollection filterNot isExempt filter returnsView

    val grouped = needOverride.groupBy(_.owner).toSeq.sortBy { case (owner, _) => viewType baseTypeIndex owner }
    val report = grouped.map {
      case (owner, syms) => s"\n$owner\n${"-" * 70}\n${syms.map(_.defString).sorted.mkString("\n")}"
    }.mkString("\n")
    println(report)
  }

  def checkViews() {
    import collection._
    checkView(typeOf[TraversableView[_, _]],        typeOf[TraversableViewLike[_, _, _]])
    checkView(typeOf[IterableView[_, _]],           typeOf[IterableViewLike[_, _, _]])
    checkView(typeOf[SeqView[_, _]],                typeOf[SeqViewLike[_, _, _]])
    checkView(typeOf[mutable.IndexedSeqView[_, _]], typeOf[SeqViewLike[_, _, _]])
    checkView(typeOf[immutable.StreamView[_, _]],   typeOf[immutable.StreamViewLike[_, _, _]])
    // Parallel views not checked, assuming we will drop them in 2.11
  }
}
