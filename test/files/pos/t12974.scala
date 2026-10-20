//> using options -Xsource:3
package style

object bug {
  private[style] trait Style

  private[style] class StyleSheet private (private val stylesByType: Map[Class[? <: Style], Style]) {
    private def +(style: Style): StyleSheet = new StyleSheet(this.stylesByType + (style.getClass -> style))
  }
}

trait Prot {
  protected trait Style
  def foo(style: Style): Set[Class[? <: Style]] = Set(style.getClass)
}

trait Priv {
  private trait Style
  private def foo(style: Style): Set[Class[? <: Style]] = Set(style.getClass)
}
