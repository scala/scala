
trait X {
  // error: in XML literal: name expected, but char '!' cannot start a name
  def x = <![CDATA[hi & bye]]> <![CDATA[red & black]]>
}
