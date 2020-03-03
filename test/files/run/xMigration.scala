import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code = s"""
:paste -raw
package scala { object T { @scala.annotation.migration("text", "2.8.0") def muh: String = "muuh" } }
\u0004
scala.T.muh    // no warn
:setting -Xmigration:none
scala.T.muh    // no warn
:setting -Xmigration:any
scala.T.muh    // warn
:setting -Xmigration:2.8
scala.T.muh    // no warn
:setting -Xmigration:2.7
scala.T.muh    // warn
:setting -Xmigration:2.11
scala.T.muh    // no warn
:setting -Xmigration // same as :any
scala.T.muh    // warn
  """
}
