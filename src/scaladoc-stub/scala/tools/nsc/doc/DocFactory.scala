package scala.tools.nsc
package doc

import reporters.Reporter

class DocFactory(val reporter: Reporter, val settings: doc.Settings) { processor =>
  def document(files: List[String]) {}
}
