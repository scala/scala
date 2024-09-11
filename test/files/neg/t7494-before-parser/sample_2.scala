//> using options -Xplugin:. -Xplugin-require:beforeparser
package sample

// just a sample that is compiled with the sample plugin enabled
object Sample extends App {
  // because `-Werror` doesn't work; after phase assembly warnings are issued,
  // Run.compileUnits resets the reporter (and its warning count)
  def f: Int = ""
}
