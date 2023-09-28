package gg
package table

trait A {
  def transform: Unit = {
    buildNonemptyObjects(0)
  }

  def buildNonemptyObjects(a: Int): Unit = ()
}
