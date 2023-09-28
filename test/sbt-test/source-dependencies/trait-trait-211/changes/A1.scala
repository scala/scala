package gg
package table

trait A {
  def transform: Unit = {
    // the use site is updated
    buildNonemptyObjects(0, 1)
  }

  // add extra parameter here
  def buildNonemptyObjects(a: Int, b: Int): Unit = ()
}
