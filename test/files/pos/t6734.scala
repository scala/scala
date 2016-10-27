
//single file badimp.scala
// adding package object gives not found: type SortedMap
package object badimp

package badimp {

  // move before package object works
  import scala.collection.immutable.SortedMap

  case class Nodal private[badimp] (value: String, children: SortedMap[String, Int])

  // adding target object restores sanity
  // but adding it before the import does not
  //object Nodal
}

package client {
  trait X {
    import scala.collection.immutable.SortedMap
    def f = badimp.Nodal("test", SortedMap[String, Int]())   // ensure Nodal.apply was created
  }
}
