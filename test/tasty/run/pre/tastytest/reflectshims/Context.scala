package tastytest.reflectshims

trait Context {

  type TreeShim = universe.TreeShim

  val universe: Universe

}
