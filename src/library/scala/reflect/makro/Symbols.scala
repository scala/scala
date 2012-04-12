package scala.reflect.makro

trait Symbols {
  self: Context =>

  /** Can this symbol be loaded by a reflective mirror?
   *
   *  Scalac relies on `ScalaSignature' annotation to retain symbols across compilation runs.
   *  Such annotations (also called "pickles") are applied on top-level classes and include information
   *  about all symbols reachable from the annotee. However, local symbols (e.g. classes or definitions local to a block)
   *  are typically unreachable and information about them gets lost.
   *
   *  This method is useful for macro writers who wish to save certain ASTs to be used at runtime.
   *  With `isLocatable' it's possible to check whether a tree can be retained as is, or it needs special treatment.
   */
  def isLocatable(sym: Symbol): Boolean
}