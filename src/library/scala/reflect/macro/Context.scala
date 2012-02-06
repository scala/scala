package scala.reflect
package macro

trait Context extends api.Universe {
  
  /** Mark a variable as captured; i.e. force boxing in a *Ref type.
   */
  def captureVariable(vble: Symbol): Unit
  
  /** Mark given identifier as a reference to a captured variable itself
   *  suppressing dereferencing with the `elem` field.
   */
  def referenceCapturedVariable(id: Ident): Tree

  /** Given a tree or type, generate a tree that when executed at runtime produces the original tree or type.
   *  For instance, given the abstract syntax tree representation of the `x + 1` expression:
   *
   *    Apply(Select(Ident("x"), "+"), List(Literal(Constant(1))))
   *
   *  The reifier transforms it to the following tree:
   *
   *    $mr.Apply($mr.Select($mr.Ident($mr.newFreeVar("x", <Int>, x), "+"), List($mr.Literal($mr.Constant(1))))))
   *
   *  The transformation looks mostly straightforward, but it has its tricky parts:
   *    * Reifier retains symbols and types defined outside the reified tree, however
   *      locally defined entities get erased and replaced with their original trees
   *    * Free variables are detected and wrapped in symbols of the type FreeVar
   *    * Mutable variables that are accessed from a local function are wrapped in refs
   *    * Since reified trees can be compiled outside of the scope they've been created in,
   *      special measures are taken to ensure that all freeVars remain visible
   *
   *  Typical usage of this function is to retain some of the trees received/created by a macro
   *  into the form that can be inspected (via pattern matching) or compiled/run (by a reflective ToolBox) during the runtime.
   */
  def reify(tree: Tree): Tree
}
