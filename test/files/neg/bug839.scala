// see pending/pos/bug112606A.scala
package test;
trait Test {
  trait Global {
    type Tree;
    def get : Tree;
  }
  trait TreeBuilder {
    val global : Global;
    def set(tree : global.Tree) = {}
  }
  val nsc : Global;
  trait FileImpl {
    object treeBuilder extends TreeBuilder {
      val global : nsc.type = nsc;
    }
    // OK
    treeBuilder.set(nsc.get);
  }
  val file0 : FileImpl;
  // OK
  file0.treeBuilder.set(nsc.get);
  def file : FileImpl;
  // type mismatch
  file.treeBuilder.set(nsc.get);
}
