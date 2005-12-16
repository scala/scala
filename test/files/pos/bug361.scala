// $Id$

class Bug361Global extends Bug361Trees;

abstract class Bug361Trees: Bug361Global {

  abstract class Tree {
    var pos: int = 0;
  }

  object posAssigner {
    def atPos[T <: Tree](pos: int, tree: T): T = {
      tree.pos = pos; tree
    }
  }

  def atPos[T <: Tree](pos: int)(tree: T): T = posAssigner.atPos(pos, tree);
}
