class bug361Global with bug361Trees;

abstract class bug361Trees: bug361Global {

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
