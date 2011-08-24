class Bug361Global extends Bug361Trees

abstract class Bug361Trees { self: Bug361Global =>

  abstract class Tree {
    var pos: Int = 0
  }

  object posAssigner {
    def atPos[T <: Tree](pos: Int, tree: T): T = {
      tree.pos = pos; tree
    }
  }

  def atPos[T <: Tree](pos: Int)(tree: T): T = posAssigner.atPos(pos, tree)
}
