object CI {
  trait TreeDisplay {
    type TreeNode <: ITreeNode
    trait ITreeNode {
      def display(): Unit
    }
  }

  trait TreeDisplayExp {
    def getRoot(): TreeNode
    type TreeNode <: ITreeNodeExp
    trait ITreeNodeExp {}
  }

  trait TreeDisplayFinal extends TreeDisplay with TreeDisplayExp {
    type TreeNode <: ITreeNode with ITreeNodeExp
  }
  abstract class SimpleTreeDisplay extends TreeDisplay { self: TreeDisplayFinal =>
    def display() { this.getRoot().display() }
  }
}
