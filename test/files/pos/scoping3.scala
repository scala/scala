// $Id$

object CI {
   trait TreeDisplay {
	type TreeNode <: ITreeNode;
	trait ITreeNode {
	  def display(): unit;
	}
   }
   trait TreeDisplayExp {
	def getRoot(): TreeNode;
	type TreeNode <: ITreeNodeExp;
	trait ITreeNodeExp {}
   }
   trait TreeDisplayFinal extends TreeDisplay with TreeDisplayExp {
   	type TreeNode <: ITreeNode with ITreeNodeExp;
   }
   abstract class SimpleTreeDisplay requires TreeDisplayFinal extends
TreeDisplay {
     def display() = { this.getRoot().display(); }
   }
}
