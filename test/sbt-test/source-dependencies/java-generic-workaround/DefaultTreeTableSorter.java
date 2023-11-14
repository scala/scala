import javax.swing.DefaultRowSorter;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreePath;

public class DefaultTreeTableSorter<T extends TreeModel, C extends TreeColumnModel, I>
		implements TreeTableSorter<T,C>, TreeTableSorter.SortCycle {

	public class NodeSorter extends DefaultRowSorter<T,I> implements SortCycle {

		protected TreeTableWrapper getTreeTableModelWrapper() {
			return (TreeTableWrapper)getModelWrapper();
		}
		protected abstract class TreeTableWrapper extends ModelWrapper<T,I> {}
	}
}
