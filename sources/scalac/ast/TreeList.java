/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
\*                                                                      */

// $Id$

package scalac.ast;

/** List of trees.
 */

public final class TreeList {
    Tree[] trees;
    int len;

    public TreeList(Tree[] ts) {
	trees = ts;
	len = ts.length;
    }

    public TreeList() {
	this(new Tree[4]);
	len = 0;
    }

    public void append(Tree tree) {
        if (len == trees.length) {
            Tree[] ts = new Tree[len * 2];
            System.arraycopy(trees, 0, ts, 0, len);
            trees = ts;
        }
        trees[len++] = tree;
    }

    public void append(Tree[] ts) {
        for (int j = 0; j < ts.length; j++)
            append(ts[j]);
    }

    public void append(TreeList tl) {
        for (int j = 0; j < tl.len; j++)
            append(tl.trees[j]);
    }

    public int length() {
        return len;
    }

    public Tree get(int i) {
        return trees[i];
    }

    public Tree first() {
        return trees[0];
    }

    public Tree removeLast() {
        return trees[--len];
    }

    public Tree[] toArray() {
        Tree[] ts = new Tree[len];
        System.arraycopy(trees, 0, ts, 0, len);
        return ts;
    }

    public Tree[] copyTo(Tree[] ts) {
	return copyTo(ts, 0);
    }

    public Tree[] copyTo(Tree[] ts, int from) {
        System.arraycopy(trees, 0, ts, from, len);
        return ts;
    }
}
