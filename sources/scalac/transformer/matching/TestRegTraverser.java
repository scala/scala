package scalac.transformer.matching ;

import scalac.ast.Tree;
import scalac.util.Name;
import scalac.symtab.Symbol ;
import scalac.ast.Traverser ;

import Tree.Ident;
import Tree.Bind;

 class TestRegTraverser extends Traverser {

    boolean result;

    public TestRegTraverser() {
	super();
	result = false;
    }

    public void traverse(Tree tree) {
	switch (tree) {
	case Alternative(Tree[] ts):
	case Bind(_, _):
	    result = true;
	    break;
	default:
	    super.traverse( tree );
	}
    }

    static boolean apply( Tree t ) {
	TestRegTraverser trt = new TestRegTraverser();
	trt.traverse( t );
	//System.err.println("TestRegTraverser says "+t+" -> "+trt.result);
	return trt.result;
    }

}
