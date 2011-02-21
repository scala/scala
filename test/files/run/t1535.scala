class ASTNode {
    lazy val x = 42
}

class BlockStmt extends ASTNode

class ClassDecl extends BlockStmt {
    lazy val y = true
}

object Test extends App {
    val n = new ClassDecl ()
    println (n.x)
    println (n.y)
}
