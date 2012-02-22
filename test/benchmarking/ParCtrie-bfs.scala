




import collection.parallel.mutable.ParCtrie


object Bfs extends testing.Benchmark {
  val length = sys.props("length").toInt
  val par = sys.props("par").toInt
  
  type Node = (Int, Int);
  type Parent = (Int, Int);
  
  def up(n: Node) = (n._1, n._2 - 1);
  def down(n: Node) = (n._1, n._2 + 1);
  def left(n: Node) = (n._1 - 1, n._2);
  def right(n: Node) = (n._1 + 1, n._2);
  
  // create a map and a target
  val target = (length / 2, length / 2);
  val map = Array.tabulate(length, length)((x, y) => (x % 3) != 0 || (y % 3) != 0 || (x, y) == target)
  def onMap(n: Node) = n._1 >= 0 && n._1 < length && n._2 >= 0 && n._2 < length
  
  // open and closed lists
  val open = ParCtrie[Node, Parent]()
  val closed = ParCtrie[Node, Parent]()
  
  collection.parallel.ForkJoinTasks.defaultForkJoinPool.setParallelism(par)
  
  override def setUp() {
    open.clear()
    closed.clear()
    
    // a couple of starting positions
    open((0, 0)) = null
    open((length - 1, length - 1)) = null
    open((0, length - 1)) = null
    open((length - 1, 0)) = null
  }
  
  def run() = {
    // greedy bfs path search
    while (open.nonEmpty && !open.contains(target)) {
      for ((node, parent) <- open) {
        def expand(next: Node) {
          if (onMap(next) && map(next._1)(next._2) && !closed.contains(next) && !open.contains(next)) {
            open(next) = node
          }
        }
        expand(up(node))
        expand(down(node))
        expand(left(node))
        expand(right(node))
        closed(node) = parent
        open.remove(node)
      }
    }
  }
  
  override def tearDown() {
    // print path
    var pathnode = open(target)
    while (closed.contains(pathnode)) {
      print(pathnode + "->")
      pathnode = closed(pathnode)
    }
    println()
  }
  
}

