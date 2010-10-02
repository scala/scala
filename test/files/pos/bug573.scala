package lampion.collections;

object DirX {
  abstract class Dir {
    def reverse : Dir;
  }
  object BEFORE extends Dir {
    def reverse = AFTER;
  }
  object AFTER  extends Dir {
    def reverse = BEFORE;
  }
}

import DirX._;

abstract class Linked {
  type Node <: Node0;

  abstract class Node0 {
    self: Node =>

  	var next : Node = _;
    var prev : Node = _;

    def get(dir : Dir) = if (dir == BEFORE) prev; else next;
    private def set(dir : Dir, node : Node) =
      if (dir == BEFORE) prev = node; else next = node;

    def link(dir : Dir, node : Node) = {
      assert(get(dir) == null);
      assert(node.get(dir.reverse) == null);
      set(dir, node);
      node.set(dir.reverse, self);
    }


    def end(dir : Dir) : Node = {
      if (get(dir) == null) this;
      else get(dir).end(dir);
    }
  }
}
