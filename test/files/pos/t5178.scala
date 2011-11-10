abstract class FileOps {
  def withLock[R](start: Long = 0): Option[R]
}

trait DefaultFileOps {
  self: DefaultPath =>
  
  override def withLock[R](start: Long = 5): Option[R] = None
}

class DefaultPath extends FileOps with DefaultFileOps { }
