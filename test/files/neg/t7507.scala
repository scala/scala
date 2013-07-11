trait Cake extends Slice {
  private[this] val bippy = ()
}

trait Slice { self: Cake =>
  locally(bippy)
}
