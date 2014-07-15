trait Slice {
  private[this] val bippy = () // must be private[this]
  locally(bippy)
}
