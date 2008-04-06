package scala.swing

trait EditorComponent extends Component {
  val contentModified: Publisher
}