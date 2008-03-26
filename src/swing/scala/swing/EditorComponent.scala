package swing

trait EditorComponent extends Component {
  val contentModified: Publisher
}