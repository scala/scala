package scala.swing
package test

object ListViewDemo extends SimpleSwingApplication {
  def top = new MainFrame {
    case class City(name: String, country: String, population: Int, capital: Boolean)
    val items = List(City("Lausanne", "Switzerland", 129273, false),
                              City("Paris", "France", 2203817, true),
                              City("New York", "USA", 8363710 , false),
                              City("Berlin", "Germany", 3416300, true),
                              City("Tokio", "Japan", 12787981, true))
    import ListView._
    contents = new FlowPanel(new ScrollPane(new ListView(items) {
      renderer = Renderer(_.name)
    }))
                             //new ScrollPane(new Table(items)))
  }
}
