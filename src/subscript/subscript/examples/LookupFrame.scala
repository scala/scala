package subscript.test
import scala.swing._
import scala.swing.event._
import subscript.Predef._
import subscript.swing.SimpleSubscriptApplication
import subscript.swing.Scripts._
import subscript._
import subscript.DSL._
import subscript.Predef._
import subscript.vm._

// Subscript sample application: a text entry field with a search button, that simulates the invocation of a background search
//
// Note: the main part of this source file has been manually compiled from Subscript code into plain Scala

object LookupFrame extends LookupFrameApplication

class LookupFrameApplication extends SimpleSubscriptApplication {
  
  val outputTA     = new TextArea        {editable      = false}
  val searchButton = new Button("Go")    {enabled       = false}
  val searchLabel  = new Label("Search") {preferredSize = new Dimension(45,26)}
  val searchTF     = new TextField       {preferredSize = new Dimension(100, 26)}
  
  val top          = new MainFrame {
    title          = "LookupFrame - Subscript"
    location       = new Point    (100,100)
    preferredSize  = new Dimension(300,300)
    contents       = new BorderPanel {
      add(new FlowPanel(searchLabel, searchTF, searchButton), BorderPanel.Position.North) 
      add(outputTA, BorderPanel.Position.Center) 
    }
  }
  override def  live = _execute(_live())
  
override def script
           live     = ...searchSequence
def script..
  
  searchSequence    = searchCommand showSearchingText searchInDatabase showSearchResults
  searchCommand     = searchButton
  showSearchingText = @{gui(there)}: {outputTA.text = "Searching: "+searchTF.text}
  showSearchResults = @{gui(there)}: {outputTA.text = "Found: "+here.index+" items"}
  searchInDatabase  = {* Thread.sleep(2000) *} // simulate a time consuming action

implicit def script vkey(??k: Key.Value) = vkey2(top, ??k)

/* translated into:

  override def _live     = _script(this, 'live             ) {_seq(_loop, _searchSequence)}
  def _searchSequence    = _script(this, 'searchSequence   ) {_seq(_searchCommand, _showSearchingText, _searchInDatabase, _showSearchResults)}
  def _searchCommand     = _script(this, 'searchCommand    ) {_clicked(searchButton)}
  def _showSearchingText = _script(this, 'showSearchingText) {_at{gui0} (_normal0 {                         outputTA.text = "Searching: "+searchTF.text})}
  def _showSearchResults = _script(this, 'showSearchResults) {_at{gui0} (_normal{(here: N_code_normal) => outputTA.text = "Found: "+here.index+" items"})}
  def _searchInDatabase  = _script(this, 'searchInDatabase ) {_threaded0{Thread.sleep(2000)}}
               
  // bridge method   
  override def live = _execute(_live             )
  * 
  */
}
