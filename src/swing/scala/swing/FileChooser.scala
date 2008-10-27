package scala.swing

import java.io.File
import javax.swing._
import javax.swing.filechooser._

object FileChooser {
  /**
   * The result of a file dialog. The precise meaning of the <code>Approve</code>
   * result depends on the specific dialog type. Could be "save" or "open" for
   * example.
   */
  object Result extends Enumeration {
    val Cancel = Value(JFileChooser.CANCEL_OPTION)
    val Approve = Value(JFileChooser.APPROVE_OPTION)
    val Error = Value(JFileChooser.ERROR_OPTION)
  }

  /**
   * The kind of elements a user can select in a file dialog.
   */
  object SelectionMode extends Enumeration {
    val FilesOnly = Value(JFileChooser.FILES_ONLY)
    val DirectoriesOnly = Value(JFileChooser.DIRECTORIES_ONLY)
    val FilesAndDirectories = Value(JFileChooser.FILES_AND_DIRECTORIES)
  }
}

/**
 * Used to open file dialogs.
 *
 * @see javax.swing.JFileChooser
 */
class FileChooser(dir: File) {
  import FileChooser._
  lazy val peer: JFileChooser = new JFileChooser(dir)

  def this() = this(null)

  def showOpenDialog(over: Component): Result.Value = Result(peer.showOpenDialog(over.peer))
  def showSaveDialog(over: Component): Result.Value = Result(peer.showSaveDialog(over.peer))
  def showDialog(over: Component, approveText: String): Result.Value = Result(peer.showDialog(over.peer, approveText))

  def controlButtonsAreShown: Boolean = peer.getControlButtonsAreShown
  def controlButtonsAreShown_=(b: Boolean) { peer.setControlButtonsAreShown(b) }

  def title: String = peer.getDialogTitle
  def title_=(t: String) { peer.setDialogTitle(t) }

  def accessory: Component = Component.wrapperFor(peer.getAccessory)
  def accessory_=(c: Component) { peer.setAccessory(c.peer) }

  def fileHidingEnabled: Boolean = peer.isFileHidingEnabled
  def fileHidingEnabled_=(b: Boolean) { peer.setFileHidingEnabled(b) }
  def fileSelectionMode: SelectionMode.Value = SelectionMode(peer.getFileSelectionMode)
  def fileSelectionMode_=(s: SelectionMode.Value) { peer.setFileSelectionMode(s.id) }
  def fileFilter: FileFilter = peer.getFileFilter
  def fileFilter_=(f: FileFilter) { peer.setFileFilter(f) }

  def selectedFile: File = peer.getSelectedFile
  def selectedFile_=(file: File) { peer.setSelectedFile(file) }
  def selectedFiles: Seq[File] = peer.getSelectedFiles
  def selectedFiles_=(files: File*) { peer.setSelectedFiles(files.toArray) }

  def multiSelectionEnabled: Boolean = peer.isMultiSelectionEnabled
  def multiSelectionEnable_=(b: Boolean) { peer.setMultiSelectionEnabled(b) }

  def iconFor(f: File) = peer.getIcon(f)
  def descriptionFor(f: File) = peer.getDescription(f)
  def nameFor(f: File) = peer.getName(f)
  def typeDescriptionFor(f: File) = peer.getTypeDescription(f)
  def traversable(f: File) = peer.isTraversable(f)

  def acceptAllFileFilter = peer.getAcceptAllFileFilter
}
