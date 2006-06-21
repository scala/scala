package scala.actors;


/*
 * SPanel.scala
 * GUI for simple texts
 *
 */
import java.awt._;
import java.awt.event._;
import javax.swing.event._;
import javax.swing._;
import java.io._;



class SPanel (WIDTH:int, HEIGHT:int, title:String)  extends JFrame{

  private var textArea:JTextArea = null;
  private var resetButton:JButton = null;

  private var scrollPane:JScrollPane = null;
  private var panel:JPanel = null;
  private var contentPane:Container = null;
  private var levelChoice:JComboBox  = null;
  private var formatChoice:JComboBox  = null;

  //init
  setTitle(title);
  setSize(WIDTH, HEIGHT);
  this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

  contentPane = getContentPane();
  panel = new JPanel();
    contentPane.add(panel, BorderLayout.SOUTH);

  // Add a text area with scroll bars
  textArea = new JTextArea(8, 40);
  scrollPane = new JScrollPane(textArea);
  contentPane.add(scrollPane, BorderLayout.CENTER);

  // Add a "reset textarea" button
  resetButton = new JButton("Clear");
  panel.add(resetButton);
  resetButton.addActionListener(
    new ActionListener() {
      def actionPerformed(evt:ActionEvent):Unit= {
	textArea.setText("");
      }
    }
  );

  this.repaint();
  this.show();


  def addText(text:String):Unit = {
    textArea.append(text+'\n');

    if ( textArea.getHeight() > scrollPane.getHeight() ) {
      scrollPane.getVerticalScrollBar().setValue(scrollPane.getVerticalScrollBar().getMaximum());
    }
    repaint();
  }
/*
  def  actionPerformed(ActionEvent e):Unit {
  }
*/

 override def  paint(g:Graphics ): unit = {
   super.paint( g );
 }

}
