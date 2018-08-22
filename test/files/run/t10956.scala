
import scala.tools.partest.ReplTest
import scala.tools.nsc.Settings


/*
 * Huge import clause resulted in long "result string" construction.
 * That would blow the stack on typecheck or refchecks.
 */
object Test extends ReplTest {
  def code = 
    """
$intp.isettings.maxPrintString = 0
:paste < EOF
import java.awt.AWTError,java.awt.Dialog,java.awt.KeyEventDispatcher,java.awt.Robot,java.awt.AWTEvent,java.awt.Dimension,java.awt.KeyEventPostProcessor,java.awt.ScrollPane,java.awt.AWTEventMulticaster,java.awt.DisplayMode,java.awt.KeyboardFocusManager,java.awt.ScrollPaneAdjustable,java.awt.AWTException,java.awt.Event,java.awt.Label,java.awt.Scrollbar,java.awt.AWTKeyStroke,java.awt.EventQueue,java.awt.LayoutManager,java.awt.SecondaryLoop,java.awt.AWTPermission,java.awt.FileDialog,java.awt.LayoutManager2,java.awt.Shape,java.awt.ActiveEvent,java.awt.FlowLayout,java.awt.LinearGradientPaint,java.awt.SplashScreen,java.awt.Adjustable,java.awt.FocusTraversalPolicy,java.awt.List,java.awt.Stroke,java.awt.AlphaComposite,java.awt.Font,java.awt.MediaTracker,java.awt.SystemColor,java.awt.BasicStroke,java.awt.FontFormatException,java.awt.Menu,java.awt.SystemTray,java.awt.BorderLayout,java.awt.FontMetrics,java.awt.MenuBar,java.awt.TextArea,java.awt.BufferCapabilities,java.awt.Frame,java.awt.MenuComponent,java.awt.TextComponent,java.awt.Button,java.awt.GradientPaint,java.awt.MenuContainer,java.awt.TextField,java.awt.Canvas,java.awt.Graphics,java.awt.MenuItem,java.awt.TexturePaint,java.awt.CardLayout,java.awt.Graphics2D,java.awt.MenuShortcut,java.awt.Toolkit,java.awt.Checkbox,java.awt.GraphicsConfigTemplate,java.awt.MouseInfo,java.awt.Transparency,java.awt.CheckboxGroup,java.awt.GraphicsConfiguration,java.awt.MultipleGradientPaint,java.awt.TrayIcon,java.awt.CheckboxMenuItem,java.awt.GraphicsDevice,java.awt.PageAttributes,java.awt.Window,java.awt.Choice,java.awt.GraphicsEnvironment,java.awt.Paint,java.awt.color,java.awt.Color,java.awt.GridBagConstraints,java.awt.PaintContext,java.awt.datatransfer,java.awt.Component,java.awt.GridBagLayout,java.awt.Panel,java.awt.dnd,java.awt.ComponentOrientation,java.awt.GridBagLayoutInfo,java.awt.Point,java.awt.event,java.awt.Composite,java.awt.GridLayout,java.awt.PointerInfo,java.awt.font,java.awt.CompositeContext,java.awt.HeadlessException,java.awt.Polygon,java.awt.geom,java.awt.Container,java.awt.IllegalComponentStateException,java.awt.PopupMenu,java.awt.im,java.awt.ContainerOrderFocusTraversalPolicy,java.awt.Image,java.awt.PrintGraphics,java.awt.image,java.awt.Cursor,java.awt.ImageCapabilities,java.awt.PrintJob,java.awt.peer,java.awt.DefaultFocusTraversalPolicy,java.awt.Insets,java.awt.RadialGradientPaint,java.awt.print,java.awt.DefaultKeyboardFocusManager,java.awt.ItemSelectable,java.awt.Rectangle,java.awt.Desktop,java.awt.JobAttributes,java.awt.RenderingHints
EOF
    """
}
