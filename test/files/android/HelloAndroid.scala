//package examples.hello3

import android.app.Activity
import android.os.Bundle
import android.widget.TextView

//class HelloAndroid extends Activity {
class Test extends Activity {
  /** Called when the activity is first created. */
  override def onCreate(icicle: Bundle) {
    super.onCreate(icicle)
    val tv = new TextView(this)
    tv setText "Hello, Android (Scala)"
    setContentView(tv)
  }
}
