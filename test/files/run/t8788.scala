package android {
  package os {
    trait Parcelable {
      trait Creator[T]
    }
  }
}

class C extends android.os.Parcelable

object Test extends App {
  val field = classOf[C].getField("CREATOR")
  println(field)
  println(field.toGenericString)
}
