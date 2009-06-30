object Test extends Application {
  List.range(1,11).view.patch(5, List(100,101), 2)
}
