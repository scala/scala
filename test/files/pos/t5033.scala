trait Eater {
  type Food[T]
}

trait Fruit {
  type Seed
}

trait PipExtractor {
  def extract(a: Fruit)(b: Eater): b.Food[a.Seed]
}

trait LaserGuidedPipExtractor extends PipExtractor {
  def extract(f: Fruit)(g: Eater): g.Food[f.Seed]
}