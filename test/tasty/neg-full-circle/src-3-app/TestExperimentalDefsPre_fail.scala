import downstream.ExperimentalDefsPre.*

object TestExperimentalDefsPre:
  def test = new SubExperimentalNotExperimental

  class SubSubExperimental extends SubExperimentalNotExperimental
