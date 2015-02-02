package scala.test.scaladoc {

  /** Check the interaction between SI-3314 and diagrams
   *  - the three enumerations below should get valid content diagrams:
   *                       Value
   *              __________/|\__________
   *             /   /   /   |   \   \   \
   *            Mon Tue Wed Thu Fri Sat Sun
   *
   *  - each member should receive an inheritance diagram:
   *                       Value
   *                         |
   *                         |
   *           {Mon,Tue,Wed,Thu,Fri,Sat,Sun}
   */
  package diagrams {

    /** @contentDiagram
     *  @inheritanceDiagram hideDiagram */
    trait WeekDayTraitWithDiagram extends Enumeration {
      type WeekDay = Value
      /** @inheritanceDiagram */
      object Mon extends WeekDay
      /** @inheritanceDiagram */
      object Tue extends WeekDay
      /** @inheritanceDiagram */
      object Wed extends WeekDay
      /** @inheritanceDiagram */
      object Thu extends WeekDay
      /** @inheritanceDiagram */
      object Fri extends WeekDay
      /** @inheritanceDiagram */
      object Sat extends WeekDay
      /** @inheritanceDiagram */
      object Sun extends WeekDay
    }

    /** @contentDiagram
     *  @inheritanceDiagram hideDiagram */
    class WeekDayClassWithDiagram extends Enumeration {
      type WeekDay = Value
      /** @inheritanceDiagram */
      object Mon extends WeekDay
      /** @inheritanceDiagram */
      object Tue extends WeekDay
      /** @inheritanceDiagram */
      object Wed extends WeekDay
      /** @inheritanceDiagram */
      object Thu extends WeekDay
      /** @inheritanceDiagram */
      object Fri extends WeekDay
      /** @inheritanceDiagram */
      object Sat extends WeekDay
      /** @inheritanceDiagram */
      object Sun extends WeekDay
    }

    /** @contentDiagram
     *  @inheritanceDiagram hideDiagram */
    object WeekDayObjectWithDiagram extends Enumeration {
      type WeekDay = Value
      /** @inheritanceDiagram */
      object Mon extends WeekDay
      /** @inheritanceDiagram */
      object Tue extends WeekDay
      /** @inheritanceDiagram */
      object Wed extends WeekDay
      /** @inheritanceDiagram */
      object Thu extends WeekDay
      /** @inheritanceDiagram */
      object Fri extends WeekDay
      /** @inheritanceDiagram */
      object Sat extends WeekDay
      /** @inheritanceDiagram */
      object Sun extends WeekDay
    }
  }
}