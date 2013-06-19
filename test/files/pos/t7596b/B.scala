class DAOBase[E]{
  type TableType <: Config.driver.Table[E]
}
class SitesDAO extends DAOBase[String]{
  type TableType = Sites
}
