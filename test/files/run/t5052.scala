object Test extends App {
  assert(<elem attr={null:String} /> xml_== <elem />)
  assert(<elem attr={None} /> xml_== <elem />)
  assert(<elem /> xml_== <elem attr={null:String} />)
  assert(<elem /> xml_== <elem attr={None} />)
}
