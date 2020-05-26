trait BaseVariable {
  type T <: Any
  var value: T
  val name: String
  override def toString: String = name
}

case class Variable(val name: String, var value: String = null) extends BaseVariable {
  type T = String
}

case class UdVariable(name: String, var value: Any) extends BaseVariable {
  type T = Any
}

case class SetVariable(val name: String, var value: Set[String] = null) extends BaseVariable {
  type T = Set[String]
}

case class MapVariable(val name: String, var value: Map[String, String] = null) extends BaseVariable {
  type T = Map[String, String]
}