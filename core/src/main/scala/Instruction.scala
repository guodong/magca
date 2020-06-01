import network.Port

abstract class Inst {
  val isUdf: Boolean = true
  val gv: Variable
  val inputs: List[BaseVariable]
  val output: BaseVariable
  val table = new Table()
  var table_sigma: Table = null
  var table_psi: Map[String, Table] = Map.empty
  def tabulation(): Unit
}

class SysInst(
                val gv: Variable,
                val guardState: Boolean = true,
                val op: String,
                val ctx: Any,
                val inputs: List[BaseVariable],
                val output: BaseVariable) extends Inst{
  override val isUdf: Boolean = false
  override def toString: String = {
    if (gv == null) {
      s"${output} = $op(${inputs.mkString(", ")})"
    } else {
      if (guardState) {
        s"if $gv: ${output} = $op(${inputs.mkString(", ")})"
      } else {
        s"if !$gv: ${output} = $op(${inputs.mkString(", ")})"
      }
    }
  }
  def tabulation(): Unit = {
    if (gv != null) {
      table.attributes += gv
    }
    table.attributes += output
    table.attributes ++= inputs
    if (op == "in") {
      ctx match {
        case x: Map[Int, Port] => {
          for ((k,v) <- x) {
            var entry: Entry = null
            if (gv != null) {
              val gvv = if (guardState) "1" else "0"
              entry = new Entry(2, Map(gv -> gvv, inputs(0) -> k, output -> "1"))
            } else {
              entry = new Entry(2, Map(inputs(0) -> k, output -> "1"))
            }
            table.entries += entry
          }
        }
        case x: Set[Port] => {
          for (k <- x) {
            var entry: Entry = null
            if (gv != null) {
              val gvv = if (guardState) "1" else "0"
              entry = new Entry(2, Map(gv -> gvv, inputs(0) -> k, output -> "1"))
            } else {
              entry = new Entry(2, Map(inputs(0) -> k, output -> "1"))
            }
            table.entries += entry
          }
        }
      }
      if (gv != null) {
        val gvv = if (guardState) "1" else "0"
        val default_entry = new Entry(1, Map(gv -> gvv, inputs(0) -> "*", output -> "0"))
        table.entries += default_entry
      } else {
        val default_entry = new Entry(1, Map(inputs(0) -> "*", output -> "0"))
        table.entries += default_entry
      }

      if (gv != null) {
        // inverse gvv
        val igvv = if (guardState) "0" else "1"
        var data: Map[BaseVariable, Any] = Map(gv -> igvv, output -> "*")
        inputs.foreach(i => data += i -> "*")
        data = data.filterKeys(table.attributes).toMap
        val default_entry = new Entry(0, data)
        table.entries += default_entry
      }

    } else if (op == "get") {
      for ((k,v) <- ctx.asInstanceOf[Map[Int, Port]]) {
        if (gv != null) {
          val gvv = if (guardState)  "1" else "0"
          val entry = new Entry(1, Map(gv -> gvv, inputs(0) -> k, output -> v))
          table.entries += entry
        } else {
          val entry = new Entry(1, Map(inputs(0) -> k, output -> v))
          table.entries += entry
        }
      }
      if (gv != null) {
        // inverse gvv
        val igvv = if (guardState) "0" else "1"
        var data: Map[BaseVariable, Any] = Map(gv -> igvv, output -> "*")
        inputs.foreach(i => data += i -> "*")
        data = data.filterKeys(table.attributes).toMap
        val default_entry = new Entry(0, data)
        table.entries += default_entry
      }
    } else if (op == "phi") {
      if (gv != null) {
        val gvv = if (guardState) "1" else "0"
        val e1 = new Entry(1, Map(gv -> gvv, inputs(0) -> "1", inputs(1) -> "*", inputs(2) -> "*", output -> "*"))
        val e2 = new Entry(1, Map(gv -> gvv, inputs(0) -> "0", inputs(1) -> "*", inputs(2) -> "*", output -> "*"))
        table.entries += e1
        table.entries += e2
      } else {
        val e1 = new Entry(1, Map(inputs(0) -> "1", inputs(1) -> "*", inputs(2) -> "*", output -> "*"))
        val e2 = new Entry(1, Map(inputs(0) -> "0", inputs(1) -> "*", inputs(2) -> "*", output -> "*"))
        table.entries += e1
        table.entries += e2
      }
      if (gv != null) {
        // inverse gvv
        val igvv = if (guardState) "0" else "1"
        var data: Map[BaseVariable, Any] = Map(gv -> igvv, output -> "*")
        inputs.foreach(i => data += i -> "*")
        data = data.filterKeys(table.attributes).toMap
        val default_entry = new Entry(0, data)
        table.entries += default_entry
      }
    }
  }
}

class UdfInst1[iT, oT](
                        val gv: Variable,
                        val guardState: Boolean = true,
                        val f: iT => oT,
                        val inputs: List[BaseVariable],
                        val output: BaseVariable) extends Inst{
  override def toString: String = {
    if (gv == null) {
      s"${output} = $f(${inputs.mkString(", ")})"
    } else {
      if (guardState) {
        s"if $gv: ${output} = $f(${inputs.mkString(", ")})"
      } else {
        s"if !$gv: ${output} = $f(${inputs.mkString(", ")})"
      }
    }
  }
  def tabulation(): Unit = {
    if (gv != null) {
      table.attributes += gv
    }
    table.attributes += output
    table.attributes ++= inputs
    var data: Map[BaseVariable, Any] = Map.empty
    if (gv != null) {
      val gvv = if (guardState) "1" else "0"
      data += (gv -> gvv)
    }
    for (input <- inputs) {
      data += (input -> "*")
    }
    data += (output -> "*")
    val entry = new Entry(1, data)
    table.entries += entry

    if (gv != null) {
      // inverse gvv
      val igvv = if (guardState) "0" else "1"
      var data: Map[BaseVariable, Any] = Map(gv -> igvv, output -> "*")
      inputs.foreach(i => data += i -> "*")
      data = data.filterKeys(table.attributes).toMap
      val default_entry = new Entry(0, data)
      table.entries += default_entry
    }
  }
}
class UdfInst2[iT1, iT2, oT](
                    val gv: Variable,
                    val guardState: Boolean = true,
                    val f: (iT1, iT2) => oT,
                    val inputs: List[BaseVariable],
                    val output: BaseVariable) extends Inst {
  override def toString: String = {
    if (gv == null) {
      s"${output} = $f(${inputs.mkString(", ")})"
    } else {
      if (guardState) {
        s"if $gv: ${output} = $f(${inputs.mkString(", ")})"
      } else {
        s"if !$gv: ${output} = $f(${inputs.mkString(", ")})"
      }
    }
  }
  def tabulation(): Unit = {
    if (gv != null) {
      table.attributes += gv
    }
    table.attributes += output
    table.attributes ++= inputs
    var data: Map[BaseVariable, Any] = Map.empty
    if (gv != null) {
      val gvv = if (guardState) "1" else "0"
      data += (gv -> gvv)
    }
    for (input <- inputs) {
        data += (input -> "*")
    }
    data += (output -> "*")
    val entry = new Entry(1, data)
    table.entries += entry

    if (gv != null) {
      // inverse gvv
      val igvv = if (guardState) "0" else "1"
      var data: Map[BaseVariable, Any] = Map(gv -> igvv, output -> "*")
      inputs.foreach(i => data += i -> "*")
      data = data.filterKeys(table.attributes).toMap
      val default_entry = new Entry(0, data)
      table.entries += default_entry
    }
  }
}
class Instruction(
                   val gv: Variable,
                   val guardState: Boolean = true,
                   val op: String,
                   val inputs: List[BaseVariable],
                   val output: Variable) {
  val table = new Table()
  var table_sigma: Table = null
  var table_psi: Map[String, Table] = Map.empty

  override def toString: String = {
    if (gv == null) {
      s"${output} = $op(${inputs.mkString(", ")})"
    } else {
      if (guardState) {
        s"if $gv: ${output} = $op(${inputs.mkString(", ")})"
      } else {
        s"if !$gv: ${output} = $op(${inputs.mkString(", ")})"
      }
    }
  }

  def getGuardValue: String = {
    if (guardState) "1" else "0"
  }

  def tabulation(): Unit = {
    if (gv != null) {
      table.attributes += gv
    }
    table.attributes += output
    table.attributes ++= inputs
    return
    if (op == "in") {
      table.attributes += inputs(1)
      inputs(0) match {
        case x:MapVariable => {
          for ((k,v) <- x.value) {
            var entry: Entry = null
            if (gv != null) {
              val gvv = if (guardState) "1" else "0"
              entry = new Entry(1, Map(gv -> gvv, inputs(1) -> k, output -> "1"))
            } else {
              entry = new Entry(1, Map(inputs(1) -> k, output -> "1"))
            }
            table.entries += entry
          }
          if (gv != null) {
            val gvv = if (guardState) "1" else "0"
            val default_entry = new Entry(0, Map(gv -> gvv, inputs(1) -> "*", output -> "0"))
            table.entries += default_entry
          } else {
            val default_entry = new Entry(0, Map(inputs(1) -> "*", output -> "0"))
            table.entries += default_entry
          }
        }
        case x: SetVariable => {
          for (k <- x.value) {
            var entry: Entry = null
            if (gv != null) {
              val gvv = if (guardState) "1" else "0"
              entry = new Entry(0, Map(gv -> gvv, inputs(1) -> k, output -> "1"))
            } else {
              entry = new Entry(0, Map(inputs(1) -> k, output -> "1"))
            }
            table.entries += entry
          }
        }
      }

    } else if (op == "get") {
      table.attributes += inputs(1)
      inputs(0) match {
        case x: MapVariable => {
          for ((k,v) <- x.value) {
            if (gv != null) {
              val gvv = if (guardState)  "1" else "0"
              val entry = new Entry(1, Map(gv -> gvv, inputs(1) -> k, output -> v))
              table.entries += entry
            } else {
              val entry = new Entry(1, Map(inputs(1) -> k, output -> v))
              table.entries += entry
            }
          }
        }
      }

    } else if (op == "assign") {
      if (gv != null) {
        val gvv = if (guardState)  "1" else "0"
        inputs(0) match {
          case x: SetVariable => {
            for (i <- x.value) {
              val entry = new Entry(0, Map(gv -> gvv, output -> i))
              table.entries += entry
            }
          }
        }
      } else {
        inputs(0) match {
          case x: SetVariable => {
            for (i <- x.value) {
              val entry = new Entry(0, Map(output -> i))
              table.entries += entry
            }
          }
        }
      }
    } else if (op == "shortestPath" || op == "stp") {
      table.attributes ++= inputs
//      var data: Map[BaseVariable, Any] = Map.empty
//      if (gv != null) {
//        val gvv = if (guardState)  "1" else "0"
//        data += (gv -> gvv)
//      }
//      for (input <- inputs) {
//        if (input.value != null) {
//          data += (input -> input.value)
//        } else {
//          data += (input -> "*")
//        }
//      }
//      data += (output -> "*")
//      val entry = new Entry(1, data)
//      table.entries += entry
    } else if (op == "phi") {
      table.attributes ++= inputs
      if (gv != null) {
        val gvv = getGuardValue
        val e1 = new Entry(0, Map(gv -> gvv, inputs(0) -> "1", inputs(1) -> "*", inputs(2) -> "*", output -> "*"))
        val e2 = new Entry(0, Map(gv -> gvv, inputs(0) -> "0", inputs(1) -> "*", inputs(2) -> "*", output -> "*"))
        table.entries += e1
        table.entries += e2
      } else {
        val e1 = new Entry(0, Map(inputs(0) -> "1", inputs(1) -> "*", inputs(2) -> "*", output -> "*"))
        val e2 = new Entry(0, Map(inputs(0) -> "0", inputs(1) -> "*", inputs(2) -> "*", output -> "*"))
        table.entries += e1
        table.entries += e2
      }
    }
    // add entry with guard not satisfied
    if (gv != null) {
      // inverse gvv
      val igvv = if (guardState) "0" else "1"
      var data: Map[BaseVariable, Any] = Map(gv -> igvv, output -> "*")
      inputs.foreach(i => data += i -> "*")
      data = data.filterKeys(table.attributes).toMap
      val default_entry = new Entry(0, data)
      table.entries += default_entry
    }
  }

}