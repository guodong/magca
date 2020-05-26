import network.{Path, Topology}
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef._
import scalax.collection.mutable.Graph

import scala.collection.mutable.ArrayBuffer

object IR {
  val variables: ArrayBuffer[BaseVariable] = new ArrayBuffer[BaseVariable]()
  val instructions: ArrayBuffer[Instruction] = new ArrayBuffer[Instruction]()

  var dfg: Graph[Instruction, DiEdge] = Graph()

  def getVariableByName(name: String): Option[BaseVariable] = {
    variables.find(v => v.name == name)
  }

  def addInstruction(inst: Instruction) {
    if (inst.gv != null && !variables.contains(inst.gv)) {
      variables += inst.gv
    }
    for (i <- inst.inputs) {
      if (!variables.contains(i)) {
        variables += i
      }
    }
    if (!variables.contains(inst.output)) {
      variables += inst.output
    }
    instructions.addOne(inst)
  }

  def dump() {
    instructions.foreach(i => {
      println(i)
    })
  }

  def tabulation(): Unit = {
    instructions.foreach(i => i.tabulation())
  }

  def dumpTables(): Unit = {
    instructions.foreach(i => i.table.dump())
  }

  def genDFG(): Unit = {
    def getNode(v: BaseVariable): Instruction = {
      val n = dfg.nodes.find(n => n.toOuter.output == v)
      n match {
        case Some(x) => x.toOuter
        case None => null
      }
    }

    def getInstructionByOutput(output: Variable): Option[Instruction] = {
      for (inst <- instructions) {
        if (inst.output == output)
          return Some(inst)
      }

      return None
    }

    instructions.foreach(i => dfg.add(i))

    for (inst <- instructions) {
      if (inst.gv != null) {
        val src = getNode(inst.gv)
        if (src != null)
          dfg.add(src ~> inst)
      }

      for (input <- inst.inputs) {
        val src = getNode(input)
        if (src != null)
          dfg.add(src ~> inst)
      }
    }
  }

  def dumpDFG(): Unit = {
    println(dfg)
  }

  def explore(): Unit = {
    def getNode(v: Variable): Instruction = {
      val n = dfg.nodes.find(n => n.toOuter.output == v)
      n match {
        case Some(x) => x.toOuter
        case None => null
      }
    }

    dfg.topologicalSort() match {
      case Right(topOrder) => {
        for (node <- topOrder) {
          val inst = node.toOuter
          if (node.diPredecessors.size == 0) {
            inst.table_sigma = inst.table
          } else {
            var tbs = new ArrayBuffer[Table]()
            for (p <- node.diPredecessors) {
              tbs += p.toOuter.table_sigma
            }
            if (false) { // (inst.op == "shortestPath" || inst.op == "stp") {
              val b_sigma_t = tbs.reduce((t1, t2) => t1.join(t2))
              val input_sigma_t = b_sigma_t.project(node.toOuter.table.attributes)
              input_sigma_t.dump()
            } else {
              tbs += node.toOuter.table
              val sigma_t = tbs.reduce((t1, t2) => t1.join(t2))
              inst.table_sigma = sigma_t

              // extract black-box function
              if (inst.op == "shortestPath" || inst.op == "stp") {
                for (e <- inst.table_sigma.entries) {
                  if (inst.op == "shortestPath") {
                    // set output attribute to execution result
                    e.data = e.data.updated(inst.output, Topology.shortestPath(e.data(inst.inputs(0)).toString, e.data(inst.inputs(1)).toString))
                  } else if (inst.op == "stp") {
                    e.data = e.data.updated(inst.output, "stp_path")
                  }
                }
              }

              if (inst.op == "phi") {
                for (e <- inst.table_sigma.entries) {
                  if (e.data(inst.inputs(0)) == "1") {
                    e.data = e.data.updated(inst.output, e.data(inst.inputs(1)))
                  } else {
                    e.data = e.data.updated(inst.output, e.data(inst.inputs(2)))
                  }
                }
              }

              inst.table_sigma.dump()
            }
          }
        }
      }
      case Left(cycleNode) => throw new Error(s"Graph contains a cycle at node: ${cycleNode}.")
    }
  }

  def localize(sw: String): Unit = {
    val sink_inst = instructions.last
    dfg.topologicalSort() match {

      case Right(topOrder) => {
        println(topOrder)
        for (node <- topOrder.toList.reverse) {
          if (node.toOuter == sink_inst) {
            node.toOuter.table_psi += (sw -> sink_inst.table_sigma.filter(e => e.data(sink_inst.output) match {
              case p: Path => p.isTraverse(sw)
              case _ => false
            }))
          } else {
            var tbs = new ArrayBuffer[Table]()
            for (p <- node.diSuccessors) {
              tbs += p.toOuter.table_psi(sw)
            }
            tbs += node.toOuter.table
            val psi_t = tbs.reduce((t1, t2) => t1.join(t2))
            node.toOuter.table_psi += (sw -> psi_t)
          }
        }
      }
      case Left(cycleNode) => throw new Error(s"Graph contains a cycle at node: ${cycleNode}.")
    }
  }

  def dumpLocalizedTables(sw: String): Unit = {
    println("++++ localized tables +++++")
    instructions.foreach(inst => inst.table_psi(sw).project(inst.table.attributes).dump())
  }

  def dumpFinalTable(sw: String): Unit = {
    instructions.last.table_psi(sw).project(Set(getVariableByName("ingestion").orNull,
      getVariableByName("pkt.l2.dst").orNull,
      getVariableByName("return").orNull)).dump()
  }
}
