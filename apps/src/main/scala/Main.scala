import network.{Path, Port}

object Main extends App {
  var macTable: Map[Int, Port] = Map(100 -> new Port("p1", null))
  //  Topology.fromJson(Source.fromResource("topo.json").mkString)
  //  var ingestion: Variable = new Variable("ingestion")
  var pktL2Dst: Variable = Variable("pkt.l2.dst")
  //  _if(_contains(macTable, pktL2Dst), () => {
  //
  //    val dst: Port = _get(macTable, pktL2Dst)
  //    _return(_udf("shortestPath", ingestion, dst))
  //  }, () => {
  //
  //  })
  //  @use
  //  val ds: Map[String, Any] = Map(
  //    "macTable" -> Map[Int, Port]
  //  )

  @Mthread
  def l2_custom(pkt: Packet, ingestion: Port): Path = {
    //    if (!macTable.contains(pkt.l2.src)) {
    //      macTable += (pkt.l2.src -> ingestion)
    //    }
    if (macTable.contains(pkt.l2.dst)) {
      return Routing.shortestPath(ingestion, macTable(pkt.l2.dst))
    } else {
      return Routing.stp(ingestion)
    }
  }

  val eiPorts: Set[Port] = Set(new Port("p0", null), new Port("p1", null))

  def gen(): Unit = {
    val pktL2Dst = new Variable("pkt.l2.dst")
    val ingestion = Variable("ingestion")
    val gi = Variable("gi")
    val dst = Variable("dst")
    val g0 = Variable("g0")
    val return0 = Variable("return0")
    val return1 = Variable("return1")
    val ret = Variable("ret")
//    exe eiPorts sop "in" on List(ingestion) is gi
//    M.obj(eiPorts).op("in").inputs(List(ingestion)).is(gi)
//    M.obj(macTable).op("in").inputs(List(pktL2Dst)).is(g0).when(gi)
//    M.obj(macTable).op("get").inputs(List(pktL2Dst)).is(dst).when(g0)
//    M.obj(null).op(Routing.shortestPath).inputs(List(ingestion, dst)).is(return0).when(g0)
    IR.insts += new SysInst(null, true, "in", eiPorts, List(ingestion), gi)
    IR.insts += new SysInst(gi, true, "in", macTable, List(pktL2Dst), g0)
    IR.insts += new SysInst(g0, true, "get", macTable, List(pktL2Dst), dst)
    IR.insts += new UdfInst2(g0, true, Routing.shortestPath, List(ingestion, dst), return0)
    IR.insts += new UdfInst1(g0, false, Routing.stp, List(dst), return1)
    IR.insts += new SysInst(gi, true, "phi", null, List(g0, return0, return1), ret)
//    M.SI1(null, true, eiPorts, "in", List(ingestion), gi)
//    M.I1(null, true, eiPorts.contains, List(ingestion), gi)
//    M.I1(gi, true, macTable.contains, List(pktL2Dst), g0)
//    M.I1(g0, true, macTable.get, List(pktL2Dst), dst)
//    M.I2(g0, true, Routing.shortestPath, List(ingestion, dst), return0)
//    M.I1(g0, false, Routing.stp, List(ingestion), return1)
//    M.I3(gi, true, phi, List(g0, return0, return1), ret)
  }

  object exe {
    class Ctx {

    }
    def apply(target: Any): Ctx = new Ctx()
  }

  def phi(g: Variable, a: Variable, b: Variable): Unit = {

  }

  gen()
  IR.dumpInsts()
  IR.tabulationInst()
  IR.dumpTablesInst()
  IR.genDFGInst()
  println(IR.dfgInst)
  IR.exploreInst()

  //  def gen(): Unit = {
  //    new _var("ingestion")
  //    _var("pkt.l2.dst")
  //    val g0 = _var("g0")
  //    _contains("ei_ports", "ingestion", g0)
  //    _if(g0, () => {
  //      _get("macTable", "pkt.l2.dst", "dst")
  //      _udf("shortestPath", List("ingestion", "dst"), "return0")
  //      _return("return0")
  //    }, () => {
  //      _udf("stp", List("ingestion"), "return1")
  //      _return("return1")
  //    })
  //  }

  //  def _contains(target: String, key: String, output: String): Unit = {
  //
  //  }
//  def I(f: () => (), inputs: List[Any]): Unit = {
//
//  }
  def _if(a: Any, b: Any, c: Any) {}
  def _return(a: Any) {}
  def inst(f: Int => Boolean, inputs: List[Any]): Unit = {}
}
class I {}
object I {
  def inst1[iT, oT](f: iT => oT, inputs: List[Any]): Unit = {
    println("inst1")
  }
  def inst2[iT1, iT2, oT](f: (iT1,iT2) => oT, inputs: List[Any]): Unit = {
    println("inst2")
  }
}
case class I2(f: (_,_) => _, inputs: List[Any]) {
}

//class I[iT, oT](f: (iT) => (oT), inputs: List[Any]) {
//
//}