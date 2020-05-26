import network.{Path, Port}

object Main extends App {
  var macTable: Map[Int, Port] = Map.empty
//  Topology.fromJson(Source.fromResource("topo.json").mkString)

  @Mthread
  def l2_custom(pkt: Packet, ingestion: Port): Path = {
    if (!macTable.contains(pkt.l2.src)) {
      macTable += (pkt.l2.src -> ingestion)
    }
    if (macTable.contains(pkt.l2.dst)) {
      val dst = macTable.get(pkt.l2.dst)
      return Routing.shortestPath(ingestion, dst)
    } else {
      return Routing.stp(ingestion)
    }
  }
}

