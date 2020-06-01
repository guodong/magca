package network

import io.circe.generic.auto._
import io.circe.parser
import scalax.collection.edge.Implicits._
import scalax.collection.edge.LUnDiEdge
import scalax.collection.mutable.Graph

class Port(val id: String, val node: Node, var peer: Port = null) {
  override def toString: String = id
}

trait Node {
  val id: String
  var ports: Set[Port]
}

case class Host(val id: String) extends Node {
  override var ports: Set[Port] = Set.empty
}

case class Switch(val id: String) extends Node {
  override var ports: Set[Port] = Set.empty
}

class Link(val p0: Port, val p1: Port, val bw: String) {
  def getPortByNode(node: Node): Option[Port] = {
    if (p0.node == node) {
      return Some(p0)
    } else if (p1.node == node) {
      return Some(p1)
    }
    None
  }
}

case class Topo2(hosts: Set[Host], switches: Set[Switch], links: Set[Link]) {

}

case class Topo(hosts: Map[String, Set[String]], switches: Map[String, Set[String]], links: List[List[String]])

object Topology {
  //  val tree = json.JSON.parseFull(Source.fromResource("topo.json").mkString)
  val g = Graph.empty[Node, LUnDiEdge]
  var allPorts: Set[Port] = Set.empty

  def addSwitch(switch: Switch): Unit = {
    g += switch
  }

  def addHost(host: Host): Unit = {
    g += host
  }

  def getNode(name: String): Node = {
    val n = g.nodes.find(n => n.id == name)
    n match {
      case Some(x) => x.toOuter
      case None => null
    }
  }

  def addLink(node1: Node, node2: Node, link: Link): Unit = {
    g += (node1 ~+ node2) (link)
  }

  def getPortById(id: String): Option[Port] = {
    allPorts.find(p => p.id == id)
  }

  def fromJson(json: String): Option[Topo] = {
    val obj = parser.decode[Topo](json)
    obj match {
      case Left(error) => {
        println("parse json fail")
        println(error.getMessage)
        return null
      }
      case Right(topo) => {
        for ((id, ports) <- topo.switches) {
          val sw = new Switch(id)
          for (p <- ports) {
            val port = new Port(p, sw)
            allPorts += port
            sw.ports += port
          }
          addSwitch(sw)
        }
        for ((id, ports) <- topo.hosts) {
          val host = new Host(id)
          for (p <- ports) {
            val port = new Port(p, host)
            allPorts += port
            host.ports += port
          }
          addHost(host)
        }
        for (link_info <- topo.links) {
          val p0 = allPorts.find(p => p.id == link_info(0)).orNull
          val p1 = allPorts.find(p => p.id == link_info(1)).orNull
          p0.peer = p1
          p1.peer = p0
          val link = new Link(p0, p1, link_info(2))
          addLink(p0.node, p1.node, link)
        }
        return Some(topo)
      }
    }

  }

  def shortestPath(src: String, dst: String): Path = {
    def n(outer: Node): g.NodeT = g get outer

    var srcSw: Node = null
    var dstSw: Node = null
    if (src.contains(":")) { // e.g. "e1:1"
      srcSw = getNode(src.split(":")(0))
    } else {
      srcSw = getNode(src)
    }

    if (dst.contains(":")) {
      dstSw = getNode(dst.split(":")(0))
    } else {
      dstSw = getNode(dst)
    }
    if (srcSw == null || dstSw == null) {
      return null
    }
    val p: Option[g.Path] = n(srcSw) shortestPathTo n(dstSw)
    p match {
      case Some(value) => {
        toStandardPath(value, getPortById(src).orNull, getPortById(dst).orNull)
      }
      case None => null
    }
  }

  def toStandardPath(p: g.Path, src: Port, dst: Port): Path = {
    val path = new Path
    var pe: PathElement = new PathElement(src, Set())
    p.edges.foreach(e => e.toOuter.label match {
      case l: Link => pe.egresses += l.getPortByNode(pe.ingress.node).orNull
        path.elements += pe
        l.getPortByNode(pe.ingress.node) match {
          case Some(ap) => pe = new PathElement(ap.peer, Set())
          case _ => println("no such port")
        }

    })
    pe.egresses += dst
    path.elements += pe
    path
  }
}
