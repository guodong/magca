import network.Path

object Routing {
  //  def shortestPath(src: Port, dst: Port): Path = {
  //    new Path
  //  }
  def shortestPath(src: Any, dst: Any): Path = {
    val p = new Path
    p.nm = "sssp"
    p
  }

  def stp(root: Any): Path = {
    val p = new Path
    p.nm = "stp"
    p
  }
}
