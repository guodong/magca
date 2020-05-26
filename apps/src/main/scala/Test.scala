//import Main.{macTable, topo}
//
//import scala.annotation.StaticAnnotation
//import scala.reflect.runtime.universe._
//
//class CustomAnnotation(name: String, num: Int) extends StaticAnnotation
//
//@CustomAnnotation("Annotation for Class", 2333)
//class x {
//  def l2_custom(pkt: Packet, ingestion: Port): Path = {
//    if (!macTable.contains(pkt.l2.src)) {
//      macTable += (pkt.l2.src -> ingestion)
//    }
//    if (macTable.contains(pkt.l2.dst)) {
//      val dst = macTable.get(pkt.l2.dst).orNull
//      return Routing.shortestPath(topo, ingestion, dst)
//    } else {
//      return Routing.stp(topo, ingestion)
//    }
//  }
//}
//
//object Main1 extends App {
//
//  // 获取类型注解
//  val tpe: Type = typeOf[x]
//  val symbol: Symbol = tpe.typeSymbol //获取类型符号信息
//  val annotation: Annotation = symbol.annotations.head
//  val tree: Tree = annotation.tree //获取语法树
//
//  println(showRaw(tree)) //打印语法树
//  val Apply(_, Literal(Constant(name: String)) :: Literal(Constant(num: Int)) :: Nil) = tree
//  println(s"Annotation args: name -> $name, num -> $num")
//
//}
