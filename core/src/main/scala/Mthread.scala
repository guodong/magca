import network.Topology

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

@compileTimeOnly("enable macro paradise to expand macro annotations")
class Mthread extends StaticAnnotation {
  println("xx")

  def macroTransform(annottees: Any*): Any = macro MthreadImpl.impl
}


object MthreadImpl {
  val topo_json = "{\n  \"hosts\": {\n    \"h1\": [\"h1\"],\n    \"h2\": [\"h2\"],\n    \"h3\": [\"h3\"]\n  },\n  \"switches\": {\n    \"s1\": [\"s1:1\",\"s1:2\",\"s1:3\"],\n    \"s2\": [\"s2:1\",\"s2:2\",\"s2:3\"],\n    \"s3\": [\"s3:1\",\"s3:2\",\"s3:3\"],\n    \"s4\": [\"s4:1\",\"s4:2\",\"s4:3\"],\n    \"s5\": [\"s5:1\",\"s5:2\",\"s5:3\"]\n  },\n  \"links\": [\n    [\"h1\", \"s1:1\", \"10g\"], [\"s1:2\",\"s5:1\", \"10g\"],[\"s1:3\",\"s4:1\", \"10g\"],\n    [\"h2\", \"s2:1\", \"10g\"], [\"s2:2\",\"s4:2\", \"10g\"],[\"s2:3\",\"s5:2\", \"10g\"],\n    [\"h3\", \"s3:1\", \"10g\"], [\"s3:2\",\"s4:3\", \"10g\"],[\"s3:3\",\"s5:3\", \"10g\"]\n  ]\n}"
  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    Topology.fromJson(topo_json)
    import c.universe._

    // building test IR
    val x = this.getClass.getResource("topo.json")
    println(x.getFile.mkString)
//    Topology.fromJson(x)
    val topo = new UdVariable("topo", "nw topo")
    val ingestion = new Variable("ingestion")
    val ingestion_ports = new SetVariable("ingestion_ports", Set("s1:1", "s2:1", "s3:1"))
//    IR.addInstruction(new Instruction(null, true, "assign", List(ingestion_ports), ingestion))
    val macTable = new MapVariable("macTable", Map("00:00:00:00:00:01"->"s1:1","00:00:00:00:00:02"->"s2:1","00:00:00:00:00:03"->"s3:1"))
    val pktL2Dst = new Variable("pkt.l2.dst")
    val gi = new Variable("gi")
    IR.addInstruction(new Instruction(null, true, "in", List(ingestion_ports, ingestion), gi))
    val g0 = new Variable("g0")
    IR.addInstruction(new Instruction(gi, true, "in", List(macTable, pktL2Dst), g0))
    val dst = new Variable("dst")
    IR.addInstruction(new Instruction(g0, true, "get", List(macTable, pktL2Dst), dst))
    val return0 = new Variable("return0")
    val return1 = new Variable("return1")
    val ret = new Variable("return")
    IR.addInstruction(new Instruction(g0, true, "shortestPath", List(ingestion, dst), return0))
    IR.addInstruction(new Instruction(g0, false, "stp", List(ingestion), return1))
    IR.addInstruction(new Instruction(gi, true, "phi", List(g0, return0, return1), ret))
    IR.dump()

    IR.tabulation()
    IR.dumpTables()
    IR.genDFG()
    IR.dumpDFG()
    IR.explore()
    IR.localize("s5")
    IR.dumpLocalizedTables("s5")
    IR.dumpFinalTable("s5")

    object t extends Traverser {
      override def traverse(tree: c.universe.Tree): Unit = {
        tree match {
          case If(cond, thenp, elsep) =>
            println(showRaw(cond))
            super.traverseTrees(cond.children)
            super.traverseTrees(thenp.children)
            super.traverseTrees(elsep.children)
          case _ => super.traverseTrees(tree.children)
        }
      }

      //        override def traverse(tree: Tree): Unit = {
      //          tree match {
      //            case q"if ($expr)" => {
      //              println(name)
      //            }
      //            case _ =>
      //          }
      //          super.traverseTrees(tree.children)
      //        }
    }
    val inputs = annottees.map(_.tree).toList
    //    t.traverse(inputs(0))
    for (i <- inputs) {
      println(showRaw(i))
      t.traverse(i)
    }
    c.Expr[Any](Literal(Constant()))
    //    val (annottee, expandees) = inputs match {
    //      case (param: Function) :: (rest @ (_ :: _)) => (param, rest)
    //      case (param: TypeDef) :: (rest @ (_ :: _)) => (param, rest)
    //      case _ => (EmptyTree, inputs)
    //    }
    //    println((annottee, expandees))
    //    val outputs = expandees
    //    c.Expr[Any](Block(outputs, Literal(Constant(()))))
    //    val x = Apply(Select(Ident(TermName("x")), TermName("$plus")), List(Literal(Constant(2))))
    //    traverser.traverse(annottees(0).tree)
    //    annottees.map(e => traverser.traverse((e.tree)))
    //      annottees.map(_.tree) match {
    //        case a@ q"$mods def $methodName[..$tpes](...$args): $returnType = { ..$body }" :: Nil => {
    //          traverser.traverse(body(0))
    //        }
    //      }
    //    annottees.tree
    //    println(annottees)
    //    val result = {
    //      annottees.map(_.tree).toList match {
    //        case q"$mods def $methodName[..$tpes](...$args): $returnType = { ..$body }" :: Nil => {
    //          println(showRaw(body))
    //          traverser.traverse(body(0))
    //          q"""$mods def $methodName[..$tpes](...$args): $returnType =  {
    //            val start = System.nanoTime()
    //            val result = {..$body}
    //            val end = System.nanoTime()
    //            println(${methodName.toString} + " elapsed time: " + (end - start) + "ns")
    //            result
    //          }"""
    //        }
    //        case _ => c.abort(c.enclosingPosition, "Annotation @Benchmark can be used only with methods")
    //      }
    //    }
    //    c.Expr[Any](ture)
  }
}
