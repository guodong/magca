import scala.reflect.runtime.universe.{TypeTag, typeTag}
import scala.reflect.runtime.{universe => ru}

object M {
  def getTypeTag[T: TypeTag](t: T) = typeTag[T].tpe
  def getTypeTag[T: TypeTag] = ru.typeOf[T]
  def I1[iT, oT](gv: Variable, gs: Boolean, f: iT => oT, inputs: List[Variable], output: Variable): Unit = {
    IR.addInstruction(new Instruction(gv, gs, f.toString(), inputs, output))
//    println(f(1.asInstanceOf[iT]))
  }
  def I2[iT1, iT2, oT](gv: Variable, gs: Boolean, f: (iT1, iT2) => oT, inputs: List[Variable], output: Variable): Unit = {
    println(f.getClass.getName)
    IR.addInstruction(new Instruction(gv, gs, f.toString(), inputs, output))
  }
  def I3[iT1, iT2, iT3, oT](gv: Variable, gs: Boolean, f: (iT1, iT2, iT3) => oT, inputs: List[Variable], output: Variable): Unit = {
    IR.addInstruction(new Instruction(gv, gs, f.toString(), inputs, output))
  }

//  def SI1(gv: Variable, gs: Boolean, t: Any, f: String, inputs: List[Variable], output: Variable): Unit = {
//    IR.addInstruction(new Instruction(gv, gs, f, List(t) ++ inputs, output))
//  }

  class Iobj(val obj: Any) {
    var _gv: Variable = null
    var _gvState: Boolean = true
    var _op: Any = ""
    var _inputs: List[Variable] = List.empty
    var _output: Variable = _
    def op(s: Any): Iobj = {
      _op = s
      this
    }
    def inputs(i: List[Variable]): Iobj = {
      _inputs = i
      this
    }
    def is(i: Variable): Iobj = {
      _output = i
      println(obj)
      this
    }
    def when(i: Variable): Iobj = {
      _gv = i
      this
    }
  }

  def obj(o: Any): Iobj = {
    new Iobj(o)
  }
}
