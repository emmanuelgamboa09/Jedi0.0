package context

import expression.Identifier
import value._

import scala.annotation.tailrec

object alu {

  def execute(opcode: Identifier, args: List[Value]): Value = opcode.name match {
    case "add" => add(args)            // n-ary
    case "mul" => mul(args)            // n-ary
    case "sub" => sub(args)            // n-ary
    case "div" => div(args)            // n-ary
    case "less" => less(args)          // binary
    case "equals" => same(args)        // binary
    case "more" => more(args)          // binary
    case "unequals" => unequals(args)  // binary
    case "not" => not(args)            // unary
//     TBC
  }

  private def add(args: List[Value]): Value = {
    @tailrec
    def helper(result: Addable, unseen: List[Value]): Addable =
      if(unseen == Nil) result
      else helper(result + unseen.head, unseen.tail)

    if(args.size < 2) throw new TypeException("2 or more inputs required by add")
    args.head match {
      case _: Addable => helper(args.head.asInstanceOf[Addable], args.tail )
      case _ => throw new TypeException("Inputs to + must be addable")
    }
  }

  private def mul(args: List[Value]) : Value = {
    @tailrec
    def helper(result: Numeric, unseen: List[Value]): Numeric = {
      if(unseen == Nil) result
      else helper(result * unseen.head, unseen.tail)
    }

    if(args.size < 2) throw new TypeException("2 or more inputs required by mul")
    args.head match {
      case _: Numeric => helper(args.head.asInstanceOf[Numeric], args.tail )
      case _ => throw new TypeException("Inputs to * must be numeric")
    }
  }

  private def sub(args: List[Value]) : Value = {
    @tailrec
    def helper(result: Numeric, unseen: List[Value]): Value = {
      if(unseen == Nil) result
      else helper(result - unseen.head, unseen.tail)
    }

    if(args.size < 2) throw new TypeException("2 or more inputs required by sub")
    args.head match {
      case _: Numeric => helper(args.head.asInstanceOf[Numeric], args.tail )
      case _ => throw new TypeException("Inputs to + must be numeric")
    }
  }

  private def div(args: List[Value]) : Value = {
    @tailrec
    def helper(result: Numeric, unseen: List[Value]): Value = {
      if(unseen == Nil) result
      else if(unseen.head == 0) throw new IllegalValueException("Can't Divide by 0")
      else helper(result / unseen.head, unseen.tail)
    }

    if(args.size < 2) throw new TypeException("2 or more inputs required by div")
    args.head match {
      case _: Numeric => helper(args.head.asInstanceOf[Numeric], args.tail )
      case _ => throw new TypeException("Inputs to / must be numeric")
    }
  }

  private def same(args: List[Value]): Value ={
    if(args.size != 2) throw new TypeException("2 inputs required by ==")
    if(!args.head.isInstanceOf[Ordered[Value]]) throw new TypeException("Inputs to same must be orderable")
    Boole(args.head.equals(args(1)))
  }

  private def unequals(args: List[Value]): Value ={
    if(args.size != 2) throw new TypeException("2 inputs required by !=")
    if(!args.head.isInstanceOf[Ordered[Value]]) throw new TypeException("Inputs to unequals must be orderable")
    Boole(!args.head.equals(args(1)))
  }


  private def less(args: List[Value]): Value = {
    if(args.size != 2) throw new TypeException("2 inputs required by <")
    if(!args.head.isInstanceOf[Ordered[Value]]) throw new TypeException("Inputs to less must be orderable")
    Boole(args.head.asInstanceOf[Ordered[Value]] < args(1))
  }

  private def more(args: List[Value]): Value = {
    if(args.size != 2) throw new TypeException("2 inputs required by >")
    if(!args.head.isInstanceOf[Ordered[Value]]) throw new TypeException("Inputs to more must be orderable")
    Boole(args.head.asInstanceOf[Ordered[Value]] > args(1))
  }

  private def not(args: List[Value]): Value = {
    if(args.size != 1) throw new TypeException("Only 1 input required by !")
    if(!args.head.isInstanceOf[Boole]) throw new TypeException("Input to not must be a value")
    Boole(!args.head.asInstanceOf[Boole].value)
  }
  // etc.
}
