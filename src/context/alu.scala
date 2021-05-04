package context

import expression.Identifier
import value._

import scala.annotation.tailrec

case object alu {

  def execute(opcode: Identifier, args: List[Value]): Value = opcode.name match {
    case "add" => add(args) // n-ary
    case "mul" => mul(args) // n-ary
    case "sub" => sub(args) // n-ary
    case "div" => div(args) // n-ary
    case "less" => less(args) // binary
    case "equals" => same(args) // binary
    case "more" => more(args) // binary
    case "unequals" => unequals(args) // binary
    case "not" => not(args) // unary
    //     TBC
    case "write" => write(args)
    case "list" => list(args)
    case "car" => car(args)
    case "cdr" => cdr(args)
    case "var" => makeVar(args)
    case "dereference" => dereference(args)
    case "nil" => getEmpty(args)
    case _ => throw new UndefinedException(opcode)
  }

  private def add(args: List[Value]): Value = {
    @tailrec
    def helper(result: Addable, unseen: List[Value]): Addable =
      if (unseen == Nil) result
      else helper(result + unseen.head, unseen.tail)

    if (args.size < 2) throw new TypeException("2 or more inputs required by add")
    args.head match {
      case _: Addable => helper(args.head.asInstanceOf[Addable], args.tail)
      case _ => throw new TypeException("Inputs to + must be addable")
    }
  }

  private def mul(args: List[Value]): Value = {
    @tailrec
    def helper(result: Numeric, unseen: List[Value]): Numeric = {
      if (unseen == Nil) result
      else helper(result * unseen.head, unseen.tail)
    }

    if (args.size < 2) throw new TypeException("2 or more inputs required by mul")
    args.head match {
      case _: Numeric => helper(args.head.asInstanceOf[Numeric], args.tail)
      case _ => throw new TypeException("Inputs to * must be numeric")
    }
  }

  private def sub(args: List[Value]): Value = {
    @tailrec
    def helper(result: Numeric, unseen: List[Value]): Value = {
      if (unseen == Nil) result
      else helper(result - unseen.head, unseen.tail)
    }

    if (args.size < 2) throw new TypeException("2 or more inputs required by sub")
    args.head match {
      case _: Numeric => helper(args.head.asInstanceOf[Numeric], args.tail)
      case _ => throw new TypeException("Inputs to + must be numeric")
    }
  }

  private def div(args: List[Value]): Value = {
    @tailrec
    def helper(result: Numeric, unseen: List[Value]): Value = {
      if (unseen == Nil) result
      else if (unseen.head == 0) throw new IllegalValueException("Can't Divide by 0")
      else helper(result / unseen.head, unseen.tail)
    }

    if (args.size < 2) throw new TypeException("2 or more inputs required by div")
    args.head match {
      case _: Numeric => helper(args.head.asInstanceOf[Numeric], args.tail)
      case _ => throw new TypeException("Inputs to / must be numeric")
    }
  }

  private def same(args: List[Value]): Value = {
    if (args.size != 2) throw new TypeException("2 inputs required by ==")
    if (!args.head.isInstanceOf[Ordered[Value]]) throw new TypeException("Inputs to same must be orderable")
    Boole(args.head.equals(args(1)))
  }

  private def unequals(args: List[Value]): Value = {
    if (args.size != 2) throw new TypeException("2 inputs required by !=")
    if (!args.head.isInstanceOf[Ordered[Value]]) throw new TypeException("Inputs to unequals must be orderable")
    Boole(!args.head.equals(args(1)))
  }


  private def less(args: List[Value]): Value = {
    if (args.size != 2) throw new TypeException("2 inputs required by <")
    if (!args.head.isInstanceOf[Ordered[Value]]) throw new TypeException("Inputs to less must be orderable")
    Boole(args.head.asInstanceOf[Ordered[Value]] < args(1))
  }

  private def more(args: List[Value]): Value = {
    if (args.size != 2) throw new TypeException("2 inputs required by >")
    if (!args.head.isInstanceOf[Ordered[Value]]) throw new TypeException("Inputs to more must be orderable")
    Boole(args.head.asInstanceOf[Ordered[Value]] > args(1))
  }

  private def not(args: List[Value]): Value = {
    if (args.size != 1) throw new TypeException("Only 1 input required by !")
    if (!args.head.isInstanceOf[Boole]) throw new TypeException("Input to not must be a value")
    !args.head.asInstanceOf[Boole]
  }

  private def write(value: List[Value]): Value = {
    for (i <- value.indices)
      println(value(i))
    Notification.DONE
  }

  private def list(args: List[Value]): Value = {
    @tailrec
    def helper(first: Pair, result: Pair, unseen: List[Value]): Value = {
      if (unseen == Nil) {
        result.second = empty
        return first
      }
      val temp = Pair(unseen.head)
      result.second = temp
      helper(first, temp, unseen.tail)
    }

    val temp = Pair(args.head, Pair())
    helper(temp, temp, args.tail)
  }

  private def car(args: List[Value]): Value = {
    if (args.size != 1) throw new TypeException("Only 1 input required by car");
    if (!args.head.isInstanceOf[Pair]) throw new TypeException("Must be a pair or list inorder to use cars")
    args.head.asInstanceOf[Pair].first
  }

  private def cdr(args: List[Value]): Value = {
    if (args.size != 1) throw new TypeException("Only 1 input required by car");
    if (!args.head.isInstanceOf[Pair]) throw new TypeException("Must be a pair or list inorder to use cars")
    args.head.asInstanceOf[Pair].second
  }

  private def makeVar(args: List[Value]): Value = {
    if (args.size != 1) throw new TypeException("Only 1 assignment is allowed per variable")
    Variable(args.head)
  }

  private def dereference(args: List[Value]): Value = {
    if (!args.head.isInstanceOf[Variable]) throw new TypeException("Can only dereference variables")
    args.head.asInstanceOf[Variable].content
  }

  private def getEmpty(args: List[Value]): Value = {
    if (args.nonEmpty)
      throw new TypeException("nil() takes in no parameters")
    empty
  }
}
