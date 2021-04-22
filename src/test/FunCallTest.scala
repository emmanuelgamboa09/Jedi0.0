package test

import context._
import expression._
import value._


object FunCallTest extends App {
  val globalEnvironment = new Environment
  val operands = List(Exact(6), Exact(7))
  var exp = FunCall(Identifier("add"), operands)
  println(exp.execute(globalEnvironment))
  exp = FunCall(Identifier("less"), operands)
  println(exp.execute(globalEnvironment))
  exp = FunCall(Identifier("mul"), operands)
  println(exp.execute(globalEnvironment))
}