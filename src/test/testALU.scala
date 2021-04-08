package test

import context._
import expression._
import value._

object testALU extends App {
  try {
    println(alu.execute(Identifier("add"), List(Exact(5), Exact(6), Exact(7))))     // 18
    println(alu.execute(Identifier("mul"), List(Exact(5), Exact(6), Exact(-7))))     // -210
    println(alu.execute(Identifier("sub"), List(Exact(5), Exact(6), Exact(7))))     // -8
    println(alu.execute(Identifier("add"), List(Chars("abc"), Exact(6), Exact(7)))) // abc67
    println(alu.execute(Identifier("div"), List(Inexact(1), Exact(6), Exact(8))))  //0.020833...
    println(alu.execute(Identifier("less"), List(Chars("abc"), Chars("def"))))  // true
    println(alu.execute(Identifier("equals"), List(Chars("abc"), Chars("abc"))))  // true
    println(alu.execute(Identifier("unequals"), List(Chars("abc"), Chars("abc"))))  // false
    println(alu.execute(Identifier("not"), List(Boole(true))))   // false
    println(alu.execute(Identifier("more"), List(Chars("abc"), Chars("def"))))          //false
  } catch {
    case e: Exception => println(e)
  }
}