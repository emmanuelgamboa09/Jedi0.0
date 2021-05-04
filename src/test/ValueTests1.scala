package test

import value._

object ValueTests1 extends App {
  try {
    println(Exact(4) + Exact(9)) // 13
    println(Inexact(9)) // 9.0
    println(Exact(4) + Inexact(9.1)) // 13.1
    println(Exact(4) * Inexact(9.1)) // 36.4
    println(Exact(4) - Inexact(9.1)) // -5.1
    println(Exact(4) / Inexact(9.1)) // 0.43956043956043955
    println(-Exact(4)) // -4
    println(Exact(4) < Inexact(9.1)) // true
    println(Exact(4) == Exact(4)) // true
    println(Exact(4) == Inexact(4)) // true

    println(Chars("abc") + Exact(42)) // abc42
    println(Chars("abc") + Inexact(42)) // abc42.0
    println(Chars("abc") + Chars("def")) // abcdef
    println(Chars("abc") < Chars("def")) // true
    println(Chars("abc") == Chars("abc")) // true
    println(Chars("abcde").subChars(Exact(1), Exact(4))) // bcd
    println(Chars("abcde").size) // 5

    println(Boole(true) && Boole(false) || !Boole.FALSE) // true
    println(Boole.TRUE || {
      print("eager "); Boole.FALSE
    }) // eager true

    //     println(Exact(4) + Chars("abc"))      // context.TypeException: Numeric operand required
    //     println(Inexact(9) / Inexact(0.0))    // context.IllegalValueException: Divide by 0!
  } catch {
    case e: Exception => println(e)
  }

}
