package context

import expression._
import value._

import scala.annotation.tailrec
import scala.util.parsing.combinator._


/*
 * Notes:
 * disjunction reduces to conjunction reduces to equality ... reduces to term
 * if A reduces to B, then B will have higher precedence than A
 * Example: sum reduces to product, so a + b * c = a + (b * c)
 * Had to make some big corrections to numeral regex
 * This could probably have been a singleton
 */

class Jedi1Parsers extends RegexParsers {

  def expression: Parser[Expression] = declaration | conditional | disjunction | failure("Invalid expression")

  def declaration: Parser[Declaration] = "def" ~ identifier ~ "=" ~ expression ^^ {
    case "def"~id~"="~exp => Declaration(id, exp)
  }

  def conditional: Parser[Conditional] = "if" ~ "(" ~ expression ~ ")" ~ expression ~ opt("else" ~ expression) ^^ {
    case "if"~"("~cond~")"~cons~None => Conditional(cond, cons)
    case "if"~"("~cond~")"~cons~Some("else"~alt) => Conditional(cond, cons, alt)
  }

  def  disjunction: Parser[Expression] = conjunction ~ rep("||" ~> conjunction) ^^ {
    case con ~ Nil => con
    case con ~ more => Disjunction(con::more)
  }

  // conjunction ::= equality ~ ("&&" ~ equality)*
  def conjunction: Parser[Expression] = equality ~ rep("&&" ~> equality) ^^ {
    case con ~ Nil => con
    case con ~ more => Conjunction(con::more)
  }
  // equality ::= inequality ~ ("==" ~ inequality)?
  def equality: Parser[Expression] = inequality ~ opt("==" ~ inequality) ^^ {
    case con ~ None => con
    case con ~ some => parseEquality(con, some.get)
  }

  private def parseEquality(some: Expression, list: ~[String, Expression]) = {
    list match {
      case ~("==", exp) =>FunCall(Identifier("equals"), List(some, exp))
    }
  }
  // inequality ::= sum ~ (("<" | ">" | "!=") ~ sum)?
  def inequality: Parser[Expression] = sum ~ opt(("<"|">"|"!=") ~ sum) ^^ {
    case con ~ None => con
    case con ~ some => parseInequality(con, some.get)
  }

  private def parseInequality(some: Expression, list: ~[String, Expression]) = {
    list match {
      case ~("<", exp) => FunCall(Identifier("less"), List(some, exp))
      case ~(">", exp)=> FunCall(Identifier("more"), List(some, exp))
      case ~("!=", exp)=> FunCall(Identifier("unequals"), List(some, exp))
    }
  }
  // sum ::= product ~ ("+" | "-") ~ product)*
  def sum: Parser[Expression] = product ~ rep(("+"|"-") ~ product) ^^ {
    case p ~ more => parseSums(p, more)
    case p ~ Nil => p
  }

  // use tail recursion to imitate left reduce
  // parses a - b + c into add(sub(a, b), c)
  @tailrec
  private def parseSums(result: Expression, unseen: List[String ~ Expression]): Expression = {
    def combiner(exp: Expression, next: String~Expression) =
      next match {
        case "+" ~ p => FunCall(Identifier("add"), List(exp, p))
        case "-" ~ p => FunCall(Identifier("sub"), List(exp, p))
      }
    if (unseen == Nil) result
    else parseSums(combiner(result, unseen.head), unseen.tail)
  }

  // product ::= term ~ (("*" | "/") ~ term)*
  def product: Parser[Expression] = term ~ rep(("*"|"/") ~ term) ^^ {
    case t ~ terms => parseProduct(t, terms)
  }

  @tailrec
  private def parseProduct(result: Expression, unseen: List[String ~ Expression]): Expression = {
    def combiner(exp: Expression, next: String~Expression) =
      next match {
        case "*" ~ p => FunCall(Identifier("mul"), List(exp, p))
        case "/" ~ p => FunCall(Identifier("div"), List(exp, p))
      }
    if(unseen == Nil) result
    else parseProduct(combiner(result, unseen.head), unseen.tail)
  }

  def term: Parser[Expression]  = funCall | literal | "("~>expression<~")"

  def literal: Parser[Expression] = boole | inexact | exact | chars | identifier


  // chars ::= any characters bracketed by quotes
  def chars: Parser[Chars] = """\"[^"]+\"""".r ^^ (characters => Chars(characters.substring(1, characters.length - 1)))

  // exact ::= 0|(\+|-)?[1-9][0-9]*
  def exact: Parser[Exact] = """0|(\\+|-)?[1-9][0-9]*""".r ^^ (exact => Exact(exact.toInt))
  // inexact ::= (\+|-)?[0-9]+\.[0-9]+
  def inexact: Parser[Inexact] = """(\+|-)?[0-9]*\.[0-9]+""".r ^^ (inexact => Inexact(inexact.toDouble))
  // boole ::= true|false
  def boole: Parser[Boole] = """true|false""".r ^^ (boole => Boole(boole.toBoolean))
  // identifier ::= [a-zA-Z][a-zA-Z0-9]*
  def identifier: Parser[Identifier] = """[a-zA-Z][a-zA-Z0-9]*""".r ^^ (identifier => Identifier(identifier))
  // funCall ::= identifier ~ operands
  def funCall: Parser[FunCall] = identifier ~ operands ^^ {
    case identifier ~ operands => FunCall(identifier, operands)
  }

  // operands ::= "(" ~ (expression ~ ("," ~ expression)*)? ~ ")"
  def operands: Parser[List[Expression]] = "(" ~> opt(expression ~ rep("," ~> expression)) <~ ")" ^^ {
    case None => Nil
    case Some(con ~ Nil) => List(con)
    case Some(con ~ more) => con::more
  }
}

