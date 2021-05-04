package context

import expression._

class Jedi2Parsers extends Jedi1Parsers {

  // params parser
  // a parameter list is zero or more comma-separated identifiers bracketed by parentheses:
  // params ::= "(" ~ (identifier ~ ("," ~ identifier)*)? ~ ")"
  // def exact: Parser[Exact] = """0|(\\+|-)?[1-9][0-9]*""".r ^^ (exact => Exact(exact.toInt))

  def params: Parser[List[Identifier]] = "(" ~> opt(identifier ~ rep("," ~> identifier)) <~ ")" ^^ {
    case Some(con ~ Nil) => List(con)
    case Some(con ~ more) => con :: more
    case _ => Nil
  }

  // lambda parser
  // lambda ::= "lambda" ~ params ~ expression
  def lambda: Parser[Lambda] = "lambda" ~ params ~ expression ^^ {
    case "lambda" ~ params ~ expression => Lambda(params, expression)
  }
  // block parser
  // a block is one or more semi-colon separated expressions bracketed by curly braces:
  // block ::= "{" ~ expression ~ (";" ~ expression)* ~ "}"

  def block: Parser[Block] = "{" ~ expression ~ rep(";" ~> expression) ~ "}" ^^ {
    case "{" ~ expression ~ Nil ~ "}" => Block(List(expression))
    case "{" ~ expression ~ expressions ~ "}" => Block(expression :: expressions)
  }

  def freeze: Parser[MakeThunk] = "freeze" ~ "(" ~ expression ~ ")" ^^ {
    case "freeze" ~ "(" ~ expression ~ ")" => MakeThunk(expression)
  }

  def delay: Parser[MakeText] = "delay" ~ "(" ~ expression ~ ")" ^^ {
    case "delay" ~ "(" ~ expression ~ ")" => MakeText(expression)
  }

  def cons: Parser[MakePair] = "cons" ~ "(" ~ expression ~ "," ~ expression ~ ")" ^^ {
    case "cons" ~ "(" ~ expression1 ~ "," ~ expression2 ~ ")" => MakePair(expression1, expression2)
  }

  def list: Parser[Expression] = "list" ~ "(" ~ expression ~ rep("," ~> expression) ~ ")" ^^ {
    case "list" ~ "(" ~ expression ~ expressions ~ ")" => FunCall(Identifier("list"), expression :: expressions)
  }

  def car: Parser[Expression] = "car" ~ "(" ~ expression ~ ")" ^^ {
    case "car" ~ "(" ~ expression ~ ")" => FunCall(Identifier("car"), List(expression))
  }

  def cdr: Parser[Expression] = "cdr" ~ "(" ~ expression ~ ")" ^^ {
    case "cdr" ~ "(" ~ expression ~ ")" => FunCall(Identifier("cdr"), List(expression))
  }


  // override of term parser
  override def term: Parser[Expression] = cons | car | cdr | list | lambda | freeze | delay | funCall | block | literal | "(" ~> expression <~ ")"
}
