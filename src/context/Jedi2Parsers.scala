package context

import expression._

class Jedi2Parsers extends Jedi1Parsers {

  // params parser
  // a parameter list is zero or more comma-separated identifiers bracketed by parentheses:
  // params ::= "(" ~ (identifier ~ ("," ~ identifier)*)? ~ ")"
 // def exact: Parser[Exact] = """0|(\\+|-)?[1-9][0-9]*""".r ^^ (exact => Exact(exact.toInt))

  def params: Parser[List[Identifier]] = "(" ~> opt(identifier ~ rep("," ~> identifier)) <~ ")" ^^ {
    case Some(con ~ Nil) => List(con)
    case Some(con ~ more) => con::more
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

  def block: Parser[Block] = "{" ~ expression ~ rep(";" ~> expression) ~ "}" ^^{
    case "{" ~ expression ~ Nil ~ "}" => Block(List(expression))
    case "{" ~ expression ~ expressions ~ "}" => Block(expression::expressions)
  }


  // override of term parser
  override def term: Parser[Expression]  = lambda | funCall | block | literal | "("~>expression<~")"
}
