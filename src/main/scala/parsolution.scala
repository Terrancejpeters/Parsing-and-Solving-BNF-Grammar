import hw.parsing._
import scala.util.parsing.combinator._
import java.lang.Math
object ArithEval extends ArithEvalLike {
	def eval(e: Expr): Double = 
	e match{
		case Add(a,b) => eval(a) + eval(b)
		case Sub(a,b) => eval(a) - eval(b)
		case Mul(a,b) => eval(a) * eval(b)
		case Div(a,b) => eval(a) / eval(b)
		case Num(a) => a
		case Exponent(a,b) => Math.pow(eval(a),eval(b))
	}
}
object ArithParser extends ArithParserLike {
	// number: PackratParser[Double]is defined in ArithParserLike
	lazy val atom: PackratParser[Expr] = number ^^ {case x => Num(x)} | "(" ~ expr ~ ")" ^^ {case _ ~ expr ~ _ => expr}
	lazy val exponent: PackratParser[Expr] = exponent ~ "^" ~ atom ^^ {case a ~ _ ~ b => Exponent(a,b)} | atom
	lazy val add: PackratParser[Expr] = add ~ "+" ~ mul ^^ {case a ~ _ ~ b => Add(a,b)} | add ~ "-" ~ mul ^^ {case a ~ _ ~ b => Sub(a,b)} | mul
	lazy val mul: PackratParser[Expr] = mul ~ "*" ~ exponent ^^ {case a ~ _ ~ b => Mul(a,b)} | mul ~ "/" ~ exponent ^^ {case a ~ _ ~ b => Div(a,b)} | exponent
	lazy val expr: PackratParser[Expr] = add
}
object ArithPrinter extends ArithPrinterLike {
	def print(e: Expr): String = 
	e match {
		// case Num(n) => "n"
		// case Add(n1, n2) => "(".concat(print(n1)).concat(" + ").concat(print(n2)).concat(")")
		// case Sub(n1, n2) => "(".concat(print(n1)).concat(" - ").concat(print(n2)).concat(")")
		// case Mul(n1, n2) => "(".concat(print(n1)).concat(" * ").concat(print(n2)).concat(")")
		// case Div(n1, n2) => "(".concat(print(n1)).concat(" / ").concat(print(n2)).concat(")")
		// case Exponent(n1, n2) => print(n1).concat(" ^ ").concat(print(n2))
		
		case Num(a) => a.toString()
		case Add(a,b) => print(a) + " + " + print(b)
		case Sub(a,b) => print(a) + " - " + print(b)
		case Mul(a,b) => print(a) + " * " + print(b)
		case Div(a,b) => print(a) + " / " + print(b)
		case Exponent(a,b) => print(a) + " ^ " + print(b)
	}
}