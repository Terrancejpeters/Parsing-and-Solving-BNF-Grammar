import org.scalatest.prop.Checkers
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import ArithParser._
import ArithEval._
import ArithPrinter._
import scala.util.parsing.combinator._
import hw.parsing._

class TrivialTestSuite extends org.scalatest.FunSuite {
	test ("several objects must be defined") {
		val parser: hw.parsing.ArithParserLike = ArithParser
		val printer: hw.parsing.ArithPrinterLike = ArithPrinter
		val eval: hw.parsing.ArithEvalLike = ArithEval
	}


	test("Just run the shit I guess?"){
		val One = Num(1)
		val Two = Num(2)
		val Three = Num(3)

		val Trial = "1+(3^3)*23+1-5/5"
		val tExpr = parseArith(Trial)
		assert(eval(Add(One,Two)) == eval(Three))
		println(eval(parseArith(Trial)))
		println(parseArith(print(tExpr)))
		println(eval(parseArith(print(tExpr))))
	}
}