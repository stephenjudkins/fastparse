package test.fastparse
import fastparse._
import utest._

object RegressionTests extends TestSuite {

  import Parsed.{Success, Failure}

  val tests = Tests {

    'unitClassCastWhileCombiningParsers - {
      import SingleLineWhitespace._

      sealed trait Expr
      case class And(a: Expr, b: Expr) extends Expr
      case class Or(a: Expr, b: Expr) extends Expr
      case class Lit(a: String) extends Expr


      def parseLit[_: P]:P[Expr] = P(CharIn("a-z").!.map(Lit(_)))

      val operatorsByPredecence: List[(String, (Expr, Expr) => Expr)] = List(
        "AND" -> (And(_, _)),
        "OR" -> (Or(_, _))
      )
      def parseExpr[_: P] =  P(operatorsByPredecence.foldLeft(parseLit) { case (e, (s, o)) =>
        e.rep(1, IgnoreCase(s)).map(_.reduce(o(_, _)))
      })

      val s = "a AND b"
      assert(parse(s, parseExpr(_)) == Success(And(Lit("a"), Lit("b")), 7))
    }
  }

}
