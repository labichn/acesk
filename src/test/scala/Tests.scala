package dvh.acesk

import org.scalatest.FunSuite

import Ops._

import scala.util.parsing.input.Position

class TestSuite extends FunSuite {

  val var1: String = "x"
  val var2: String = "y"
  val var3: String = "z"
  val var4: String = "adamalix"
  val fun1: String = "(lambda x . x)"
  val fun2: String = "(λ x . (lambda y . x))"
  val fun3: String = "(lambda foo . (λ bar . bar))"
  val fun4: String = "(λ x . ((((λ p . (λ a . (λ x . ((p a) x)))) " +
                               "(isZero x)) " +
                              "(add1 x)) " +
                             "0))"
  val fun5: String = "(λx.(λp a x.p a x) (isZero x) (add1 x) 0)"
  val pri1: String = "0"
  val pri2: String = "42"
  val pri3: String = "-3"
  val app1: String = "("+fun1+" "+pri1+")"
  val app2: String = "("+var1+" "+pri2+")"
  val app3: String = "("+fun3+" "+pri3+")"
  val pop1: String = "(add1 "+pri1+")"
  val pop2: String = "(^ "+pri3+" "+pri1+")"
  val pop3: String = "(- "+pri2+" "+pri1+")"

  test("parsing if0") {
    assert(parse("(if0 0 1 2)") === IfZero(Con(0), Con(1), Con(2)))
  }

  test("parsing variables") {
    assert(parse(var1) === Var("x"))
    assert(parse(var2) === Var("y"))
    assert(parse(var3) === Var("z"))
    assert(parse(var4) === Var("adamalix"))
  }


  test("parsing functions") {
    assert(parse(fun1) === Lam(Var("x"), Var("x")))
    assert(parse(fun2) === Lam(Var("x"), Lam(Var("y"), Var("x"))))
    assert(parse(fun3) === Lam(Var("foo"), Lam(Var("bar"), Var("bar"))))
    assert(parse(fun4) === parse(fun5))
  }


  test("parsing primitives") {
    assert(parse(pri1) === Con(0))
    assert(parse(pri2) === Con(42))
    assert(parse(pri3) === Con(-3))
  }


  test("parsing applications") {
    assert(parse(app1) === App(parse(fun1), parse(pri1)))
    assert(parse(app2) === App(parse(var1), parse(pri2)))
    assert(parse(app3) === App(parse(fun3), parse(pri3)))
  }


  test("parsing primitive operations") {
    assert(parse(pop1) === Oper(Add1, List(Con(0))))
    assert(parse(pop2) === Oper(Exp, List(Con(-3), Con(0))))
    assert(parse(pop3) === Oper(Sub, List(Con(42), Con(0))))
  }

  test("parsing letrec") {
    assert(parse("(letrec [(x 0)] x)") === Letrec(List((Var("x"), Con(0))), Var("x")))
    assert(parse("(letrec [(x 0)(y 1)] x)") === Letrec(List((Var("x"), Con(0)), (Var("y"), Con(1))), Var("x")))
  }
  object dummy extends Position {
    def line = -1; def column = -1
    protected def lineContents = ""
  }

  test("analyzing values") {
    assert(analyze(parse(pri1)).size === 3)
    assert(analyze(parse(pri2)).size === 3)
  }

  def ansFilter(s: Set[State]) =
    s.filter(_ match { case Ans(_, _) => true case _ => false })

  test("analizing if0") {
    val set0 = analyze(parse("(if0 0 1 2)"))
    val ans0 = ansFilter(set0)
    assert(set0.size === 6)
    assert(ans0.size === 1)
    val set1 = analyze(parse("(if0 (add1 0) 1 2)"))
    val ans1 = ansFilter(set1)
    assert(set1.size === 11)
    assert(ans1.size === 2)
  }

  test("recursion") {
    def fact(n: Int) = "(letrec [(fact (λn.(if0 n 1 (* n (fact (sub1 n))))))] (fact "+n+"))"
    val set0 = analyze(parse(fact(0)))
    val ans0 = ansFilter(set0)
    assert(set0.size === 15)
    assert(ans0.size === 1)
    assert(analyze(parse(fact(1))).size === 53)
    assert(analyze(parse(fact(10))).size === 53)
    // Testing these is a nightmare. The equal sizes for fact(1) and fact(10)
    // is a good sign, though.

    def mutual(n: Int) =
      """
(letrec [(true (λx y.x))
         (false (λx y.y))
         (and (λx y.x y false))
         (or (λx y.x true y))
         (not (λx.x false true))
         (evenP (λn.(if0 n true (oddP (- n 1)))))
         (oddP (λn.(if0 n false (evenP (- n 1)))))]
  (oddP """+n+"""))
"""
    assert(analyze(parse(mutual(0))).size === 27)
    assert(analyze(parse(mutual(1))).size === 92)
    assert(analyze(parse(mutual(10))).size === 92)
  }
            
}
