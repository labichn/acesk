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
    assert(parse(var1) === Var('x))
    assert(parse(var2) === Var('y))
    assert(parse(var3) === Var('z))
    assert(parse(var4) === Var('adamalix))
  }


  test("parsing functions") {
    assert(parse(fun1) === Fun(Var('x), Var('x)))
    assert(parse(fun2) === Fun(Var('x), Fun(Var('y), Var('x))))
    assert(parse(fun3) === Fun(Var('foo), Fun(Var('bar), Var('bar))))
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

  test("evaluating values") {
    assert(eval(parse(pri1)).size === Set((Closure(Con(0), MtEnv), MtStore, MtKon)).size)
    assert(eval(parse(pri2)).size === Set((Closure(Con(42), MtEnv), MtStore, MtKon)).size)
  }

  test("parsing letrec") {
    assert(parse("(letrec [(x 0)] x)") === Letrec(List((Var('x), Con(0))), Var('x)))
    assert(parse("(letrec [(x 0)(y 1)] x)") === Letrec(List((Var('x), Con(0)), (Var('y), Con(1))), Var('x)))
  }

  test("evaluating if0") {
    val k0 = MtKon
    val k1 = If(MtEnv, Con(1), Con(2), PosLoc.dummy)
    val s0 = MtStore
    val s1 = s0 bind (PosLoc.dummy, k0)
    assert(eval(parse("(if0 0 1 2)")).size === 4)
    val s2 = s1 bind (PosLoc.dummy, k1)
    val k2 = Op(Ops.Add1, Nil, Nil, PosLoc.dummy)
    assert(eval(parse("(if0 (add1 0) 1 2)")).size === 8)
  }

  test("recursion") {
    def fact(n: Int) = "(letrec [(fact (λn.(if0 n 1 (* n (fact (sub1 n))))))] (fact "+n+"))"
    assert(eval(parse(fact(0))).size === 11)
    assert(eval(parse(fact(1))).size === 64)
    assert(eval(parse(fact(10))).size === 64)
    // Testing these is a nightmare. The equal sizes for fact(1) and fact(10)
    // is a good sign, though.

    val mutual =
      """
(letrec [(true (λx y.x))
         (false (λx y.y))
         (and (λx y.x y false))
         (or (λx y.x true y))
         (not (λx.x false true))
         (evenP (λn.(if0 n true (oddP (- n 1)))))
         (oddP (λn.(if0 n false (evenP (- n 1)))))]
  (oddP 10))
"""
//    assert(eval(parse(mutual)) === Set(???))
  }
            
}
