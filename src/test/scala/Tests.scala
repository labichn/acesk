package dvh.cek

import org.scalatest.FunSuite

import Ops._

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
    assert(eval(parse(pri1)) === Set((Closure(Con(0), EmptyEnv), EmptyStore, EmptyKon)))
  }

/*  test("evaluating expressions") {
    assert(eval(parse("("+fun1+" "+pri1+")")) === Con(0))
    assert(eval(parse("(isZero ((λ x . x) 0))")) === Fun(Var('x), Fun(Var('y), Var('x))))
    assert(eval(parse("(isZero (- 5 (+ 5 ((lambda x . x) 0))))")) === Fun(Var('x), Fun(Var('y), Var('x))))
    assert(eval(parse("(isZero (* 42 ((λ x . x) (- 6 5))))")) === Fun(Var('x), Fun(Var('y), Var('y))))
    assert(eval(parse("("+fun4+" 0)")) === Con(1))
    assert(eval(parse("("+fun4+" 1)")) === Con(0))
    assert(eval(parse("("+fun5+" 0)")) === Con(1))
    assert(eval(parse("("+fun5+" 1)")) === Con(0))
    assert(eval(parse("(if0 0 1 2)")) === Con(1))
    assert(eval(parse("(if0 1 1 2)")) === Con(2))
  }

  test("variable scoping") {
    assert(eval(parse("((λx.((λx.(+ x 1)) (+ x 1))) 0)")) === Con(2))
    assert(eval(parse("((λx.((λx.((λx.(+ x 1)) (+ x 1))) (+ x 1))) 0)")) === Con(3))
  }

  test("evaluating booleans") {
    val falsuh: String = "(λx y.y)"
    val tuhrue: String = "(λx y.x)"
    val ifthunk: String = "(λp t e.p t e 0)"
    val and: String = "(λx y.x y "+falsuh+")"
    val or: String = "(λx y.x "+tuhrue+" y)"
    val not: String = "(λx.x "+falsuh+" "+tuhrue+")"
    assert(parse("(λp t e.p t e 0)") === Fun(Var('p), Fun(Var('t), Fun(Var('e), App(App(App(Var('p), Var('t)), Var('e)), Con(0))))))
    assert(parse("(λx y.y)") === Fun(Var('x), Fun(Var('y), Var('y))))
    assert(eval(parse("((λp t e.p t e 0) (λx y.x) (λx.0) (λx.1))")) === Con(0))
    assert(eval(parse("("+ifthunk+" "+falsuh+" (λx.0) (λx.1))")) === Con(1))
    assert(eval(parse("("+ifthunk+" ("+not+" "+falsuh+") (λx.0) (λx.1))")) === Con(0))
  }

  test("parsing letrec") {
    assert(parse("(letrec [(x 0)] x)") === Letrec(List((Var('x), Con(0))), Var('x)))
    assert(parse("(letrec [(x 0)(y 1)] x)") === Letrec(List((Var('x), Con(0)), (Var('y), Con(1))), Var('x)))
  }

  test("evaluating set and letrec") {
    val falsuh: String = "(λx y.y)"
    val tuhrue: String = "(λx y.x)"
    val ifthunk: String = "(λp t e.p t e 0)"
    val and: String = "(λx y.x y "+falsuh+")"
    val or: String = "(λx y.x "+tuhrue+" y)"
    val not: String = "(λx.x "+falsuh+" "+tuhrue+")"
    val fact: String =
      "(λn.ifthunk (isZero n) "+
                  "(λx.1) "+
                  "(λx.(* n (fact (- n 1)))))"
    val oddP: String =
      "(λn.ifthunk (isZero n) "+
                  "(λx.false) "+
                  "(λx.evenP (- n 1)))"
    val evenP: String =
      "(λn.ifthunk (isZero n) "+
                  "(λx.true) "+
                  "(λx.oddP (- n 1)))"
    val mutual =
      """
(letrec [(true (λx y.x))
         (false (λx y.y))
         (ifthunk (λp t e.p t e 0))
         (and (λx y.x y false))
         (or (λx y.x true y))
         (not (λx.x false true))
         (evenP (λn.ifthunk (isZero n) (λx.true) (λx.oddP (- n 1))))
         (oddP (λn.ifthunk (isZero n) (λx.false) (λx.evenP (- n 1))))]
  (oddP 21))
"""
    assert(eval(parse(mutual)) === parse("(λx y.x)"))
    val test: String =
      "(letrec [(true "+tuhrue+")"+
               "(false "+falsuh+")"+
               "(ifthunk "+ifthunk+")"+
               "(and "+and+")"+
               "(or "+or+")"+
               "(not "+not+")"+
               "(fact "+fact+")"+
               "(oddP "+oddP+")"+
               "(evenP "+evenP+")] "+
        "(ifthunk (evenP 8) "+
                 "(λx.fact 4) "+
                 "(λx.fact 3)))"
    assert(eval(parse("("+ifthunk+" ("+not+" ("+and+" "+tuhrue+" "+tuhrue+")) (λx.1) (λx.10))")) === parse("10"))
    assert(eval(parse(test)) === Con(24))
    assert(eval(parse("((λx.((λy.x) (set x (+ x 1)))) 12)")) === Con(13))
    assert(eval(parse("(letrec {(x 0) "+
                               "(y (λx.x))} "+
                        "(y x))")) === Con(0))
    assert(eval(parse("(letrec {(x 0) "+
                               "(y (λx.x)) "+
                               "(z (+ x 3))} "+
                        "(y z))")) === Con(3))
  }*/
            
}
