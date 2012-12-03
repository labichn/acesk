package dvh.acesk

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
    assert(eval(parse(pri2)) === Set((Closure(Con(42), EmptyEnv), EmptyStore, EmptyKon)))
  }

  test("parsing letrec") {
    assert(parse("(letrec [(x 0)] x)") === Letrec(List((Var('x), Con(0))), Var('x)))
    assert(parse("(letrec [(x 0)(y 1)] x)") === Letrec(List((Var('x), Con(0)), (Var('y), Con(1))), Var('x)))
  }

  test("evaluating if0") {
    val k0 = EmptyKon
    val k1 = If(EmptyEnv, Con(1), Con(2), Location(0))
    val s0 = EmptyStore
    val s1 = ConsStore(Location(0), Set(k0), s0)
    assert(eval(parse("(if0 0 1 2)")) ===
      Set((Closure(IfZero(Con(0), Con(1), Con(2)), EmptyEnv), s0, k0),
          (Closure(Con(0), EmptyEnv), s1, k1),
          (Closure(Con(1), EmptyEnv), s1, k0)))
    val s2 = ConsStore(Location(1), Set(k1), s1)
    val k2 = Op(Ops.Add1, Nil, Nil, Location(1))
    assert(eval(parse("(if0 (add1 0) 1 2)")) ===
      Set((Closure(IfZero(Oper(Ops.Add1, List(Con(0))), Con(1), Con(2)), EmptyEnv), s0, k0),
          (Closure(Oper(Ops.Add1, List(Con(0))), EmptyEnv), s1, k1),
          (Closure(Con(0), EmptyEnv), s2, k2),
          (Closure(Num, EmptyEnv), s2, k1),
          (Closure(Con(1), EmptyEnv), s2, k0),
          (Closure(Con(2), EmptyEnv), s2, k0)))
  }

  test("mutual") {
    val mutual =
      """
(letrec [(true (λx y.x))
         (false (λx y.y))
         (and (λx y.x y false))
         (or (λx y.x true y))
         (not (λx.x false true))
         (evenP (λn.(if0 n true (oddP (- n 1)))))
         (oddP (λn.(if0 n false (evenP (- n 1)))))]
  (oddP 1))
"""
    assert(eval(parse(mutual)) === parse("(lambda x y.x)"))
  }
            
}
/*
(<(add1 0) ∅>,<function1>,if(1, 2, [0]))
(<2 ∅>,<function1>,mt)
(<0 ∅>,<function1>,op(add1, (), (), κ))
(<(if0 (add1 0) 1 2) ∅>,mt,mt)
(<allyournumarebelongtous ∅>,<function1>,if(1, 2, [0])

(<(add1 0) ∅>,<function1>,if(1, 2, [0]))
(<2 ∅>,<function1>,if(1, 2, [0]))
(<0 ∅>,<function1>,op(add1, (), (), κ))
(<1 ∅>,<function1>,if(1, 2, [0]))
(<(if0 (add1 0) 1 2) ∅>,mt,mt)
(<allyournumarebelongtous ∅>,<function1>,if(1, 2, [0])))
*/
