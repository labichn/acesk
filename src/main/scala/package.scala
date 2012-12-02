package dvh

import scala.annotation.tailrec
import scala.math.pow

import cek.Ops._

package object cek {

  def parse(s: String): Expression =
    ISWIMParser.parse(ISWIMParser.expr, s).getOrElse(Con(-1))

  def eval(e: Expression): Set[State] =
    avalCesk(Closure(e, EmptyEnv), EmptyStore, EmptyKon, Set.empty, false)

  private def printDebug(ce: Closure, s: Store, k: Kontinuation) {
    println("=========== DEBUG ===========")
    println("<M, ρ>: "+ce)
    println("     σ: "+s)
    println("     κ: "+k)
  }

/*  private def collectGarbage(grays: List[Location],
                             blacks: List[Location],
                             whites: List[Location],
                             s: Store): List[Location] = {
   (grays, blacks, whites) match {
     case (gs, bs, w::ws) if gs.contains(w) => s(w) match {
       case Some(ce) =>
         collectGarbage(gs:::(ce.ll).filter(!(w::bs).contains(_)), w::bs, ws, s)
       case _ => collectGarbage(gs.filter(!(w::bs).contains(_)), w::bs, ws, s)
     }
     case (gs, bs, w::ws) =>
       collectGarbage(gs, bs, ws, s)
     case (_, bs, Nil) => bs
   }
                             }*/
  type State = (Closure, Store, Kontinuation)

  private def avalCesk(ce: Closure, s: Store, k: Kontinuation, ss: Set[State], d: Boolean = false): Set[State] = {
    def evalCesk(ce1: Closure, s1: Store, k1: Kontinuation): Set[State] =
      avalCesk(ce1, s1, k1, ss.+((ce, s, k)), d)
    if (d) { printDebug(ce, s, k) }
//    val bs = collectGarbage(ce.ll:::k.ll, Nil, s.domain, s)
//    (ce, bs.foldLeft[ListStore](EmptyStore)((a, n) => ConsStore(n, s(n), a)), k) match {
    (ce, s, k) match {
      case (Closure(IfZero(p, t, el), e), s, k) =>
        val l = s.next
        evalCesk(Closure(p, e), s.bind(l, k), If(t, el, l))
      case (Closure(Con(0), e), s, If(t, el, l)) =>
        s(l).foldLeft[Set[State]](Set.empty)((a, n) => n match {
            case k: Kontinuation => a++evalCesk(Closure(t, e), s, k)
            case _ => a
        })
      case (Closure(v: Value, e), s, If(t, el, l)) =>
        s(l).foldLeft[Set[State]](Set.empty)((a, n) => n match {
            case k: Kontinuation => a++evalCesk(Closure(el, e), s, k)
            case _ => a
        })
      case (Closure(App(m, n), e), s, k) => // cesk1
        val l = s.next
        evalCesk(Closure(m, e), s.bind(l, k), Ar(Closure(n, e), l))
      case (Closure(Oper(o, m::ms), e), s, k) => // cesk2
        val l = s.next
        evalCesk(Closure(m, e), s.bind(l, k), Op(o, Nil, ms map { n => Closure(n, e) }, l))
      case (Closure(v: Value, e), s, Fn(Closure(Fun(x, m), e1), l)) => // cesk3
        val l1 = s.next
        s(l).foldLeft[Set[State]](Set.empty)((a, n) => n match {
            case k: Kontinuation => a++evalCesk(Closure(m, e1.bind(x, l1)), s.bind(l1, Closure(v, e)), k)
            case _ => a
        })
      case (Closure(v: Value, e), s, Ar(Closure(m, e1), l)) => // cesk4
        evalCesk(Closure(m, e1), s, Fn(Closure(v, e), l))
      case (Closure(v: Value, e), s, Op(o, vcs, Nil, l)) => // cesk5
        s(l).foldLeft[Set[State]](Set.empty)((a, n) => n match {
            case k: Kontinuation => a++evalCesk(Closure(reduce(o, (Closure(v, e)::vcs).reverse), EmptyEnv), s, k)
            case _ => a
        })
      case (Closure(v: Value, e), s, Op(o, vcs, c::cs, l)) => // cesk6
        evalCesk(c, s, Op(o, Closure(v, e)::vcs, cs, l))
      case (Closure(v: Var, e), s, k) =>  // cesk7
        s(e(v)).foldLeft[Set[State]](Set.empty)((a, n) =>
          n match {
          case ce: Closure => a++evalCesk(ce, s, k)
          case _ => a
        })
      case (Closure(SetBang(x, m), e), s, k) => // cesk8
        val l = s.next
        evalCesk(Closure(m, e), s.bind(l, k), St(e(x), l))
      case (Closure(v: Value, e), s, St(l, l1)) => // cesk9
        s(l).foldLeft[Set[State]](Set.empty)((a, n) =>
          n match {
          case ce1: Closure => a++s(l1).foldLeft[Set[State]](Set.empty)((a1, n1) =>
            n1 match {
              case k: Kontinuation => a1++evalCesk(ce1, s.rebind(l, ce), k)
              case _ => a1
            })
            case _ => a
          })
      case (Closure(Letrec((x, m)::xms, n), e), s, k) => // al3
        val l = s.next
        val e1 = e.bind(x, l)
        val l1 = s.next
        evalCesk(Closure(m, e1), s.alloc(l).bind(l1, k), Lr(x::xms.map(_._1), Nil, xms.map(_._2), e1, n, l))
      case (Closure(v: Value, e), s, Lr(x::y::xs, vvs, m::ms, e1, n, l)) =>
        val l1 = s.next
        val e2 = e1.bind(y, l1)
        evalCesk(Closure(m, e2), s.rebind(e(x), Closure(v, e2)).alloc(l1), Lr(y::xs, (x, v)::vvs, ms, e2, n, l))
      case (Closure(v: Value, e), s, Lr(x::Nil, vvs, Nil, e1, body, l)) =>
        val s1 = ((x, v)::vvs).reverse.foldRight(s)((n, a) => a.rebind(e1(n._1), Closure(n._2, e1)))
        s(l).foldLeft[Set[State]](Set.empty)((a, n) => n match {
            case k: Kontinuation => a++evalCesk(Closure(body, e1), s1, k)
            case _ => a
        })
      case (Closure(v: Value, e), s, EmptyKon) => { // al1
        if (d) println("=> "+v)
        ss.+((ce, s, k))
      }
      case (ce, s, k) =>
        throw new RuntimeException("Bad code!")
    }
  }

  private def reduce(o: Ops, vs: List[Closure]): Value = (o, vs map { c => c.m }) match {
    case (Add1, Con(n)::Nil) => Num
    case (Sub1, Con(n)::Nil) => Num
    case (Add, Con(m)::Con(n)::Nil) => Num
    case (Sub, Con(m)::Con(n)::Nil) => Num
    case (Mul, Con(m)::Con(n)::Nil) => Num
    case (Exp, Con(m)::Con(n)::Nil) => Num
    case _ => throw new RuntimeException("Bad primitive application: "+Ops.toString(o)+" "+vs)
  }

}
