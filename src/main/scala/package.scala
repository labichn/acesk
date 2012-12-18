package dvh

import acesk.Ops._

import scala.math.pow
import scala.util.parsing.input.Position

package object acesk {

  def parse(s: String): Expression =
    ISWIMParser.parse(ISWIMParser.expr, s).get

  private val sep = ", "

  def printDebug(s: State) {
    println(s match {
      case Ev(m, ρ, σ, κ, cn) => "Ev("+m+sep+ρ+sep+σ+sep+κ+sep+cn+")"
      case Co(κ, v, σ) => "Co("+κ+sep+v+sep+σ+")"
      case Ap(u, v, κ, σ, cn, lb) => "Ap("+u+sep+v+κ+σ+sep+cn+sep+lb+")"
      case Ans(σ, v) => "Ans("+σ+sep+v+")"
    })
  }

  type Addr = String
  type Contour = Int
  type Label = Position
  type Push = (Store, Kontinuation, Contour, Label) => (Addr, Store)
  type Bind = (Environment, Store, Var, Storable, Contour, Label) => (Environment, Store, Contour)
  type Alloc = (Environment, Store, Var, Contour, Label) => (Addr, Environment, Store)

  def analyze(m: Expression): Set[State] = {
    val d = false
    val cn0 = 0
    val s0 = Ev(m, MtEnv, MtStore, MtKont, cn0)
    if (d) { println("\n\nStarting analysis of expr:\n"+m+"\n") }
    val ss = analyze(s0, Set.empty, d)
    if (d) { println(toString(ss)) }
    ss
  }

  private def toString(ss: Set[State]) =
    if (ss.size > 0)
      "State Set: {"+ss.foldLeft("\n}")((a, n) => "\n  "+n+a)
    else "{}"

  private def concat(lb: Label, cn: Contour): Addr =
    String.valueOf(lb.line) + String.valueOf(lb.column) + String.valueOf(cn)

  private val push: Push = (σ, κ, cn, lb) => {
    val l = lb.toString
    (l, σ bind (l, κ))
  }
  private val bind: Bind = (ρ, σ, x, v, cn, lb) => {
    val l = x.name+lb
    (ρ bind (x, l), σ bind (l, v), cn+1)
  }
  private val alloc: Alloc = (ρ, σ, x, cn, lb) => {
    val l = x.name+lb
    (l, ρ bind (x, l), σ alloc l)
  }

  private def analyze(s: State, ss: Set[State], d: Boolean): Set[State] = {
    def ana(s1: State, ss1: Set[State] = ss): Set[State] = {
      if (ss1 contains s) { ss1 }
      else analyze(s1, ss1+s, d)
    }
    if (d) { printDebug(s) }
    s match {
      case Ev(v: Var, ρ, σ, κ, _) => 
        σ(ρ(v)).foldLeft[Set[State]](ss)((a, n) => n match {
          case _: Kontinuation => a
          case v: Storable => ana(Co(κ, v, σ), a)
        })
      case Ev(l: Literal, ρ, σ, κ, _) => ana(Co(κ, l, σ))
      case Ev(Lam(x, e), ρ, σ, κ, _) => ana(Co(κ, Closure(x, e, ρ), σ))
      case Ev(ap@App(m, n), ρ, σ, κ, cn) =>
        push(σ, κ, cn, ap.lb) match {
          case (a, σ1) => ana(Ev(m, ρ, σ1, Ar(n, ρ, a, cn, ap.lb), cn))
        }
      case Ev(i@IfZero(p, t, e), ρ, σ, κ, cn) =>
        push(σ, κ, cn, i.lb) match {
          case (a, σ1) => ana(Ev(p, ρ, σ1, Fi(t, e, ρ, a, cn), cn))
        }
      case Ev(o@Oper(op, m::ms), ρ, σ, κ, cn) =>
        push(σ, κ, cn, o.lb) match {
          case (a, σ1) => ana(Ev(m, ρ, σ1, Op(op, Nil, ms, ρ, a, cn, o.lb), cn))
        }
      case Ev(sb@SetBang(x, m), ρ, σ, κ, cn) =>
        push(σ, κ, cn, sb.lb) match {
          case (a, σ1) => ana(Ev(m, ρ, σ1, St(ρ(x), a, cn, sb.lb), cn))
        }
      case Ev(l@Letrec((x, m)::xms, n), ρ, σ, κ, cn) =>
        alloc(ρ, σ, x, cn, l.lb) match {
          case (a, ρ1, σ1) => push(σ1, κ, cn, l.lb) match {
            case (a1, σ2) => ana(Ev(m, ρ1, σ2, Lr(x::xms.map(_._1), Nil, xms.map(_._2), ρ1, n, a1, cn, l.lb), cn))
          }
        }
      case Ap(Closure(x, body, ρ), v, κ, σ, cn, lb) =>
        bind(ρ, σ, x, v, cn, lb) match {
          case (ρ1, σ1, cn1) => ana(Ev(body, ρ1, σ1, κ, cn1))
        }
      case Co(Ar(e, ρ, a, cn, lb), v, σ) => ana(Ev(e, ρ, σ, Fn(v, a, cn, lb), cn))
      case Co(Fn(u, a, cn, lb), v, σ) => σ(a).foldLeft[Set[State]](ss)((a, n) => n match {
        case k: Kontinuation => ana(Ap(u, v, k, σ, cn, lb), a)
        case _ => a
      })
      case Co(Fi(e0, e1, ρ, a, cn), v, σ) =>
        def next(e: Expression, acc: Set[State]) = σ(a).foldLeft[Set[State]](acc)((a, n) => n match {
          case k: Kontinuation => ana(Ev(e, ρ, σ, k, cn), a)
          case _ => a
        })
        v match {
          case Num => next(e0, ss)++next(e1, ss)
          case Con(0) => next(e0, ss)
          case _ => next(e1, ss)
        }
      case Co(Op(op, vs, Nil, ρ, a, cn, lb), v, σ) => σ(a).foldLeft[Set[State]](ss)((a, n) => n match {
        case k: Kontinuation => ana(Co(k, reduce(op, (v::vs).reverse), σ), a)
        case _ => a
      })
      case Co(Op(op, vs, m::ms, ρ, a, cn, lb), v, σ) =>
        ana(Ev(m, ρ, σ, Op(op, v::vs, ms, ρ, a, cn, lb), cn))
      case Co(St(a, a1, cn, lb), v, σ) => σ(a).foldLeft[Set[State]](ss)((acc, n) => n match {
        case k: Kontinuation => acc
        case v1 => σ(a1).foldLeft[Set[State]](acc)((acc1, n1) => n1 match {
          case k: Kontinuation => ana(Co(k, v1, σ rebind (a, v)), acc1)
          case _ => acc1
        })
      })
      case Co(Lr(x::y::xs, vvs, m::ms, ρ, body, a, cn, lb), v, σ) => alloc(ρ, σ, y, cn, lb) match {
        case (a1, ρ1, σ1) => ana(Ev(m, ρ1, σ1, Lr(y::xs, (x, v)::vvs, ms, ρ1, body, a, cn, lb), cn))
      }
      case Co(Lr(x::Nil, vvs, Nil, ρ, body, a, cn, lb), v, σ) =>
        val σ1 = ((x, v)::vvs).foldLeft(σ)((a, n) => n match {
          case (x, Closure(y, m, _)) => a rebind (ρ(x), Closure(y, m, ρ))
          case (x, l) => a rebind (ρ(x), l)
        })
        σ1(a).foldLeft[Set[State]](ss)((a, n) => n match {
          case k: Kontinuation => ana(Ev(body, ρ, σ1, k, cn), a)
          case _ => a
        })
      case Co(MtKont, v, σ) => ana(Ans(σ, v))
      case Ans(σ, v) => ss+s
      case _ =>
        if (d) { println(toString(ss)) }
        scala.sys.error("terrible.")
    }
  }

  private def reduce(o: Ops, vs: List[Storable]): Storable = (o, vs) match {
    case (Add1, l) if l.length == 1 => Num
    case (Sub1, l) if l.length == 1 => Num
    case (Add, l) if l.length == 2 => Num
    case (Sub, l) if l.length == 2 => Num
    case (Mul, l) if l.length == 2 => Num
    case (Exp, l) if l.length == 2 => Num
    case _ => throw new RuntimeException("Bad primitive application: "+Ops.toString(o)+" "+vs)
  }
/*
  private def collectGarbage(grays: List[PosLoc],
                             blacks: List[PosLoc],
                             whites: List[PosLoc],
                             s: PosStore): List[PosLoc] =
   (grays, blacks, whites) match {
     case (gs, bs, w::ws) if gs.contains(w) =>
       val set = s(w)
       if (set.size > 0) {
         val locs = set.foldLeft[List[PosLoc]](Nil)((a, n) => n.ll:::a)
         collectGarbage(gs:::locs.filter(!(w::bs).contains(_)), w::bs, ws, s)
       } else {
         collectGarbage(gs.filter(!(w::bs).contains(_)), w::bs, ws, s)
       }
     case (gs, bs, w::ws) =>
       collectGarbage(gs, bs, ws, s)
     case (_, bs, Nil) => bs
   }*/

}
