(* expressions *)

exception Runtime ;;

(* Expressions definition and functions *)

type expr = Con of int
            | Var of string
            | Fun of expr * expr
            | App of expr * expr
            | Oper of string * (expr list)
            | Set of expr * expr
            | Letrec of ((expr * expr) list) * expr


(* expr_equal : expr expr -> bool *)
let rec expr_equal m n = match m, n with
  | App (m, n), App (o, p) -> (expr_equal m o) && (expr_equal n p)
  | Con i, Con j -> i = j
  | Var s, Var t -> s = t
  | Fun (x, m), Fun (y, n) -> (expr_equal x y) && (expr_equal m n)
  | Oper (o1, l1), Oper (o2, l2) -> (o1 = o2) && (expr_list_equal l1 l2 true)
  | Set (x, m), Set (y, n) -> (expr_equal x y) && (expr_equal m n)
  | Letrec (l1, m), Letrec (l2, n) -> (letrec_clause_list_equal l1 l2 true) && (expr_equal m n)
  | _ -> false
and expr_list_equal l1 l2 a = match l1, l2 with
  | [], [] -> a
  | h1::t1, h2::t2 -> expr_list_equal t1 t2 (a && (expr_equal h1 h2))
  | _ -> false
and letrec_clause_list_equal l1 l2 a = match l1, l2 with
  | [], [] -> a
  | (x, m)::t1, (y, n)::t2 -> letrec_clause_list_equal t1 t2 (a && (expr_equal x y) && (expr_equal m n))
  | _ -> false 

(* string_of_expr expr -> string *)
let rec string_of_expr x = match x with
  |    Con i      -> string_of_int i
  |    Var s      -> s
  |    Fun (x, m) -> "(λ" ^ (string_of_expr x) ^ "." ^ (string_of_expr m) ^ ")"
  |    App (m, n) -> "(" ^ (string_of_expr m) ^ " " ^ (string_of_expr n) ^ ")"
  |   Oper (o, l) -> "(" ^ o ^ " " ^ (List.fold_right (fun n a -> a ^ (string_of_expr n)) l "") ^ ")"
  |    Set (x, m) -> "(set " ^ (string_of_expr x) ^ " " ^ (string_of_expr m) ^ ")"
  | Letrec (l, m) -> "(letrec " ^ (string_of_lrclause l "") ^ " " ^ (string_of_expr m) ^ ")"
and string_of_lrclause l a = match l with
  | [] -> "(" ^ a ^ ")"
  | (x, m)::cdr -> string_of_lrclause cdr (a ^ "(" ^ (string_of_expr x) ^ " " ^ (string_of_expr m) ^ ")")

(* flat_map : ('a -> b' list) ('a list) ('b list) *)
let rec flat_map f l acc = match l with
  | [] -> acc
  | car::cdr -> flat_map f cdr (acc @ (f car))

(* av : expr -> Var list *)
let rec av m = match m with
  |    Con i      -> []
  |    Var s      -> []
  |    Fun (v, n) -> List.filter (fun x -> not (expr_equal v x)) (av n)
  |    App (n, o) -> (av n) @ (av o)
  |   Oper (o, l) -> flat_map av l []
  |    Set (x, n) -> x::(av n)
  | Letrec (l, n) ->
    let xs = List.map (fun p -> match p with | (x, _) -> x) l in
    let test = (fun x -> List.fold_right (fun y a -> a || (expr_equal x y)) xs false) in
    List.filter test ((flat_map (fun p -> match p with | (x, o) -> av o) l []) @ (av n))

(* fv : expr -> Var list *)
let rec fv m = match m with
  |    Con i      -> []
  |    Var s      -> []
  |    Fun (v, n) -> List.filter (fun x -> not (expr_equal v x)) (fv n)
  |    App (n, o) -> (fv n) @ (fv o)
  |   Oper (o, l) -> flat_map fv l []
  |    Set (x, n) -> x::(fv n)
  | Letrec (l, n) ->
    let xs = List.map (fun p -> match p with | (x, _) -> x) l in
    let test = (fun x -> List.fold_right (fun y a -> a || (expr_equal x y)) xs false) in
    List.filter test ((flat_map (fun p -> match p with | (x, o) -> fv o) l []) @ (fv n))

type loc = Loc of int

(* loc_equal : loc loc -> bool *)
let loc_equal l1 l2 = match l1, l2 with | Loc a, Loc b -> a = b

type env = MtEnv
           | ConsEnv of expr * loc * env

exception Env_Lookup
(* env_lookup : env variable -> loc *)
let rec env_lookup e x = match e with
  | MtEnv -> raise Env_Lookup
  | ConsEnv (y, l, e1) when expr_equal x y -> l
  | ConsEnv (_, _, e1) -> env_lookup e1 x

(* env_bind : env Var loc -> env *)
let env_bind e x l = ConsEnv (x, l, e)

(* env_domain : env -> Var list *)
let rec env_domain e = match e with
  | MtEnv -> []
  | ConsEnv (x, _, e1) -> x::(env_domain e1)

(* env_range : env -> loc list *)
let rec env_range e = match e with
  | MtEnv -> []
  | ConsEnv (_, l, e1) -> l::(env_range e1)

(* env_ll : env -> loc list *)
let env_ll e = match e with
  | MtEnv -> []
  | (_ : env) -> env_range e

type closure = Closure of expr * env

(* closure_ll : closure -> loc list *)
let closure_ll c = match c with | Closure (m, e) -> env_ll e

type store = MtStore
             | ConsStore of loc * (closure option) * store

exception Store_Lookup
(* store_lookup : store loc -> closure *)
let rec store_lookup s l = match s with
  | ConsStore (l1, oc, s1) when loc_equal l l1 -> oc
  | ConsStore (l1, _, s1) -> store_lookup s1 l
  | _ -> raise Store_Lookup

(* store_alloc : store loc -> store *)
let store_alloc s l = ConsStore (l, None, s)

(* store_bind : store loc closure -> store *)
let store_bind s l c = ConsStore (l, Some c, s)

(* store_rebind : store loc closure -> store *)
let rec store_rebind s l c = match s with
  | MtStore -> MtStore
  | ConsStore (l1, _, s1) when l = l1 -> ConsStore (l1, Some c, s1)
  | ConsStore (l1, o, s1) -> ConsStore (l1, o, store_rebind s1 l c)

(* next : loc -> loc *)
let next l = match l with | Loc n -> Loc (n + 1)

(* next_loc : store -> loc *)
let next_loc s = match s with
  | MtStore -> Loc 0
  | ConsStore (l1, _, _) -> next l1

(* store_domain : store -> loc list *)
let rec store_domain s = match s with
  | MtStore -> []
  | ConsStore (l, _, s1) -> l::(store_domain s1)

(* range : store -> closure list *)
let rec store_range s = match s with
  | MtStore -> []
  | ConsStore (_, Some c, e1) -> c::(store_range e1)
  | ConsStore (_, None, e1) -> store_range e1

type kont = MtKont
            | Fn of closure * kont
            | Ar of closure * kont
            | Op of string * (closure list) * (closure list) * kont
            | St of loc * kont
            | Lr of (expr list) * ((expr * expr) list) * (expr list) * env * expr * kont

(* kont_ll : kont -> loc list *)
let rec kont_ll k = match k with
  | MtKont -> []
  | Fn (c, k) -> (closure_ll c) @ (kont_ll k)
  | Ar (c, k) -> (closure_ll c) @ (kont_ll k)
  | Op (_, vs, cs, k) -> (flat_map closure_ll vs []) @ (flat_map closure_ll cs []) @ (kont_ll k)
  | St (l, k) -> l::(kont_ll k)
  | Lr (_, _, _, e, _, k) -> (env_ll e) @ (kont_ll k)

exception Reduction
let reduce o vs = match o, vs with
  | "add1", [Closure (Con i, _)] -> Con (i+1)
  | "sub1", [Closure (Con i, _)] -> Con (i-1)
  | "isZero", [Closure (Con 0, _)] -> Fun (Var "x", Fun (Var "y", Var "x"))
  | "isZero", [Closure (Con _, _)] -> Fun (Var "x", Fun (Var "y", Var "y"))
  | "+", [Closure (Con i, _); Closure (Con j, _)] -> Con (i+j)
  | "-", [Closure (Con i, _); Closure (Con j, _)] -> Con (i-j)
  | "*", [Closure (Con i, _); Closure (Con j, _)] -> Con (i*j)
  | "/", [Closure (Con i, _); Closure (Con j, _)] -> Con (i/j)
(*| "^", [Closure (Con i, _); Closure (Con j, _)] -> Con (i**j)*)
  | _ -> raise Reduction

let rec contains xs x f = match xs with
  | y::ys -> f y x || contains ys x f
  | [] -> false

let rec collect_garbage gs bs ws s = match gs, bs, ws with
  | gs, bs, w::ws ->
    if contains gs w loc_equal then (match store_lookup s w with
    | Some ce ->
      let gs1 = List.filter (fun x -> not (contains (w::bs) x loc_equal)) (List.append gs (closure_ll ce)) in
      collect_garbage gs1 (w::bs) ws s
    | _ ->
      let gs1 = List.filter (fun x -> not (contains (w::bs) x loc_equal)) gs in
      collect_garbage gs1 (w::bs) ws s)
    else collect_garbage gs bs ws s
  | _, bs, [] -> bs

let rec eval_cesk ce s k =
  let bs = collect_garbage (List.append (closure_ll ce) (kont_ll k)) [] (store_domain s) s in
  let s1 = List.fold_left (fun a n -> ConsStore (n, store_lookup s n, a)) MtStore bs in
  match ce, s1, k with
  (* cesk7 *)
  | Closure (Var y, e), s, k -> (match store_lookup s (env_lookup e (Var y)) with
    | Some ce -> eval_cesk ce s k
    | _ -> raise Runtime)
(* cesk1 *)
  | Closure (App (m,  n), e), s, k ->
    eval_cesk (Closure (m, e)) s (Ar (Closure (n, e), k))
(* cesk2 *)
  | Closure (Oper (o, m::ms), e), s, k ->
    eval_cesk (Closure (m, e)) s (Op (o, [], List.map (fun x -> Closure (x, e)) ms, k))
(* cesk3 *)
  | Closure (v, e), s, Fn (Closure (Fun (x, m), e1), k1) ->
    let l = next_loc s in eval_cesk (Closure (m, env_bind e1 x l)) (store_bind s l (Closure (v, e))) k1
(* cesk4 *)
  | Closure (v, e), s, Ar (Closure (m, e1), k1) ->
    eval_cesk (Closure (m, e1)) s (Fn (Closure (v, e), k1))
(* cesk5 *)
  | Closure (v, e), s, Op (o, vcs, [], k1) ->
    eval_cesk (Closure (reduce o (List.rev ((Closure (v, e))::vcs)), MtEnv)) s k1
(* cesk6 *)
  | Closure (v, e), s, Op (o, vcs, c::cs, k1) ->
    eval_cesk c s (Op (o, (Closure (v, e))::vcs, cs, k1))
(* cesk8 *)
  | Closure (Set (x, m), e), s, k ->
    eval_cesk (Closure (m, e)) s (St (env_lookup e x, k))
(* cesk9 *)
  | Closure (v, e), s, St (l, k1) -> (match store_lookup s l with
    | Some ce -> eval_cesk ce (store_rebind s l ce) k1
    | _ -> raise Runtime)
(* al3 *)
  | Closure (Letrec ((x, m)::xms, n), e), s, k ->
    let l = next_loc s in
    let e1 = env_bind e x l in
    eval_cesk (Closure (m, e1)) (store_alloc s l) (Lr (x::(List.map (fun (a,b) -> a) xms), [], (List.map (fun (a,b) -> b) xms), e1, n, k))
  | Closure (v, e), s, Lr (x::y::xs, vvs, m::ms, e1, n, k1) ->
    let l = next_loc s in
    let e2 = env_bind e1 y l in
    eval_cesk (Closure (m, e2)) (store_alloc (store_rebind s (env_lookup e x) (Closure (v, e2))) l) (Lr (y::xs, (x, v)::vvs, ms, e2, n, k1))
  | Closure (v, e), s, Lr ([x], vvs, [], e1, n, k1) ->
    eval_cesk (Closure (n, e1)) (List.fold_right (fun (x, m) a -> (store_rebind a (env_lookup e1 x) (Closure (m, e1)))) (List.rev ((x, v)::vvs)) s) k1
  | Closure (v, e), s, MtKont -> v
  | _ -> raise Runtime


let eval m = eval_cesk (Closure (m, MtEnv)) MtStore MtKont

let mutual = Letrec ([(Var "true", Fun (Var "x", Fun (Var "y", Var "x")));
                      (Var "false", Fun (Var "x", Fun (Var "y", Var "y")));
                      (Var "ift", Fun (Var "p", Fun (Var "t", Fun (Var "e", App (App (App (Var "p", Var "t"), Var "e"), Con 0)))));
                      (Var "evenP", Fun (Var "n", App (App (App (Var "ift",
                                                                 Oper ("isZero", [Var "n"])),
                                                            Fun (Var "x", Var "true")),
                                                       Fun (Var "x", App (Var "oddP", Oper ("-", [Var "n"; Con 1]))))));
                      (Var "oddP", Fun (Var "n", App (App (App (Var "ift",
                                                                Oper ("isZero", [Var "n"])),
                                                           Fun (Var "x", Var "false")),
                                                      Fun (Var "x", App (Var "evenP", Oper ("-", [Var "n"; Con 1]))))))],
                     App (Var "oddP", Con 21)) ;;

print_endline (string_of_expr (eval (Fun (Var "x", Fun (Var "y", Var "x"))))) ;; (* => (λxy.x) *)
print_endline (string_of_expr (eval (Oper ("add1", [Con 0])))) ;; (* => 1 *)
print_endline (string_of_expr (eval (Letrec ([(Var "x", Con 0)], Var "x")))) ;; (* => 0 *)
print_endline (string_of_expr (eval (Letrec ([(Var "x", Con 0);(Var "y", Con 1)], Var "y")))) ;; (* => 1 *)
print_endline (string_of_expr (eval (Oper ("add1", [Oper ("+", [Con 0;Con 1])])))) ;; (* => 2 *)
print_endline (string_of_expr (eval (Letrec ([(Var "x", Con 0)], Oper ("+", [Var "x"; Oper ("add1", [Var "x"])]))))) ;;
print_endline (string_of_expr (eval mutual)) ;;
