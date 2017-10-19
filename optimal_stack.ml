


type opcode = DUP | DROP | SWAP | CDR | CAR | PUSH | POP | PAIR | UNPAIR | UNPIAR
type values = Var of string | Pair of values * values
type stack = Invalid | Stack of (values list) * (values list)
                 
let rec run = function
  | ( [ ], s) -> s
  | (   DUP::code, Stack          (h::s, r)) -> run (code, Stack (h::h::s, r))
  | (  DROP::code, Stack          (h::s, r)) -> run (code, Stack (s,       r))
  | (  SWAP::code, Stack       (a::b::s, r)) -> run (code, Stack (b::a::s, r))
  | (   CAR::code, Stack (Pair (a,b)::s, r)) -> run (code, Stack (a::s,    r))
  | (   CDR::code, Stack (Pair (a,b)::s, r)) -> run (code, Stack (b::s,    r))
  | (   POP::code, Stack          (h::s, r)) -> run (code, Stack (s,    h::r))
  | (  PUSH::code, Stack          (s, h::r)) -> run (code, Stack (h::s,    r))
  | (  PAIR::code, Stack  (a::b::s,      r)) -> run (code, Stack ((Pair (a,b))::s, r))
  | (UNPAIR::code, Stack  (Pair (a,b)::s,r)) -> run (code, Stack(a::b::s, r))
  | (UNPIAR::code, Stack  (Pair (a,b)::s,r)) -> run (code, Stack(b::a::s, r))
  | ( _          ,                      _  ) -> Invalid

let rec opcost = function
  | UNPAIR -> 4 (* DUP; CDR; SWAP; CAR *)
  | UNPIAR -> 4 (* DUP; CAR; SWAP; CDR *)
  | _ -> 1


type solution = {mutable cost : float; mutable code : opcode list}

module IntSet = Set.Make(struct type t = string let compare = compare end)

let present_variables x = 
  let rec list_variables = function
    | Stack ((Var a)::s, r)       -> a::(list_variables (Stack (s,r)))
    | Stack ((Pair (a,b))::s, r)  -> list_variables (Stack (a::b::s,r))
    | Stack ([], (Var a)::r)      -> a::(list_variables (Stack ([],r)))
    | Stack ([], (Pair (a,b))::r) -> list_variables (Stack ([],a::b::r))
    | _ -> [] in
  IntSet.of_list (list_variables x)

(* An admissible heuristic for the cost of reaching sb from sa.
   It must always underestimate the cost
*)
let heuristic sa sb =
  (* rules:
     a) each pair that needs to be destructured takes at least one op
     b) each pair that needs to be formed takes at least one op
     c) if some variables are missing, we're doomed to fail
     d) if we have extra variables, they'll take at least one op to get rid of
     e) drop should never follow dup
     f) invalid stacks are irrecoverable
  *)
  let maxint = 10000 in
  (* rule f *)
  if sa = Invalid then
    maxint
  else begin
    (* rule a *)
    let varB = present_variables sb and varA =  present_variables sa in
    if not IntSet.(diff varB varA |> is_empty) then
      maxint
    else (* rule b *)
      let n = IntSet.(diff varA varB |> cardinal) in n
  end
        
let optimize sa sb =
  let nodes = Heap.one sa (0 + heuristic sa sb, 0, []) in
  let rec optimize_aux () =
    if Heap.size nodes <= 0 then
      None
    else begin
      let (s, (cost, total, code)) = Heap.pop nodes in
      if s = sb then
        Some (cost, List.rev code)
      else begin
        List.iter
          (fun opcode -> let sa = (run ([opcode], s)) and
            newcost = opcost opcode + cost in
            let v = (newcost + heuristic sa sb, newcost, opcode::code) in
            if Heap.mem nodes sa then
              Heap.decrease nodes sa v
            else
              Heap.insert nodes sa v
          )
          [DUP; DROP; SWAP; CDR; CAR; PUSH; POP; PAIR; UNPAIR; UNPIAR];
        optimize_aux ()
      end
    end
  in optimize_aux ()
  
