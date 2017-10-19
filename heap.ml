(* we store pairs of ('v * 'k) in the array to use the default comparison *)
type ('k, 'v) t = {
  mutable a: ('v * 'k) array;
  mutable n: int;
  ht:('k, int) Hashtbl.t }


let one k v =
  let ht = Hashtbl.create 1000 in
  let a = [|(v,k)|] in
  begin
    Hashtbl.add ht k 0;
    {a; n=1; ht}
  end
  
let size heap = heap.n

let track heap i =
  Hashtbl.add heap.ht (snd heap.a.(i)) i

let rec heap_up heap i =
  if i = 0 || heap.a.(i) > heap.a.((i-1)/2) then
    ()
  else begin
    let tmp = heap.a.(i) in
    begin
      heap.a.(i) <- heap.a.((i-1)/2);
      heap.a.((i-1)/2) <- tmp;
      track heap i;
      track heap ((i-1)/2);
      heap_up heap ((i-1)/2)
    end
  end 
  
let rec heap_down heap i  =
  if 2*i+2 < heap.n then begin
    if heap.a.(2*i+1) < heap.a.(2*i+2) then begin
      if heap.a.(i) > heap.a.(2*i+1) then begin
        let tmp = heap.a.(i) in
        heap.a.(i) <- heap.a.(2*i+1);
        heap.a.(2*i+1) <- tmp;
        track heap i;
        track heap (2*i+1);
        heap_down heap (2*i+1)
      end
    end
    else begin
      if heap.a.(i) > heap.a.(2*i+2) then begin
        let tmp = heap.a.(i) in
        heap.a.(i) <- heap.a.(2*i+2);
        heap.a.(2*i+2) <- tmp;
        track heap i;
        track heap (2*i+2);
        heap_down heap (2*i+2)
      end
    end
  end
  else begin
    if 2*i+1 < heap.n && heap.a.(i) > heap.a.(2*i+1) then begin
      let tmp = heap.a.(i) in
      heap.a.(i) <- heap.a.(2*i+1);
      heap.a.(2*i+1) <- tmp;
      track heap i;
      track heap (2*i+1);
      heap_down heap (2*i+1)
    end
    else
      ()
  end
  
let pop heap =
  if heap.n <= 0 then failwith "empty heap";
  let x = heap.a.(0) and n = heap.n in
  begin
    Hashtbl.remove heap.ht (snd heap.a.(0));
    heap.a.(0) <- heap.a.(n-1);
    heap.n <- n - 1;
    track heap 0;
    heap_down heap 0;
    let (v, k) = x in (k, v) 
  end
  
let mem heap k =
  Hashtbl.mem heap.ht k
    
let insert heap k v =
  begin
    heap.n <- heap.n + 1;
    if heap.n > (Array.length heap.a) then
      heap.a <- (Array.append heap.a (Array.make heap.n (v,k)));
    heap.a.(heap.n - 1) <- (v,k);
    track heap (heap.n-1);
    heap_up heap (heap.n - 1)
  end
  
let decrease heap k v =
  let i = Hashtbl.find heap.ht k in begin
    if heap.a.(i) > (v, k) then begin
      heap.a.(i) <- (v, k);
      heap_up heap i
    end
  end
  
