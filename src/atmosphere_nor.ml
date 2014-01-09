(*****************************************************************************
 *                 Module décrivant l'atmosphère normalisée                  *
 *****************************************************************************)
open Constantes
;;

let videvalue = (0.0,-270.0);;

let table = [|
  (      0.0,( 101300.0 ,   15.0));
  (    500.0,(  95500.0 ,   12.0));
  (   1000.0,(  90000.0 ,    8.5));
  (   1500.0,(  84500.0 ,    5.5));
  (   2000.0,(  79400.0 ,    2.0));
  (   2500.0,(  74600.0 ,   -1.0));
  (   3000.0,(  70000.0 ,   -4.5));
  (   3500.0,(  65800.0 , 	-7.5));
  (   4000.0,(  61700.0 ,  -11.0));
  (   5000.0,(  54100.0 ,  -17.5));
  (   6000.0,(  47100.0 ,  -24.0));
  (   7000.0,(  41100.0 ,  -30.5));
  (   8000.0,(  35700.0 ,  -37.0));
  (   9000.0,(  30700.0 ,	 -43.5));
  (  10000.0,(  26500.0 ,	 -50.0));
  (  11000.0,(  22700.0 ,	 -56.5));
  (  12000.0,(  19400.0 ,	 -56.5));
  (  13000.0,(  16500.0 ,	 -56.5));
  (  14000.0,(  14100.0 ,	 -56.5));
  (  15000.0,(  11900.0 ,	 -56.5));
  (  20000.0,(   5500.0 ,	 -46.0));
  (  30000.0,(	  1100.0,	 -38.0));
  (  40000.0,(	   300.0,	  -5.0));
  (  50000.0,(	    90.0,	   1.0));
  (  60000.0,(	     2.5,	 -20.0));
  ( 100000.0,(	  4.0E-2,	 -64.0));
  ( 200000.0,(	  1.3E-4,	 822.0));
  ( 300000.0,(	  2.0E-5,	 953.0));
  ( 400000.0,(	  4.4E-6,	 973.0));
  ( 500000.0,(	  1.1E-6,  977.0));
|] ;;

let get_idx h =
  let idx = ref 0 in
  try
    while !idx < (Array.length table - 1) do
      if h <= fst (table.(!idx)) then
        raise Exit
      else incr idx
    done;
    None
  with Exit -> Some(!idx - 1)
;;


let get_pression h =
  match get_idx h with
      None -> fst videvalue
    | Some(i) -> begin
        let (a0,(p0,_)) = table.(i)
        and (a1,(p1,_)) = table.(i+1) in
          let b = (p1 -. p0) /. (a1 -. a0)
          and c = p0 in
            ((h -. a0) *. b) +. c
      end
;;

let get_temperature h =
  match get_idx h with
      None -> fst videvalue
    | Some(i) -> begin
        let (a0,(_,t0)) = table.(i)
        and (a1,(_,t1)) = table.(i+1) in
          let b = (t1 -. t0) /. (a1 -. a0)
          and c = t0 in
            ((h -. a0) *. b) +. c
      end
;;

let get_rho h =
  let (p,t) =
    match get_idx h with
        None -> videvalue
      | Some(i) -> begin
          let (a0,(p0,t0)) = table.(i)
          and (a1,(p1,t1)) = table.(i+1) in
            (((h -. a0) *. ((p1 -. p0) /. (a1 -. a0))) +. p0,
             ((h -. a0) *. ((t1 -. t0) /. (a1 -. a0))) +. t0)
        end
  in p /. (specif_air *. (deg_to_kelvin t))
  (* on a toujours deg_to_kelvin t != 0 *)
;;
