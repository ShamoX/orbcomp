(*****************************************************************************
 *                            Module de vecteur                              *
 *****************************************************************************)

type vecteur = {x : float ; y : float ; z : float }

let norm v = sqrt ((v.x *. v.x) +. (v.y *. v.y) +. (v.z *. v.z))
and norm_sq v = (v.x *. v.x) +. (v.y *. v.y) +. (v.z *. v.z)
and angle_xy v = (atan2 v.y v.x)
and angle_yx v = (atan2 v.x v.y)
and to_string v = Printf.sprintf "(%f,%f,%f)" v.x v.y v.z
;;

let mult_sv a v = { x = a *. v.x ; y = a *. v.y ; z = a *. v.z }
and div_sv a v = { x = a /. v.x ; y = a /. v.y ; z = a /. v.z }
and div_vs v a = { x = v.x /. a ; y = v.y /. a ; z = v.z /. a }
and add_vv v1 v2 = { x = v1.x +. v2.x ; y = v1.y +. v2.y ; z = v1.z +. v2.z}
;;
