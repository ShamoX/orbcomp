(*****************************************************************************
 *                 Module contenant les constantes physiques                 *
 *****************************************************************************)

let gravitation = 6.67428e-11
(* m^3 kg^-1 s^-2 *)
and rayon_terrestre = 6378137.
(* m *)
and vitesse_rot_terre = 465.11
(* m s^-1 *)
and masse_terrestre = 5.9736E24
(* kg *)
and specif_air = 287.05
(* J kg^-1 K^-1 *)
and pi = 3.14159265358979312
;;

let deg_to_kelvin t = t +. 273.15
and rad_to_deg r = (180. *. r) /. pi
and deg_to_rad d = (pi *. d) /. 180.
;;
