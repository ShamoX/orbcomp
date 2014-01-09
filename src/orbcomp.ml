open Printf
open Vecteur
open Engin
;;


let usage = "";;
(**************************************************************************
 *                                Variables                               *
 **************************************************************************)
let masse_v = ref 250000.0
and v_ini_x = ref 500.0 and v_ini_y = ref 50.0 and v_ini_z = ref 0.0
and a_ini = ref 10000.0
and c_t = ref (-.0.1) and c_d = ref 0.0 and c_p = ref 0.0
and s_v = ref 1.0 and s_l = ref 1.0
and pas_incr = ref 0.01
and temps_total = ref 100.0
;;

Arg.parse [
  ("--masse",Arg.Set_float(masse_v),sprintf "Object masse in kg (default %f kg)" !masse_v);
  ("--vitesse_ini",Arg.Tuple(
      [Arg.Set_float(v_ini_x);Arg.Set_float(v_ini_y);Arg.Set_float(v_ini_z)]),
      sprintf "Vitesse initiale (default (%f,%f,%f) m/s)" !v_ini_x !v_ini_y !v_ini_z);
  ("--altitude_ini",Arg.Set_float(a_ini),sprintf "Altitude initiale de l'objet (default %f m)" !a_ini);
  ("--coef_trainee",Arg.Set_float(c_t),sprintf "Coefficient de trainée de l'objet (default %f)" !c_t);
  ("--coef_derive",Arg.Set_float(c_d),sprintf "Coefficient de dérive de l'objet (default %f)" !c_d);
  ("--coef_portance",Arg.Set_float(c_p),sprintf "Coefficient de portance de l'objet (default %f)" !c_p);
  ("--surface_verticale",Arg.Set_float(s_v),sprintf "Surface verticale de l'objet (default %f m^2)" !s_v);
  ("--surface_laterale",Arg.Set_float(s_l),sprintf "Surface latérale de l'objet (default %f m^2)" !s_l);
] ignore usage;
let v_ini = {x = !v_ini_x ; y = !v_ini_y ; z = !v_ini_z} in
let vaisseau = new engin ~masse:!masse_v ~coef_traine:!c_t ~coef_derive:!c_d ~coef_portance:!c_p
          ~surface_v:!s_v ~surface_l:!s_l ~vitesse_ini:v_ini ~altitude_ini:!a_ini ()
and cur_time = ref 0.0
in
printf "Starting simulation for %.3f seconds ; computation step : %f seconds\n" !temps_total !pas_incr;
printf "\n%.3f %.0f %.0f" !cur_time vaisseau#altitude (norm vaisseau#vitesse);
while !cur_time < !temps_total do
  let tmp = ref 0.0 in
    while !tmp < 1.0 do
      tmp := !tmp +. !pas_incr;
      vaisseau#calcul !pas_incr
    done;
    cur_time := !cur_time +. !tmp;
    printf "\n%.3f %.0f %.0f" !cur_time vaisseau#altitude (norm vaisseau#vitesse)
done;
printf "\nDone !!\n";;

