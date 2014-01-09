(*****************************************************************************
 *             Module décrivant les phénomènes de frottements                *
 *****************************************************************************)
open Constantes
open Vecteur
;;

class frot ~coef_traine ~coef_derive ~coef_portance ~surface_v ~surface_l () =
object
  val mutable c_x = coef_traine
  val mutable c_y = coef_derive
  val mutable c_z = coef_portance
  val mutable s_v = surface_v
  val mutable s_l = surface_l
  method get_force ~vitesse ~rho () =
    let q = 0.5 *. rho *. (norm_sq vitesse)
    in
    { x = c_x *. q *. s_v;
      y = c_z *. q *. s_v;
      z = c_y *. q *. s_l }
end
