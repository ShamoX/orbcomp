(*****************************************************************************
 *                    Module décrivant un système mobile                     *
 *****************************************************************************)
open Constantes
open Frottement
open Atmosphere_nor
open Vecteur
;;

class engin ~masse ~vitesse_ini ~altitude_ini ~coef_traine ~coef_derive
            ~coef_portance ~surface_v ~surface_l () =
object (self)
  val _f = new frot ~coef_traine ~coef_derive ~coef_portance ~surface_v
                    ~surface_l ()
  val _m = masse
  val _v_i = vitesse_ini
  val mutable _v = vitesse_ini
  val _a_i = altitude_ini
  val mutable _a = altitude_ini
  method altitude = _a
  method vitesse = _v
  method gravity_force () =
    let d = (rayon_terrestre +. _a) in
    {x = 0.0; y = -. (gravitation *. _m *. masse_terrestre) /. ( d *. d); z = 0.0}
  method calcul temps =
    if (_a <= min_float) then begin
      _a <- 0.0;
      _v <- {x = 0.0 ; y = 0.0 ; z = 0.0}
    end else begin
      let frot = _f#get_force ~vitesse:_v ~rho:(get_rho _a) ()
      and alpha = angle_xy _v
      in
        (*Printf.printf "Vitesse = %s ; angle = %f ; Frottement = %s\n" (Vecteur.to_string _v)
          (rad_to_deg alpha) (Vecteur.to_string frot);*)
      let forces = add_vv {
            x = ((cos alpha) *. frot.x) +. ((sin alpha) *. frot.y);
            y = ((cos alpha) *. frot.y) +. ((sin alpha) *. frot.x);
            z = frot.z
            } (self#gravity_force ())
      in
        (*Printf.printf "g = %s\n" (Vecteur.to_string (div_vs (self#gravity_force ()) _m));
        Printf.printf "Toutes les forces : %s\n" (Vecteur.to_string forces);*)
      let a = mult_sv temps (div_vs forces _m) in
        (*Printf.printf "Accélération résultante : %s\n" (Vecteur.to_string a);*)
        _a <- _a +. (_v.y *. temps);
        _v <- add_vv _v a
    end
end;;

class engin_ap ~masse ~vitesse_ini ~altitude_ini ~coef_traine ~coef_derive
            ~coef_portance ~surface_v ~surface_l ~s () =
object (self)
  inherit engin ~masse ~vitesse_ini ~altitude_ini ~coef_traine ~coef_derive
            ~coef_portance ~surface_v ~surface_l () as e
end
