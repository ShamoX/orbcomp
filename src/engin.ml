(*****************************************************************************
 *                    Module décrivant un système mobile                     *
 *****************************************************************************)
open Constantes
open Frottement
open Atmosphere_nor
open Vecteur
;;

exception PropulsorNotFound

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
  val mutable _prop : (virt_prop * float) list = []
  val mutable _debug = false
  method set_debug d = _debug <- d
  method get_debug = _debug
  method altitude = _a
  method vitesse = _v
  method gravity_force () =
    let d = (rayon_terrestre +. _a) in
    {x = 0.0; y = 0.0; z = -. (gravitation *. _m *. masse_terrestre) /. ( d *. d)}
  method get_masse () = List.fold_left (fun a (_,b) -> a +. b) _m _prop
  method add_engine p fuel_masse =
    (* Adding a propulsor mean with a given quantity of fuel masse which must be
     * expressed in kg. *)
    _prop <- (p, fuel_masse) :: _prop
  method get_self () = self
  method consums p mc =
    (* propulsor may consum @mc kg of its corresponding fuel *)
    let really_consumed = ref mc in
    let rec consum = function
      [] -> raise PropulsorNotFound
      | (prop, fuel)::p_list -> begin
        if prop <> p then (prop, fuel) :: (consum p_list)
        else begin
          if (fuel <= mc) then begin
            really_consumed := fuel;
            (prop, 0.0) :: p_list
          end else
            (prop, fuel -. mc) :: p_list
        end
      end
    in _prop <- consum _prop;
    !really_consumed
  method get_orientationAngle () =
    (* Give the angles for the central axes of the ship regarding the
     * referential *)
    (* today, the orientation is the same than the speed orientation *)
    { xy= (angle_xy _v) ; yz= (angle_yz _v) ; xz= (angle_xz _v)}
  method calcul dt =
    if (_a <= min_float) then begin
      _a <- 0.0;
      _v <- {x = 0.0 ; y = 0.0 ; z = 0.0}
    end else begin
      let frot = _f#get_force ~vitesse:_v ~rho:(get_rho _a) ()
      and alpha = angle_xy _v
      in
        if _debug then
          Printf.printf "Vitesse = %s ; angle = %f ; Frottement = %s\n" (Vecteur.to_string _v)
          (rad_to_deg alpha) (Vecteur.to_string frot);
      let primary_forces = add_vv {
            x = ((cos alpha) *. frot.x) +. ((sin alpha) *. frot.y);
            y = ((cos alpha) *. frot.y) +. ((sin alpha) *. frot.x);
            z = frot.z
            } (self#gravity_force ())
      and add_f pf ((engine : virt_prop), _) =
        let f_engine = engine#calcul dt in
        if _debug then
          Printf.printf "\t%s -- %s\n" engine#shortdesc (Vecteur.to_string f_engine);
        add_vv pf f_engine
      in
      let forces = List.fold_left
        add_f
        primary_forces _prop
      in
        if _debug then begin
          Printf.printf "g = %s\n" (Vecteur.to_string (div_vs (self#gravity_force ()) _m));
          Printf.printf "Toutes les forces : %s\n" (Vecteur.to_string forces)
        end;
      let a = mult_sv dt (div_vs forces (self#get_masse ())) in
        if _debug then
          Printf.printf "Accélération résultante : %s\n" (Vecteur.to_string a);
        _a <- _a +. (_v.z *. dt);
        _v <- add_vv _v a
    end
end
and virtual virt_prop () =
  object
    val mutable _shortdesc = "virt_prop"
    method virtual set_power : float -> unit
    (* Set the engine power it must be a percentage !! *)
    method virtual get_power : unit -> float
    (* Get the current percentage power of the engin *)
    method virtual calcul : float -> Vecteur.vecteur
    (* Renvoie la force appliqué à l'engin envoyé en argument.
     *  - premier argument est le temps de calcul pour cette étape de calcul
     *  - deuxième argument est l'engin sur lequel la force est appliquée
     *)
    method shortdesc = _shortdesc
  end
;;

class engin_ap ~masse ~vitesse_ini ~altitude_ini ~coef_traine ~coef_derive
            ~coef_portance ~surface_v ~surface_l ~s () =
object (self)
  inherit engin ~masse ~vitesse_ini ~altitude_ini ~coef_traine ~coef_derive
            ~coef_portance ~surface_v ~surface_l () as e
end
