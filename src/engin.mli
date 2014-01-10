open Vecteur
;;

class virtual virt_prop : unit ->
  object ('a)
    method virtual set_power : float -> unit
    (* Set the engine power it must be a percentage !! *)
    method virtual get_power : unit -> float
    (* Get the current percentage power of the engin *)
    method virtual calcul : float -> Vecteur.vecteur
    (* Renvoie la force appliqué à l'engin envoyé en argument.
     *  - premier argument est le temps de calcul pour cette étape de calcul
     *  - deuxième argument est l'engin sur lequel la force est appliquée
     *)
  end

class engin : masse:float -> vitesse_ini:Vecteur.vecteur -> altitude_ini:float ->
  coef_traine:float -> coef_derive:float -> coef_portance:float ->
    surface_v:float -> surface_l:float -> unit ->
object ('a)
  method altitude : float
  method vitesse : Vecteur.vecteur
  method gravity_force : unit -> Vecteur.vecteur
  method get_masse : unit -> float
  method add_engine : virt_prop -> float -> unit
  method get_self : unit -> 'a
  method consums : virt_prop -> float -> float
  method get_orientationAngle : unit -> orientation
  method calcul : float -> unit
end

