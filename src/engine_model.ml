open Vecteur
open Engin
;;

class snecma_vulcain2 engin () =
  object (self)
    inherit virt_prop ()
    val _e = engin
    val mutable _power = 8000000.0 (* in Newton *)
    val mutable _consumption = 360.0 (* kg/s *)
    val mutable _on = false
    method set_power p =
      _on <- p >= 50.0
    method get_power () = if _on then 100. else 0.
    method calcul dt =
      if _on then begin
        let may_consum = (_consumption /. dt) in
        if _e#consums self may_consum < may_consum then _on <- false; (* no more fuel ! *)
        (* instead of that we could compute own much force has been applied... *)
        let orientation = _e#get_orientationAngle () in
        mult_sv _power {x= cos orientation.xz ; y= 0.0; z= sin orientation.xz}
      end else
        {x= 0.0; y= 0.0; z= 0.0}
  end
