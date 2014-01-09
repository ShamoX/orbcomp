(*****************************************************************************
 *               Module décrivant un système de propulsion                   *
 *****************************************************************************)


class virtual virt_prop () =
  object
    method virtual set_power : float -> unit
    (* Set the engine power it must be a percentage !! *)
    method virtual get_power : unit -> float
    (* Get the current percentage power of the engin *)
    method virtual calcul : float -> Engin.engin -> Vecteur.vecteur
    (* Renvoie la force appliqué à l'engin envoyé en argument.
     *  - premier argument est le temps de calcul pour cette étape de calcul
     *  - deuxième argument est l'engin sur lequel la force est appliquée
     *)
  end

class snecma_vulcain2 () =
  object
    val mutable _t_u = 0.0
    val mutable _on = false
    method set_power p =
      n <- p >= 50.0
    method calcul dt e = begin
      ()
    end
  end
and snecma_m88 () =
  object
    method calcul dt e = begin
      ()
    end
  end
