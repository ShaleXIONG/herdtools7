module FaultSet = MySet.Make(
    struct
        type t = string option * string
        let compare (lhs_opt_label, lhs_var) (rhs_opt_label, rhs_var) = 
            let opt_label = 
                Option.compare String.compare lhs_opt_label rhs_opt_label in
            if opt_label = 0 then
                String.compare lhs_var rhs_var
            else opt_label
    end
    )

