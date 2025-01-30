module FaultSet = MySet.Make(
    struct
        type t = string option * bool option * string
        let compare (lhs_label, lhs_bool, lhs_var) (rhs_label, rhs_bool, rhs_var) = 
            let label = 
                Option.compare String.compare lhs_label rhs_label in
            let boolean =
                Option.compare Bool.compare lhs_bool rhs_bool in
            match label, boolean with
            | 0, 0 -> String.compare lhs_var rhs_var
            | 0, boolean -> boolean
            | label, _ -> label
    end
    )

