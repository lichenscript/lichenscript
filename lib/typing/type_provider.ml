
type provider = <
  resolve: (string array * string array) -> Core_type.VarSym.t option
>

let default_provider: provider = object

  method resolve _ = None

end
