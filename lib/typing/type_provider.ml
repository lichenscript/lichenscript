
type provider = <
  resolve: string array -> Core_type.TypeValue.t option
>

let default_provider: provider = object

  method resolve _ = None

end
