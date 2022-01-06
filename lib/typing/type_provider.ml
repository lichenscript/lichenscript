
type provider = <
  resolve: (string array * string array) -> int option
>

let default_provider: provider = object

  method resolve _ = None

end
