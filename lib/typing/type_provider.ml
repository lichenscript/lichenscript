
type provider = <
  resolve: (string * string) -> int option
>

let default_provider: provider = object

  method resolve _ = None

end
