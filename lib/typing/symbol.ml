
type kind =
  | Local
  | Global

let counter = ref 0

type var_sym = {
  vsym_id:       int;
  vsym_name:     string;
  vsym_kind:     kind;
  vsym_scope_id: int;
  vsym_builtin:  bool;
}

and type_sym = {
  tsym_id:       int;
  tsym_name:     string;
  tsym_kind:     kind;
  tsym_scope_id: int;
  tsym_builtin:  bool;
}

let mk_builtin_global_tsym ~scope_id name =
  let tsym_id = !counter in
  counter := tsym_id + 1;
  {
    tsym_id;
    tsym_name = name;
    tsym_kind = Global;
    tsym_scope_id = scope_id;
    tsym_builtin = true;
  }

let mk_local_vsym ~scope_id name =
  let vsym_id = !counter in
  counter := vsym_id + 1;
  {
    vsym_id;
    vsym_name = name;
    vsym_kind = Local;
    vsym_scope_id = scope_id;
    vsym_builtin = false;
  }

let mk_local_tsym ~scope_id name =
  let tsym_id = !counter in
  counter := tsym_id + 1;
  {
    tsym_id;
    tsym_name = name;
    tsym_kind = Local;
    tsym_scope_id = scope_id;
    tsym_builtin = false;
  }
