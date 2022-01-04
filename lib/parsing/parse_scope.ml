
type ty = 
| PScope_Module
| PScope_Block
| PString_Function

type t = {
  ty: ty;
  prev: t option;
}

let create ?prev ty =
  {
    ty; prev;
  }
