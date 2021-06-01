
type 'M t = 'M * t'

and kind =
  | Block
  | Line

and t' = {
  kind: kind;
  text: string;
  on_newline: bool;
}
[@@deriving show]
