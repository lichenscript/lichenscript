
(* open Token
open Lex_env *)

let lexeme = Sedlexing.Utf8.lexeme

let sub_lexeme = Sedlexing.Utf8.sub_lexeme

let letter = [%sedlex.regexp? 'a' .. 'z' | 'A' .. 'Z' | '$']

let id_letter = [%sedlex.regexp? letter | '_']

let digit = [%sedlex.regexp? '0' .. '9']

let digit_non_zero = [%sedlex.regexp? '1' .. '9']

let decintlit = [%sedlex.regexp? '0' | ('1' .. '9', Star digit)]

(* DecimalIntegerLiteral *)

let alphanumeric = [%sedlex.regexp? digit | letter]

let word = [%sedlex.regexp? (letter, Star alphanumeric)]

let hex_digit = [%sedlex.regexp? digit | 'a' .. 'f' | 'A' .. 'F']

let non_hex_letter = [%sedlex.regexp? 'g' .. 'z' | 'G' .. 'Z' | '$']

let bin_digit = [%sedlex.regexp? '0' | '1']

let oct_digit = [%sedlex.regexp? '0' .. '7']

(* This regex could be simplified to (digit Star (digit OR '_' digit))
 * That makes the underscore and failure cases faster, and the base case take x2-3 the steps
 * As the codebase contains more base cases than underscored or errors, prefer this version *)
let underscored_bin =
  [%sedlex.regexp? Plus bin_digit | (bin_digit, Star (bin_digit | ('_', bin_digit)))]

let underscored_oct =
  [%sedlex.regexp? Plus oct_digit | (oct_digit, Star (oct_digit | ('_', oct_digit)))]

let underscored_hex =
  [%sedlex.regexp? Plus hex_digit | (hex_digit, Star (hex_digit | ('_', hex_digit)))]

let underscored_digit = [%sedlex.regexp? Plus digit | (digit_non_zero, Star (digit | ('_', digit)))]

let underscored_decimal = [%sedlex.regexp? Plus digit | (digit, Star (digit | ('_', digit)))]

(* Different ways you can write a number *)
let binnumber = [%sedlex.regexp? ('0', ('B' | 'b'), underscored_bin)]

let octnumber = [%sedlex.regexp? ('0', ('O' | 'o'), underscored_oct)]

let legacyoctnumber = [%sedlex.regexp? ('0', Plus oct_digit)]

(* no underscores allowed *)

let legacynonoctnumber = [%sedlex.regexp? ('0', Star oct_digit, '8' .. '9', Star digit)]

let hexnumber = [%sedlex.regexp? ('0', ('X' | 'x'), underscored_hex)]

let scinumber =
  [%sedlex.regexp?
    ( ((decintlit, Opt ('.', Opt underscored_decimal)) | ('.', underscored_decimal)),
      ('e' | 'E'),
      Opt ('-' | '+'),
      underscored_digit )]

let wholenumber = [%sedlex.regexp? (underscored_digit, Opt '.')]

let floatnumber = [%sedlex.regexp? (Opt underscored_digit, '.', underscored_decimal)]

let binbigint = [%sedlex.regexp? (binnumber, 'n')]

let octbigint = [%sedlex.regexp? (octnumber, 'n')]

let hexbigint = [%sedlex.regexp? (hexnumber, 'n')]

let scibigint = [%sedlex.regexp? (scinumber, 'n')]

let wholebigint = [%sedlex.regexp? (underscored_digit, 'n')]

let floatbigint = [%sedlex.regexp? ((floatnumber | (underscored_digit, '.')), 'n')]

(* 2-8 alphanumeric characters. I could match them directly, but this leads to
 * ~5k more lines of generated lexer
   let htmlentity = "quot" | "amp" | "apos" | "lt" | "gt" | "nbsp" | "iexcl"
     | "cent" | "pound" | "curren" | "yen" | "brvbar" | "sect" | "uml" | "copy"
     | "ordf" | "laquo" | "not" | "shy" | "reg" | "macr" | "deg" | "plusmn"
     | "sup2" | "sup3" | "acute" | "micro" | "para" | "middot" | "cedil" | "sup1"
     | "ordm" | "raquo" | "frac14" | "frac12" | "frac34" | "iquest" | "Agrave"
     | "Aacute" | "Acirc" | "Atilde" | "Auml" | "Aring" | "AElig" | "Ccedil"
     | "Egrave" | "Eacute" | "Ecirc" | "Euml" | "Igrave" | "Iacute" | "Icirc"
     | "Iuml" | "ETH" | "Ntilde" | "Ograve" | "Oacute" | "Ocirc" | "Otilde"
     | "Ouml" | "times" | "Oslash" | "Ugrave" | "Uacute" | "Ucirc" | "Uuml"
     | "Yacute" | "THORN" | "szlig" | "agrave" | "aacute" | "acirc" | "atilde"
     | "auml" | "aring" | "aelig" | "ccedil" | "egrave" | "eacute" | "ecirc"
     | "euml" | "igrave" | "iacute" | "icirc" | "iuml" | "eth" | "ntilde"
     | "ograve" | "oacute" | "ocirc" | "otilde" | "ouml" | "divide" | "oslash"
     | "ugrave" | "uacute" | "ucirc" | "uuml" | "yacute" | "thorn" | "yuml"
     | "OElig" | "oelig" | "Scaron" | "scaron" | "Yuml" | "fnof" | "circ" | "tilde"
     | "Alpha" | "Beta" | "Gamma" | "Delta" | "Epsilon" | "Zeta" | "Eta" | "Theta"
     | "Iota" | "Kappa" | "Lambda" | "Mu" | "Nu" | "Xi" | "Omicron" | "Pi" | "Rho"
     | "Sigma" | "Tau" | "Upsilon" | "Phi" | "Chi" | "Psi" | "Omega" | "alpha"
     | "beta" | "gamma" | "delta" | "epsilon" | "zeta" | "eta" | "theta" | "iota"
     | "kappa" | "lambda" | "mu" | "nu" | "xi" | "omicron" | "pi" | "rho"
     | "sigmaf" | "sigma" | "tau" | "upsilon" | "phi" | "chi" | "psi" | "omega"
     | "thetasym" | "upsih" | "piv" | "ensp" | "emsp" | "thinsp" | "zwnj" | "zwj"
     | "lrm" | "rlm" | "ndash" | "mdash" | "lsquo" | "rsquo" | "sbquo" | "ldquo"
     | "rdquo" | "bdquo" | "dagger" | "Dagger" | "bull" | "hellip" | "permil"
     | "prime" | "Prime" | "lsaquo" | "rsaquo" | "oline" | "frasl" | "euro"
     | "image" | "weierp" | "real" | "trade" | "alefsym" | "larr" | "uarr" | "rarr"
     | "darr" | "harr" | "crarr" | "lArr" | "uArr" | "rArr" | "dArr" | "hArr"
     | "forall" | "part" | "exist" | "empty" | "nabla" | "isin" | "notin" | "ni"
     | "prod" | "sum" | "minus" | "lowast" | "radic" | "prop" | "infin" | "ang"
     | "and" | "or" | "cap" | "cup" | "'int'" | "there4" | "sim" | "cong" | "asymp"
     | "ne" | "equiv" | "le" | "ge" | "sub" | "sup" | "nsub" | "sube" | "supe"
     | "oplus" | "otimes" | "perp" | "sdot" | "lceil" | "rceil" | "lfloor"
     | "rfloor" | "lang" | "rang" | "loz" | "spades" | "clubs" | "hearts" | "diams"
 *)
let htmlentity =
  [%sedlex.regexp?
    ( alphanumeric,
      alphanumeric,
      Opt alphanumeric,
      Opt alphanumeric,
      Opt alphanumeric,
      Opt alphanumeric,
      Opt alphanumeric,
      Opt alphanumeric )]

(* https://tc39.github.io/ecma262/#sec-white-space *)
let whitespace =
  [%sedlex.regexp?
    ( 0x0009 | 0x000B | 0x000C | 0x0020 | 0x00A0 | 0xfeff | 0x1680
    | 0x2000 .. 0x200a
    | 0x202f | 0x205f | 0x3000 )]

(* minus sign in front of negative numbers
   (only for types! regular numbers use T_MINUS!) *)
let neg = [%sedlex.regexp? ('-', Star whitespace)]

let line_terminator_sequence = [%sedlex.regexp? '\n' | '\r' | "\r\n" | 0x2028 | 0x2029]

let line_terminator_sequence_start = [%sedlex.regexp? '\n' | '\r' | 0x2028 | 0x2029]

let hex_quad = [%sedlex.regexp? (hex_digit, hex_digit, hex_digit, hex_digit)]

let unicode_escape = [%sedlex.regexp? ("\\u", hex_quad)]

let codepoint_escape = [%sedlex.regexp? ("\\u{", Plus hex_digit, '}')]

let js_id_start = [%sedlex.regexp? '$' | '_' | id_start | unicode_escape | codepoint_escape]

let js_id_continue =
  [%sedlex.regexp? '$' | '_' | 0x200C | 0x200D | id_continue | unicode_escape | codepoint_escape]

let pos_at_offset env offset =
  { Loc.line = Lex_env.line env; column = offset - Lex_env.bol_offset env }

let loc_of_offsets env start_offset end_offset =
  {
    Loc.source = Lex_env.source env;
    start = pos_at_offset env start_offset;
    _end = pos_at_offset env end_offset;
  }

let print_lex () =
  let _tok = Loc.mk_pos 1 2 in
  print_endline "lex"
