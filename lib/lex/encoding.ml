(*
 * Copyright 2022 Vincent Chan
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *)

let get_utf16_code content =
  let first_char = String.get content 0 in
  let char_code = Char.code first_char in
  if char_code < 128 then
    char_code
  else (
    match char_code with
    | 0xc0 | 0xc1 | 0xc2 | 0xc3
    | 0xc4 | 0xc5 | 0xc6 | 0xc7
    | 0xc8 | 0xc9 | 0xca | 0xcb
    | 0xcc | 0xcd | 0xce | 0xcf
    | 0xd0 | 0xd1 | 0xd2 | 0xd3
    | 0xd4 | 0xd5 | 0xd6 | 0xd7
    | 0xd8 | 0xd9 | 0xda | 0xdb
    | 0xdc | 0xdd | 0xde | 0xdf
      -> (
        let char_code = char_code land 0x1F in
        let next = String.get content 1 in
        let next_char = Char.code next in
        (char_code lsl 6) lor (next_char land 0x3F)
      )

    | 0xe0 | 0xe1 | 0xe2 | 0xe3
    | 0xe4 | 0xe5 | 0xe6 | 0xe7
    | 0xe8 | 0xe9 | 0xea | 0xeb
    | 0xec | 0xed | 0xee | 0xef
      -> (
        let char_code = char_code land 0xF in
        let next = String.get content 1 in
        let next_char = Char.code next in
        let char_code = (char_code lsl 6) lor (next_char land 0x3F) in
        let next = String.get content 2 in
        let next_char = Char.code next in
        let char_code = (char_code lsl 6) lor (next_char land 0x3F) in
        char_code
      )

    | 0xf0 | 0xf1 | 0xf2 | 0xf3
    | 0xf4 | 0xf5 | 0xf6 | 0xf7
      -> (
        let char_code = char_code land 0x7 in
        let next = String.get content 1 in
        let next_char = Char.code next in
        let char_code = (char_code lsl 6) lor (next_char land 0x3F) in
        let next = String.get content 2 in
        let next_char = Char.code next in
        let char_code = (char_code lsl 6) lor (next_char land 0x3F) in
        let next = String.get content 3 in
        let next_char = Char.code next in
        let char_code = (char_code lsl 6) lor (next_char land 0x3F) in
        char_code
      )

    | 0xf8 | 0xf9 | 0xfa | 0xfb
      -> (
        let char_code = char_code land 0x3 in
        let next = String.get content 1 in
        let next_char = Char.code next in
        let char_code = (char_code lsl 6) lor (next_char land 0x3F) in
        let next = String.get content 2 in
        let next_char = Char.code next in
        let char_code = (char_code lsl 6) lor (next_char land 0x3F) in
        let next = String.get content 3 in
        let next_char = Char.code next in
        let char_code = (char_code lsl 6) lor (next_char land 0x3F) in
        let next = String.get content 4 in
        let next_char = Char.code next in
        let char_code = (char_code lsl 6) lor (next_char land 0x3F) in
        char_code
      )


    | 0xfc | 0xfd
      -> (
        let char_code = char_code land 0x1 in
        let next = String.get content 1 in
        let next_char = Char.code next in
        let char_code = (char_code lsl 6) lor (next_char land 0x3F) in
        let next = String.get content 2 in
        let next_char = Char.code next in
        let char_code = (char_code lsl 6) lor (next_char land 0x3F) in
        let next = String.get content 3 in
        let next_char = Char.code next in
        let char_code = (char_code lsl 6) lor (next_char land 0x3F) in
        let next = String.get content 4 in
        let next_char = Char.code next in
        let char_code = (char_code lsl 6) lor (next_char land 0x3F) in
        let next = String.get content 5 in
        let next_char = Char.code next in
        let char_code = (char_code lsl 6) lor (next_char land 0x3F) in
        char_code
      )

    | _ -> char_code
  )
