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
open Core

module TermColor = struct

  let reset = "\x1b[0m"
  let bold = "\x1b[1m"
  let dim = "\x1b[37m"
  let grey = "\x1b[90m"
  let underline = "\x1b[4m"

  let red = "\x1b[31m"
  let green = "\x1b[32m"
  let blue = "\x1b[34m"

  let cyan = "\x1b[36m"
  let magenta = "\x1b[35m"
  let yello = "\x1b[33m"
  let disable_auto_wrap = "\x1b[?7l"
  let line_up = "\x1b[1A"
  let clear_line = "\x1b[1K"
  let enable_auto_wrap = "\x1b[?7h"
  
end

let print_error_prefix () =
  Out_channel.print_string (TermColor.red ^ "Error: " ^ TermColor.reset)
