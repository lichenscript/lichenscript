
const i32_max = 0x7FFFFFFF;
const i32_min = 0x7FFFFFFF * -1 + 1;

const unionSym = Symbol("union");
const tupleSym = Symbol("tuple");
const clsNameSym = Symbol("clsName");

function i32_add(a, b) {
  a = a|0;  // give hinting to v8
  b = b|0;

  let tmp = a + b;
  if (tmp > i32_max) {
    return tmp & i32_max;
  }

  return tmp|0;
}

function i32_sub(a, b) {
  a = a|0;  // give hinting to v8
  b = b|0;
  return (a - b)|0;
}

function i32_mult(a, b) {
  a = a|0;  // give hinting to v8
  b = b|0;
  return (a * b)|0;
}

function i32_div(a, b) {
  a = a|0;  // give hinting to v8
  b = b|0;
  return (a / b)|0;
}

function i32_lshift(a, b) {
  a = a|0;  // give hinting to v8
  b = b|0;
  return (a << b)|0;
}

function i32_rshift(a, b) {
  a = a|0;  // give hinting to v8
  b = b|0;
  return (a >> b)|0;
}

function i32_mod(a, b) {
  a = a|0;  // give hinting to v8
  b = b|0;
  return (a % b)|0;
}

function i32_mod(a, b) {
  a = a|0;  // give hinting to v8
  b = b|0;
  return (a % b)|0;
}

function f32_add(a, b) {
  return Math.fround(a + b);
}

function f32_sub(a, b) {
  return Math.fround(a - b);
}

function f32_mult(a, b) {
  return Math.fround(a * b);
}

function f32_div(a, b) {
  return Math.fround(a / b);
}

function lc_std_char_code() {
  return String.prototype.charCodeAt.call(this);
}

function lc_std_char_to_string() {
  return this[0];
}

function lc_std_print(...args) {
  let content = '';
  for (let i = 0; i < args.length; i++) {
    if (typeof args[i] === "object" && args[i].__proto__[clsNameSym]) {
      content += args[i].__proto__[clsNameSym] + " {...}";
    } else if (Array.isArray(args[i])) {
      if (typeof args[i][0] === 'object' && args[i][0][unionSym]) {
        content += args[i][0].members[args[i][1]];
        if (args[i].length > 2) {
          let tmp = '(';

          for (let j = 2; j < args[i].length; j++) {
            tmp += args[i][j];
            if (j < args[i].length - 1) {
              tmp += ", "
            }
          }

          tmp += ')'
          content += tmp;
        }
      } else if (args[i][0] === tupleSym) {
        let tmp = '(';

        for (let j = 1; j < args[i].length; j++) {
          tmp += args[i][j];
          if (j < args[i].length - 1) {
            tmp += ", "
          }
        }

        tmp += ')'
        content += tmp;
      } else {
        let tmp = '[';

        for (let j = 0; j < args[i].length; j++) {
          tmp += args[i][j];
          if (j < args[i].length - 1) {
            tmp += ", "
          }
        }

        tmp += ']'
        content += tmp;
      }
    } else {
      content += args[i];
    }
  }
  console.log(content);
}

const LCC_Option = {
  [unionSym]: 1,
  name: "Option",
  members: [
    "Some",
    "None",
  ]
};

const LCC_Result = {
  [unionSym]: 1,
  name: "Result",
  members: [
    "Ok",
    "Error",
  ]
};

function lc_std_exit(code) {
  const process = require('process');
  process.exit(code);
}

function lc_std_array_get_length() {
  return this.length;
}

function lc_std_array_push(elm) {
  Array.prototype.push.call(this, elm);
}

function lc_std_array_resize(size, value) {
  if (size === this.length) {
    return;
  }

  if (size < this.length) {
    this.length = size;
    return;
  }

  for (let i = this.length; i < size; i++) {
    this.push(value);
  }
}

function lc_std_array_filter(filter) {
  return Array.prototype.filter.call(this, filter);
}

function lc_std_array_slice(start, end) {
  return Array.prototype.slice.call(this, start, end);
}

function lc_std_array_map(f) {
  return Array.prototype.map.call(this, f);
}

function lc_std_array_sort(cmp) {
  return Array.prototype.sort.call(this, cmp);
}

function lc_std_map_get(key, value) {
  const tmp = Map.prototype.get.call(this, key, value);
  if (tmp) {
    return [LCC_Option, 0, tmp];
  }
  return [LCC_Option, 1];
}

function lc_std_map_set(key, value) {
  Map.prototype.set.call(this, key, value);
}

function lc_std_map_remove(key, value) {
  const tmp = Map.prototype.delete.call(this, key, value);
  if (tmp) {
    return [LCC_Option, 0, tmp];
  }
  return [LCC_Option, 1];
}

function lc_std_map_size() {
  return this.size;
}

function lc_std_string_slice() {
  return String.prototype.slice.apply(this, arguments);
}

function lc_std_string_get_length() {
  return this.length;
}

function lc_std_string_get_char(index) {
  return this[index];
}

function lc_std_string_concat(a, b) {
  return a + b;
}

function lc_std_panic(message) {
  console.log("panic");
  throw new Error("panic: " + message);
}
