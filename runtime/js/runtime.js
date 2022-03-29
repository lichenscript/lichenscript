
const i32_max = 0x7FFFFFFF;
const i32_min = -0x7FFFFFFF - 1;
const i64_max = 0x7FFFFFFFFFFFFFFFn;
const i64_min = -0x7FFFFFFFFFFFFFFFn - 1n;

const unionSym = Symbol("union");
const tupleSym = Symbol("tuple");
const clsNameSym = Symbol("clsName");

function i32_add(a, b) {
  a = a|0;  // give hinting to v8
  b = b|0;

  a += b;
  if (a > i32_max) {
    return (a - i32_max) + i32_min - 1;
  }

  return a|0;
}

function i64_add(a, b) {
  a += b;
  if (a > i64_max) {
    return (a - i64_max) + i64_min - 1n;
  }
  return a;
}

function i32_sub(a, b) {
  a = a|0;  // give hinting to v8
  b = b|0;
  a -= b;
  if (a < i32_min) {
    return (a - i32_min) + i32_max + 1;
  }
  return a|0;
}

function i64_sub(a, b) {
  a -= b;
  if (a < i64_min) {
    return (a - i64_min) + i64_max + 1n;
  }
  return a;
}

function i32_mult(a, b) {
  a = a|0;  // give hinting to v8
  b = b|0;
  return (a * b)|0;
}

function i64_mult(a, b) {
  a *= b;
  if (a > i64_max) {
    return 0;
  }
  return a;
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

function i32_bit_not(e) {
  e = ~e;
  return e|0;
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

function lc_to_string(v) {
  let content = "";

  if (typeof v === "undefined") {
    return "()";
  } else if (typeof v === "object" && v.__proto__[clsNameSym]) {
    content += v.__proto__[clsNameSym] + " {...}";
  } else if (Array.isArray(v)) {
    if (typeof v[0] === 'object' && v[0][unionSym]) {
      content += v[0].members[v[1]];
      if (v.length > 2) {
        let tmp = '(';

        for (let j = 2; j < v.length; j++) {
          tmp += lc_to_string(v[j]);
          if (j < v.length - 1) {
            tmp += ", "
          }
        }

        tmp += ')'
        content += tmp;
      }
    } else if (v[0] === tupleSym) {
      let tmp = '(';

      for (let j = 1; j < v.length; j++) {
        tmp += lc_to_string(v[j]);
        if (j < v.length - 1) {
          tmp += ", "
        }
      }

      tmp += ')'
      content += tmp;
    } else {
      let tmp = '[';

      for (let j = 0; j < v.length; j++) {
        tmp += lc_to_string(v[j]);
        if (j < v.length - 1) {
          tmp += ", "
        }
      }

      tmp += ']'
      content += tmp;
    }
  } else {
    content += v;
  }

  return content;
}

function lc_std_print(...args) {
  let content = '';
  for (let i = 0; i < args.length; i++) {
    if (i != 0) {
      content += ' ';
    }
    content += lc_to_string(args[i]);
  }
  console.log(content);
}

function LCC_Object_toString() {
  return "Object";
}

const LCC_Object = {
  [clsNameSym]: "Object",
  toString: LCC_Object_toString,
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

const LCC_Map = {
  __proto__: LCC_Object,
  [clsNameSym]: "Map",
  getIterator: LCC_Map_getIterator
}

const LCC_MapIterator = {
  __proto__: LCC_Object,
  [clsNameSym]: "MapIterator",
  next: LCC_MapIterator_next
}

function lc_std_map_new() {
  const data = new Map();
  return {
    __proto__: LCC_Map,
    data,
  };
}

function LCC_Map_getIterator() {
  const map = this.data;
  const rawIter = map[Symbol.iterator]();
  return {
    __proto__: LCC_MapIterator,
    map,
    rawIter,
  };
}

function LCC_MapIterator_next() {
  const rawIter = this.rawIter;
  if (typeof rawIter === 'undefined') {
    return [LCC_Option, 1];
  }
  const nextItem = rawIter.next();
  if (!nextItem.done) {
    const [key, value] = nextItem.value;
    return [LCC_Option, 0, [tupleSym, key, value]];
  }
  delete this.rawIter;
  return [LCC_Option, 1];
}

function lc_std_map_get(key, value) {
  const tmp = Map.prototype.get.call(this.data, key, value);
  if (tmp) {
    return [LCC_Option, 0, tmp];
  }
  return [LCC_Option, 1];
}

function lc_std_map_set(key, value) {
  Map.prototype.set.call(this.data, key, value);
}

function lc_std_map_remove(key, value) {
  const tmp = Map.prototype.delete.call(this.data, key, value);
  if (tmp) {
    return [LCC_Option, 0, tmp];
  }
  return [LCC_Option, 1];
}

function lc_std_map_size() {
  return this.data.size;
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
  throw new Error("panic: " + lc_to_string(message));
}

function lc_std_get_args() {
  if (typeof process === 'undefined') {
    return [];
  }
  return [...process.argv];
}

const LCC_Buffer = {
  __proto__: LCC_Object,
  [clsNameSym]: "Buffer"
}

function lc_std_new_buffer() {
  const content = new Uint8Array(32);
  return {
    __proto__: LCC_Buffer,
    content,
    size: 0,
    toString: lc_buffer_to_string,
  };
}


function lc_buffer_extends_if_need(len) {
  while ((this.size + len) > this.content.length) {
    const newLen = (this.content.length | 0) * 2;
    const newArray = new Uint8Array(newLen);
    newArray.set(this.content, 0);
    this.content = newArray;
  }
}

function lc_buffer_concat(buf) {
  lc_buffer_extends_if_need.call(this, buf.length);
  this.content.set(buf, this.size);
  this.size += buf.length;
}

function lc_buffer_to_string() {
  const decoder = new TextDecoder();
  return decoder.decode(new Uint8Array(this.content.buffer, 0, this.size));
}

function lc_std_buffer_add_string(str) {
  const buffer = new TextEncoder();
  const u8Str = buffer.encode(str);
  lc_buffer_concat.call(this, u8Str);
}

function lc_std_buffer_add_any(obj) {
  const str = obj.toString();
  lc_std_buffer_add_string.call(this, str);
}
