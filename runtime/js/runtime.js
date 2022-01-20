
const i32_max = 0x7FFFFFFF;
const i32_min = 0x7FFFFFFF * -1 + 1;

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

  let tmp = a - b;

  return tmp|0;
}
