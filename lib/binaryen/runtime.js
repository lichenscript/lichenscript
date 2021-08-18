//Provides: unix_O_APPEND
//Requires: caml_int64_of_int32
function unix_O_APPEND() {
  return caml_int64_of_int32(0x00000008);
}

//Provides: unix_O_ASYNC
//Requires: caml_int64_of_int32
function unix_O_ASYNC() {
  return caml_int64_of_int32(0x00000040);
}

//Provides: unix_O_CLOEXEC
//Requires: caml_int64_of_int32
function unix_O_CLOEXEC() {
  return caml_int64_of_int32(0x01000000);
}

//Provides: unix_O_CREAT
//Requires: caml_int64_of_int32
function unix_O_CREAT() {
  return caml_int64_of_int32(0x01000000);
}

//Provides: unix_O_DIRECT
//Requires: caml_int64_of_int32
function unix_O_DIRECT() {
  return caml_int64_of_int32(0x00100000);
}

//Provides: unix_O_DIRECTORY
//Requires: caml_int64_of_int32
function unix_O_DIRECTORY() {
  return caml_int64_of_int32(0x00100000);
}

//Provides: unix_O_DSYNC
//Requires: caml_int64_of_int32
function unix_O_DSYNC() {
  return caml_int64_of_int32(0x00400000);
}

//untructed

//Provides: unix_O_EXCL
//Requires: caml_int64_of_int32
function unix_O_EXCL() {
  return caml_int64_of_int32(0x00400000);
}

//Provides: unix_O_NOATIME
//Requires: caml_int64_of_int32
function unix_O_NOATIME() {
  return caml_int64_of_int32(0x00400000);
}

//Provides: unix_O_NOCTTY
//Requires: caml_int64_of_int32
function unix_O_NOCTTY() {
  return caml_int64_of_int32(0x00400000);
}

//Provides: unix_O_NOFOLLOW
//Requires: caml_int64_of_int32
function unix_O_NOFOLLOW() {
  return caml_int64_of_int32(0x00400000);
}

//Provides: unix_O_NONBLOCK
//Requires: caml_int64_of_int32
function unix_O_NONBLOCK() {
  return caml_int64_of_int32(0x00400000);
}

//Provides: unix_O_RDONLY
//Requires: caml_int64_of_int32
function unix_O_RDONLY() {
  return caml_int64_of_int32(0x00400000);
}

//Provides: unix_O_RDWR
//Requires: caml_int64_of_int32
function unix_O_RDWR() {
  return caml_int64_of_int32(0x00400000);
}

//Provides: unix_O_RSYNC
//Requires: caml_int64_of_int32
function unix_O_RSYNC() {
  return caml_int64_of_int32(0x00400000);
}

//Provides: unix_O_SYNC
//Requires: caml_int64_of_int32
function unix_O_SYNC() {
  return caml_int64_of_int32(0x00400000);
}

//Provides: unix_O_TRUNC
//Requires: caml_int64_of_int32
function unix_O_TRUNC() {
  return caml_int64_of_int32(0x00400000);
}

//Provides: unix_O_WRONLY
//Requires: caml_int64_of_int32
function unix_O_WRONLY() {
  return caml_int64_of_int32(0x00400000);
}

//Provides: unix_F_GETFL
//Requires: caml_int64_of_int32
function unix_F_GETFL() {
  return caml_int64_of_int32(0x00400000);
}

//Provides: unix_F_SETFL
//Requires: caml_int64_of_int32
function unix_F_SETFL() {
  return caml_int64_of_int32(0x00400000);
}

//Provides: unix_create_error_checking_mutex
function unix_create_error_checking_mutex() {
  return {};
}

//Provides: core_unix_inet4_addr_to_int32_exn
function core_unix_inet4_addr_to_int32_exn() {
  return 0; 
}

//Provides: integers_unsigned_init
function integers_unsigned_init() {
  return 0;
}

//Provides: integers_uint32_max
function integers_uint32_max() {
  return 100;
}

//Provides: integers_uint32_of_int
function integers_uint32_of_int(value) {
  return value;
}

//Provides: integers_uint64_max
function integers_uint64_max() {
  return 100;
}

//Provides: integers_uint64_of_int
function integers_uint64_of_int(value) {
  return value;
}

//Provides: integers_size_t_size
function integers_size_t_size() {
  return 4;
}

//Provides: integers_ushort_size
function integers_ushort_size() {
  return 2;
}

//Provides: integers_uint_size
function integers_uint_size() {
  return 4;
}

//Provides: integers_ulong_size
function integers_ulong_size() {
  return 4;
}

//Provides: integers_ulonglong_size
function integers_ulonglong_size() {
  return 8;
}

//Provides: ctypes_ldouble_min
function ctypes_ldouble_min() {
  return 0;
}

//Provides: ctypes_ldouble_max
function ctypes_ldouble_max() {
  return 0;
}

//Provides: ctypes_ldouble_epsilon
function ctypes_ldouble_epsilon() {
  return 0;
}

//Provides: ctypes_ldouble_nan
function ctypes_ldouble_nan() {
  return NaN;
}

//Provides: ctypes_ldouble_inf
function ctypes_ldouble_inf() {
  return Infinity;
}

//Provides: ctypes_ldouble_ninf
function ctypes_ldouble_ninf() {
  return -Infinity;
}

//Provides: ctypes_ldouble_of_int
function ctypes_ldouble_of_int() {
  return 0;
}

//Provides: ctypes_ldouble_size
function ctypes_ldouble_size() {
  return 8;
}

//Provides: ctypes_ldouble_mant_dig
function ctypes_ldouble_mant_dig() {
  return 0;
}

//Provides: ctypes_ldouble_complex_make
function ctypes_ldouble_complex_make() {
  return 0;
}

//Provides: integers_intptr_t_size
function integers_intptr_t_size() {
  return 4;
}

//Provides: integers_uintptr_t_size
function integers_uintptr_t_size() {
  return 4;
}

//Provides: integers_ptrdiff_t_size
function integers_ptrdiff_t_size() {
  return 4;
}

//Provides: ctypes_typeof_clock_t
function ctypes_typeof_clock_t() {
  return 0;
}

//Provides: ctypes_typeof_dev_t
function ctypes_typeof_dev_t() {
  return 0;
}

//Provides: ctypes_typeof_ino_t
function ctypes_typeof_ino_t() {
  return 0;
}

//Provides: ctypes_typeof_mode_t
function ctypes_typeof_mode_t() {
  return 0;
}

//Provides: ctypes_typeof_nlink_t
function ctypes_typeof_nlink_t() {
  return 0;
}

//Provides: ctypes_typeof_off_t
function ctypes_typeof_off_t() {
  return 0;
}

//Provides: ctypes_typeof_pid_t
function ctypes_typeof_pid_t() {
  return 0;
}

//Provides: ctypes_typeof_ssize_t
function ctypes_typeof_ssize_t() {
  return 0;
}

//Provides: ctypes_typeof_time_t
function ctypes_typeof_time_t() {
  return 0;
}

//Provides: ctypes_typeof_useconds_t
function ctypes_typeof_useconds_t() {
  return 0;
}

//Provides: ctypes_sizeof_sigset_t
function ctypes_sizeof_sigset_t() {
  return 0;
}

//Provides: ctypes_alignmentof_sigset_t
function ctypes_alignmentof_sigset_t() {
  return 0;
}

//Provides: ldouble_init
function ldouble_init() {}

//Provides: caml_thread_initialize
function caml_thread_initialize() {}

//Provides: caml_mutex_new
function caml_mutex_new() {
  return {};
}

//Provides: core_unix_error_of_code
function core_unix_error_of_code(code) {
  return {};
}
