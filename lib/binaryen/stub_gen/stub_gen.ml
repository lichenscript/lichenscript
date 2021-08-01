open Core
open Bindings

let codegen =
  let sys_argvs = Sys.get_argv() in
  let last = Array.last sys_argvs in
  if String.equal last "-c" then (
    printf "gen C\n";
    Format.fprintf Format.str_formatter "#include <binaryen-c.h>
    #include <stdlib.h>
    #include <stdio.h>
    #include <string.h>
    #include <fcntl.h>
    #include <unistd.h>
    #include <sys/mman.h>

    static void dump_bytes_to_path(void* bytes, size_t len, const char* path) {
      int fd;
      fd = open(path, O_RDWR | O_CREAT | O_TRUNC, 0666);
      if (fd < 0) {
        printf(\"can not open file\");
        return;
      }

      ftruncate(fd, len);

      void* src = mmap(NULL, len, PROT_READ | PROT_WRITE,
        MAP_SHARED, fd, 0);
      if (src < 0 || src == 0) {
        close(fd);
        printf(\"map to file failed\");
        return;
      }

      memcpy(src, bytes, len);
      munmap(src, len);
      close(fd);
    }

    static void clean_binary_result(BinaryenModuleAllocateAndWriteResult result) {
      if (result.binary) {
        free(result.binary);
      }
      if (result.sourceMap) {
        free(result.sourceMap);
      }
    }\n";
    Cstubs.write_c Format.str_formatter ~prefix:"binaryen_stub" (module Bindings);
    let c_content = Format.flush_str_formatter () in
    Out_channel.write_all "b.c" ~data:c_content
  ) else (
    Cstubs.write_ml Format.str_formatter ~prefix:"binaryen_stub" (module Bindings);
    let ml_content = Format.flush_str_formatter () in
    Out_channel.write_all "b.ml" ~data:ml_content
  )
