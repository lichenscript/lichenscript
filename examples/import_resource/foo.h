

#ifndef FOO_DEF
#define FOO_DEF

#include <stdio.h>

LCValue ext_foo(LCRuntime*rt, LCValue this, int argc, LCValue* args) {
	printf("foo\n");
	return MK_NULL();
}

#endif
