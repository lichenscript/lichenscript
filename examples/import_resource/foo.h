

#ifndef FOO_DEF
#define FOO_DEF

#include <stdio.h>

LCValue ext_foo(LCRuntime* rt, LCValue this, int argc, LCValue* args) {
	const char* str = LCToUTF8(rt, args[0]);
	printf("name: %s\n", str);
	LCFreeUTF8(rt, str);
	return MK_NULL();
}

#endif
