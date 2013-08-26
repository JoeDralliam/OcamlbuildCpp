#include <caml/mlvalues.h>

typedef enum os
{
    os_linux = 0,
    os_windows = 1,
    os_mac = 2
} os;


typedef enum compiler
{
    gcc = 0,
    clang,
    msvc,
    mingw,
    cygwin
} compiler;


typedef enum msvc_version
{
    vc6 = 0,
    vc7,
    vc7_1,
    vc8,
    vc9,
    vc10,
    vc11,
    msvc_unknown
} msvc_version;





#if defined(__linux__)
const os current_os = os_linux;
#elif defined(_WIN32)
const os current_os = os_windows;
#elif defined(__APPLE__) && defined(__MACH__)
const os current_os = os_mac;
#else
#error "Unsupported OS"
#endif

#if defined(__clang__)
const compiler default_compiler = clang;
#elif defined(__MINGW32__)
const compiler default_compiler = mingw;
#elif defined(__CYGWIN__)
const compiler default_compiler = cygwin;
#elif defined(__GNUC__)
const compiler default_compiler = gcc;
#elif defined(_MSC_VER)
const compiler default_compiler = msvc;
#else
#error "Unsupported compiler"
#endif


CAMLprim value conf_os(value unit)
{
    return Val_int((int)current_os);
}


CAMLprim value conf_compiler(value unit)
{
    return Val_int((int)default_compiler);
}

#if defined(_MSC_VER) && _MSC_VER >= 1200

CAMLprim value conf_msvc_version_major(value unit)
{
    return Val_int((_MSC_VER - 600) / 100)
}

CAMLprim value conf_msvc_version_minor(value unit)
{
    return Val_int((_MSC_VER % 100)
}

#else

CAMLprim value conf_msvc_version_major(value unit)
{
    return Val_int(-1);
}

CAMLprim value conf_msvc_version_minor(value unit)
{
    return Val_int(-1);
}

#endif

