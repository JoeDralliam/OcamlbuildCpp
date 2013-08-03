#include <caml/mlvalues.h>

typedef enum os
{
    os_linux = 0,
    os_windows = 1,
    os_mac = 2
} os;



#if defined(__linux__)
const os current_os = os_linux;
#elif defined(_WIN32)
const os current_os = os_windows;
#elif defined(__APPLE__) && defined(__MACH__)
const is current_os = os_mac;
#else
#error "Unsupported OS"
#endif


CAMLprim value conf_os(value unit)
{
    return Val_int((int)current_os);
}
