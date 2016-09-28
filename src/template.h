#include <stdint.h>

#define EFI_FORWARD_DECLARATION(x) typedef struct x x;
#define EFIAPI __attribute__((annotate("efi_fun_decl")))

/* Unfortunately, function arguments do not support attributes.
 * Instead, in/out/optional params are marked by fake arguments.*/
struct efi_arg_in {};
struct efi_arg_out {};
struct efi_arg_optional {};
#define IN struct efi_arg_in,
#define OUT struct efi_arg_out,
#define OPTIONAL ,struct efi_arg_optional

typedef struct efi_status {} EFI_STATUS;
typedef struct efi_uintn {} UINTN;
typedef struct efi_intn {} INTN;
typedef struct efi_bool {} BOOLEAN;
typedef struct efi_int8 {} INT8;
typedef struct efi_uint8 {} UINT8;
typedef struct efi_int16 {} INT16;
typedef struct efi_uint16 {} UINT16;
typedef struct efi_int32 {} INT32;
typedef struct efi_uint32 {} UINT32;
typedef struct efi_int64 {} INT64;
typedef struct efi_uint64 {} UINT64;
typedef struct efi_char8 {} CHAR8;
typedef struct efi_char16 {} CHAR16;

typedef struct efi_guid {} EFI_GUID;

