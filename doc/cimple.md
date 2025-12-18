# Cimple Language Peculiarities

Cimple is a strict subset of the C programming language, designed to enforce a
clear and consistent coding style. All valid Cimple code is also valid C code
that can be compiled by standard C compilers. However, the reverse is not true;
Cimple imposes several additional constraints to improve code readability,
maintainability, and to simplify static analysis.

This document outlines the key areas where Cimple's syntax and conventions
differ from standard C.

## 1. Global Scope

### File Structure

Cimple files may optionally begin with a specially formatted license and
copyright block enclosed in `/* ... */` comments. This is the only construct
that may precede other top-level declarations.

### Global Variable Declarations

All variables declared at the global or file scope (i.e., outside of any
function) must be declared `const` or `extern const`. Mutable global or static
variables are prohibited.

**Invalid:**

```c
int global_counter = 0;
static int file_scope_value;
```

**Valid:**

```c
const int MAX_USERS = 100;
extern const Config global_config;
static const char * const DEFAULT_NAME = "default";
```

## 2. Naming Conventions

Cimple enforces strict naming conventions for different types of identifiers.

- **Variables and Functions**: Must use `snake_case` or `camelCase` starting
  with a lowercase letter (e.g., `my_variable`, `calculateValue`).
- **Structs, Unions, and Enum Types**: Must use `Upper_Snake_Case` (e.g.,
  `My_Struct`, `Address_Info`, `Error_Code`).
- **Standard Type Typedefs**: Typedefs for data types must end with `_t`
  (e.g., `uint32_t`, `status_t`).
- **Function Type Typedefs**: Typedefs for function types must end with `_cb`
  (for "callback") (e.g., `network_event_cb`).
- **Constants, Enum Members, and Goto Labels**: Must use
  `UPPER_CASE_WITH_UNDERSCORES` (e.g., `MAX_CONNECTIONS`, `DEFAULT_PORT`,
  `ERROR_HANDLE`, `STATUS_OK`).

## 3. Syntax and Structure

### One Declaration Per Statement

To improve readability and avoid common mistakes, Cimple enforces that each
variable declaration must be its own separate statement. Declaring multiple
variables in a single statement, such as `int a, b;`, is not allowed.

This rule is especially important for pointer types, as it prevents common bugs.
For example, in the statement `char *x, y;`, a developer might mistakenly assume
both `x` and `y` are pointers, when in fact only `x` is a pointer to a `char`,
while `y` is just a `char`. Cimple avoids this ambiguity altogether.

**Invalid:**

```c
int a, b;
char *x, y;
```

**Valid:**

```c
int a;
int b;
char *x;
char y;
```

### Typedef Requirement for Function Types

Function pointers in Cimple must be defined using a `typedef`. The `typedef`
must define a function type (not a function pointer type), and its name must end
with the `_cb` suffix.

**Invalid:**

```c
// Direct function pointer declaration
void (*my_func_ptr)(int);

// Typedef for a function pointer type
typedef void (*my_func_ptr_t)(int);
my_func_ptr_t ptr;
```

**Valid:**

```c
// 1. Define the function type with a _cb suffix
typedef void my_func_cb(int);

// 2. Use the function type to declare the function pointer
my_func_cb *ptr;
```

### Named Parameters Required

All parameters in a function definition or a function type `typedef` must have a
name. Unnamed parameters are not allowed.

**Invalid:**

```c
void my_func(int); // Unnamed parameter in declaration

typedef void my_func_cb(int**); // Unnamed parameter in typedef
```

**Valid:**

```c
void my_func(int arg);

typedef void my_func_cb(int** p);
```

### Mandatory Compound Statements for Control Flow

The bodies of `for`, `while`, and `do-while` loops must be enclosed in braces
(`{...}`), even if the body consists of a single statement.

**Invalid:**

```c
while (condition)
    do_something();

for (int i = 0; i < 10; i++)
    printf("%d\n", i);
```

**Valid:**

```c
while (condition) {
    do_something();
}

for (int i = 0; i < 10; i++) {
    printf("%d\n", i);
}
```

### Restricted `if`/`else` Statement Bodies

The body of an `if` or `else` statement must be either a compound statement
(`{...}`) or a single `return` statement. Other single statements, such as
assignments or function calls, are not permitted without being enclosed in
braces.

**Invalid:**

```c
if (condition)
    x = 1;
```

**Valid:**

```c
if (condition) {
    x = 1;
}

if (condition)
    return;
```

### Structured `switch` Cases

Cimple enforces a more structured `switch` statement that prevents accidental
fallthrough. Each `case` block must end explicitly with a `break;`, `return;`,
or another `case` label to indicate intentional fallthrough. Arbitrary
statements are not allowed to precede a `break` or `return` without being
enclosed in a compound statement.

### `for` Loop Initialization and Advancement

The initialization and advancement clauses of a `for` loop are restricted.

- The **initialization** clause must be either a single variable declaration
  (with optional initialization) or a single assignment expression. It cannot
  be empty or contain multiple comma-separated expressions.
- The **advancement** clause must be a single expression statement (like `i++`
  or `i += 1`).

**Invalid:**

```c
// Multiple initializations
for (int i = 0, j = 0; i < 10; i++) { ... }

// Empty initialization
for (; i < 10; i++) { ... }
```

**Valid:**

```c
for (int i = 0; i < 10; i++) { ... }

for (i = 0; i < 10; i++) { ... }
```

### `goto` Label Naming

Labels for `goto` statements must follow the same `UPPER_SNAKE_CASE` convention
as constants.

**Invalid:**

```c
my_label:
  // ...
goto my_label;
```

**Valid:**

```c
CLEANUP:
  // ...
goto CLEANUP;
```

### Null Pointer Representation

The `nullptr` keyword must be used to represent a null pointer. Using the
integer literal `0` for null pointers is prohibited.

**Invalid:**

```c
int *p = 0;
if (ptr == 0) { ... }
```

**Valid:**

```c
int *p = nullptr;
if (ptr == nullptr) { ... }
```

### Preprocessor Directives

To improve readability and make it easier to track nested conditional
compilation blocks, every `#endif` directive must be followed by a comment
indicating which flag it is closing.

**Invalid:**

```c
#ifdef ENABLE_FEATURE
  // ...
#endif
```

**Valid:**

```c
#ifdef ENABLE_FEATURE
  // ...
#endif /* ENABLE_FEATURE */
```

### Statement-Like Macros

Macros that behave like statements must be wrapped in a `do { ... } while (0)`
block to ensure correct scoping and parsing.

**Invalid:**

```c
#define MY_MACRO(x) x = 1; y = 2;
```

**Valid:**

```c
#define MY_MACRO(x) do { x = 1; y = 2; } while (0)
```

### Pointer Depth Limit

Cimple restricts pointer declarations to a maximum of two levels of indirection
(`**`). Pointers with three or more levels of indirection are not supported and
will cause a parse error.

**Valid:**

```c
int *p;
const char **argv;
```

**Invalid:**

```c
int ***p;
My_Struct ****s;
```

### Restricted Compound Literals

Cimple only supports a very restricted form of C's compound literals, intended
primarily for zero-initialization with a constant expression.

**Valid:**

```c
My_Struct s = (My_Struct){0};
```

### Restricted Expression Statements

In standard C, nearly any expression can become a statement by appending a
semicolon. Cimple is much stricter and only permits the following expressions as
standalone statements:

- Assignments
- Function calls
- Pre-increment and pre-decrement operations

**Invalid:**

```c
1 + 2; // Not a valid statement
x;     // Accessing a variable is not a statement
```

**Valid:**

```c
x = y;
my_function();
++counter;
```

### Unsupported Literals

The Cimple lexer does not support all of C's numeric literal formats.

- **Octal literals** (e.g., `077`) are not supported.
- **Scientific notation** for floating-point numbers (e.g., `6.02e23`) is not
  supported.

### No Empty Statements

A lone semicolon (`;`), which constitutes an empty statement in standard C, is
not a valid statement in Cimple and will result in a parse error.

### No Anonymous or Nested Aggregate Types

To enforce naming conventions and simplify static analysis, all structs,
unions, and enums must be defined at the top level (global scope) and must be
named. Anonymous aggregate types and aggregate types defined within another
struct or union are not supported.

**Invalid:**

```c
struct My_Struct {
    int x;
    union { // Anonymous nested union
        int a;
        float b;
    } data;
};
```

**Valid:**

```c
union My_Union {
    int a;
    float b;
};

struct My_Struct {
    int x;
    union My_Union data;
};
```

## 4. Supported C Features

### Variadic Functions

Cimple supports the standard C syntax for declaring variadic functions using an
ellipsis (`...`).

**Valid:**

```c
void my_printf(const char *format, ...);
```

## 5. Custom Keywords and Qualifiers

Cimple introduces several custom keywords and qualifiers that are parsed as
first-class language features. These are typically implemented as macros in the
underlying C code but are treated as keywords by the Cimple parser.

- **Nullability**: `_Nonnull` and `_Nullable` are used to specify pointer
  nullability.
- **Ownership and Type Modifiers**: `owner`, `bitwise`, and `force` are custom
  type qualifiers.
- **Variable Length Arrays (VLAs)**: Cimple uses a specific macro-like syntax
  for declaring VLAs: `VLA(type, name, size);`.

  **Example:**

  ```c
  VLA(uint8_t, buffer, length);
  ```

## 6. Unsupported C Features

The following standard C features are intentionally not supported in Cimple:

- **Operators**: The post-increment (`x++`) and post-decrement (`x--`)
  operators are not supported. Only pre-increment (`++x`) and pre-decrement
  (`--x`) are allowed.
- **Keywords**: The `register` and `volatile` keywords are not recognized.
- **K&R-style Function Definitions**: Old-style function definitions where
  parameter types are declared between the argument list and the function body
  are not allowed. All functions must use modern, prototyped parameter lists.
- **Compound Expressions**: The GNU C extension for compound expressions
  (e.g., `({ int y = foo(); y; })`) is considered deprecated and is not
  supported.

## 7. Built-in Types

Cimple has built-in knowledge of certain standard C types. You do not need to
provide `typedef`s for these in your source code for the parser to understand
them.

- `size_t`: This is recognized as a built-in unsigned integer type, equivalent
  to `unsigned long`. These built-in types are reserved and cannot be
  redefined.
- **Fixed-width Integers**: The standard fixed-width integer types from
  `<stdint.h>` (e.g., `uint8_t`, `int8_t`, `uint16_t`, `int16_t`, `uint32_t`,
  `int32_t`, `uint64_t`, `int64_t`) are built-in. These are the preferred
  types for integer variables, rather than using types like `unsigned char`,
  `short`, `int`, or `long`.
