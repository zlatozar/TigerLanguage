## List of basic LLVM IR instructions

[Mapping High Level Constructs to LLVM IR](https://mapping-high-level-constructs-to-llvm-ir.readthedocs.io/en/latest/README.html)

Here is some examples:

### Instruction to load and store global variables

```llvm
@global_var = global i32 2
define i32 @main() {
  %1 = load i32, i32* @global_var ; load global variable’s value
  store i32 1, i32* @global_var   ; store 1 to global variable
  ret i32 %1                      ; return %1
}
```

### Instruction to allocate and store stack-allocated local variables

```llvm
define i32 @main() {
  %a = alloca i32       ; allocate variable a on stack
  store i32 1, i32* %a  ; store value 1 to address %a
  ret i32 1
}
```

### Arithmetic instructions

```llvm
define i32 @main() {
  %1= alloca i32
  store i32 1, i32* %1
  %a = load i32, i32* %1
  %sum = add i32 %a, 6       ; add instruction
  %sub = sub i32 6, %a       ; subtract instruction
  %product = mul i32 %a, 6   ; multiply instruction
  %division = sdiv i32 6, %a ; division instruction
  ret i32 1
}
```

### Comparison instructions

```llvm
define i32 @main() {
  %1 = alloca i32
  store i32 1, i32* %1
  %a = load i32, i32* %1
  %equal     = icmp eq i32 %a, 6
  %not_equal = icmp ne i32 %a, 6
  %less_than             = icmp slt i32 %a, 6
  %less_than_or_equal    = icmp sle i32 %a, 6
  %greater_than          = icmp sgt i32 %a, 6
  %greater_than_or_equal = icmp sge i32 %a, 6
  ret i32 1
}
```

### Branching instructions

```llvm
define i32 @max(i32 %a, i32 %b) {

entry:
  %0 = icmp sgt i32 %a, %b                         ; compare %a and %b
  br i1 %0, label %true_block, label %false_block  ; conditional branching

true_block:             ; preds = %entry
  br label %merge_block ; unconditional branching

false_block:
  br label %merge_block

; preds = %true_block, %false_block
merge_block:
  ; phi instruction selects return_val based on the previously executed block in the control flow
  %return_val = phi i32[%a, %true_block], [%b, %false_block]
  ret i32 1
}

```

### Function call instruction

```llvm
define i32 @main() {
  %result = call i32 @do_nothing(i32 1)   ; call function do_nothing() with value 1
  ret i32 %result
}

define i32 @do_nothing(i32 %a) {
  ret i32 %a
}
```

### Struct related instructions

```llvm
%Foo_struct = type {i32, i32}       ; %Foo_struct = {a: int, b: int}

; external function to allocate heap memory
declare i8* @malloc(i32)

define %Foo_struct* @create_foo() nounwind {

  ; allocate heap memory for Foo_struct
  %foo_address = call i8* @malloc(i32 8)
  %foo = bitcast i8* %foo_address to %Foo_struct*

  ; Compute the address of the first element of %Foo_struct with
  ; instruction getelementptr then save value 3 to that address
  %a = getelementptr %Foo_struct* %foo, i32 0, i32 0
  store i32 3, i32* %a

  ; Save value 4 to that address
  %b = getelementptr %Foo_struct* %foo, i32 0, i32 0
  store i32 4, i32* %b

  ; Load values of thefirst and second element of struct from their addresses
  %a_val = load i32, i32* %a, align 4
  %b_val = load i32, i32* %b, align 4
  %sum = add i32 %a_val, %b_val
  ret %foo
}
```

### Array related instructions

```llvm
; external function to allocate on heap
declare i8* @malloc(i32)

define i32* @create_array() nounwind {
  ; allocate heap memory for array of size 3
  %array_address = call i8* @malloc(i32 12)
  %array = bitcast i8* %array_address to i32*

  ; Compute the address of the second element of the array and save value 3 to that address
  %a = getelementptr i32* %array, i32 1
  store i32 3, i32* %a

  ; Compute the address of the third element of the array and save value 4 to that address
  %b = getelementptr i32* %array, i32 2
  store i32 4, i32* %b

  ; Load values of thesecond and thirdelement of struct from their addresses
  %a_val = load i32, i32* %a, align 4
  %b_val = load i32, i32* %b, align 4
  %sum = add i32 %a_val, %b_val
  ret %array
}
```
