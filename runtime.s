#
# Using this file then there is no need to use
# sysspim.s or to compile runtime.c
#
#
# NOTE: Do not rename labels because they names are
#       defined in Tiger Standard Library
#
# NOTE: https://godbolt.org/ could be used
#       to translate C to MIPS assembler

  .text

# int *initArray(int size, int init) {
#   int i;
#   int *a = (int *) malloc(size *sizeof(int));
#
#   for (i = 0; i < size; i++)
#     a[i] = init;
#
#   return a;
# }

initArray:
  move  $a3, $a0
  addiu $a0, $a0, 1
  sll   $a0, $a0, 2
  li    $v0, 9
  syscall
  move  $a2, $v0
  sw    $a3, ($a2)
  sub   $a0, $a0, 4
  add   $a2, $a2, 4
  b     Lrun1

Lrunt2:
  sw    $a1, ($a2)
  sub   $a0, $a0, 4
  add   $a2, $a2, 4

Lrun1:
  bgtz  $a0, Lrunt2
  jr    $ra

# int *allocRecord(int size) {
#   int i;
#   int *p, *a;
#
#   p = a = (int *) malloc(size);
#
#   for (i = 0; i < size; i += sizeof(int))
#     *p++ = 0;
#
#   return a;
# }

allocRecord:
  li   $v0, 9
  syscall
  move $a2, $v0
  b    Lrunt4

Lrunt3:
  sw   $0, ($a2)
  sub  $a0, $a0, 4
  add  $a2, $a2, 4

Lrunt4:
  bgtz $a0, Lrunt3
  jr   $ra

# struct string {
#   int length;
#   unsigned char chars[1];
# };

# int stringEqual(struct string *s, struct string *t) {
#   int i;
#   if (s == t) return 1;
#
#   if (s->length != t->length) return 0;
#
#   for (i = 0; i < s->length; i++)
#     if (s->chars[i] != t->chars[i]) return 0;
#
#   return 1;
# }

stringEqual:
  beq   $a0, $a1, Lrunt10
  lw    $a2, ($a0)
  lw    $a3, ($a1)
  addiu $a0, $a0, 4
  addiu $a1, $a1, 4
  beq   $a2, $a3, Lrunt11

Lrunt13:
  li    $v0, 0
  j     $ra

Lrunt12:
  lbu   $t0, ($a0)
  lbu   $t1, ($a1)
  bne   $t0, $t1, Lrunt13
  addiu $a0, $a0, 1
  addiu $a1, $a1, 1
  addiu $a2, $a2, -1

Lrunt11:
  bgez  $a2, Lrunt12

Lrunt10:
  li    $v0, 1
  j     $ra

# void print(struct string *s) {
#   int i;
#   unsigned char *p = s->chars;
#
#   for (i = 0; i < s->length; i++, p++)
#     putchar(*p);
# }

  .data
  .align 4
putchar_buf:	.byte 0 0
  .text

print:
  lw    $t1, 0($a0)
  beqz  $t1, Lrunt56
  add   $t0, $a0, 4
  la    $a0, putchar_buf

Lrunt57:
  lbu   $t3, ($t0)
  sb    $t3, 0($a0)
  li    $v0, 4
  syscall
  addiu $t0, $t0, 1
  addiu $t1, $t1, -1
  bgtz  $t1, Lrunt57

Lrunt56:
  jr $ra

# void flush() {
#   fflush(stdout);
# }

flush:
  jr $ra

# struct string consts[256];
#
# int main() {
#   int i;
#   for (i = 0; i < 256; i++) {
#     consts[i].length = 1;
#     consts[i].chars[0] = i;
#   }
#
#   return __main(0 /* static link!? */);
# }

  .data
  .align 4
Runtconsts: .space 2048
Runtempty:  .word  0
  .text

main:
  li    $a0, 0
  la    $a1, Runtconsts
  li    $a2, 1

Lrunt20:
  sw    $a2, ($a1)
  sb    $a0, 4($a1)
  addiu $a1, $a1, 8
  addiu $a0, $a0, 1
  slt   $a3, $a0, 256
  bnez  $a3, Lrunt20
  li    $a0, 0
  # See how main method is defined in FrontEnd.fs
  j     __main

# int ord(struct string *s) {
#
#   if (s->length == 0)
#     return -1;
#
#   else
#     return s->chars[0];
# }

ord:
  lw   $a1, ($a0)
  li   $v0, -1
  beqz $a1, Lrunt5
  lbu  $v0, 4($a0)

Lrunt5:
  jr   $ra

# struct string empty = { 0, "" };
#
# struct string *chr(int i) {
#   if (i < 0 || i >= 256) {
#     printf("chr(%d) out of range\n", i);
#     exit(1);
#   }
#
#   return consts + i;
# }

  .data
Lrunt30: .asciiz "character out of range\n"
  .text

chr:
  srl  $a1, $a0, 8
  bnez $a1, Lrunt31
  sll  $a0, $a0, 3
  la   $v0, Runtconsts($a0)
  j    $ra

Lrunt31:
  li   $v0, 4
  la   $a0, Lrunt30
  syscall
  li   $v0, 10
  syscall

# int size(struct string *s) {
#   return s->length;
# }

size:
  lw $v0, ($a0)
  jr $ra

# struct string *substring(struct string *s, int first, int n) {
#
#   if (first < 0 || first + n > s->length) {
#     printf("substring([%d], %d, %d) out of range\n", s->length, first, n);
#     exit(1);
#   }
#
#   if (n == 1) return consts + s->chars[first]; {
#
#     struct string *t = (struct string *) malloc(sizeof(int) + n);
#
#     int i;
#     t->length = n;
#
#     for (i = 0; i < n; i++)
#       t->chars[i] = s->chars[first + i];
#
#     return t;
#   }
# }

  .data
Lrunt40:  .asciiz "substring out of bounds\n"
  .text

substring:
  lw    $t1, ($a0)
  bltz  $a1, Lrunt41
  add   $t2, $a1, $a2
  sgt   $t3, $t2, $t1
  bnez  $t3, Lrunt41
  add   $t1, $a0, $a1
  addiu $t1, $t1, 4
  bne   $a2, 1, Lrunt42
  lbu   $a0, ($t1)
  b     chr

Lrunt42:
  bnez  $a2, Lrunt43
  la    $v0, Runtempty
  jr    $ra

Lrunt43:
  addi  $a0, $a2, 4
  li    $v0, 9
  syscall
  move  $t2, $v0
  sw    $a2, ($t2)
  addiu $t2, $t2, 4

Lrunt44:
  lbu   $t3, ($t1)
  sb    $t3, ($t2)
  addiu $t1, $t1, 1
  addiu $t2, $t2, 1
  addiu $a2, $a2, -1
  bgtz  $a2 ,Lrunt44
  jr    $ra

Lrunt41:
  li    $v0, 4
  la    $a0, Lrunt40
  syscall
  li    $v0, 10
  syscall

# struct string *concat(struct string *a, struct string *b) {
#
#   if (a->length == 0)
#     return b;
#
#   else if (b->length == 0)
#     return a;
#
#   else {
#     int i, n = a->length + b->length;
#
#     struct string *t = (struct string * ) malloc(sizeof(int) + n);
#
#     t->length = n;
#
#     for (i = 0; i < a->length; i++)
#       t->chars[i] = a->chars[i];
#
#     for (i = 0; i < b->length; i++)
#       t->chars[i + a->length] = b->chars[i];
#
#     return t;
#   }
# }

concat:
  lw    $t0, ($a0)
  lw    $t1, ($a1)
  beqz  $t0, Lrunt50
  beqz  $t1, Lrunt51
  addiu $t2, $a0, 4
  addiu $t3, $a1, 4
  add   $t4, $t0, $t1
  addiu $a0, $t4, 4
  li    $v0,9
  syscall
  addiu $t5, $v0, 4
  sw    $t4, ($v0)

Lrunt52:
  lbu   $a0, ($t2)
  sb    $a0, ($t5)
  addiu $t2, $t2, 1
  addiu $t5, $t5, 1
  addiu $t0, $t0, -1
  bgtz  $t0, Lrunt52

Lrunt53:
  lbu   $a0, ($t3)
  sb    $a0, ($t5)
  addiu $t3, $t3, 1
  addiu $t5, $t5, 1
  addiu $t1, $t1, -1
  bgtz  $t1, Lrunt53
  j     $ra

Lrunt50:
  move $v0, $a1
  j    $ra

Lrunt51:
  move $v0, $a0
  j    $ra

# int not(int i){
#   return !i;
# }

_not:
  seq $v0, $a0, 0
  jr $ra

# # undef getchar
#
# struct string *getchar() {
#   int i = getc(stdin);
#
#   if (i == EOF)
#     return &empty;
#   else
#     return consts + i;
# }

  .data
  .align 4
getchar_buf:	.byte 0 0
  .text

getchar:
  la   $a0, getchar_buf
  li   $a1, 2
  li   $v0, 8
  syscall
  lb   $v0, ($a0)
  bnez $v0, Lrunt58
  la   $v0, Runtempty
  jr   $ra

Lrunt58:
  sll  $v0, $v0, 3
  la   $v0, Runtconsts($v0)
  jr   $ra

# void checkArrayBounds(int *a, int i) {
#   if (*a < 0 || *a >= i) {
#     printf("index %d out of range\n", i);
#     exit(1);
#   }
# }

  .data
Lrunt54: .asciiz "index out of range\n"
  .text

checkArrayBounds:
  lw   $a2, ($a0)
  bltz $a1, Lrunt55
  bge  $a1, $a2, Lrunt55
  jr   $ra

Lrunt55:
  li   $v0, 4
  la   $a0, Lrunt54
  syscall
  li   $v0, 10
  syscall

# void exit(int i)

exit:
  li $v0, 17
  syscall
