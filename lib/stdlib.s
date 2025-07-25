.global print_number
.global exit
.equ WRITE, 64

.section .text
.macro push_byte byte
    addi sp, sp, -1
    li t0, \byte
    sb t0, 0(sp)
.endm

.macro abs reg
    bgez \reg, skip_if_positive
    li t0, -1
    mul \reg, \reg, t0
    skip_if_positive:
.endm

.macro print_byte byte
    addi sp, sp, -1
    li t0, \byte
    sb t0, 0(sp)
    li a0, 1
    mv a1, sp
    li a2, 1
    li a7, WRITE
    ecall
    addi sp, sp, 1
.endm

# number from a0
print_number:
    mv s0, a0
    li s1, 10       # base
    li s2, 1        # cnt
    slti s3, s0, 0  # sign

    push_byte '\n'
    
    abs s0

    pn_loop:
        rem t0, s0, s1
        div s0, s0, s1
        addi t0, t0, '0'
        addi sp, sp, -1
        sb t0, 0(sp)
        addi s2, s2, 1

    bnez s0, pn_loop
    
    
    beqz s3, skippp
    push_byte '-'
    addi s2, s2, 1
    skippp:

    li a0, 1
    mv a1, sp
    mv a2, s2
    li a7, WRITE
    ecall    
        
    add sp, sp, s2
    ret

exit:
    li a0, 0
    li a7, 93
    ecall
