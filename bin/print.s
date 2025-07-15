.section .data
.balign 16
buffer: .dword 0

.section .text

.global print_number
.global print_stack # save_sp -> <generated code> -> print_stack
.global save_sp # uses buffer
.global exit

.equ WRITE, 64

.macro push_byte byte
    addi sp, sp, -1
    li t0, \byte
    lb t0, (sp)
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

.macro print_signed source, len, sign
    beqz \sign, skip_minus
    addi \len, \len, 1
    push_byte '-'
    skip_minus:
    li a0, 1
    mv a1, \source
    mv a2, \len
    li a7, WRITE
    ecall
.endm

print_stack:
    mv s4, ra

    call count_len_for_print_stack

    mv s1, a0 # cnt
    mv s2, a1 # step
    mv s3, sp # pos

    ps_loop:
        lw a0, 0(s3)
        call print_number
        
        addi s1, s1, -1
        add s3, s3, s2

        print_byte ' '
        
        bnez s1, ps_loop

    mv ra, s4
    li a0, 0
    ret

# number from a0
print_number:
    li a1, 10       # base
    li a2, 0        # cnt

    slti a3, a0, 0  # sign
    abs a0
    
    pn_loop:
        remu t0, a0, a1
        divu a0, a0, a1
        addi t0, t0, '0'
        addi sp, sp, -1
        sb t0, 0(sp)

        addi a2, a2, 1

    bnez a0, pn_loop
    
        print_signed sp, a2, a3
        add sp, sp, a2
        ret

save_sp:
    la t0, buffer
    sd sp, 0(t0)
    ret

count_len_for_print_stack:
    la t0, buffer
    ld t1, 0(t0)
    sub t2, t1, sp
    li a1, 16
    divu a0, t2, a1
    ret

exit:
    li a0, 0
    li a7, 93
    ecall
