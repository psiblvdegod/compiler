let generate_label () =
  let length = 16 in
  let buffer = Bytes.create length in
  for i = 0 to length - 1 do
    let random_letter = Char.chr (Char.code 'a' + Random.int 26) in
    Bytes.set buffer i random_letter
  done;
  buffer |> Bytes.to_string

module Reg = struct
  let sp = "sp"
  let t0 = "t0"
  let t1 = "t1"
  let t2 = "t2"
  let t3 = "t3"
  let t4 = "t4"
  let s1 = "s1"
  let s2 = "s2"
  let a0 = "a0"
  let a1 = "a1"
  let a2 = "a2"
  let a3 = "a3"
  let a4 = "a4"
  let a5 = "a5"
  let a6 = "a6"
  let a7 = "a7"
  let zero = "x0"
end

module Instr = struct
  open Printf

  let li reg lit = [ sprintf "li %s, %d\n" reg lit ]
  let mv dest src = [ sprintf "mv %s, %s\n" dest src ]
  let addi dest src lit = [ sprintf "addi %s, %s, %d\n" dest src lit ]
  let add dest left right = [ sprintf "add %s, %s, %s\n" dest left right ]
  let sub dest left right = [ sprintf "sub %s, %s, %s\n" dest left right ]
  let mul dest left right = [ sprintf "mul %s, %s, %s\n" dest left right ]
  let div dest left right = [ sprintf "div %s, %s, %s\n" dest left right ]
  let rem dest left right = [ sprintf "rem %s, %s, %s\n" dest left right ]
  let slt dest left right = [ sprintf "slt %s, %s, %s\n" dest left right ]
  let sgt dest left right = [ sprintf "slt %s, %s, %s\n" dest right left ]

  let sle dest left right =
    [
      sprintf "slt %s, %s, %s\n" dest right left;
      sprintf "seqz %s, %s\n" dest dest;
    ]

  let sge dest left right =
    [
      sprintf "slt %s, %s, %s\n" dest left right;
      sprintf "seqz %s, %s\n" dest dest;
    ]

  let seq dest left right =
    [
      sprintf "xor %s, %s, %s\n" dest left right;
      sprintf "seqz %s, %s\n" dest dest;
    ]

  let sne dest left right =
    [
      sprintf "xor %s, %s, %s\n" dest left right;
      sprintf "snez %s, %s\n" dest dest;
    ]

  let lw_from_stack reg delta = [ sprintf "lw %s, %d(sp)\n" reg delta ]
  let ld_from_stack reg delta = [ sprintf "ld %s, %d(sp)\n" reg delta ]
  let sw_to_stack reg delta = [ sprintf "sw %s, %d(sp)\n" reg delta ]
  let sd_to_stack reg delta = [ sprintf "sd %s, %d(sp)\n" reg delta ]
  let sb_to_stack reg delta = [ sprintf "sb %s, %d(sp)\n" reg delta ]
  let lb dest src = [ sprintf "lb %s, (%s)\n" dest src ]
  let sb dest src = [ sprintf "sb %s, (%s)\n" dest src ]
  let jump label = [ sprintf "j %s\n" label ]

  let branch_true reg label =
    [ "li t0, 1\n"; sprintf "beq %s, t0, %s\n" reg label ]

  let branch_false reg label = [ sprintf "beqz %s, %s\n" reg label ]
  let ecall = [ "ecall\n" ]
end

module Str = struct
  open Reg
  open Instr

  let rec create_asciz str = [] @ mmap_page @ store_str a0 (str ^ "\x00")

  and store_str dest str =
    let ls = String.to_seq str |> List.of_seq in
    let add ch =
      [] @ li t0 (Char.code ch) @ [ "sb t0, 0(t1)\n" ] @ addi t1 t1 1
    in
    let init = mv t1 dest in
    List.fold_left (fun acc ch -> acc @ add ch) init ls

  and str_len_asciz =
    let loop_label = generate_label () in
    let end_loop_label = generate_label () in
    [] @ mv a1 a0 @ li a0 0
    @ [ loop_label ^ ":\n" ]
    @ lb t0 a1 @ seq t1 t0 zero
    @ branch_true t1 end_loop_label
    @ addi a1 a1 1 @ addi a0 a0 1 @ jump loop_label
    @ [ end_loop_label ^ ":\n" ]

  and mmap_page =
    [] @ li a0 0 @ li a1 4096 @ li a2 0x3 @ li a3 0x22 @ li a4 (-1) @ li a5 0
    @ li a7 222 @ ecall
end

module Print = struct
  open Reg
  open Instr
  open Str

  let rec print_bytes_from src len =
    [] @ mv t1 src @ mv t2 len @ li a0 1 @ mv a1 t1 @ mv a2 t2 @ li a7 64
    @ ecall

  and print_str_imm str =
    let len = String.length str in
    [] @ addi sp sp (-len) @ store_str sp str @ li t0 len
    @ print_bytes_from sp t0 @ addi sp sp len

  and print_bool reg =
    let true_label = generate_label () in
    let false_label = generate_label () in
    let end_label = generate_label () in
    [] @ branch_true reg true_label
    @ branch_false reg false_label
    @ [ true_label ^ ":\n" ]
    @ print_str_imm "true" @ jump end_label
    @ [ false_label ^ ":\n" ]
    @ print_str_imm "false"
    @ [ end_label ^ ":\n" ]

  and abs reg =
    let skip_label = generate_label () in
    [] @ slt t0 reg zero @ branch_false t0 skip_label @ li t1 (-1)
    @ mul reg reg t1
    @ [ skip_label ^ ":\n" ]

  and print_number reg =
    let acc =
      [] @ mv a1 reg @ li a2 0 (* len *) @ li a3 10
      (* base *) @ slt a4 a1 zero
      @ abs a1
    in
    let loop_label = generate_label () in
    let acc =
      acc
      @ [ loop_label ^ ":\n" ]
      @ rem t0 a1 a3 @ div a1 a1 a3
      @ addi t0 t0 (Char.code '0')
      @ addi sp sp (-1) @ sb_to_stack t0 0 @ addi a2 a2 1 @ seq t1 a1 zero
      @ branch_false t1 loop_label
    in
    let skip_label = generate_label () in
    acc @ branch_false a4 skip_label @ addi sp sp (-1)
    @ li t0 (Char.code '-')
    @ sb_to_stack t0 0 @ addi a2 a2 1
    @ [ skip_label ^ ":\n" ]
    @ mv s1 a2 @ print_bytes_from sp a2 @ add sp sp s1
end
