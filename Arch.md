# Architecture

This file details the (possible) architecture for this CPU.

## Instructions
0x0: `ADD reg, imm8/reg`  => reg = reg + imm8/reg

0x1: `SUB reg, imm8/reg`  => reg = reg - imm8/reg

0x2: `ADC reg, imm8/reg`  => reg = reg + imm8/reg + C

0x3: `SBC reg, imm8/reg`  => reg = reg - imm8/reg - C

0x4: `NAND reg, imm8/reg` => reg = reg & imm8/reg

0x5: `OR reg, imm8/reg`   => reg = reg | imm8/reg

0x6: `CMP reg, imm8/reg`  => S = reg vs. imm8/reg (See [Status](#status))

0x7: `MV reg, imm8/reg`   => reg = imm8/reg

0x8: `LD reg, [HL/imm16]` => reg = [HL/imm16] 

0x9: `ST reg, [HL/imm16]` => [HL/imm16] = reg

0xA: `LDA imm16`          => HL = imm16

0xB: `PUSH reg/imm8`      => [++SP] = reg/imm8

0xC: `POP reg`            => reg = [SP--]

0xD: `JNZ reg/imm8`       => imm8/reg != 0 ? PC = HL : NOP

0xE: `IN reg, reg/imm8`   => reg = PORT[reg/imm8]

0xF: `OUT reg/imm8, reg`  => PORT[reg/imm8] = reg 

## Layout
Each instruction, when translated to machine code, should be formatted like so: `XXXXYZZZ AAAAAAAA BBBBBBBB`

All instructions will take up 3 bytes

X: [Instruction](#instructions)
Y: 1 if `A` is an immediate, or 0 if `A` is a register
Z: First register, or 0 if none is provided (`JNZ`, `PUSH`, `LDA`)

A: Second register/ Immediate high byte, or 0 if none is provided (`POP`)
B: Immediate low byte, of 0 if none is provided (All except `LW`, `SW`, and `LDA`)

## Registers
`A`: GP
`B`: GP
`C`: GP
`D`: GP
`E`: GP
`H`: High byte (Memory addressing & Jumps)
`L`: Low byte (Memory addressing & Jumps)
`S`: Status (See [Status](#status))

## Status

The status register stores flags for later use.
`C`: Carry bit
`E`: Equal bit
`L`: Less-than bit
`G`: Greater-than bit
