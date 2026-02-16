/*
 * Copyright (c) 2017 Trail of Bits, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/* BEXTR r32, r/m32, r32 — control[7:0]=start, control[15:8]=len */
TEST_BEGIN_64(BEXTRr32r32r32, 2)
TEST_IGNORE_FLAGS(SF AF PF)
TEST_INPUTS(
    0xDEADBEEF, 0x0400,   /* start=0, len=4  → 0xF */
    0xDEADBEEF, 0x0804,   /* start=4, len=8  → 0xEE */
    0xFFFFFFFF, 0x2000,   /* start=0, len=32 → 0xFFFFFFFF */
    0xFFFFFFFF, 0x0100,   /* start=0, len=1  → 1 */
    0x12345678, 0x1010,   /* start=16,len=16 → 0x1234 */
    0x00000000, 0x0800,   /* start=0, len=8  → 0 (ZF=1) */
    0xFFFFFFFF, 0x0020,   /* start=32,len=0  → 0 (start >= bitwidth) */
    0xFFFFFFFF, 0x0021)   /* start=33,len=0  → 0 (start > bitwidth) */

    mov eax, ARG1_32
    mov ecx, ARG2_32
    bextr edx, eax, ecx
TEST_END_64

TEST_BEGIN_64(BEXTRr64r64r64, 2)
TEST_IGNORE_FLAGS(SF AF PF)
TEST_INPUTS(
    0xDEADBEEFCAFEBABE, 0x0400,   /* start=0, len=4  → 0xE */
    0xDEADBEEFCAFEBABE, 0x0804,   /* start=4, len=8  → 0xAB */
    0xFFFFFFFFFFFFFFFF, 0x4000,   /* start=0, len=64 → all ones */
    0xFFFFFFFFFFFFFFFF, 0x0100,   /* start=0, len=1  → 1 */
    0x123456789ABCDEF0, 0x2020,   /* start=32,len=32 → 0x12345678 */
    0x0000000000000000, 0x0800,   /* start=0, len=8  → 0 (ZF=1) */
    0xFFFFFFFFFFFFFFFF, 0x0040,   /* start=64,len=0  → 0 */
    0xFFFFFFFFFFFFFFFF, 0x0041)   /* start=65,len=0  → 0 */

    mov rax, ARG1_64
    mov rcx, ARG2_64
    bextr rdx, rax, rcx
TEST_END_64
