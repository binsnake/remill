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

TEST_BEGIN(POPCNTr16r16, 1)
TEST_INPUTS(
    0,
    1,
    0xFFFF,
    0x8000,
    0x5555,
    0xAAAA,
    0x00FF,
    0x0001)

    mov eax, ARG1_32
    popcnt dx, ax
TEST_END

TEST_BEGIN(POPCNTr32r32, 1)
TEST_INPUTS(
    0,
    1,
    0xFFFFFFFF,
    0x80000000,
    0x55555555,
    0xAAAAAAAA,
    0x000000FF,
    0x00000001)

    mov eax, ARG1_32
    popcnt edx, eax
TEST_END

TEST_BEGIN_64(POPCNTr64r64, 1)
TEST_INPUTS(
    0,
    1,
    0xFFFFFFFFFFFFFFFF,
    0x8000000000000000,
    0x5555555555555555,
    0xAAAAAAAAAAAAAAAA,
    0x00000000000000FF,
    0x0000000000000001)

    mov rax, ARG1_64
    popcnt rdx, rax
TEST_END_64
