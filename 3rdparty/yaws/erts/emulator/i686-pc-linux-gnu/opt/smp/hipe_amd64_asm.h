/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2004-2011. All Rights Reserved.
 *
 * The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved online at http://www.erlang.org/.
 *
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 *
 * %CopyrightEnd%
 */


#ifndef HIPE_AMD64_ASM_H
#define HIPE_AMD64_ASM_H


#define AMD64_LEAF_WORDS	24
#define LEAF_WORDS	24

/*
 * Workarounds for Darwin.
 */

/* Not Darwin */
#define TEXT		.section ".text"
#define CSYM(NAME)	NAME
#define ASYM(NAME)	NAME
#define GLOBAL(NAME)	.global NAME
#define SET_SIZE(NAME)	.size NAME,.-NAME
#define TYPE_FUNCTION(NAME)	.type NAME,@function


/*
 * Reserved registers.
 */
#define P		%rbp

#define AMD64_HP_IN_REGISTER	1
#if AMD64_HP_IN_REGISTER
#define AMD64_HEAP_POINTER 15
#define SAVE_HP	movq %r15, P_HP(P)
#define RESTORE_HP	movq P_HP(P), %r15
#else
#define SAVE_HP		/*empty*/
#define RESTORE_HP	/*empty*/
#endif

#define AMD64_FCALLS_IN_REGISTER 0
#if AMD64_FCALLS_IN_REGISTER
#define AMD64_FCALLS_REGISTER 11
#define SAVE_FCALLS	movq %r11, P_FCALLS(P)
#define RESTORE_FCALLS	movq P_FCALLS(P), %r11
#else
#define SAVE_FCALLS	/*empty*/
#define RESTORE_FCALLS	/*empty*/
#endif

#define AMD64_HEAP_LIMIT_IN_REGISTER 0
#if AMD64_HEAP_LIMIT_IN_REGISTER
#define AMD64_HEAP_LIMIT_REGISTER 12
#define RESTORE_HEAP_LIMIT	movq P_HP_LIMIT(P), %r12
#else
#define RESTORE_HEAP_LIMIT	/*empty*/
#endif

#define NSP		%rsp
#define SAVE_CSP	movq	%rsp, P_CSP(P)
#define RESTORE_CSP	movq	P_CSP(P), %rsp

#define AMD64_SIMULATE_NSP	0

/*
 * Context switching macros.
 */
#define SWITCH_C_TO_ERLANG_QUICK	\
	SAVE_CSP; \
	movq P_NSP(P), NSP

#define SWITCH_ERLANG_TO_C_QUICK	\
	movq NSP, P_NSP(P); \
	RESTORE_CSP

#define SAVE_CACHED_STATE	\
	SAVE_HP;		\
	SAVE_FCALLS

#define RESTORE_CACHED_STATE	\
	RESTORE_HP;		\
	RESTORE_HEAP_LIMIT;	\
	RESTORE_FCALLS

#define SWITCH_C_TO_ERLANG	\
	RESTORE_CACHED_STATE;	\
	SWITCH_C_TO_ERLANG_QUICK

#define SWITCH_ERLANG_TO_C	\
	SAVE_CACHED_STATE;	\
	SWITCH_ERLANG_TO_C_QUICK

/*
 * Argument (parameter) registers.
 */
#define AMD64_NR_ARG_REGS	4
#define NR_ARG_REGS		4


#define ARG0	%rsi
#define ARG1	%rdx
#define ARG2	%rcx
#define ARG3	%r8

/*
 * TEMP_RV:
 *	Used in nbif_stack_trap_ra to preserve the return value.
 *	Must be a C callee-save register.
 *	Must be otherwise unused in the return path.
 */
#define TEMP_RV		%rbx


#define LOAD_ARG_REGS	movq P_ARG0(P), %rsi ; movq P_ARG1(P), %rdx ; movq P_ARG2(P), %rcx ; movq P_ARG3(P), %r8 ; 

#define STORE_ARG_REGS	movq %rsi, P_ARG0(P) ; movq %rdx, P_ARG1(P) ; movq %rcx, P_ARG2(P) ; movq %r8, P_ARG3(P) ; 

#define NSP_CALL(FUN)	call FUN
#define NSP_RETN(NPOP)	ret $NPOP
#define NSP_RET0	ret

/* #define NBIF_ARG_1_0	# movq	%rsi, %rsi */
/* #define NBIF_ARG_2_0	# movq	%rsi, %rsi */
/* #define NBIF_ARG_2_1	# movq	%rdx, %rdx */
/* #define NBIF_ARG_3_0	# movq	%rsi, %rsi */
/* #define NBIF_ARG_3_1	# movq	%rdx, %rdx */
/* #define NBIF_ARG_3_2	# movq	%rcx, %rcx */
/* #define NBIF_ARG_5_0	# movq	%rsi, %rsi */
/* #define NBIF_ARG_5_1	# movq	%rdx, %rdx */
/* #define NBIF_ARG_5_2	# movq	%rcx, %rcx */
/* #define NBIF_ARG_5_3	# movq	%r8, %r8 */
/* #define NBIF_ARG_5_4	movq	8(%rsp), %r9 */


/* #define NBIF_RET_0	NSP_RET0 */
/* #define NBIF_RET_1	NSP_RET0 */
/* #define NBIF_RET_2	NSP_RET0 */
/* #define NBIF_RET_3	NSP_RET0 */
/* #define NBIF_RET_5	NSP_RETN(8) */

#endif /* HIPE_AMD64_ASM_H */
