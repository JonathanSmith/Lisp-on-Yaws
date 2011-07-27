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


#ifndef HIPE_PPC_ASM_H
#define HIPE_PPC_ASM_H

/*
 * Handle 32 vs 64-bit.
 */

/* 32-bit PowerPC */
#define LOAD	lwz
#define STORE	stw
#define CMPI	cmpwi

/*
 * Tunables.
 */

#define PPC_LEAF_WORDS	16

/*
 * Workarounds for Darwin.
 */

/* Not Darwin */

/* 32-bit */
#define CSYM(NAME)	NAME
#define OPD(NAME)	/*empty*/

#define ASYM(NAME)	NAME
#define GLOBAL(NAME)	.global NAME
#define SEMI		;
#define SET_SIZE(NAME)	.size NAME,.-NAME
#define TYPE_FUNCTION(NAME)	.type NAME,@function
#define lo16(X)		X@l
#define ha16(X)		X@ha

/*
 * Standard register names.
 */
#define r0	0
#define r1	1
#define r2	2
#define r3	3
#define r4	4
#define r5	5
#define r6	6
#define r7	7
#define r8	8
#define r9	9
#define r10	10
#define r11	11
#define r12	12
#define r13	13
#define r14	14
#define r15	15
#define r16	16
#define r17	17
#define r18	18
#define r19	19
#define r20	20
#define r21	21
#define r22	22
#define r23	23
#define r24	24
#define r25	25
#define r26	26
#define r27	27
#define r28	28
#define r29	29
#define r30	30
#define r31	31

/*
 * Reserved registers.
 */
#define P	r31
#define NSP	r30
#define HP	r29
#define TEMP_LR	r28

/*
 * Context switching macros.
 *
 * RESTORE_CONTEXT and RESTORE_CONTEXT_QUICK do not affect
 * the condition register.
 */
#define SAVE_CONTEXT_QUICK	\
	mflr	TEMP_LR

#define RESTORE_CONTEXT_QUICK	\
	mtlr	TEMP_LR

#define SAVE_CACHED_STATE	\
	STORE	HP, P_HP(P) SEMI\
	STORE	NSP, P_NSP(P)

#define RESTORE_CACHED_STATE	\
	LOAD	HP, P_HP(P) SEMI\
	LOAD	NSP, P_NSP(P)

#define SAVE_CONTEXT_BIF	\
	mflr	TEMP_LR SEMI	\
	STORE	HP, P_HP(P)

#define RESTORE_CONTEXT_BIF	\
	mtlr	TEMP_LR SEMI	\
	LOAD	HP, P_HP(P)

#define SAVE_CONTEXT_GC	\
	mflr	TEMP_LR SEMI	\
	STORE	TEMP_LR, P_NRA(P) SEMI	\
	STORE	NSP, P_NSP(P) SEMI	\
	STORE	HP, P_HP(P)

#define RESTORE_CONTEXT_GC	\
	mtlr	TEMP_LR SEMI	\
	LOAD	HP, P_HP(P)

/*
 * Argument (parameter) registers.
 */
#define PPC_NR_ARG_REGS	4
#define NR_ARG_REGS	4


#define ARG0	r4
#define ARG1	r5
#define ARG2	r6
#define ARG3	r7

/*
 * TEMP_ARG0:
 *	Used in nbif_stack_trap_ra to preserve the return value.
 *	Must be a C callee-save register.
 *	Must be otherwise unused in the return path.
 *
 * TEMP_ARG0:
 *	Used in hipe_ppc_inc_stack to preserve the return address
 *	(TEMP_LR contains the caller's saved return address).
 *	Must be a C callee-save register.
 *	Must be otherwise unused in the call path.
 */
#define TEMP_ARG0	r27


#define LOAD_ARG_REGS	lwz r4, P_ARG0(P) SEMI lwz r5, P_ARG1(P) SEMI lwz r6, P_ARG2(P) SEMI lwz r7, P_ARG3(P) SEMI 

#define STORE_ARG_REGS	stw r4, P_ARG0(P) SEMI stw r5, P_ARG1(P) SEMI stw r6, P_ARG2(P) SEMI stw r7, P_ARG3(P) SEMI 


/* #define NBIF_ARG_1_0	mr	r3, r4 */
/* #define NBIF_ARG_2_0	mr	r3, r4 */
/* #define NBIF_ARG_2_1	mr	r3, r5 */
/* #define NBIF_ARG_3_0	mr	r3, r4 */
/* #define NBIF_ARG_3_1	mr	r3, r5 */
/* #define NBIF_ARG_3_2	mr	r3, r6 */
/* #define NBIF_ARG_5_0	mr	r3, r4 */
/* #define NBIF_ARG_5_1	mr	r3, r5 */
/* #define NBIF_ARG_5_2	mr	r3, r6 */
/* #define NBIF_ARG_5_3	mr	r3, r7 */
/* #define NBIF_ARG_5_4	lwz	r3, 0(NSP) */

/* #define NBIF_RET_0	blr */
/* #define NBIF_RET_1	blr */
/* #define NBIF_RET_2	blr */
/* #define NBIF_RET_3	blr */
/* #define NBIF_RET_5	addi	NSP, NSP, 4
	blr */

/* #define QUICK_CALL_RET_F_0 b F */
/* #define QUICK_CALL_RET_F_1 b F */
/* #define QUICK_CALL_RET_F_2 b F */
/* #define QUICK_CALL_RET_F_3 b F */
/* #define QUICK_CALL_RET_F_5 addi NSP, NSP, 4 SEMI b F */

#endif /* HIPE_PPC_ASM_H */
