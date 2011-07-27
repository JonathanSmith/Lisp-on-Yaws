/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2005-2011. All Rights Reserved.
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


#ifndef HIPE_ARM_ASM_H
#define HIPE_ARM_ASM_H

/*
 * Tunables.
 */

#define ARM_LEAF_WORDS	16

/*
 * Reserved registers.
 */
#define P	r11
#define NSP	r10
#define HP	r9
#define TEMP_LR	r8

/*
 * Context switching macros.
 *
 * RESTORE_CONTEXT and RESTORE_CONTEXT_QUICK do not affect
 * the condition register.
 */
#define SAVE_CONTEXT_QUICK	\
	mov	TEMP_LR, lr

#define RESTORE_CONTEXT_QUICK	\
	mov	lr, TEMP_LR

#define SAVE_CACHED_STATE	\
	str	HP, [P, #P_HP];	\
	str	NSP, [P, #P_NSP]

#define RESTORE_CACHED_STATE	\
	ldr	HP, [P, #P_HP];	\
	ldr	NSP, [P, #P_NSP]

#define SAVE_CONTEXT_BIF	\
	mov	TEMP_LR, lr;	\
	str	HP, [P, #P_HP]

#define RESTORE_CONTEXT_BIF	\
	ldr	HP, [P, #P_HP]

#define SAVE_CONTEXT_GC	\
	mov	TEMP_LR, lr;	\
	str	lr, [P, #P_NRA];	\
	str	NSP, [P, #P_NSP];	\
	str	HP, [P, #P_HP]

#define RESTORE_CONTEXT_GC	\
	ldr	HP, [P, #P_HP]

/*
 * Argument (parameter) registers.
 */
#define ARM_NR_ARG_REGS	3
#define NR_ARG_REGS	3


#define ARG0	r1
#define ARG1	r2
#define ARG2	r3

/*
 * TEMP_ARG0:
 *	Used in nbif_stack_trap_ra to preserve the return value.
 *	Must be a C callee-save register.
 *	Must be otherwise unused in the return path.
 *
 * TEMP_ARG0:
 *	Used in hipe_arm_inc_stack to preserve the return address
 *	(TEMP_LR contains the caller's saved return address).
 *	Must be a C callee-save register.
 *	Must be otherwise unused in the call path.
 */
#define TEMP_ARG0	r7


#define LOAD_ARG_REGS	ldr r1, [P, #P_ARG0] ; ldr r2, [P, #P_ARG1] ; ldr r3, [P, #P_ARG2] ; 

#define STORE_ARG_REGS	str r1, [P, #P_ARG0] ; str r2, [P, #P_ARG1] ; str r3, [P, #P_ARG2] ; 


/* #define NBIF_ARG_1_0	# mov	r1, r1 */
/* #define NBIF_ARG_2_0	# mov	r1, r1 */
/* #define NBIF_ARG_2_1	# mov	r2, r2 */
/* #define NBIF_ARG_3_0	# mov	r1, r1 */
/* #define NBIF_ARG_3_1	# mov	r2, r2 */
/* #define NBIF_ARG_3_2	# mov	r3, r3 */
/* #define NBIF_ARG_5_0	# mov	r1, r1 */
/* #define NBIF_ARG_5_1	# mov	r2, r2 */
/* #define NBIF_ARG_5_2	# mov	r3, r3 */
/* #define NBIF_ARG_5_3	ldr	r4, [NSP, #4] */
/* #define NBIF_ARG_5_4	ldr	r5, [NSP, #0] */

/* #define NBIF_RET_0	mov pc, TEMP_LR */
/* #define NBIF_RET_1	mov pc, TEMP_LR */
/* #define NBIF_RET_2	mov pc, TEMP_LR */
/* #define NBIF_RET_3	mov pc, TEMP_LR */
/* #define NBIF_RET_5	add	NSP, NSP, #8
	mov pc, TEMP_LR */

/* #define QUICK_CALL_RET_F_0 b F */
/* #define QUICK_CALL_RET_F_1 b F */
/* #define QUICK_CALL_RET_F_2 b F */
/* #define QUICK_CALL_RET_F_3 b F */
/* #define QUICK_CALL_RET_F_5 add NSP, NSP, #8 ; b F */

#endif /* HIPE_ARM_ASM_H */
