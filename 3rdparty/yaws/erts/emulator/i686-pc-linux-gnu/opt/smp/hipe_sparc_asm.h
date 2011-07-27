/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2007-2011. All Rights Reserved.
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


#ifndef HIPE_SPARC_ASM_H
#define HIPE_SPARC_ASM_H

/*
 * Tunables.
 */

#define SPARC_LEAF_WORDS	16

/*
 * Reserved registers.
 */
#define RA	%o7
#define P	%i0
#define NSP	%i1
#define HP	%i2
#define TEMP_RA	%i3

/*
 * Context switching macros.
 *
 * RESTORE_CONTEXT and RESTORE_CONTEXT_QUICK do not affect
 * the condition register.
 */
#define SAVE_CONTEXT_QUICK	\
	mov	RA, TEMP_RA

#define RESTORE_CONTEXT_QUICK	\
	mov	TEMP_RA, RA

#define SAVE_CACHED_STATE	\
	st	HP, [P+P_HP];	\
	st	NSP, [P+P_NSP]

#define RESTORE_CACHED_STATE	\
	ld	[P+P_HP], HP;	\
	ld	[P+P_NSP], NSP

#define SAVE_CONTEXT_BIF	\
	mov	RA, TEMP_RA;	\
	st	HP, [P+P_HP]

#define RESTORE_CONTEXT_BIF	\
	mov	TEMP_RA, RA;	/* XXX unnecessary */\
	ld	[P+P_HP], HP

#define SAVE_CONTEXT_GC	\
	mov	RA, TEMP_RA;	\
	st	RA, [P+P_NRA];	\
	st	NSP, [P+P_NSP];	\
	st	HP, [P+P_HP]

#define RESTORE_CONTEXT_GC	\
	mov	TEMP_RA, RA;	/* XXX unnecessary */\
	ld	[P+P_HP], HP

/*
 * Argument (parameter) registers.
 */
#define SPARC_NR_ARG_REGS	4
#define NR_ARG_REGS	4


#define ARG0	%o1
#define ARG1	%o2
#define ARG2	%o3
#define ARG3	%o4

/*
 * TEMP_ARG0:
 *	Used in nbif_stack_trap_ra to preserve the return value.
 *	Must be a C callee-save register.
 *	Must be otherwise unused in the return path.
 *
 * TEMP_ARG0:
 *	Used in hipe_sparc_inc_stack to preserve the return address
 *	(TEMP_RA contains the caller's saved return address).
 *	Must be a C callee-save register.
 *	Must be otherwise unused in the call path.
 *
 * TEMP_ARG0:
 *	Used to pass the callee address in native-to-BEAM traps
 *	(nbif_callemu).
 *	Must be otherwise unused in the call path.
 *
 * TEMP_ARG1:
 *	Used to pass the callee arity in native-to-BEAM traps
 *      (nbif_callemu).
 *	Must be otherwise unused in the call path.
 */
#define TEMP_ARG0	%i4
#define TEMP_ARG1	%i5


#define LOAD_ARG_REGS	ld [P+P_ARG0], %o1 ; ld [P+P_ARG1], %o2 ; ld [P+P_ARG2], %o3 ; ld [P+P_ARG3], %o4 ; 

#define STORE_ARG_REGS	st %o1, [P+P_ARG0] ; st %o2, [P+P_ARG1] ; st %o3, [P+P_ARG2] ; st %o4, [P+P_ARG3] ; 


/* #define NBIF_ARG_1_0	mov	%o1, r1 */
/* #define NBIF_ARG_2_0	mov	%o1, r1 */
/* #define NBIF_ARG_2_1	mov	%o2, r2 */
/* #define NBIF_ARG_3_0	mov	%o1, r1 */
/* #define NBIF_ARG_3_1	mov	%o2, r2 */
/* #define NBIF_ARG_3_2	mov	%o3, r3 */
/* #define NBIF_ARG_5_0	mov	%o1, r1 */
/* #define NBIF_ARG_5_1	mov	%o2, r2 */
/* #define NBIF_ARG_5_2	mov	%o3, r3 */
/* #define NBIF_ARG_5_3	mov	%o4, r4 */
/* #define NBIF_ARG_5_4	ld	[NSP+0], r5 */

/* #define NBIF_RET_0	jmpl	TEMP_RA+8, %g0
	nop */
/* #define NBIF_RET_1	jmpl	TEMP_RA+8, %g0
	nop */
/* #define NBIF_RET_2	jmpl	TEMP_RA+8, %g0
	nop */
/* #define NBIF_RET_3	jmpl	TEMP_RA+8, %g0
	nop */
/* #define NBIF_RET_5	jmpl	TEMP_RA+8, %g0
	add NSP, 4, NSP */

/* #define QUICK_CALL_RET_F_0 ba F; nop */
/* #define QUICK_CALL_RET_F_1 ba F; nop */
/* #define QUICK_CALL_RET_F_2 ba F; nop */
/* #define QUICK_CALL_RET_F_3 ba F; nop */
/* #define QUICK_CALL_RET_F_5 ba F; add NSP, 4, NSP */

#endif /* HIPE_SPARC_ASM_H */
