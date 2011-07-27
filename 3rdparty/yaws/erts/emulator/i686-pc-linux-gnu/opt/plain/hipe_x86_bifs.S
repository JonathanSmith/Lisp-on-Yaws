/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2001-2011. All Rights Reserved.
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


/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2002-2011. All Rights Reserved.
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


#ifndef HIPE_X86_ASM_H
#define HIPE_X86_ASM_H

/*
 * Tunables.
 */

#define X86_LEAF_WORDS	24
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
#define P	%ebp

#define X86_HP_IN_ESI	1
#if X86_HP_IN_ESI
#define SAVE_HP		movl %esi, P_HP(P)
#define RESTORE_HP	movl P_HP(P), %esi
#else
#define SAVE_HP		/*empty*/
#define RESTORE_HP	/*empty*/
#endif

#define NSP		%esp
#define SAVE_CSP	movl %esp, P_CSP(P)
#define RESTORE_CSP	movl P_CSP(P), %esp

#define X86_SIMULATE_NSP	0

/*
 * Context switching macros.
 */
#define SWITCH_C_TO_ERLANG_QUICK	\
	SAVE_CSP; \
	movl P_NSP(P), NSP

#define SWITCH_ERLANG_TO_C_QUICK	\
	movl NSP, P_NSP(P); \
	RESTORE_CSP

#define SAVE_CACHED_STATE	\
	SAVE_HP

#define RESTORE_CACHED_STATE	\
	RESTORE_HP

#define SWITCH_C_TO_ERLANG	\
	RESTORE_CACHED_STATE;	\
	SWITCH_C_TO_ERLANG_QUICK

#define SWITCH_ERLANG_TO_C	\
	SAVE_CACHED_STATE;	\
	SWITCH_ERLANG_TO_C_QUICK

/*
 * Argument (parameter) registers.
 */
#define X86_NR_ARG_REGS	3
#define NR_ARG_REGS	3

#define ARG0	%eax
#define ARG1	%edx
#define ARG2	%ecx

/*
 * TEMP_RV:
 *	Used in nbif_stack_trap_ra to preserve the return value.
 *	Must be a C callee-save register.
 *	Must be otherwise unused in the return path.
 */
#define TEMP_RV	%ebx

/*
 * TEMP_NSP:
 *	Used in BIF wrappers to permit copying stacked parameter from
 *	the native stack to the C stack.
 *	Set up by NBIF_COPY_NSP(arity) and used by NBIF_ARG(arity,argno).
 *	TEMP_NSP may alias the last BIF argument register.
 *	NBIF_COPY_NSP and NBIF_ARG currently fail if ARITY > NR_ARG_REGS!
 */
#define TEMP_NSP	%edi


#define LOAD_ARG_REGS	movl P_ARG0(P), ARG0 ; movl P_ARG1(P), ARG1 ; movl P_ARG2(P), ARG2 ; 

#define STORE_ARG_REGS	movl ARG0, P_ARG0(P) ; movl ARG1, P_ARG1(P) ; movl ARG2, P_ARG2(P) ; 

#define NSP_CALL(FUN)	call FUN
#define NSP_RETN(NPOP)	ret $NPOP
#define NSP_RET0	ret

/* #define NBIF_COPY_NSP_0	 */
/* #define NBIF_COPY_NSP_1	 */
/* #define NBIF_COPY_NSP_2	 */
/* #define NBIF_COPY_NSP_3	 */
/* #define NBIF_COPY_NSP_5	movl	%esp, TEMP_NSP */


/* #define NBIF_ARG_OPND_1_0	ARG0 */
/* #define NBIF_ARG_OPND_2_0	ARG0 */
/* #define NBIF_ARG_OPND_2_1	ARG1 */
/* #define NBIF_ARG_OPND_3_0	ARG0 */
/* #define NBIF_ARG_OPND_3_1	ARG1 */
/* #define NBIF_ARG_OPND_3_2	ARG2 */
/* #define NBIF_ARG_OPND_5_0	ARG0 */
/* #define NBIF_ARG_OPND_5_1	ARG1 */
/* #define NBIF_ARG_OPND_5_2	ARG2 */
/* #define NBIF_ARG_OPND_5_3	8(TEMP_NSP) */
/* #define NBIF_ARG_OPND_5_4	4(TEMP_NSP) */

/* #define NBIF_ARG_REG_0_P	movl P,(%esp) */


/* #define NBIF_RET_0	NSP_RET0 */
/* #define NBIF_RET_1	NSP_RET0 */
/* #define NBIF_RET_2	NSP_RET0 */
/* #define NBIF_RET_3	NSP_RET0 */
/* #define NBIF_RET_5	NSP_RETN(8) */

#define STORE_CALLER_SAVE	movl ARG0, P_ARG0(P) ; movl ARG1, P_ARG1(P) ; movl ARG2, P_ARG2(P) ; 
#define LOAD_CALLER_SAVE	movl P_ARG0(P), ARG0 ; movl P_ARG1(P), ARG1 ; movl P_ARG2(P), ARG2 ; 

#endif /* HIPE_X86_ASM_H */

#include "hipe_literals.h"

#if THE_NON_VALUE == 0
#define TEST_GOT_EXN	testl	%eax,%eax
#else
#define TEST_GOT_EXN	cmpl	$THE_NON_VALUE,%eax
#endif

#define TEST_GOT_MBUF		movl P_MBUF(P), %edx; testl %edx, %edx; jnz 3f; 2:
#define JOIN3(A,B,C)		A##B##C
#define HANDLE_GOT_MBUF(ARITY)	3: call JOIN3(nbif_,ARITY,_gc_after_bif); jmp 2b

/*
 * standard_bif_interface_1(nbif_name, cbif_name)
 * standard_bif_interface_2(nbif_name, cbif_name)
 * standard_bif_interface_3(nbif_name, cbif_name)
 *
 * Generate native interface for a BIF with 1-3 parameters and
 * standard failure mode.
 */






/*
 * fail_bif_interface_0(nbif_name, cbif_name)
 *
 * Generate native interface for a BIF with 0 parameters and
 * standard failure mode.
 */


/*
 * nofail_primop_interface_0(nbif_name, cbif_name)
 * nofail_primop_interface_1(nbif_name, cbif_name)
 * nofail_primop_interface_2(nbif_name, cbif_name)
 * nofail_primop_interface_3(nbif_name, cbif_name)
 *
 * Generate native interface for a primop with implicit P
 * parameter, 0-3 ordinary parameters and no failure mode.
 * Also used for guard BIFs.
 */








/*
 * nocons_nofail_primop_interface_0(nbif_name, cbif_name)
 * nocons_nofail_primop_interface_1(nbif_name, cbif_name)
 * nocons_nofail_primop_interface_2(nbif_name, cbif_name)
 * nocons_nofail_primop_interface_3(nbif_name, cbif_name)
 * nocons_nofail_primop_interface_5(nbif_name, cbif_name)
 *
 * Generate native interface for a primop with implicit P
 * parameter, 0-3 or 5 ordinary parameters, and no failure mode.
 * The primop cannot CONS or gc.
 */










/*
 * noproc_primop_interface_0(nbif_name, cbif_name)
 * noproc_primop_interface_1(nbif_name, cbif_name)
 * noproc_primop_interface_2(nbif_name, cbif_name)
 * noproc_primop_interface_3(nbif_name, cbif_name)
 * noproc_primop_interface_5(nbif_name, cbif_name)
 *
 * Generate native interface for a primop with no implicit P
 * parameter, 0-3 or 5 ordinary parameters, and no failure mode.
 * The primop cannot CONS or gc.
 */










/*
 * x86-specific primops.
 */

#ifndef HAVE_nbif_handle_fp_exception
#define HAVE_nbif_handle_fp_exception
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_handle_fp_exception))
ASYM(nbif_handle_fp_exception):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C_QUICK

	/* make the call on the C stack */
	call	CSYM(erts_restore_fpu)

	/* switch to native stack */
	SWITCH_C_TO_ERLANG_QUICK

	/* return */
	NSP_RET0
	SET_SIZE(ASYM(nbif_handle_fp_exception))
	TYPE_FUNCTION(ASYM(nbif_handle_fp_exception))
#endif

/*
 * Implement gc_bif_interface_0 as nofail_primop_interface_0.
 */


/*
 * Implement gc_bif_interface_N as standard_bif_interface_N (N=1,2).
 */



/*
 * Implement gc_nofail_primop_interface_1 as nofail_primop_interface_1.
 */


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
/*
 *
 * List all non architecture-specific BIFs and primops, and
 * classify each as belonging to one of the classes below.
 * This list is included in hipe_${ARCH}_bifs.m4, which is
 * responsible for translating these classifications to the
 * best possible native code wrappers.
 *
 * XXX: We should have a more detailed BIF classification
 * with a number of orthogonal properties (e.g., UPDATES_HP,
 * NEEDS_NSP, CAN_FAIL, CAN_GC, etc), from which we should
 * generate appropriate interfaces.
 *
 * The classification is expressed in terms of the resources
 * and BIF failure modes described below.
 *
 * Resources:
 * - NSP: native stack pointer
 *   NSP is read by GC BIFs and primops, and hipe_handle_exception().
 *   NSP is updated at compiler-inserted calls to hipe_inc_nstack().
 *   No other BIF or primop may access NSP.
 * - NSP_LIMIT: native stack limit
 *   NSP_LIMIT is only updated at compiler-inserted calls to inc_stack.
 *   Everywhere else, the cached value equals the value stored in P.
 * - NRA: native return address
 *   NRA is read by GC BIFs and primops, and hipe_handle_exception().
 *   No BIF or primop may update NRA.
 * - HP: heap pointer
 *   All BIFs can read and update HP.
 *   Primops with access to P that do not access HP are called "nocons".
 * - HP_LIMIT: heap limit
 *   HP_LIMIT is only updated by GC BIFs and primops.
 *   Everywhere else, the cached value equals the value stored in P.
 * - FCALLS: reduction counter
 *   All BIFs can read and update FCALLS (because BEAM abuses FCALLS
 *   to trigger GCs). XXX: can we avoid that overhead?
 *   All nocons primops do not access FCALLS.
 *   All other primops with access to P can read and update FCALLS.
 * - P: pointer to the state record for the process
 *
 * BIF failure modes:
 * - none: may not signal any exception
 *   The BIF wrapper needs no checks before returning.
 * - standard: may signal any exception
 *   The BIF wrapper must check for an exception before returning.
 *   Zero-arity BIFs signal no exceptions, except in a small number
 *   of cases explicitly enumerated here.
 */

/****************************************************************
 *			BIF CLASS DESCRIPTIONS			*
 ****************************************************************/

/*
 * standard_bif_interface_1(nbif_name, cbif_name)
 * standard_bif_interface_2(nbif_name, cbif_name)
 * standard_bif_interface_3(nbif_name, cbif_name)
 *
 * A BIF with implicit P parameter, 1-3 ordinary parameters,
 * which may fail.
 * HP and FCALLS may be read and updated.
 * HP_LIMIT, NSP, NSP_LIMIT, and NRA may not be accessed.
 */

/*
 * fail_bif_interface_0(nbif_name, cbif_name)
 *
 * A zero-arity BIF which may fail, otherwise
 * identical to standard_bif_interface_N.
 */

/*
 * nofail_primop_interface_0(nbif_name, cbif_name)
 * nofail_primop_interface_1(nbif_name, cbif_name)
 * nofail_primop_interface_2(nbif_name, cbif_name)
 * nofail_primop_interface_3(nbif_name, cbif_name)
 *
 * A primop or guard BIF with no failure mode, otherwise
 * identical to standard_bif_interface_N.
 */

/*
 * gc_bif_interface_0(nbif_name, cbif_name)
 * gc_bif_interface_1(nbif_name, cbif_name)
 * gc_bif_interface_2(nbif_name, cbif_name)
 *
 * A BIF which may do a GC or walk the native stack.
 * May read NSP, NSP_LIMIT, NRA, HP, HP_LIMIT, and FCALLS.
 * May update HP, HP_LIMIT, and FCALLS.
 * May not update NSP, NSP_LIMIT, or NRA.
 * Otherwise identical to standard_bif_interface_N.
 */

/*
 * gc_nofail_primop_interface_1(nbif_name, cbif_name)
 *
 * A primop with implicit P parameter, 1 ordinary parameter,
 * and no failure mode.
 * May read NSP, NSP_LIMIT, NRA, HP, HP_LIMIT, and FCALLS.
 * May update HP, HP_LIMIT, and FCALLS.
 * May not update NSP, NSP_LIMIT, or NRA.
 */

/*
 * nocons_nofail_primop_interface_0(nbif_name, cbif_name)
 * nocons_nofail_primop_interface_1(nbif_name, cbif_name)
 * nocons_nofail_primop_interface_2(nbif_name, cbif_name)
 * nocons_nofail_primop_interface_3(nbif_name, cbif_name)
 * nocons_nofail_primop_interface_5(nbif_name, cbif_name)
 *
 * A primop with implicit P parameter, 0-3 or 5 ordinary parameters,
 * and no failure mode.
 * HP, HP_LIMIT, FCALLS, NSP, NSP_LIMIT, and NRA may not be accessed.
 */

/*
 * noproc_primop_interface_0(nbif_name, cbif_name)
 * noproc_primop_interface_1(nbif_name, cbif_name)
 * noproc_primop_interface_2(nbif_name, cbif_name)
 * noproc_primop_interface_3(nbif_name, cbif_name)
 * noproc_primop_interface_5(nbif_name, cbif_name)
 *
 * A primop with no P parameter, 0-3 or 5 ordinary parameters,
 * and no failure mode.
 * HP, HP_LIMIT, FCALLS, NSP, NSP_LIMIT, and NRA may not be accessed.
 */

/****************************************************************
 *			BIF CLASSIFICATION			*
 ****************************************************************/

/*
 * Zero-arity BIFs that can fail.
 */

#ifndef HAVE_nbif_memory_0
#define HAVE_nbif_memory_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_memory_0))
ASYM(nbif_memory_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(memory_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_0_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_memory_0))
	TYPE_FUNCTION(ASYM(nbif_memory_0))
#endif

#ifndef HAVE_nbif_processes_0
#define HAVE_nbif_processes_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_processes_0))
ASYM(nbif_processes_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(processes_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_0_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_processes_0))
	TYPE_FUNCTION(ASYM(nbif_processes_0))
#endif

/*
 * BIFs and primops that may do a GC (change heap limit and walk the native stack).
 * XXX: erase/1 and put/2 cannot fail
 */

#ifndef HAVE_nbif_check_process_code_2
#define HAVE_nbif_check_process_code_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_check_process_code_2))
ASYM(nbif_check_process_code_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(hipe_check_process_code_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_check_process_code_2))
	TYPE_FUNCTION(ASYM(nbif_check_process_code_2))
#endif

#ifndef HAVE_nbif_erase_1
#define HAVE_nbif_erase_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_erase_1))
ASYM(nbif_erase_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(erase_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_erase_1))
	TYPE_FUNCTION(ASYM(nbif_erase_1))
#endif

#ifndef HAVE_nbif_garbage_collect_0
#define HAVE_nbif_garbage_collect_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_garbage_collect_0))
ASYM(nbif_garbage_collect_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(garbage_collect_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_garbage_collect_0))
	TYPE_FUNCTION(ASYM(nbif_garbage_collect_0))
#endif

#ifndef HAVE_nbif_garbage_collect_1
#define HAVE_nbif_garbage_collect_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_garbage_collect_1))
ASYM(nbif_garbage_collect_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(hipe_garbage_collect_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_garbage_collect_1))
	TYPE_FUNCTION(ASYM(nbif_garbage_collect_1))
#endif

#ifndef HAVE_nbif_gc_1
#define HAVE_nbif_gc_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_gc_1))
ASYM(nbif_gc_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(hipe_gc)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_gc_1))
	TYPE_FUNCTION(ASYM(nbif_gc_1))
#endif

#ifndef HAVE_nbif_put_2
#define HAVE_nbif_put_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_put_2))
ASYM(nbif_put_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(put_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_put_2))
	TYPE_FUNCTION(ASYM(nbif_put_2))
#endif

/*
 * Debug BIFs that need read access to the full state.
 * hipe_bifs:nstack_used_size/0 only needs read access to NSP.
 * They are classified as GC BIFs for simplicity.
 */

#ifndef HAVE_nbif_hipe_bifs_show_nstack_1
#define HAVE_nbif_hipe_bifs_show_nstack_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_show_nstack_1))
ASYM(nbif_hipe_bifs_show_nstack_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(hipe_show_nstack_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_hipe_bifs_show_nstack_1))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_show_nstack_1))
#endif

#ifndef HAVE_nbif_hipe_bifs_show_pcb_1
#define HAVE_nbif_hipe_bifs_show_pcb_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_show_pcb_1))
ASYM(nbif_hipe_bifs_show_pcb_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(hipe_bifs_show_pcb_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_hipe_bifs_show_pcb_1))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_show_pcb_1))
#endif

#ifndef HAVE_nbif_hipe_bifs_nstack_used_size_0
#define HAVE_nbif_hipe_bifs_nstack_used_size_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_nstack_used_size_0))
ASYM(nbif_hipe_bifs_nstack_used_size_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(hipe_bifs_nstack_used_size_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_hipe_bifs_nstack_used_size_0))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_nstack_used_size_0))
#endif

/*
 * Arithmetic operators called indirectly by the HiPE compiler.
 */

#ifndef HAVE_nbif_add_2
#define HAVE_nbif_add_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_add_2))
ASYM(nbif_add_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(erts_mixed_plus)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_add_2))
	TYPE_FUNCTION(ASYM(nbif_add_2))
#endif

#ifndef HAVE_nbif_sub_2
#define HAVE_nbif_sub_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_sub_2))
ASYM(nbif_sub_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(erts_mixed_minus)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_sub_2))
	TYPE_FUNCTION(ASYM(nbif_sub_2))
#endif

#ifndef HAVE_nbif_mul_2
#define HAVE_nbif_mul_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_mul_2))
ASYM(nbif_mul_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(erts_mixed_times)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_mul_2))
	TYPE_FUNCTION(ASYM(nbif_mul_2))
#endif

#ifndef HAVE_nbif_div_2
#define HAVE_nbif_div_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_div_2))
ASYM(nbif_div_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(erts_mixed_div)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_div_2))
	TYPE_FUNCTION(ASYM(nbif_div_2))
#endif

#ifndef HAVE_nbif_intdiv_2
#define HAVE_nbif_intdiv_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_intdiv_2))
ASYM(nbif_intdiv_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(intdiv_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_intdiv_2))
	TYPE_FUNCTION(ASYM(nbif_intdiv_2))
#endif

#ifndef HAVE_nbif_rem_2
#define HAVE_nbif_rem_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_rem_2))
ASYM(nbif_rem_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(rem_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_rem_2))
	TYPE_FUNCTION(ASYM(nbif_rem_2))
#endif

#ifndef HAVE_nbif_bsl_2
#define HAVE_nbif_bsl_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_bsl_2))
ASYM(nbif_bsl_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(bsl_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_bsl_2))
	TYPE_FUNCTION(ASYM(nbif_bsl_2))
#endif

#ifndef HAVE_nbif_bsr_2
#define HAVE_nbif_bsr_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_bsr_2))
ASYM(nbif_bsr_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(bsr_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_bsr_2))
	TYPE_FUNCTION(ASYM(nbif_bsr_2))
#endif

#ifndef HAVE_nbif_band_2
#define HAVE_nbif_band_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_band_2))
ASYM(nbif_band_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(band_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_band_2))
	TYPE_FUNCTION(ASYM(nbif_band_2))
#endif

#ifndef HAVE_nbif_bor_2
#define HAVE_nbif_bor_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_bor_2))
ASYM(nbif_bor_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(bor_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_bor_2))
	TYPE_FUNCTION(ASYM(nbif_bor_2))
#endif

#ifndef HAVE_nbif_bxor_2
#define HAVE_nbif_bxor_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_bxor_2))
ASYM(nbif_bxor_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(bxor_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_bxor_2))
	TYPE_FUNCTION(ASYM(nbif_bxor_2))
#endif

#ifndef HAVE_nbif_bnot_1
#define HAVE_nbif_bnot_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_bnot_1))
ASYM(nbif_bnot_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(bnot_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_bnot_1))
	TYPE_FUNCTION(ASYM(nbif_bnot_1))
#endif

/*
 * Miscellaneous primops.
 */

#ifndef HAVE_nbif_set_timeout
#define HAVE_nbif_set_timeout
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_set_timeout))
ASYM(nbif_set_timeout):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(hipe_set_timeout)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_set_timeout))
	TYPE_FUNCTION(ASYM(nbif_set_timeout))
#endif

#ifndef HAVE_nbif_conv_big_to_float
#define HAVE_nbif_conv_big_to_float
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_conv_big_to_float))
ASYM(nbif_conv_big_to_float):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(hipe_conv_big_to_float)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_conv_big_to_float))
	TYPE_FUNCTION(ASYM(nbif_conv_big_to_float))
#endif

#ifndef HAVE_nbif_rethrow
#define HAVE_nbif_rethrow
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_rethrow))
ASYM(nbif_rethrow):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(hipe_rethrow)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_rethrow))
	TYPE_FUNCTION(ASYM(nbif_rethrow))
#endif

#ifndef HAVE_nbif_find_na_or_make_stub
#define HAVE_nbif_find_na_or_make_stub
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_find_na_or_make_stub))
ASYM(nbif_find_na_or_make_stub):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(hipe_find_na_or_make_stub)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_find_na_or_make_stub))
	TYPE_FUNCTION(ASYM(nbif_find_na_or_make_stub))
#endif

#ifndef HAVE_nbif_nonclosure_address
#define HAVE_nbif_nonclosure_address
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_nonclosure_address))
ASYM(nbif_nonclosure_address):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(hipe_nonclosure_address)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_nonclosure_address))
	TYPE_FUNCTION(ASYM(nbif_nonclosure_address))
#endif

#ifndef HAVE_nbif_fclearerror_error
#define HAVE_nbif_fclearerror_error
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_fclearerror_error))
ASYM(nbif_fclearerror_error):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C_QUICK

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(hipe_fclearerror_error)

	/* switch to native stack */
	SWITCH_C_TO_ERLANG_QUICK

	/* return */
	NSP_RET0
	SET_SIZE(ASYM(nbif_fclearerror_error))
	TYPE_FUNCTION(ASYM(nbif_fclearerror_error))
#endif

/*
 * Mbox primops with implicit P parameter.
 */

#ifndef HAVE_nbif_select_msg
#define HAVE_nbif_select_msg
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_select_msg))
ASYM(nbif_select_msg):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C_QUICK

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(hipe_select_msg)

	/* switch to native stack */
	SWITCH_C_TO_ERLANG_QUICK

	/* return */
	NSP_RET0
	SET_SIZE(ASYM(nbif_select_msg))
	TYPE_FUNCTION(ASYM(nbif_select_msg))
#endif

/*
 * Primops without any P parameter.
 * These cannot CONS or gc.
 */

#ifndef HAVE_nbif_cmp_2
#define HAVE_nbif_cmp_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_cmp_2))
ASYM(nbif_cmp_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C_QUICK

	/* make the call on the C stack */
	movl ARG0,(%esp)
	movl ARG1,4(%esp)
	call	CSYM(cmp)

	/* switch to native stack */
	SWITCH_C_TO_ERLANG_QUICK

	/* return */
	NSP_RET0
	SET_SIZE(ASYM(nbif_cmp_2))
	TYPE_FUNCTION(ASYM(nbif_cmp_2))
#endif

#ifndef HAVE_nbif_eq_2
#define HAVE_nbif_eq_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_eq_2))
ASYM(nbif_eq_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C_QUICK

	/* make the call on the C stack */
	movl ARG0,(%esp)
	movl ARG1,4(%esp)
	call	CSYM(eq)

	/* switch to native stack */
	SWITCH_C_TO_ERLANG_QUICK

	/* return */
	NSP_RET0
	SET_SIZE(ASYM(nbif_eq_2))
	TYPE_FUNCTION(ASYM(nbif_eq_2))
#endif

/*
 * Bit-syntax primops with implicit P parameter.
 * XXX: all of the _2 versions cons on the ordinary heap
 * XXX: all of them can cons and thus update FCALLS
 */

#ifndef HAVE_nbif_bs_get_integer_2
#define HAVE_nbif_bs_get_integer_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_bs_get_integer_2))
ASYM(nbif_bs_get_integer_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(erts_bs_get_integer_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_bs_get_integer_2))
	TYPE_FUNCTION(ASYM(nbif_bs_get_integer_2))
#endif

#ifndef HAVE_nbif_bs_get_binary_2
#define HAVE_nbif_bs_get_binary_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_bs_get_binary_2))
ASYM(nbif_bs_get_binary_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(erts_bs_get_binary_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_bs_get_binary_2))
	TYPE_FUNCTION(ASYM(nbif_bs_get_binary_2))
#endif

#ifndef HAVE_nbif_bs_get_float_2
#define HAVE_nbif_bs_get_float_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_bs_get_float_2))
ASYM(nbif_bs_get_float_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(erts_bs_get_float_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_bs_get_float_2))
	TYPE_FUNCTION(ASYM(nbif_bs_get_float_2))
#endif

#ifndef HAVE_nbif_bs_put_utf8
#define HAVE_nbif_bs_put_utf8
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_bs_put_utf8))
ASYM(nbif_bs_put_utf8):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(hipe_bs_put_utf8)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_bs_put_utf8))
	TYPE_FUNCTION(ASYM(nbif_bs_put_utf8))
#endif

#ifndef HAVE_nbif_bs_put_utf16be
#define HAVE_nbif_bs_put_utf16be
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_bs_put_utf16be))
ASYM(nbif_bs_put_utf16be):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(hipe_bs_put_utf16be)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_bs_put_utf16be))
	TYPE_FUNCTION(ASYM(nbif_bs_put_utf16be))
#endif

#ifndef HAVE_nbif_bs_put_utf16le
#define HAVE_nbif_bs_put_utf16le
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_bs_put_utf16le))
ASYM(nbif_bs_put_utf16le):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(hipe_bs_put_utf16le)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_bs_put_utf16le))
	TYPE_FUNCTION(ASYM(nbif_bs_put_utf16le))
#endif

#ifndef HAVE_nbif_bs_validate_unicode
#define HAVE_nbif_bs_validate_unicode
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_bs_validate_unicode))
ASYM(nbif_bs_validate_unicode):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(hipe_bs_validate_unicode)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_bs_validate_unicode))
	TYPE_FUNCTION(ASYM(nbif_bs_validate_unicode))
#endif

/*
 * Bit-syntax primops without any P parameter.
 * These cannot CONS or gc.
 */

#ifndef HAVE_nbif_bs_allocate
#define HAVE_nbif_bs_allocate
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_bs_allocate))
ASYM(nbif_bs_allocate):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C_QUICK

	/* make the call on the C stack */
	movl ARG0,(%esp)
	call	CSYM(hipe_bs_allocate)

	/* switch to native stack */
	SWITCH_C_TO_ERLANG_QUICK

	/* return */
	NSP_RET0
	SET_SIZE(ASYM(nbif_bs_allocate))
	TYPE_FUNCTION(ASYM(nbif_bs_allocate))
#endif

#ifndef HAVE_nbif_bs_reallocate
#define HAVE_nbif_bs_reallocate
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_bs_reallocate))
ASYM(nbif_bs_reallocate):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C_QUICK

	/* make the call on the C stack */
	movl ARG0,(%esp)
	movl ARG1,4(%esp)
	call	CSYM(hipe_bs_reallocate)

	/* switch to native stack */
	SWITCH_C_TO_ERLANG_QUICK

	/* return */
	NSP_RET0
	SET_SIZE(ASYM(nbif_bs_reallocate))
	TYPE_FUNCTION(ASYM(nbif_bs_reallocate))
#endif

#ifndef HAVE_nbif_bs_utf8_size
#define HAVE_nbif_bs_utf8_size
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_bs_utf8_size))
ASYM(nbif_bs_utf8_size):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C_QUICK

	/* make the call on the C stack */
	movl ARG0,(%esp)
	call	CSYM(hipe_bs_utf8_size)

	/* switch to native stack */
	SWITCH_C_TO_ERLANG_QUICK

	/* return */
	NSP_RET0
	SET_SIZE(ASYM(nbif_bs_utf8_size))
	TYPE_FUNCTION(ASYM(nbif_bs_utf8_size))
#endif

#ifndef HAVE_nbif_bs_get_utf8
#define HAVE_nbif_bs_get_utf8
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_bs_get_utf8))
ASYM(nbif_bs_get_utf8):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C_QUICK

	/* make the call on the C stack */
	movl ARG0,(%esp)
	call	CSYM(erts_bs_get_utf8)

	/* switch to native stack */
	SWITCH_C_TO_ERLANG_QUICK

	/* return */
	NSP_RET0
	SET_SIZE(ASYM(nbif_bs_get_utf8))
	TYPE_FUNCTION(ASYM(nbif_bs_get_utf8))
#endif

#ifndef HAVE_nbif_bs_utf16_size
#define HAVE_nbif_bs_utf16_size
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_bs_utf16_size))
ASYM(nbif_bs_utf16_size):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C_QUICK

	/* make the call on the C stack */
	movl ARG0,(%esp)
	call	CSYM(hipe_bs_utf16_size)

	/* switch to native stack */
	SWITCH_C_TO_ERLANG_QUICK

	/* return */
	NSP_RET0
	SET_SIZE(ASYM(nbif_bs_utf16_size))
	TYPE_FUNCTION(ASYM(nbif_bs_utf16_size))
#endif

#ifndef HAVE_nbif_bs_get_utf16
#define HAVE_nbif_bs_get_utf16
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_bs_get_utf16))
ASYM(nbif_bs_get_utf16):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C_QUICK

	/* make the call on the C stack */
	movl ARG0,(%esp)
	movl ARG1,4(%esp)
	call	CSYM(erts_bs_get_utf16)

	/* switch to native stack */
	SWITCH_C_TO_ERLANG_QUICK

	/* return */
	NSP_RET0
	SET_SIZE(ASYM(nbif_bs_get_utf16))
	TYPE_FUNCTION(ASYM(nbif_bs_get_utf16))
#endif

#ifndef HAVE_nbif_bs_validate_unicode_retract
#define HAVE_nbif_bs_validate_unicode_retract
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_bs_validate_unicode_retract))
ASYM(nbif_bs_validate_unicode_retract):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C_QUICK

	/* make the call on the C stack */
	movl ARG0,(%esp)
	movl ARG1,4(%esp)
	call	CSYM(hipe_bs_validate_unicode_retract)

	/* switch to native stack */
	SWITCH_C_TO_ERLANG_QUICK

	/* return */
	NSP_RET0
	SET_SIZE(ASYM(nbif_bs_validate_unicode_retract))
	TYPE_FUNCTION(ASYM(nbif_bs_validate_unicode_retract))
#endif

/*
 * Bit-syntax primops. The ERTS_SMP runtime system requires P,
 * hence the use of nocons_nofail_primop_interface_N().
 * When ERTS_SMP is disabled, noproc_primop_interface_N()
 * should be used instead.
 */

#ifndef HAVE_nbif_bs_put_small_float
#define HAVE_nbif_bs_put_small_float
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_bs_put_small_float))
ASYM(nbif_bs_put_small_float):
	/* copy native stack pointer */
	movl	%esp, TEMP_NSP

	/* switch to C stack */
	SWITCH_ERLANG_TO_C_QUICK

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	movl 8(TEMP_NSP),%eax; movl %eax,16(%esp)
	movl 4(TEMP_NSP),%eax; movl %eax,20(%esp)
	call	CSYM(hipe_bs_put_small_float)

	/* switch to native stack */
	SWITCH_C_TO_ERLANG_QUICK

	/* return */
	NSP_RETN(8)
	SET_SIZE(ASYM(nbif_bs_put_small_float))
	TYPE_FUNCTION(ASYM(nbif_bs_put_small_float))
#endif

#ifndef HAVE_nbif_bs_put_bits
#define HAVE_nbif_bs_put_bits
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_bs_put_bits))
ASYM(nbif_bs_put_bits):
	/* copy native stack pointer */
	movl	%esp, TEMP_NSP

	/* switch to C stack */
	SWITCH_ERLANG_TO_C_QUICK

	/* make the call on the C stack */
	movl ARG0,(%esp)
	movl ARG1,4(%esp)
	movl ARG2,8(%esp)
	movl 8(TEMP_NSP),%eax; movl %eax,12(%esp)
	movl 4(TEMP_NSP),%eax; movl %eax,16(%esp)
	call	CSYM(hipe_bs_put_bits)

	/* switch to native stack */
	SWITCH_C_TO_ERLANG_QUICK

	/* return */
	NSP_RETN(8)
	SET_SIZE(ASYM(nbif_bs_put_bits))
	TYPE_FUNCTION(ASYM(nbif_bs_put_bits))
#endif


#ifndef HAVE_nbif_bs_put_big_integer
#define HAVE_nbif_bs_put_big_integer
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_bs_put_big_integer))
ASYM(nbif_bs_put_big_integer):
	/* copy native stack pointer */
	movl	%esp, TEMP_NSP

	/* switch to C stack */
	SWITCH_ERLANG_TO_C_QUICK

	/* make the call on the C stack */
	movl ARG0,(%esp)
	movl ARG1,4(%esp)
	movl ARG2,8(%esp)
	movl 8(TEMP_NSP),%eax; movl %eax,12(%esp)
	movl 4(TEMP_NSP),%eax; movl %eax,16(%esp)
	call	CSYM(hipe_bs_put_big_integer)

	/* switch to native stack */
	SWITCH_C_TO_ERLANG_QUICK

	/* return */
	NSP_RETN(8)
	SET_SIZE(ASYM(nbif_bs_put_big_integer))
	TYPE_FUNCTION(ASYM(nbif_bs_put_big_integer))
#endif


#ifndef HAVE_nbif_check_get_msg
#define HAVE_nbif_check_get_msg
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_check_get_msg))
ASYM(nbif_check_get_msg):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(hipe_check_get_msg)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_check_get_msg))
	TYPE_FUNCTION(ASYM(nbif_check_get_msg))
#endif

/*
 * SMP-specific stuff
 */

/*
 * Implement standard_bif_interface_0 as nofail_primop_interface_0.
 */


/*
 * Standard BIFs.
 * BIF_LIST(ModuleAtom,FunctionAtom,Arity,CFun,Index)
 */

/*
 *  Warning: Do not edit this file.  It was automatically
 *  generated by 'make_tables' on Wed Jul 27 10:48:24 2011.
 */


#ifndef HAVE_nbif_abs_1
#define HAVE_nbif_abs_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_abs_1))
ASYM(nbif_abs_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(abs_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_abs_1))
	TYPE_FUNCTION(ASYM(nbif_abs_1))
#endif

#ifndef HAVE_nbif_ebif_abs_1
#define HAVE_nbif_ebif_abs_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_abs_1))
ASYM(nbif_ebif_abs_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_abs_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_abs_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_abs_1))
#endif

#ifndef HAVE_nbif_adler32_1
#define HAVE_nbif_adler32_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_adler32_1))
ASYM(nbif_adler32_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(adler32_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_adler32_1))
	TYPE_FUNCTION(ASYM(nbif_adler32_1))
#endif

#ifndef HAVE_nbif_ebif_adler32_1
#define HAVE_nbif_ebif_adler32_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_adler32_1))
ASYM(nbif_ebif_adler32_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_adler32_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_adler32_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_adler32_1))
#endif

#ifndef HAVE_nbif_adler32_2
#define HAVE_nbif_adler32_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_adler32_2))
ASYM(nbif_adler32_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(adler32_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_adler32_2))
	TYPE_FUNCTION(ASYM(nbif_adler32_2))
#endif

#ifndef HAVE_nbif_ebif_adler32_2
#define HAVE_nbif_ebif_adler32_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_adler32_2))
ASYM(nbif_ebif_adler32_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_adler32_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_adler32_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_adler32_2))
#endif

#ifndef HAVE_nbif_adler32_combine_3
#define HAVE_nbif_adler32_combine_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_adler32_combine_3))
ASYM(nbif_adler32_combine_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(adler32_combine_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_adler32_combine_3))
	TYPE_FUNCTION(ASYM(nbif_adler32_combine_3))
#endif

#ifndef HAVE_nbif_ebif_adler32_combine_3
#define HAVE_nbif_ebif_adler32_combine_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_adler32_combine_3))
ASYM(nbif_ebif_adler32_combine_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(ebif_adler32_combine_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_ebif_adler32_combine_3))
	TYPE_FUNCTION(ASYM(nbif_ebif_adler32_combine_3))
#endif

#ifndef HAVE_nbif_apply_3
#define HAVE_nbif_apply_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_apply_3))
ASYM(nbif_apply_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(apply_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_apply_3))
	TYPE_FUNCTION(ASYM(nbif_apply_3))
#endif

#ifndef HAVE_nbif_ebif_apply_3
#define HAVE_nbif_ebif_apply_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_apply_3))
ASYM(nbif_ebif_apply_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(ebif_apply_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_ebif_apply_3))
	TYPE_FUNCTION(ASYM(nbif_ebif_apply_3))
#endif

#ifndef HAVE_nbif_atom_to_list_1
#define HAVE_nbif_atom_to_list_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_atom_to_list_1))
ASYM(nbif_atom_to_list_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(atom_to_list_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_atom_to_list_1))
	TYPE_FUNCTION(ASYM(nbif_atom_to_list_1))
#endif

#ifndef HAVE_nbif_ebif_atom_to_string_1
#define HAVE_nbif_ebif_atom_to_string_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_atom_to_string_1))
ASYM(nbif_ebif_atom_to_string_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_atom_to_string_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_atom_to_string_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_atom_to_string_1))
#endif

#ifndef HAVE_nbif_binary_to_list_1
#define HAVE_nbif_binary_to_list_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_binary_to_list_1))
ASYM(nbif_binary_to_list_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(binary_to_list_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_binary_to_list_1))
	TYPE_FUNCTION(ASYM(nbif_binary_to_list_1))
#endif

#ifndef HAVE_nbif_ebif_binary_to_list_1
#define HAVE_nbif_ebif_binary_to_list_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_binary_to_list_1))
ASYM(nbif_ebif_binary_to_list_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_binary_to_list_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_binary_to_list_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_binary_to_list_1))
#endif

#ifndef HAVE_nbif_binary_to_list_3
#define HAVE_nbif_binary_to_list_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_binary_to_list_3))
ASYM(nbif_binary_to_list_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(binary_to_list_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_binary_to_list_3))
	TYPE_FUNCTION(ASYM(nbif_binary_to_list_3))
#endif

#ifndef HAVE_nbif_ebif_binary_to_list_3
#define HAVE_nbif_ebif_binary_to_list_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_binary_to_list_3))
ASYM(nbif_ebif_binary_to_list_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(ebif_binary_to_list_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_ebif_binary_to_list_3))
	TYPE_FUNCTION(ASYM(nbif_ebif_binary_to_list_3))
#endif

#ifndef HAVE_nbif_binary_to_term_1
#define HAVE_nbif_binary_to_term_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_binary_to_term_1))
ASYM(nbif_binary_to_term_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(binary_to_term_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_binary_to_term_1))
	TYPE_FUNCTION(ASYM(nbif_binary_to_term_1))
#endif

#ifndef HAVE_nbif_ebif_binary_to_term_1
#define HAVE_nbif_ebif_binary_to_term_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_binary_to_term_1))
ASYM(nbif_ebif_binary_to_term_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_binary_to_term_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_binary_to_term_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_binary_to_term_1))
#endif

#ifndef HAVE_nbif_check_process_code_2
#define HAVE_nbif_check_process_code_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_check_process_code_2))
ASYM(nbif_check_process_code_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(check_process_code_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_check_process_code_2))
	TYPE_FUNCTION(ASYM(nbif_check_process_code_2))
#endif

#ifndef HAVE_nbif_ebif_check_process_code_2
#define HAVE_nbif_ebif_check_process_code_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_check_process_code_2))
ASYM(nbif_ebif_check_process_code_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_check_process_code_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_check_process_code_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_check_process_code_2))
#endif

#ifndef HAVE_nbif_crc32_1
#define HAVE_nbif_crc32_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_crc32_1))
ASYM(nbif_crc32_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(crc32_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_crc32_1))
	TYPE_FUNCTION(ASYM(nbif_crc32_1))
#endif

#ifndef HAVE_nbif_ebif_crc32_1
#define HAVE_nbif_ebif_crc32_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_crc32_1))
ASYM(nbif_ebif_crc32_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_crc32_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_crc32_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_crc32_1))
#endif

#ifndef HAVE_nbif_crc32_2
#define HAVE_nbif_crc32_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_crc32_2))
ASYM(nbif_crc32_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(crc32_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_crc32_2))
	TYPE_FUNCTION(ASYM(nbif_crc32_2))
#endif

#ifndef HAVE_nbif_ebif_crc32_2
#define HAVE_nbif_ebif_crc32_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_crc32_2))
ASYM(nbif_ebif_crc32_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_crc32_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_crc32_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_crc32_2))
#endif

#ifndef HAVE_nbif_crc32_combine_3
#define HAVE_nbif_crc32_combine_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_crc32_combine_3))
ASYM(nbif_crc32_combine_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(crc32_combine_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_crc32_combine_3))
	TYPE_FUNCTION(ASYM(nbif_crc32_combine_3))
#endif

#ifndef HAVE_nbif_ebif_crc32_combine_3
#define HAVE_nbif_ebif_crc32_combine_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_crc32_combine_3))
ASYM(nbif_ebif_crc32_combine_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(ebif_crc32_combine_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_ebif_crc32_combine_3))
	TYPE_FUNCTION(ASYM(nbif_ebif_crc32_combine_3))
#endif

#ifndef HAVE_nbif_date_0
#define HAVE_nbif_date_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_date_0))
ASYM(nbif_date_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(date_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_date_0))
	TYPE_FUNCTION(ASYM(nbif_date_0))
#endif

#ifndef HAVE_nbif_ebif_date_0
#define HAVE_nbif_ebif_date_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_date_0))
ASYM(nbif_ebif_date_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(ebif_date_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_ebif_date_0))
	TYPE_FUNCTION(ASYM(nbif_ebif_date_0))
#endif

#ifndef HAVE_nbif_delete_module_1
#define HAVE_nbif_delete_module_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_delete_module_1))
ASYM(nbif_delete_module_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(delete_module_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_delete_module_1))
	TYPE_FUNCTION(ASYM(nbif_delete_module_1))
#endif

#ifndef HAVE_nbif_ebif_delete_module_1
#define HAVE_nbif_ebif_delete_module_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_delete_module_1))
ASYM(nbif_ebif_delete_module_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_delete_module_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_delete_module_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_delete_module_1))
#endif

#ifndef HAVE_nbif_display_1
#define HAVE_nbif_display_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_display_1))
ASYM(nbif_display_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(display_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_display_1))
	TYPE_FUNCTION(ASYM(nbif_display_1))
#endif

#ifndef HAVE_nbif_ebif_display_1
#define HAVE_nbif_ebif_display_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_display_1))
ASYM(nbif_ebif_display_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_display_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_display_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_display_1))
#endif

#ifndef HAVE_nbif_display_string_1
#define HAVE_nbif_display_string_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_display_string_1))
ASYM(nbif_display_string_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(display_string_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_display_string_1))
	TYPE_FUNCTION(ASYM(nbif_display_string_1))
#endif

#ifndef HAVE_nbif_ebif_display_string_1
#define HAVE_nbif_ebif_display_string_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_display_string_1))
ASYM(nbif_ebif_display_string_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_display_string_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_display_string_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_display_string_1))
#endif

#ifndef HAVE_nbif_display_nl_0
#define HAVE_nbif_display_nl_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_display_nl_0))
ASYM(nbif_display_nl_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(display_nl_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_display_nl_0))
	TYPE_FUNCTION(ASYM(nbif_display_nl_0))
#endif

#ifndef HAVE_nbif_ebif_display_nl_0
#define HAVE_nbif_ebif_display_nl_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_display_nl_0))
ASYM(nbif_ebif_display_nl_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(ebif_display_nl_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_ebif_display_nl_0))
	TYPE_FUNCTION(ASYM(nbif_ebif_display_nl_0))
#endif

#ifndef HAVE_nbif_element_2
#define HAVE_nbif_element_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_element_2))
ASYM(nbif_element_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(element_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_element_2))
	TYPE_FUNCTION(ASYM(nbif_element_2))
#endif

#ifndef HAVE_nbif_ebif_element_2
#define HAVE_nbif_ebif_element_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_element_2))
ASYM(nbif_ebif_element_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_element_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_element_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_element_2))
#endif

#ifndef HAVE_nbif_erase_0
#define HAVE_nbif_erase_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_erase_0))
ASYM(nbif_erase_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(erase_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_erase_0))
	TYPE_FUNCTION(ASYM(nbif_erase_0))
#endif

#ifndef HAVE_nbif_ebif_erase_0
#define HAVE_nbif_ebif_erase_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_erase_0))
ASYM(nbif_ebif_erase_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(ebif_erase_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_ebif_erase_0))
	TYPE_FUNCTION(ASYM(nbif_ebif_erase_0))
#endif

#ifndef HAVE_nbif_erase_1
#define HAVE_nbif_erase_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_erase_1))
ASYM(nbif_erase_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(erase_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_erase_1))
	TYPE_FUNCTION(ASYM(nbif_erase_1))
#endif

#ifndef HAVE_nbif_ebif_erase_1
#define HAVE_nbif_ebif_erase_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_erase_1))
ASYM(nbif_ebif_erase_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_erase_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_erase_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_erase_1))
#endif

#ifndef HAVE_nbif_exit_1
#define HAVE_nbif_exit_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_exit_1))
ASYM(nbif_exit_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(exit_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_exit_1))
	TYPE_FUNCTION(ASYM(nbif_exit_1))
#endif

#ifndef HAVE_nbif_ebif_exit_1
#define HAVE_nbif_ebif_exit_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_exit_1))
ASYM(nbif_ebif_exit_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_exit_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_exit_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_exit_1))
#endif

#ifndef HAVE_nbif_exit_2
#define HAVE_nbif_exit_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_exit_2))
ASYM(nbif_exit_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(exit_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_exit_2))
	TYPE_FUNCTION(ASYM(nbif_exit_2))
#endif

#ifndef HAVE_nbif_ebif_signal_2
#define HAVE_nbif_ebif_signal_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_signal_2))
ASYM(nbif_ebif_signal_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_signal_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_signal_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_signal_2))
#endif

#ifndef HAVE_nbif_external_size_1
#define HAVE_nbif_external_size_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_external_size_1))
ASYM(nbif_external_size_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(external_size_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_external_size_1))
	TYPE_FUNCTION(ASYM(nbif_external_size_1))
#endif

#ifndef HAVE_nbif_ebif_external_size_1
#define HAVE_nbif_ebif_external_size_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_external_size_1))
ASYM(nbif_ebif_external_size_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_external_size_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_external_size_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_external_size_1))
#endif

#ifndef HAVE_nbif_float_1
#define HAVE_nbif_float_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_float_1))
ASYM(nbif_float_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(float_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_float_1))
	TYPE_FUNCTION(ASYM(nbif_float_1))
#endif

#ifndef HAVE_nbif_ebif_to_float_1
#define HAVE_nbif_ebif_to_float_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_to_float_1))
ASYM(nbif_ebif_to_float_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_to_float_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_to_float_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_to_float_1))
#endif

#ifndef HAVE_nbif_float_to_list_1
#define HAVE_nbif_float_to_list_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_float_to_list_1))
ASYM(nbif_float_to_list_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(float_to_list_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_float_to_list_1))
	TYPE_FUNCTION(ASYM(nbif_float_to_list_1))
#endif

#ifndef HAVE_nbif_ebif_float_to_string_1
#define HAVE_nbif_ebif_float_to_string_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_float_to_string_1))
ASYM(nbif_ebif_float_to_string_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_float_to_string_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_float_to_string_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_float_to_string_1))
#endif

#ifndef HAVE_nbif_fun_info_2
#define HAVE_nbif_fun_info_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_fun_info_2))
ASYM(nbif_fun_info_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(fun_info_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_fun_info_2))
	TYPE_FUNCTION(ASYM(nbif_fun_info_2))
#endif

#ifndef HAVE_nbif_ebif_fun_info_2
#define HAVE_nbif_ebif_fun_info_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_fun_info_2))
ASYM(nbif_ebif_fun_info_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_fun_info_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_fun_info_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_fun_info_2))
#endif

#ifndef HAVE_nbif_garbage_collect_0
#define HAVE_nbif_garbage_collect_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_garbage_collect_0))
ASYM(nbif_garbage_collect_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(garbage_collect_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_garbage_collect_0))
	TYPE_FUNCTION(ASYM(nbif_garbage_collect_0))
#endif

#ifndef HAVE_nbif_ebif_garbage_collect_0
#define HAVE_nbif_ebif_garbage_collect_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_garbage_collect_0))
ASYM(nbif_ebif_garbage_collect_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(ebif_garbage_collect_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_ebif_garbage_collect_0))
	TYPE_FUNCTION(ASYM(nbif_ebif_garbage_collect_0))
#endif

#ifndef HAVE_nbif_garbage_collect_1
#define HAVE_nbif_garbage_collect_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_garbage_collect_1))
ASYM(nbif_garbage_collect_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(garbage_collect_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_garbage_collect_1))
	TYPE_FUNCTION(ASYM(nbif_garbage_collect_1))
#endif

#ifndef HAVE_nbif_ebif_garbage_collect_1
#define HAVE_nbif_ebif_garbage_collect_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_garbage_collect_1))
ASYM(nbif_ebif_garbage_collect_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_garbage_collect_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_garbage_collect_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_garbage_collect_1))
#endif

#ifndef HAVE_nbif_garbage_collect_message_area_0
#define HAVE_nbif_garbage_collect_message_area_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_garbage_collect_message_area_0))
ASYM(nbif_garbage_collect_message_area_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(garbage_collect_message_area_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_garbage_collect_message_area_0))
	TYPE_FUNCTION(ASYM(nbif_garbage_collect_message_area_0))
#endif

#ifndef HAVE_nbif_ebif_garbage_collect_message_area_0
#define HAVE_nbif_ebif_garbage_collect_message_area_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_garbage_collect_message_area_0))
ASYM(nbif_ebif_garbage_collect_message_area_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(ebif_garbage_collect_message_area_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_ebif_garbage_collect_message_area_0))
	TYPE_FUNCTION(ASYM(nbif_ebif_garbage_collect_message_area_0))
#endif

#ifndef HAVE_nbif_get_0
#define HAVE_nbif_get_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_get_0))
ASYM(nbif_get_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(get_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_get_0))
	TYPE_FUNCTION(ASYM(nbif_get_0))
#endif

#ifndef HAVE_nbif_ebif_get_0
#define HAVE_nbif_ebif_get_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_get_0))
ASYM(nbif_ebif_get_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(ebif_get_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_ebif_get_0))
	TYPE_FUNCTION(ASYM(nbif_ebif_get_0))
#endif

#ifndef HAVE_nbif_get_1
#define HAVE_nbif_get_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_get_1))
ASYM(nbif_get_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(get_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_get_1))
	TYPE_FUNCTION(ASYM(nbif_get_1))
#endif

#ifndef HAVE_nbif_ebif_get_1
#define HAVE_nbif_ebif_get_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_get_1))
ASYM(nbif_ebif_get_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_get_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_get_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_get_1))
#endif

#ifndef HAVE_nbif_get_keys_1
#define HAVE_nbif_get_keys_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_get_keys_1))
ASYM(nbif_get_keys_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(get_keys_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_get_keys_1))
	TYPE_FUNCTION(ASYM(nbif_get_keys_1))
#endif

#ifndef HAVE_nbif_ebif_get_keys_1
#define HAVE_nbif_ebif_get_keys_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_get_keys_1))
ASYM(nbif_ebif_get_keys_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_get_keys_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_get_keys_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_get_keys_1))
#endif

#ifndef HAVE_nbif_group_leader_0
#define HAVE_nbif_group_leader_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_group_leader_0))
ASYM(nbif_group_leader_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(group_leader_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_group_leader_0))
	TYPE_FUNCTION(ASYM(nbif_group_leader_0))
#endif

#ifndef HAVE_nbif_ebif_group_leader_0
#define HAVE_nbif_ebif_group_leader_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_group_leader_0))
ASYM(nbif_ebif_group_leader_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(ebif_group_leader_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_ebif_group_leader_0))
	TYPE_FUNCTION(ASYM(nbif_ebif_group_leader_0))
#endif

#ifndef HAVE_nbif_group_leader_2
#define HAVE_nbif_group_leader_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_group_leader_2))
ASYM(nbif_group_leader_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(group_leader_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_group_leader_2))
	TYPE_FUNCTION(ASYM(nbif_group_leader_2))
#endif

#ifndef HAVE_nbif_ebif_group_leader_2
#define HAVE_nbif_ebif_group_leader_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_group_leader_2))
ASYM(nbif_ebif_group_leader_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_group_leader_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_group_leader_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_group_leader_2))
#endif

#ifndef HAVE_nbif_halt_0
#define HAVE_nbif_halt_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_halt_0))
ASYM(nbif_halt_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(halt_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_halt_0))
	TYPE_FUNCTION(ASYM(nbif_halt_0))
#endif

#ifndef HAVE_nbif_ebif_halt_0
#define HAVE_nbif_ebif_halt_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_halt_0))
ASYM(nbif_ebif_halt_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(ebif_halt_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_ebif_halt_0))
	TYPE_FUNCTION(ASYM(nbif_ebif_halt_0))
#endif

#ifndef HAVE_nbif_halt_1
#define HAVE_nbif_halt_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_halt_1))
ASYM(nbif_halt_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(halt_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_halt_1))
	TYPE_FUNCTION(ASYM(nbif_halt_1))
#endif

#ifndef HAVE_nbif_ebif_halt_1
#define HAVE_nbif_ebif_halt_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_halt_1))
ASYM(nbif_ebif_halt_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_halt_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_halt_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_halt_1))
#endif

#ifndef HAVE_nbif_phash_2
#define HAVE_nbif_phash_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_phash_2))
ASYM(nbif_phash_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(phash_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_phash_2))
	TYPE_FUNCTION(ASYM(nbif_phash_2))
#endif

#ifndef HAVE_nbif_phash2_1
#define HAVE_nbif_phash2_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_phash2_1))
ASYM(nbif_phash2_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(phash2_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_phash2_1))
	TYPE_FUNCTION(ASYM(nbif_phash2_1))
#endif

#ifndef HAVE_nbif_phash2_2
#define HAVE_nbif_phash2_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_phash2_2))
ASYM(nbif_phash2_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(phash2_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_phash2_2))
	TYPE_FUNCTION(ASYM(nbif_phash2_2))
#endif

#ifndef HAVE_nbif_ebif_phash2_1
#define HAVE_nbif_ebif_phash2_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_phash2_1))
ASYM(nbif_ebif_phash2_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_phash2_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_phash2_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_phash2_1))
#endif

#ifndef HAVE_nbif_ebif_phash2_2
#define HAVE_nbif_ebif_phash2_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_phash2_2))
ASYM(nbif_ebif_phash2_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_phash2_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_phash2_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_phash2_2))
#endif

#ifndef HAVE_nbif_hd_1
#define HAVE_nbif_hd_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hd_1))
ASYM(nbif_hd_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(hd_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_hd_1))
	TYPE_FUNCTION(ASYM(nbif_hd_1))
#endif

#ifndef HAVE_nbif_ebif_hd_1
#define HAVE_nbif_ebif_hd_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_hd_1))
ASYM(nbif_ebif_hd_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_hd_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_hd_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_hd_1))
#endif

#ifndef HAVE_nbif_integer_to_list_1
#define HAVE_nbif_integer_to_list_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_integer_to_list_1))
ASYM(nbif_integer_to_list_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(integer_to_list_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_integer_to_list_1))
	TYPE_FUNCTION(ASYM(nbif_integer_to_list_1))
#endif

#ifndef HAVE_nbif_ebif_integer_to_string_1
#define HAVE_nbif_ebif_integer_to_string_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_integer_to_string_1))
ASYM(nbif_ebif_integer_to_string_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_integer_to_string_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_integer_to_string_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_integer_to_string_1))
#endif

#ifndef HAVE_nbif_is_alive_0
#define HAVE_nbif_is_alive_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_is_alive_0))
ASYM(nbif_is_alive_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(is_alive_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_is_alive_0))
	TYPE_FUNCTION(ASYM(nbif_is_alive_0))
#endif

#ifndef HAVE_nbif_ebif_is_alive_0
#define HAVE_nbif_ebif_is_alive_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_is_alive_0))
ASYM(nbif_ebif_is_alive_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(ebif_is_alive_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_ebif_is_alive_0))
	TYPE_FUNCTION(ASYM(nbif_ebif_is_alive_0))
#endif

#ifndef HAVE_nbif_length_1
#define HAVE_nbif_length_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_length_1))
ASYM(nbif_length_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(length_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_length_1))
	TYPE_FUNCTION(ASYM(nbif_length_1))
#endif

#ifndef HAVE_nbif_ebif_length_1
#define HAVE_nbif_ebif_length_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_length_1))
ASYM(nbif_ebif_length_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_length_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_length_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_length_1))
#endif

#ifndef HAVE_nbif_link_1
#define HAVE_nbif_link_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_link_1))
ASYM(nbif_link_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(link_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_link_1))
	TYPE_FUNCTION(ASYM(nbif_link_1))
#endif

#ifndef HAVE_nbif_ebif_link_1
#define HAVE_nbif_ebif_link_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_link_1))
ASYM(nbif_ebif_link_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_link_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_link_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_link_1))
#endif

#ifndef HAVE_nbif_list_to_atom_1
#define HAVE_nbif_list_to_atom_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_list_to_atom_1))
ASYM(nbif_list_to_atom_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(list_to_atom_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_list_to_atom_1))
	TYPE_FUNCTION(ASYM(nbif_list_to_atom_1))
#endif

#ifndef HAVE_nbif_ebif_string_to_atom_1
#define HAVE_nbif_ebif_string_to_atom_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_string_to_atom_1))
ASYM(nbif_ebif_string_to_atom_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_string_to_atom_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_string_to_atom_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_string_to_atom_1))
#endif

#ifndef HAVE_nbif_list_to_binary_1
#define HAVE_nbif_list_to_binary_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_list_to_binary_1))
ASYM(nbif_list_to_binary_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(list_to_binary_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_list_to_binary_1))
	TYPE_FUNCTION(ASYM(nbif_list_to_binary_1))
#endif

#ifndef HAVE_nbif_ebif_list_to_binary_1
#define HAVE_nbif_ebif_list_to_binary_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_list_to_binary_1))
ASYM(nbif_ebif_list_to_binary_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_list_to_binary_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_list_to_binary_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_list_to_binary_1))
#endif

#ifndef HAVE_nbif_list_to_float_1
#define HAVE_nbif_list_to_float_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_list_to_float_1))
ASYM(nbif_list_to_float_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(list_to_float_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_list_to_float_1))
	TYPE_FUNCTION(ASYM(nbif_list_to_float_1))
#endif

#ifndef HAVE_nbif_ebif_string_to_float_1
#define HAVE_nbif_ebif_string_to_float_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_string_to_float_1))
ASYM(nbif_ebif_string_to_float_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_string_to_float_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_string_to_float_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_string_to_float_1))
#endif

#ifndef HAVE_nbif_list_to_integer_1
#define HAVE_nbif_list_to_integer_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_list_to_integer_1))
ASYM(nbif_list_to_integer_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(list_to_integer_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_list_to_integer_1))
	TYPE_FUNCTION(ASYM(nbif_list_to_integer_1))
#endif

#ifndef HAVE_nbif_ebif_string_to_integer_1
#define HAVE_nbif_ebif_string_to_integer_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_string_to_integer_1))
ASYM(nbif_ebif_string_to_integer_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_string_to_integer_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_string_to_integer_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_string_to_integer_1))
#endif

#ifndef HAVE_nbif_list_to_pid_1
#define HAVE_nbif_list_to_pid_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_list_to_pid_1))
ASYM(nbif_list_to_pid_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(list_to_pid_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_list_to_pid_1))
	TYPE_FUNCTION(ASYM(nbif_list_to_pid_1))
#endif

#ifndef HAVE_nbif_ebif_string_to_pid_1
#define HAVE_nbif_ebif_string_to_pid_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_string_to_pid_1))
ASYM(nbif_ebif_string_to_pid_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_string_to_pid_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_string_to_pid_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_string_to_pid_1))
#endif

#ifndef HAVE_nbif_list_to_tuple_1
#define HAVE_nbif_list_to_tuple_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_list_to_tuple_1))
ASYM(nbif_list_to_tuple_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(list_to_tuple_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_list_to_tuple_1))
	TYPE_FUNCTION(ASYM(nbif_list_to_tuple_1))
#endif

#ifndef HAVE_nbif_ebif_list_to_tuple_1
#define HAVE_nbif_ebif_list_to_tuple_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_list_to_tuple_1))
ASYM(nbif_ebif_list_to_tuple_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_list_to_tuple_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_list_to_tuple_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_list_to_tuple_1))
#endif

#ifndef HAVE_nbif_load_module_2
#define HAVE_nbif_load_module_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_load_module_2))
ASYM(nbif_load_module_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(load_module_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_load_module_2))
	TYPE_FUNCTION(ASYM(nbif_load_module_2))
#endif

#ifndef HAVE_nbif_ebif_load_module_2
#define HAVE_nbif_ebif_load_module_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_load_module_2))
ASYM(nbif_ebif_load_module_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_load_module_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_load_module_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_load_module_2))
#endif

#ifndef HAVE_nbif_loaded_0
#define HAVE_nbif_loaded_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_loaded_0))
ASYM(nbif_loaded_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(loaded_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_loaded_0))
	TYPE_FUNCTION(ASYM(nbif_loaded_0))
#endif

#ifndef HAVE_nbif_ebif_loaded_0
#define HAVE_nbif_ebif_loaded_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_loaded_0))
ASYM(nbif_ebif_loaded_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(ebif_loaded_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_ebif_loaded_0))
	TYPE_FUNCTION(ASYM(nbif_ebif_loaded_0))
#endif

#ifndef HAVE_nbif_localtime_0
#define HAVE_nbif_localtime_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_localtime_0))
ASYM(nbif_localtime_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(localtime_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_localtime_0))
	TYPE_FUNCTION(ASYM(nbif_localtime_0))
#endif

#ifndef HAVE_nbif_ebif_localtime_0
#define HAVE_nbif_ebif_localtime_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_localtime_0))
ASYM(nbif_ebif_localtime_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(ebif_localtime_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_ebif_localtime_0))
	TYPE_FUNCTION(ASYM(nbif_ebif_localtime_0))
#endif

#ifndef HAVE_nbif_localtime_to_universaltime_2
#define HAVE_nbif_localtime_to_universaltime_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_localtime_to_universaltime_2))
ASYM(nbif_localtime_to_universaltime_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(localtime_to_universaltime_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_localtime_to_universaltime_2))
	TYPE_FUNCTION(ASYM(nbif_localtime_to_universaltime_2))
#endif

#ifndef HAVE_nbif_ebif_localtime_to_universaltime_2
#define HAVE_nbif_ebif_localtime_to_universaltime_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_localtime_to_universaltime_2))
ASYM(nbif_ebif_localtime_to_universaltime_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_localtime_to_universaltime_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_localtime_to_universaltime_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_localtime_to_universaltime_2))
#endif

#ifndef HAVE_nbif_make_ref_0
#define HAVE_nbif_make_ref_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_make_ref_0))
ASYM(nbif_make_ref_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(make_ref_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_make_ref_0))
	TYPE_FUNCTION(ASYM(nbif_make_ref_0))
#endif

#ifndef HAVE_nbif_ebif_make_ref_0
#define HAVE_nbif_ebif_make_ref_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_make_ref_0))
ASYM(nbif_ebif_make_ref_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(ebif_make_ref_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_ebif_make_ref_0))
	TYPE_FUNCTION(ASYM(nbif_ebif_make_ref_0))
#endif

#ifndef HAVE_nbif_md5_1
#define HAVE_nbif_md5_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_md5_1))
ASYM(nbif_md5_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(md5_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_md5_1))
	TYPE_FUNCTION(ASYM(nbif_md5_1))
#endif

#ifndef HAVE_nbif_ebif_md5_1
#define HAVE_nbif_ebif_md5_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_md5_1))
ASYM(nbif_ebif_md5_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_md5_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_md5_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_md5_1))
#endif

#ifndef HAVE_nbif_md5_init_0
#define HAVE_nbif_md5_init_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_md5_init_0))
ASYM(nbif_md5_init_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(md5_init_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_md5_init_0))
	TYPE_FUNCTION(ASYM(nbif_md5_init_0))
#endif

#ifndef HAVE_nbif_ebif_md5_init_0
#define HAVE_nbif_ebif_md5_init_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_md5_init_0))
ASYM(nbif_ebif_md5_init_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(ebif_md5_init_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_ebif_md5_init_0))
	TYPE_FUNCTION(ASYM(nbif_ebif_md5_init_0))
#endif

#ifndef HAVE_nbif_md5_update_2
#define HAVE_nbif_md5_update_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_md5_update_2))
ASYM(nbif_md5_update_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(md5_update_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_md5_update_2))
	TYPE_FUNCTION(ASYM(nbif_md5_update_2))
#endif

#ifndef HAVE_nbif_ebif_md5_update_2
#define HAVE_nbif_ebif_md5_update_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_md5_update_2))
ASYM(nbif_ebif_md5_update_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_md5_update_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_md5_update_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_md5_update_2))
#endif

#ifndef HAVE_nbif_md5_final_1
#define HAVE_nbif_md5_final_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_md5_final_1))
ASYM(nbif_md5_final_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(md5_final_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_md5_final_1))
	TYPE_FUNCTION(ASYM(nbif_md5_final_1))
#endif

#ifndef HAVE_nbif_ebif_md5_final_1
#define HAVE_nbif_ebif_md5_final_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_md5_final_1))
ASYM(nbif_ebif_md5_final_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_md5_final_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_md5_final_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_md5_final_1))
#endif

#ifndef HAVE_nbif_memory_0
#define HAVE_nbif_memory_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_memory_0))
ASYM(nbif_memory_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(memory_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_memory_0))
	TYPE_FUNCTION(ASYM(nbif_memory_0))
#endif

#ifndef HAVE_nbif_ebif_memory_0
#define HAVE_nbif_ebif_memory_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_memory_0))
ASYM(nbif_ebif_memory_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(ebif_memory_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_ebif_memory_0))
	TYPE_FUNCTION(ASYM(nbif_ebif_memory_0))
#endif

#ifndef HAVE_nbif_memory_1
#define HAVE_nbif_memory_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_memory_1))
ASYM(nbif_memory_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(memory_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_memory_1))
	TYPE_FUNCTION(ASYM(nbif_memory_1))
#endif

#ifndef HAVE_nbif_ebif_memory_1
#define HAVE_nbif_ebif_memory_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_memory_1))
ASYM(nbif_ebif_memory_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_memory_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_memory_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_memory_1))
#endif

#ifndef HAVE_nbif_module_loaded_1
#define HAVE_nbif_module_loaded_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_module_loaded_1))
ASYM(nbif_module_loaded_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(module_loaded_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_module_loaded_1))
	TYPE_FUNCTION(ASYM(nbif_module_loaded_1))
#endif

#ifndef HAVE_nbif_ebif_is_loaded_1
#define HAVE_nbif_ebif_is_loaded_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_is_loaded_1))
ASYM(nbif_ebif_is_loaded_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_is_loaded_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_is_loaded_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_is_loaded_1))
#endif

#ifndef HAVE_nbif_function_exported_3
#define HAVE_nbif_function_exported_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_function_exported_3))
ASYM(nbif_function_exported_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(function_exported_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_function_exported_3))
	TYPE_FUNCTION(ASYM(nbif_function_exported_3))
#endif

#ifndef HAVE_nbif_ebif_is_loaded_3
#define HAVE_nbif_ebif_is_loaded_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_is_loaded_3))
ASYM(nbif_ebif_is_loaded_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(ebif_is_loaded_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_ebif_is_loaded_3))
	TYPE_FUNCTION(ASYM(nbif_ebif_is_loaded_3))
#endif

#ifndef HAVE_nbif_monitor_node_2
#define HAVE_nbif_monitor_node_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_monitor_node_2))
ASYM(nbif_monitor_node_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(monitor_node_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_monitor_node_2))
	TYPE_FUNCTION(ASYM(nbif_monitor_node_2))
#endif

#ifndef HAVE_nbif_ebif_monitor_node_2
#define HAVE_nbif_ebif_monitor_node_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_monitor_node_2))
ASYM(nbif_ebif_monitor_node_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_monitor_node_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_monitor_node_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_monitor_node_2))
#endif

#ifndef HAVE_nbif_monitor_node_3
#define HAVE_nbif_monitor_node_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_monitor_node_3))
ASYM(nbif_monitor_node_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(monitor_node_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_monitor_node_3))
	TYPE_FUNCTION(ASYM(nbif_monitor_node_3))
#endif

#ifndef HAVE_nbif_ebif_monitor_node_3
#define HAVE_nbif_ebif_monitor_node_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_monitor_node_3))
ASYM(nbif_ebif_monitor_node_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(ebif_monitor_node_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_ebif_monitor_node_3))
	TYPE_FUNCTION(ASYM(nbif_ebif_monitor_node_3))
#endif

#ifndef HAVE_nbif_node_1
#define HAVE_nbif_node_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_node_1))
ASYM(nbif_node_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(node_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_node_1))
	TYPE_FUNCTION(ASYM(nbif_node_1))
#endif

#ifndef HAVE_nbif_ebif_node_1
#define HAVE_nbif_ebif_node_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_node_1))
ASYM(nbif_ebif_node_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_node_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_node_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_node_1))
#endif

#ifndef HAVE_nbif_node_0
#define HAVE_nbif_node_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_node_0))
ASYM(nbif_node_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(node_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_node_0))
	TYPE_FUNCTION(ASYM(nbif_node_0))
#endif

#ifndef HAVE_nbif_ebif_node_0
#define HAVE_nbif_ebif_node_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_node_0))
ASYM(nbif_ebif_node_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(ebif_node_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_ebif_node_0))
	TYPE_FUNCTION(ASYM(nbif_ebif_node_0))
#endif

#ifndef HAVE_nbif_nodes_1
#define HAVE_nbif_nodes_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_nodes_1))
ASYM(nbif_nodes_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(nodes_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_nodes_1))
	TYPE_FUNCTION(ASYM(nbif_nodes_1))
#endif

#ifndef HAVE_nbif_ebif_nodes_1
#define HAVE_nbif_ebif_nodes_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_nodes_1))
ASYM(nbif_ebif_nodes_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_nodes_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_nodes_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_nodes_1))
#endif

#ifndef HAVE_nbif_now_0
#define HAVE_nbif_now_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_now_0))
ASYM(nbif_now_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(now_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_now_0))
	TYPE_FUNCTION(ASYM(nbif_now_0))
#endif

#ifndef HAVE_nbif_ebif_now_0
#define HAVE_nbif_ebif_now_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_now_0))
ASYM(nbif_ebif_now_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(ebif_now_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_ebif_now_0))
	TYPE_FUNCTION(ASYM(nbif_ebif_now_0))
#endif

#ifndef HAVE_nbif_open_port_2
#define HAVE_nbif_open_port_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_open_port_2))
ASYM(nbif_open_port_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(open_port_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_open_port_2))
	TYPE_FUNCTION(ASYM(nbif_open_port_2))
#endif

#ifndef HAVE_nbif_ebif_open_port_2
#define HAVE_nbif_ebif_open_port_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_open_port_2))
ASYM(nbif_ebif_open_port_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_open_port_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_open_port_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_open_port_2))
#endif

#ifndef HAVE_nbif_pid_to_list_1
#define HAVE_nbif_pid_to_list_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_pid_to_list_1))
ASYM(nbif_pid_to_list_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(pid_to_list_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_pid_to_list_1))
	TYPE_FUNCTION(ASYM(nbif_pid_to_list_1))
#endif

#ifndef HAVE_nbif_ebif_pid_to_string_1
#define HAVE_nbif_ebif_pid_to_string_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_pid_to_string_1))
ASYM(nbif_ebif_pid_to_string_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_pid_to_string_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_pid_to_string_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_pid_to_string_1))
#endif

#ifndef HAVE_nbif_port_info_1
#define HAVE_nbif_port_info_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_port_info_1))
ASYM(nbif_port_info_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(port_info_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_port_info_1))
	TYPE_FUNCTION(ASYM(nbif_port_info_1))
#endif

#ifndef HAVE_nbif_ebif_port_info_1
#define HAVE_nbif_ebif_port_info_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_port_info_1))
ASYM(nbif_ebif_port_info_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_port_info_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_port_info_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_port_info_1))
#endif

#ifndef HAVE_nbif_port_info_2
#define HAVE_nbif_port_info_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_port_info_2))
ASYM(nbif_port_info_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(port_info_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_port_info_2))
	TYPE_FUNCTION(ASYM(nbif_port_info_2))
#endif

#ifndef HAVE_nbif_ebif_port_info_2
#define HAVE_nbif_ebif_port_info_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_port_info_2))
ASYM(nbif_ebif_port_info_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_port_info_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_port_info_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_port_info_2))
#endif

#ifndef HAVE_nbif_ports_0
#define HAVE_nbif_ports_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ports_0))
ASYM(nbif_ports_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(ports_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_ports_0))
	TYPE_FUNCTION(ASYM(nbif_ports_0))
#endif

#ifndef HAVE_nbif_ebif_ports_0
#define HAVE_nbif_ebif_ports_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_ports_0))
ASYM(nbif_ebif_ports_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(ebif_ports_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_ebif_ports_0))
	TYPE_FUNCTION(ASYM(nbif_ebif_ports_0))
#endif

#ifndef HAVE_nbif_pre_loaded_0
#define HAVE_nbif_pre_loaded_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_pre_loaded_0))
ASYM(nbif_pre_loaded_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(pre_loaded_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_pre_loaded_0))
	TYPE_FUNCTION(ASYM(nbif_pre_loaded_0))
#endif

#ifndef HAVE_nbif_ebif_pre_loaded_0
#define HAVE_nbif_ebif_pre_loaded_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_pre_loaded_0))
ASYM(nbif_ebif_pre_loaded_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(ebif_pre_loaded_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_ebif_pre_loaded_0))
	TYPE_FUNCTION(ASYM(nbif_ebif_pre_loaded_0))
#endif

#ifndef HAVE_nbif_process_flag_2
#define HAVE_nbif_process_flag_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_process_flag_2))
ASYM(nbif_process_flag_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(process_flag_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_process_flag_2))
	TYPE_FUNCTION(ASYM(nbif_process_flag_2))
#endif

#ifndef HAVE_nbif_ebif_process_flag_2
#define HAVE_nbif_ebif_process_flag_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_process_flag_2))
ASYM(nbif_ebif_process_flag_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_process_flag_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_process_flag_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_process_flag_2))
#endif

#ifndef HAVE_nbif_process_flag_3
#define HAVE_nbif_process_flag_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_process_flag_3))
ASYM(nbif_process_flag_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(process_flag_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_process_flag_3))
	TYPE_FUNCTION(ASYM(nbif_process_flag_3))
#endif

#ifndef HAVE_nbif_ebif_process_flag_3
#define HAVE_nbif_ebif_process_flag_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_process_flag_3))
ASYM(nbif_ebif_process_flag_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(ebif_process_flag_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_ebif_process_flag_3))
	TYPE_FUNCTION(ASYM(nbif_ebif_process_flag_3))
#endif

#ifndef HAVE_nbif_process_info_1
#define HAVE_nbif_process_info_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_process_info_1))
ASYM(nbif_process_info_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(process_info_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_process_info_1))
	TYPE_FUNCTION(ASYM(nbif_process_info_1))
#endif

#ifndef HAVE_nbif_ebif_process_info_1
#define HAVE_nbif_ebif_process_info_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_process_info_1))
ASYM(nbif_ebif_process_info_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_process_info_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_process_info_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_process_info_1))
#endif

#ifndef HAVE_nbif_process_info_2
#define HAVE_nbif_process_info_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_process_info_2))
ASYM(nbif_process_info_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(process_info_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_process_info_2))
	TYPE_FUNCTION(ASYM(nbif_process_info_2))
#endif

#ifndef HAVE_nbif_ebif_process_info_2
#define HAVE_nbif_ebif_process_info_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_process_info_2))
ASYM(nbif_ebif_process_info_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_process_info_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_process_info_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_process_info_2))
#endif

#ifndef HAVE_nbif_processes_0
#define HAVE_nbif_processes_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_processes_0))
ASYM(nbif_processes_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(processes_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_processes_0))
	TYPE_FUNCTION(ASYM(nbif_processes_0))
#endif

#ifndef HAVE_nbif_ebif_processes_0
#define HAVE_nbif_ebif_processes_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_processes_0))
ASYM(nbif_ebif_processes_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(ebif_processes_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_ebif_processes_0))
	TYPE_FUNCTION(ASYM(nbif_ebif_processes_0))
#endif

#ifndef HAVE_nbif_purge_module_1
#define HAVE_nbif_purge_module_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_purge_module_1))
ASYM(nbif_purge_module_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(purge_module_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_purge_module_1))
	TYPE_FUNCTION(ASYM(nbif_purge_module_1))
#endif

#ifndef HAVE_nbif_ebif_purge_module_1
#define HAVE_nbif_ebif_purge_module_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_purge_module_1))
ASYM(nbif_ebif_purge_module_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_purge_module_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_purge_module_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_purge_module_1))
#endif

#ifndef HAVE_nbif_put_2
#define HAVE_nbif_put_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_put_2))
ASYM(nbif_put_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(put_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_put_2))
	TYPE_FUNCTION(ASYM(nbif_put_2))
#endif

#ifndef HAVE_nbif_ebif_put_2
#define HAVE_nbif_ebif_put_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_put_2))
ASYM(nbif_ebif_put_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_put_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_put_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_put_2))
#endif

#ifndef HAVE_nbif_register_2
#define HAVE_nbif_register_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_register_2))
ASYM(nbif_register_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(register_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_register_2))
	TYPE_FUNCTION(ASYM(nbif_register_2))
#endif

#ifndef HAVE_nbif_ebif_register_2
#define HAVE_nbif_ebif_register_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_register_2))
ASYM(nbif_ebif_register_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_register_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_register_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_register_2))
#endif

#ifndef HAVE_nbif_registered_0
#define HAVE_nbif_registered_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_registered_0))
ASYM(nbif_registered_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(registered_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_registered_0))
	TYPE_FUNCTION(ASYM(nbif_registered_0))
#endif

#ifndef HAVE_nbif_ebif_registered_0
#define HAVE_nbif_ebif_registered_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_registered_0))
ASYM(nbif_ebif_registered_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(ebif_registered_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_ebif_registered_0))
	TYPE_FUNCTION(ASYM(nbif_ebif_registered_0))
#endif

#ifndef HAVE_nbif_round_1
#define HAVE_nbif_round_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_round_1))
ASYM(nbif_round_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(round_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_round_1))
	TYPE_FUNCTION(ASYM(nbif_round_1))
#endif

#ifndef HAVE_nbif_ebif_round_1
#define HAVE_nbif_ebif_round_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_round_1))
ASYM(nbif_ebif_round_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_round_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_round_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_round_1))
#endif

#ifndef HAVE_nbif_self_0
#define HAVE_nbif_self_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_self_0))
ASYM(nbif_self_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(self_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_self_0))
	TYPE_FUNCTION(ASYM(nbif_self_0))
#endif

#ifndef HAVE_nbif_ebif_self_0
#define HAVE_nbif_ebif_self_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_self_0))
ASYM(nbif_ebif_self_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(ebif_self_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_ebif_self_0))
	TYPE_FUNCTION(ASYM(nbif_ebif_self_0))
#endif

#ifndef HAVE_nbif_setelement_3
#define HAVE_nbif_setelement_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_setelement_3))
ASYM(nbif_setelement_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(setelement_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_setelement_3))
	TYPE_FUNCTION(ASYM(nbif_setelement_3))
#endif

#ifndef HAVE_nbif_ebif_setelement_3
#define HAVE_nbif_ebif_setelement_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_setelement_3))
ASYM(nbif_ebif_setelement_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(ebif_setelement_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_ebif_setelement_3))
	TYPE_FUNCTION(ASYM(nbif_ebif_setelement_3))
#endif

#ifndef HAVE_nbif_size_1
#define HAVE_nbif_size_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_size_1))
ASYM(nbif_size_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(size_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_size_1))
	TYPE_FUNCTION(ASYM(nbif_size_1))
#endif

#ifndef HAVE_nbif_ebif_size_1
#define HAVE_nbif_ebif_size_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_size_1))
ASYM(nbif_ebif_size_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_size_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_size_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_size_1))
#endif

#ifndef HAVE_nbif_spawn_3
#define HAVE_nbif_spawn_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_spawn_3))
ASYM(nbif_spawn_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(spawn_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_spawn_3))
	TYPE_FUNCTION(ASYM(nbif_spawn_3))
#endif

#ifndef HAVE_nbif_ebif_spawn_3
#define HAVE_nbif_ebif_spawn_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_spawn_3))
ASYM(nbif_ebif_spawn_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(ebif_spawn_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_ebif_spawn_3))
	TYPE_FUNCTION(ASYM(nbif_ebif_spawn_3))
#endif

#ifndef HAVE_nbif_spawn_link_3
#define HAVE_nbif_spawn_link_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_spawn_link_3))
ASYM(nbif_spawn_link_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(spawn_link_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_spawn_link_3))
	TYPE_FUNCTION(ASYM(nbif_spawn_link_3))
#endif

#ifndef HAVE_nbif_ebif_spawn_link_3
#define HAVE_nbif_ebif_spawn_link_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_spawn_link_3))
ASYM(nbif_ebif_spawn_link_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(ebif_spawn_link_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_ebif_spawn_link_3))
	TYPE_FUNCTION(ASYM(nbif_ebif_spawn_link_3))
#endif

#ifndef HAVE_nbif_split_binary_2
#define HAVE_nbif_split_binary_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_split_binary_2))
ASYM(nbif_split_binary_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(split_binary_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_split_binary_2))
	TYPE_FUNCTION(ASYM(nbif_split_binary_2))
#endif

#ifndef HAVE_nbif_ebif_split_binary_2
#define HAVE_nbif_ebif_split_binary_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_split_binary_2))
ASYM(nbif_ebif_split_binary_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_split_binary_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_split_binary_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_split_binary_2))
#endif

#ifndef HAVE_nbif_statistics_1
#define HAVE_nbif_statistics_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_statistics_1))
ASYM(nbif_statistics_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(statistics_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_statistics_1))
	TYPE_FUNCTION(ASYM(nbif_statistics_1))
#endif

#ifndef HAVE_nbif_ebif_statistics_1
#define HAVE_nbif_ebif_statistics_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_statistics_1))
ASYM(nbif_ebif_statistics_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_statistics_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_statistics_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_statistics_1))
#endif

#ifndef HAVE_nbif_term_to_binary_1
#define HAVE_nbif_term_to_binary_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_term_to_binary_1))
ASYM(nbif_term_to_binary_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(term_to_binary_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_term_to_binary_1))
	TYPE_FUNCTION(ASYM(nbif_term_to_binary_1))
#endif

#ifndef HAVE_nbif_ebif_term_to_binary_1
#define HAVE_nbif_ebif_term_to_binary_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_term_to_binary_1))
ASYM(nbif_ebif_term_to_binary_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_term_to_binary_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_term_to_binary_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_term_to_binary_1))
#endif

#ifndef HAVE_nbif_term_to_binary_2
#define HAVE_nbif_term_to_binary_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_term_to_binary_2))
ASYM(nbif_term_to_binary_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(term_to_binary_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_term_to_binary_2))
	TYPE_FUNCTION(ASYM(nbif_term_to_binary_2))
#endif

#ifndef HAVE_nbif_ebif_term_to_binary_2
#define HAVE_nbif_ebif_term_to_binary_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_term_to_binary_2))
ASYM(nbif_ebif_term_to_binary_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_term_to_binary_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_term_to_binary_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_term_to_binary_2))
#endif

#ifndef HAVE_nbif_throw_1
#define HAVE_nbif_throw_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_throw_1))
ASYM(nbif_throw_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(throw_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_throw_1))
	TYPE_FUNCTION(ASYM(nbif_throw_1))
#endif

#ifndef HAVE_nbif_ebif_throw_1
#define HAVE_nbif_ebif_throw_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_throw_1))
ASYM(nbif_ebif_throw_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_throw_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_throw_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_throw_1))
#endif

#ifndef HAVE_nbif_time_0
#define HAVE_nbif_time_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_time_0))
ASYM(nbif_time_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(time_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_time_0))
	TYPE_FUNCTION(ASYM(nbif_time_0))
#endif

#ifndef HAVE_nbif_ebif_time_0
#define HAVE_nbif_ebif_time_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_time_0))
ASYM(nbif_ebif_time_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(ebif_time_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_ebif_time_0))
	TYPE_FUNCTION(ASYM(nbif_ebif_time_0))
#endif

#ifndef HAVE_nbif_tl_1
#define HAVE_nbif_tl_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_tl_1))
ASYM(nbif_tl_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(tl_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_tl_1))
	TYPE_FUNCTION(ASYM(nbif_tl_1))
#endif

#ifndef HAVE_nbif_ebif_tl_1
#define HAVE_nbif_ebif_tl_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_tl_1))
ASYM(nbif_ebif_tl_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_tl_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_tl_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_tl_1))
#endif

#ifndef HAVE_nbif_trunc_1
#define HAVE_nbif_trunc_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_trunc_1))
ASYM(nbif_trunc_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(trunc_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_trunc_1))
	TYPE_FUNCTION(ASYM(nbif_trunc_1))
#endif

#ifndef HAVE_nbif_ebif_trunc_1
#define HAVE_nbif_ebif_trunc_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_trunc_1))
ASYM(nbif_ebif_trunc_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_trunc_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_trunc_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_trunc_1))
#endif

#ifndef HAVE_nbif_tuple_to_list_1
#define HAVE_nbif_tuple_to_list_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_tuple_to_list_1))
ASYM(nbif_tuple_to_list_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(tuple_to_list_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_tuple_to_list_1))
	TYPE_FUNCTION(ASYM(nbif_tuple_to_list_1))
#endif

#ifndef HAVE_nbif_ebif_tuple_to_list_1
#define HAVE_nbif_ebif_tuple_to_list_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_tuple_to_list_1))
ASYM(nbif_ebif_tuple_to_list_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_tuple_to_list_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_tuple_to_list_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_tuple_to_list_1))
#endif

#ifndef HAVE_nbif_universaltime_0
#define HAVE_nbif_universaltime_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_universaltime_0))
ASYM(nbif_universaltime_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(universaltime_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_universaltime_0))
	TYPE_FUNCTION(ASYM(nbif_universaltime_0))
#endif

#ifndef HAVE_nbif_ebif_universaltime_0
#define HAVE_nbif_ebif_universaltime_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_universaltime_0))
ASYM(nbif_ebif_universaltime_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(ebif_universaltime_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_ebif_universaltime_0))
	TYPE_FUNCTION(ASYM(nbif_ebif_universaltime_0))
#endif

#ifndef HAVE_nbif_universaltime_to_localtime_1
#define HAVE_nbif_universaltime_to_localtime_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_universaltime_to_localtime_1))
ASYM(nbif_universaltime_to_localtime_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(universaltime_to_localtime_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_universaltime_to_localtime_1))
	TYPE_FUNCTION(ASYM(nbif_universaltime_to_localtime_1))
#endif

#ifndef HAVE_nbif_ebif_universaltime_to_localtime_1
#define HAVE_nbif_ebif_universaltime_to_localtime_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_universaltime_to_localtime_1))
ASYM(nbif_ebif_universaltime_to_localtime_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_universaltime_to_localtime_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_universaltime_to_localtime_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_universaltime_to_localtime_1))
#endif

#ifndef HAVE_nbif_unlink_1
#define HAVE_nbif_unlink_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_unlink_1))
ASYM(nbif_unlink_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(unlink_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_unlink_1))
	TYPE_FUNCTION(ASYM(nbif_unlink_1))
#endif

#ifndef HAVE_nbif_ebif_unlink_1
#define HAVE_nbif_ebif_unlink_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_unlink_1))
ASYM(nbif_ebif_unlink_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_unlink_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_unlink_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_unlink_1))
#endif

#ifndef HAVE_nbif_unregister_1
#define HAVE_nbif_unregister_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_unregister_1))
ASYM(nbif_unregister_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(unregister_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_unregister_1))
	TYPE_FUNCTION(ASYM(nbif_unregister_1))
#endif

#ifndef HAVE_nbif_ebif_unregister_1
#define HAVE_nbif_ebif_unregister_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_unregister_1))
ASYM(nbif_ebif_unregister_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_unregister_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_unregister_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_unregister_1))
#endif

#ifndef HAVE_nbif_whereis_1
#define HAVE_nbif_whereis_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_whereis_1))
ASYM(nbif_whereis_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(whereis_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_whereis_1))
	TYPE_FUNCTION(ASYM(nbif_whereis_1))
#endif

#ifndef HAVE_nbif_ebif_whereis_1
#define HAVE_nbif_ebif_whereis_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_whereis_1))
ASYM(nbif_ebif_whereis_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_whereis_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_whereis_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_whereis_1))
#endif

#ifndef HAVE_nbif_spawn_opt_1
#define HAVE_nbif_spawn_opt_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_spawn_opt_1))
ASYM(nbif_spawn_opt_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(spawn_opt_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_spawn_opt_1))
	TYPE_FUNCTION(ASYM(nbif_spawn_opt_1))
#endif

#ifndef HAVE_nbif_ebif_spawn_opt_1
#define HAVE_nbif_ebif_spawn_opt_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_spawn_opt_1))
ASYM(nbif_ebif_spawn_opt_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_spawn_opt_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_spawn_opt_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_spawn_opt_1))
#endif

#ifndef HAVE_nbif_setnode_2
#define HAVE_nbif_setnode_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_setnode_2))
ASYM(nbif_setnode_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(setnode_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_setnode_2))
	TYPE_FUNCTION(ASYM(nbif_setnode_2))
#endif

#ifndef HAVE_nbif_setnode_3
#define HAVE_nbif_setnode_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_setnode_3))
ASYM(nbif_setnode_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(setnode_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_setnode_3))
	TYPE_FUNCTION(ASYM(nbif_setnode_3))
#endif

#ifndef HAVE_nbif_dist_exit_3
#define HAVE_nbif_dist_exit_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_dist_exit_3))
ASYM(nbif_dist_exit_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(dist_exit_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_dist_exit_3))
	TYPE_FUNCTION(ASYM(nbif_dist_exit_3))
#endif

#ifndef HAVE_nbif_port_call_2
#define HAVE_nbif_port_call_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_port_call_2))
ASYM(nbif_port_call_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(port_call_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_port_call_2))
	TYPE_FUNCTION(ASYM(nbif_port_call_2))
#endif

#ifndef HAVE_nbif_ebif_port_call_2
#define HAVE_nbif_ebif_port_call_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_port_call_2))
ASYM(nbif_ebif_port_call_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_port_call_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_port_call_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_port_call_2))
#endif

#ifndef HAVE_nbif_port_call_3
#define HAVE_nbif_port_call_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_port_call_3))
ASYM(nbif_port_call_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(port_call_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_port_call_3))
	TYPE_FUNCTION(ASYM(nbif_port_call_3))
#endif

#ifndef HAVE_nbif_ebif_port_call_3
#define HAVE_nbif_ebif_port_call_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_port_call_3))
ASYM(nbif_ebif_port_call_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(ebif_port_call_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_ebif_port_call_3))
	TYPE_FUNCTION(ASYM(nbif_ebif_port_call_3))
#endif

#ifndef HAVE_nbif_port_command_2
#define HAVE_nbif_port_command_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_port_command_2))
ASYM(nbif_port_command_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(port_command_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_port_command_2))
	TYPE_FUNCTION(ASYM(nbif_port_command_2))
#endif

#ifndef HAVE_nbif_ebif_port_command_2
#define HAVE_nbif_ebif_port_command_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_port_command_2))
ASYM(nbif_ebif_port_command_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_port_command_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_port_command_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_port_command_2))
#endif

#ifndef HAVE_nbif_port_command_3
#define HAVE_nbif_port_command_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_port_command_3))
ASYM(nbif_port_command_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(port_command_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_port_command_3))
	TYPE_FUNCTION(ASYM(nbif_port_command_3))
#endif

#ifndef HAVE_nbif_ebif_port_command_3
#define HAVE_nbif_ebif_port_command_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_port_command_3))
ASYM(nbif_ebif_port_command_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(ebif_port_command_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_ebif_port_command_3))
	TYPE_FUNCTION(ASYM(nbif_ebif_port_command_3))
#endif

#ifndef HAVE_nbif_port_control_3
#define HAVE_nbif_port_control_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_port_control_3))
ASYM(nbif_port_control_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(port_control_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_port_control_3))
	TYPE_FUNCTION(ASYM(nbif_port_control_3))
#endif

#ifndef HAVE_nbif_ebif_port_control_3
#define HAVE_nbif_ebif_port_control_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_port_control_3))
ASYM(nbif_ebif_port_control_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(ebif_port_control_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_ebif_port_control_3))
	TYPE_FUNCTION(ASYM(nbif_ebif_port_control_3))
#endif

#ifndef HAVE_nbif_port_close_1
#define HAVE_nbif_port_close_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_port_close_1))
ASYM(nbif_port_close_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(port_close_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_port_close_1))
	TYPE_FUNCTION(ASYM(nbif_port_close_1))
#endif

#ifndef HAVE_nbif_ebif_port_close_1
#define HAVE_nbif_ebif_port_close_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_port_close_1))
ASYM(nbif_ebif_port_close_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_port_close_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_port_close_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_port_close_1))
#endif

#ifndef HAVE_nbif_port_connect_2
#define HAVE_nbif_port_connect_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_port_connect_2))
ASYM(nbif_port_connect_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(port_connect_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_port_connect_2))
	TYPE_FUNCTION(ASYM(nbif_port_connect_2))
#endif

#ifndef HAVE_nbif_ebif_port_connect_2
#define HAVE_nbif_ebif_port_connect_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_port_connect_2))
ASYM(nbif_ebif_port_connect_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_port_connect_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_port_connect_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_port_connect_2))
#endif

#ifndef HAVE_nbif_port_set_data_2
#define HAVE_nbif_port_set_data_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_port_set_data_2))
ASYM(nbif_port_set_data_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(port_set_data_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_port_set_data_2))
	TYPE_FUNCTION(ASYM(nbif_port_set_data_2))
#endif

#ifndef HAVE_nbif_ebif_port_set_data_2
#define HAVE_nbif_ebif_port_set_data_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_port_set_data_2))
ASYM(nbif_ebif_port_set_data_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_port_set_data_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_port_set_data_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_port_set_data_2))
#endif

#ifndef HAVE_nbif_port_get_data_1
#define HAVE_nbif_port_get_data_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_port_get_data_1))
ASYM(nbif_port_get_data_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(port_get_data_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_port_get_data_1))
	TYPE_FUNCTION(ASYM(nbif_port_get_data_1))
#endif

#ifndef HAVE_nbif_ebif_port_get_data_1
#define HAVE_nbif_ebif_port_get_data_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_port_get_data_1))
ASYM(nbif_ebif_port_get_data_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_port_get_data_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_port_get_data_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_port_get_data_1))
#endif

#ifndef HAVE_nbif_trace_pattern_2
#define HAVE_nbif_trace_pattern_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_trace_pattern_2))
ASYM(nbif_trace_pattern_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(trace_pattern_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_trace_pattern_2))
	TYPE_FUNCTION(ASYM(nbif_trace_pattern_2))
#endif

#ifndef HAVE_nbif_ebif_trace_pattern_2
#define HAVE_nbif_ebif_trace_pattern_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_trace_pattern_2))
ASYM(nbif_ebif_trace_pattern_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_trace_pattern_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_trace_pattern_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_trace_pattern_2))
#endif

#ifndef HAVE_nbif_trace_pattern_3
#define HAVE_nbif_trace_pattern_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_trace_pattern_3))
ASYM(nbif_trace_pattern_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(trace_pattern_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_trace_pattern_3))
	TYPE_FUNCTION(ASYM(nbif_trace_pattern_3))
#endif

#ifndef HAVE_nbif_ebif_trace_pattern_3
#define HAVE_nbif_ebif_trace_pattern_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_trace_pattern_3))
ASYM(nbif_ebif_trace_pattern_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(ebif_trace_pattern_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_ebif_trace_pattern_3))
	TYPE_FUNCTION(ASYM(nbif_ebif_trace_pattern_3))
#endif

#ifndef HAVE_nbif_trace_3
#define HAVE_nbif_trace_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_trace_3))
ASYM(nbif_trace_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(trace_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_trace_3))
	TYPE_FUNCTION(ASYM(nbif_trace_3))
#endif

#ifndef HAVE_nbif_ebif_trace_3
#define HAVE_nbif_ebif_trace_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_trace_3))
ASYM(nbif_ebif_trace_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(ebif_trace_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_ebif_trace_3))
	TYPE_FUNCTION(ASYM(nbif_ebif_trace_3))
#endif

#ifndef HAVE_nbif_trace_info_2
#define HAVE_nbif_trace_info_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_trace_info_2))
ASYM(nbif_trace_info_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(trace_info_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_trace_info_2))
	TYPE_FUNCTION(ASYM(nbif_trace_info_2))
#endif

#ifndef HAVE_nbif_ebif_trace_info_2
#define HAVE_nbif_ebif_trace_info_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_trace_info_2))
ASYM(nbif_ebif_trace_info_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_trace_info_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_trace_info_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_trace_info_2))
#endif

#ifndef HAVE_nbif_trace_delivered_1
#define HAVE_nbif_trace_delivered_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_trace_delivered_1))
ASYM(nbif_trace_delivered_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(trace_delivered_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_trace_delivered_1))
	TYPE_FUNCTION(ASYM(nbif_trace_delivered_1))
#endif

#ifndef HAVE_nbif_ebif_trace_delivered_1
#define HAVE_nbif_ebif_trace_delivered_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_trace_delivered_1))
ASYM(nbif_ebif_trace_delivered_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_trace_delivered_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_trace_delivered_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_trace_delivered_1))
#endif

#ifndef HAVE_nbif_seq_trace_2
#define HAVE_nbif_seq_trace_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_seq_trace_2))
ASYM(nbif_seq_trace_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(seq_trace_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_seq_trace_2))
	TYPE_FUNCTION(ASYM(nbif_seq_trace_2))
#endif

#ifndef HAVE_nbif_ebif_seq_trace_2
#define HAVE_nbif_ebif_seq_trace_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_seq_trace_2))
ASYM(nbif_ebif_seq_trace_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_seq_trace_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_seq_trace_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_seq_trace_2))
#endif

#ifndef HAVE_nbif_seq_trace_info_1
#define HAVE_nbif_seq_trace_info_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_seq_trace_info_1))
ASYM(nbif_seq_trace_info_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(seq_trace_info_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_seq_trace_info_1))
	TYPE_FUNCTION(ASYM(nbif_seq_trace_info_1))
#endif

#ifndef HAVE_nbif_ebif_seq_trace_info_1
#define HAVE_nbif_ebif_seq_trace_info_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_seq_trace_info_1))
ASYM(nbif_ebif_seq_trace_info_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_seq_trace_info_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_seq_trace_info_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_seq_trace_info_1))
#endif

#ifndef HAVE_nbif_seq_trace_print_1
#define HAVE_nbif_seq_trace_print_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_seq_trace_print_1))
ASYM(nbif_seq_trace_print_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(seq_trace_print_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_seq_trace_print_1))
	TYPE_FUNCTION(ASYM(nbif_seq_trace_print_1))
#endif

#ifndef HAVE_nbif_ebif_seq_trace_print_1
#define HAVE_nbif_ebif_seq_trace_print_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_seq_trace_print_1))
ASYM(nbif_ebif_seq_trace_print_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_seq_trace_print_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_seq_trace_print_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_seq_trace_print_1))
#endif

#ifndef HAVE_nbif_seq_trace_print_2
#define HAVE_nbif_seq_trace_print_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_seq_trace_print_2))
ASYM(nbif_seq_trace_print_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(seq_trace_print_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_seq_trace_print_2))
	TYPE_FUNCTION(ASYM(nbif_seq_trace_print_2))
#endif

#ifndef HAVE_nbif_ebif_seq_trace_print_2
#define HAVE_nbif_ebif_seq_trace_print_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_seq_trace_print_2))
ASYM(nbif_ebif_seq_trace_print_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_seq_trace_print_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_seq_trace_print_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_seq_trace_print_2))
#endif

#ifndef HAVE_nbif_suspend_process_2
#define HAVE_nbif_suspend_process_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_suspend_process_2))
ASYM(nbif_suspend_process_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(suspend_process_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_suspend_process_2))
	TYPE_FUNCTION(ASYM(nbif_suspend_process_2))
#endif

#ifndef HAVE_nbif_ebif_suspend_process_2
#define HAVE_nbif_ebif_suspend_process_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_suspend_process_2))
ASYM(nbif_ebif_suspend_process_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_suspend_process_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_suspend_process_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_suspend_process_2))
#endif

#ifndef HAVE_nbif_resume_process_1
#define HAVE_nbif_resume_process_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_resume_process_1))
ASYM(nbif_resume_process_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(resume_process_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_resume_process_1))
	TYPE_FUNCTION(ASYM(nbif_resume_process_1))
#endif

#ifndef HAVE_nbif_ebif_resume_process_1
#define HAVE_nbif_ebif_resume_process_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_resume_process_1))
ASYM(nbif_ebif_resume_process_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_resume_process_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_resume_process_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_resume_process_1))
#endif

#ifndef HAVE_nbif_process_display_2
#define HAVE_nbif_process_display_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_process_display_2))
ASYM(nbif_process_display_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(process_display_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_process_display_2))
	TYPE_FUNCTION(ASYM(nbif_process_display_2))
#endif

#ifndef HAVE_nbif_ebif_process_display_2
#define HAVE_nbif_ebif_process_display_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_process_display_2))
ASYM(nbif_ebif_process_display_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_process_display_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_process_display_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_process_display_2))
#endif

#ifndef HAVE_nbif_bump_reductions_1
#define HAVE_nbif_bump_reductions_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_bump_reductions_1))
ASYM(nbif_bump_reductions_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(bump_reductions_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_bump_reductions_1))
	TYPE_FUNCTION(ASYM(nbif_bump_reductions_1))
#endif

#ifndef HAVE_nbif_ebif_bump_reductions_1
#define HAVE_nbif_ebif_bump_reductions_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_bump_reductions_1))
ASYM(nbif_ebif_bump_reductions_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_bump_reductions_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_bump_reductions_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_bump_reductions_1))
#endif

#ifndef HAVE_nbif_math_cos_1
#define HAVE_nbif_math_cos_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_math_cos_1))
ASYM(nbif_math_cos_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(math_cos_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_math_cos_1))
	TYPE_FUNCTION(ASYM(nbif_math_cos_1))
#endif

#ifndef HAVE_nbif_ebif_math_cos_1
#define HAVE_nbif_ebif_math_cos_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_math_cos_1))
ASYM(nbif_ebif_math_cos_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_math_cos_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_math_cos_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_math_cos_1))
#endif

#ifndef HAVE_nbif_math_cosh_1
#define HAVE_nbif_math_cosh_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_math_cosh_1))
ASYM(nbif_math_cosh_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(math_cosh_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_math_cosh_1))
	TYPE_FUNCTION(ASYM(nbif_math_cosh_1))
#endif

#ifndef HAVE_nbif_ebif_math_cosh_1
#define HAVE_nbif_ebif_math_cosh_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_math_cosh_1))
ASYM(nbif_ebif_math_cosh_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_math_cosh_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_math_cosh_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_math_cosh_1))
#endif

#ifndef HAVE_nbif_math_sin_1
#define HAVE_nbif_math_sin_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_math_sin_1))
ASYM(nbif_math_sin_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(math_sin_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_math_sin_1))
	TYPE_FUNCTION(ASYM(nbif_math_sin_1))
#endif

#ifndef HAVE_nbif_ebif_math_sin_1
#define HAVE_nbif_ebif_math_sin_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_math_sin_1))
ASYM(nbif_ebif_math_sin_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_math_sin_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_math_sin_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_math_sin_1))
#endif

#ifndef HAVE_nbif_math_sinh_1
#define HAVE_nbif_math_sinh_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_math_sinh_1))
ASYM(nbif_math_sinh_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(math_sinh_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_math_sinh_1))
	TYPE_FUNCTION(ASYM(nbif_math_sinh_1))
#endif

#ifndef HAVE_nbif_ebif_math_sinh_1
#define HAVE_nbif_ebif_math_sinh_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_math_sinh_1))
ASYM(nbif_ebif_math_sinh_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_math_sinh_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_math_sinh_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_math_sinh_1))
#endif

#ifndef HAVE_nbif_math_tan_1
#define HAVE_nbif_math_tan_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_math_tan_1))
ASYM(nbif_math_tan_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(math_tan_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_math_tan_1))
	TYPE_FUNCTION(ASYM(nbif_math_tan_1))
#endif

#ifndef HAVE_nbif_ebif_math_tan_1
#define HAVE_nbif_ebif_math_tan_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_math_tan_1))
ASYM(nbif_ebif_math_tan_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_math_tan_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_math_tan_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_math_tan_1))
#endif

#ifndef HAVE_nbif_math_tanh_1
#define HAVE_nbif_math_tanh_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_math_tanh_1))
ASYM(nbif_math_tanh_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(math_tanh_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_math_tanh_1))
	TYPE_FUNCTION(ASYM(nbif_math_tanh_1))
#endif

#ifndef HAVE_nbif_ebif_math_tanh_1
#define HAVE_nbif_ebif_math_tanh_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_math_tanh_1))
ASYM(nbif_ebif_math_tanh_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_math_tanh_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_math_tanh_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_math_tanh_1))
#endif

#ifndef HAVE_nbif_math_acos_1
#define HAVE_nbif_math_acos_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_math_acos_1))
ASYM(nbif_math_acos_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(math_acos_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_math_acos_1))
	TYPE_FUNCTION(ASYM(nbif_math_acos_1))
#endif

#ifndef HAVE_nbif_ebif_math_acos_1
#define HAVE_nbif_ebif_math_acos_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_math_acos_1))
ASYM(nbif_ebif_math_acos_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_math_acos_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_math_acos_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_math_acos_1))
#endif

#ifndef HAVE_nbif_math_acosh_1
#define HAVE_nbif_math_acosh_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_math_acosh_1))
ASYM(nbif_math_acosh_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(math_acosh_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_math_acosh_1))
	TYPE_FUNCTION(ASYM(nbif_math_acosh_1))
#endif

#ifndef HAVE_nbif_ebif_math_acosh_1
#define HAVE_nbif_ebif_math_acosh_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_math_acosh_1))
ASYM(nbif_ebif_math_acosh_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_math_acosh_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_math_acosh_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_math_acosh_1))
#endif

#ifndef HAVE_nbif_math_asin_1
#define HAVE_nbif_math_asin_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_math_asin_1))
ASYM(nbif_math_asin_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(math_asin_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_math_asin_1))
	TYPE_FUNCTION(ASYM(nbif_math_asin_1))
#endif

#ifndef HAVE_nbif_ebif_math_asin_1
#define HAVE_nbif_ebif_math_asin_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_math_asin_1))
ASYM(nbif_ebif_math_asin_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_math_asin_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_math_asin_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_math_asin_1))
#endif

#ifndef HAVE_nbif_math_asinh_1
#define HAVE_nbif_math_asinh_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_math_asinh_1))
ASYM(nbif_math_asinh_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(math_asinh_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_math_asinh_1))
	TYPE_FUNCTION(ASYM(nbif_math_asinh_1))
#endif

#ifndef HAVE_nbif_ebif_math_asinh_1
#define HAVE_nbif_ebif_math_asinh_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_math_asinh_1))
ASYM(nbif_ebif_math_asinh_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_math_asinh_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_math_asinh_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_math_asinh_1))
#endif

#ifndef HAVE_nbif_math_atan_1
#define HAVE_nbif_math_atan_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_math_atan_1))
ASYM(nbif_math_atan_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(math_atan_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_math_atan_1))
	TYPE_FUNCTION(ASYM(nbif_math_atan_1))
#endif

#ifndef HAVE_nbif_ebif_math_atan_1
#define HAVE_nbif_ebif_math_atan_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_math_atan_1))
ASYM(nbif_ebif_math_atan_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_math_atan_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_math_atan_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_math_atan_1))
#endif

#ifndef HAVE_nbif_math_atanh_1
#define HAVE_nbif_math_atanh_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_math_atanh_1))
ASYM(nbif_math_atanh_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(math_atanh_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_math_atanh_1))
	TYPE_FUNCTION(ASYM(nbif_math_atanh_1))
#endif

#ifndef HAVE_nbif_ebif_math_atanh_1
#define HAVE_nbif_ebif_math_atanh_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_math_atanh_1))
ASYM(nbif_ebif_math_atanh_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_math_atanh_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_math_atanh_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_math_atanh_1))
#endif

#ifndef HAVE_nbif_math_erf_1
#define HAVE_nbif_math_erf_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_math_erf_1))
ASYM(nbif_math_erf_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(math_erf_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_math_erf_1))
	TYPE_FUNCTION(ASYM(nbif_math_erf_1))
#endif

#ifndef HAVE_nbif_ebif_math_erf_1
#define HAVE_nbif_ebif_math_erf_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_math_erf_1))
ASYM(nbif_ebif_math_erf_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_math_erf_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_math_erf_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_math_erf_1))
#endif

#ifndef HAVE_nbif_math_erfc_1
#define HAVE_nbif_math_erfc_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_math_erfc_1))
ASYM(nbif_math_erfc_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(math_erfc_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_math_erfc_1))
	TYPE_FUNCTION(ASYM(nbif_math_erfc_1))
#endif

#ifndef HAVE_nbif_ebif_math_erfc_1
#define HAVE_nbif_ebif_math_erfc_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_math_erfc_1))
ASYM(nbif_ebif_math_erfc_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_math_erfc_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_math_erfc_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_math_erfc_1))
#endif

#ifndef HAVE_nbif_math_exp_1
#define HAVE_nbif_math_exp_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_math_exp_1))
ASYM(nbif_math_exp_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(math_exp_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_math_exp_1))
	TYPE_FUNCTION(ASYM(nbif_math_exp_1))
#endif

#ifndef HAVE_nbif_ebif_math_exp_1
#define HAVE_nbif_ebif_math_exp_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_math_exp_1))
ASYM(nbif_ebif_math_exp_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_math_exp_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_math_exp_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_math_exp_1))
#endif

#ifndef HAVE_nbif_math_log_1
#define HAVE_nbif_math_log_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_math_log_1))
ASYM(nbif_math_log_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(math_log_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_math_log_1))
	TYPE_FUNCTION(ASYM(nbif_math_log_1))
#endif

#ifndef HAVE_nbif_ebif_math_log_1
#define HAVE_nbif_ebif_math_log_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_math_log_1))
ASYM(nbif_ebif_math_log_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_math_log_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_math_log_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_math_log_1))
#endif

#ifndef HAVE_nbif_math_log10_1
#define HAVE_nbif_math_log10_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_math_log10_1))
ASYM(nbif_math_log10_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(math_log10_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_math_log10_1))
	TYPE_FUNCTION(ASYM(nbif_math_log10_1))
#endif

#ifndef HAVE_nbif_ebif_math_log10_1
#define HAVE_nbif_ebif_math_log10_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_math_log10_1))
ASYM(nbif_ebif_math_log10_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_math_log10_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_math_log10_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_math_log10_1))
#endif

#ifndef HAVE_nbif_math_sqrt_1
#define HAVE_nbif_math_sqrt_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_math_sqrt_1))
ASYM(nbif_math_sqrt_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(math_sqrt_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_math_sqrt_1))
	TYPE_FUNCTION(ASYM(nbif_math_sqrt_1))
#endif

#ifndef HAVE_nbif_ebif_math_sqrt_1
#define HAVE_nbif_ebif_math_sqrt_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_math_sqrt_1))
ASYM(nbif_ebif_math_sqrt_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_math_sqrt_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_math_sqrt_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_math_sqrt_1))
#endif

#ifndef HAVE_nbif_math_atan2_2
#define HAVE_nbif_math_atan2_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_math_atan2_2))
ASYM(nbif_math_atan2_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(math_atan2_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_math_atan2_2))
	TYPE_FUNCTION(ASYM(nbif_math_atan2_2))
#endif

#ifndef HAVE_nbif_ebif_math_atan2_2
#define HAVE_nbif_ebif_math_atan2_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_math_atan2_2))
ASYM(nbif_ebif_math_atan2_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_math_atan2_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_math_atan2_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_math_atan2_2))
#endif

#ifndef HAVE_nbif_math_pow_2
#define HAVE_nbif_math_pow_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_math_pow_2))
ASYM(nbif_math_pow_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(math_pow_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_math_pow_2))
	TYPE_FUNCTION(ASYM(nbif_math_pow_2))
#endif

#ifndef HAVE_nbif_ebif_math_pow_2
#define HAVE_nbif_ebif_math_pow_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_math_pow_2))
ASYM(nbif_ebif_math_pow_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_math_pow_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_math_pow_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_math_pow_2))
#endif

#ifndef HAVE_nbif_start_timer_3
#define HAVE_nbif_start_timer_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_start_timer_3))
ASYM(nbif_start_timer_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(start_timer_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_start_timer_3))
	TYPE_FUNCTION(ASYM(nbif_start_timer_3))
#endif

#ifndef HAVE_nbif_ebif_start_timer_3
#define HAVE_nbif_ebif_start_timer_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_start_timer_3))
ASYM(nbif_ebif_start_timer_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(ebif_start_timer_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_ebif_start_timer_3))
	TYPE_FUNCTION(ASYM(nbif_ebif_start_timer_3))
#endif

#ifndef HAVE_nbif_send_after_3
#define HAVE_nbif_send_after_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_send_after_3))
ASYM(nbif_send_after_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(send_after_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_send_after_3))
	TYPE_FUNCTION(ASYM(nbif_send_after_3))
#endif

#ifndef HAVE_nbif_ebif_send_after_3
#define HAVE_nbif_ebif_send_after_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_send_after_3))
ASYM(nbif_ebif_send_after_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(ebif_send_after_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_ebif_send_after_3))
	TYPE_FUNCTION(ASYM(nbif_ebif_send_after_3))
#endif

#ifndef HAVE_nbif_cancel_timer_1
#define HAVE_nbif_cancel_timer_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_cancel_timer_1))
ASYM(nbif_cancel_timer_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(cancel_timer_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_cancel_timer_1))
	TYPE_FUNCTION(ASYM(nbif_cancel_timer_1))
#endif

#ifndef HAVE_nbif_ebif_cancel_timer_1
#define HAVE_nbif_ebif_cancel_timer_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_cancel_timer_1))
ASYM(nbif_ebif_cancel_timer_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_cancel_timer_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_cancel_timer_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_cancel_timer_1))
#endif

#ifndef HAVE_nbif_read_timer_1
#define HAVE_nbif_read_timer_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_read_timer_1))
ASYM(nbif_read_timer_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(read_timer_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_read_timer_1))
	TYPE_FUNCTION(ASYM(nbif_read_timer_1))
#endif

#ifndef HAVE_nbif_ebif_read_timer_1
#define HAVE_nbif_ebif_read_timer_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_read_timer_1))
ASYM(nbif_ebif_read_timer_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_read_timer_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_read_timer_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_read_timer_1))
#endif

#ifndef HAVE_nbif_make_tuple_2
#define HAVE_nbif_make_tuple_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_make_tuple_2))
ASYM(nbif_make_tuple_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(make_tuple_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_make_tuple_2))
	TYPE_FUNCTION(ASYM(nbif_make_tuple_2))
#endif

#ifndef HAVE_nbif_ebif_make_tuple_2
#define HAVE_nbif_ebif_make_tuple_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_make_tuple_2))
ASYM(nbif_ebif_make_tuple_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_make_tuple_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_make_tuple_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_make_tuple_2))
#endif

#ifndef HAVE_nbif_append_element_2
#define HAVE_nbif_append_element_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_append_element_2))
ASYM(nbif_append_element_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(append_element_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_append_element_2))
	TYPE_FUNCTION(ASYM(nbif_append_element_2))
#endif

#ifndef HAVE_nbif_ebif_append_element_2
#define HAVE_nbif_ebif_append_element_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_append_element_2))
ASYM(nbif_ebif_append_element_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_append_element_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_append_element_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_append_element_2))
#endif

#ifndef HAVE_nbif_make_tuple_3
#define HAVE_nbif_make_tuple_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_make_tuple_3))
ASYM(nbif_make_tuple_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(make_tuple_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_make_tuple_3))
	TYPE_FUNCTION(ASYM(nbif_make_tuple_3))
#endif

#ifndef HAVE_nbif_system_flag_2
#define HAVE_nbif_system_flag_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_system_flag_2))
ASYM(nbif_system_flag_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(system_flag_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_system_flag_2))
	TYPE_FUNCTION(ASYM(nbif_system_flag_2))
#endif

#ifndef HAVE_nbif_ebif_system_flag_2
#define HAVE_nbif_ebif_system_flag_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_system_flag_2))
ASYM(nbif_ebif_system_flag_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_system_flag_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_system_flag_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_system_flag_2))
#endif

#ifndef HAVE_nbif_system_info_1
#define HAVE_nbif_system_info_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_system_info_1))
ASYM(nbif_system_info_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(system_info_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_system_info_1))
	TYPE_FUNCTION(ASYM(nbif_system_info_1))
#endif

#ifndef HAVE_nbif_ebif_system_info_1
#define HAVE_nbif_ebif_system_info_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_system_info_1))
ASYM(nbif_ebif_system_info_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_system_info_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_system_info_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_system_info_1))
#endif

#ifndef HAVE_nbif_system_monitor_0
#define HAVE_nbif_system_monitor_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_system_monitor_0))
ASYM(nbif_system_monitor_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(system_monitor_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_system_monitor_0))
	TYPE_FUNCTION(ASYM(nbif_system_monitor_0))
#endif

#ifndef HAVE_nbif_ebif_system_monitor_0
#define HAVE_nbif_ebif_system_monitor_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_system_monitor_0))
ASYM(nbif_ebif_system_monitor_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(ebif_system_monitor_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_ebif_system_monitor_0))
	TYPE_FUNCTION(ASYM(nbif_ebif_system_monitor_0))
#endif

#ifndef HAVE_nbif_system_monitor_1
#define HAVE_nbif_system_monitor_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_system_monitor_1))
ASYM(nbif_system_monitor_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(system_monitor_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_system_monitor_1))
	TYPE_FUNCTION(ASYM(nbif_system_monitor_1))
#endif

#ifndef HAVE_nbif_ebif_system_monitor_1
#define HAVE_nbif_ebif_system_monitor_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_system_monitor_1))
ASYM(nbif_ebif_system_monitor_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_system_monitor_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_system_monitor_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_system_monitor_1))
#endif

#ifndef HAVE_nbif_system_monitor_2
#define HAVE_nbif_system_monitor_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_system_monitor_2))
ASYM(nbif_system_monitor_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(system_monitor_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_system_monitor_2))
	TYPE_FUNCTION(ASYM(nbif_system_monitor_2))
#endif

#ifndef HAVE_nbif_ebif_system_monitor_2
#define HAVE_nbif_ebif_system_monitor_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_system_monitor_2))
ASYM(nbif_ebif_system_monitor_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_system_monitor_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_system_monitor_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_system_monitor_2))
#endif

#ifndef HAVE_nbif_system_profile_2
#define HAVE_nbif_system_profile_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_system_profile_2))
ASYM(nbif_system_profile_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(system_profile_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_system_profile_2))
	TYPE_FUNCTION(ASYM(nbif_system_profile_2))
#endif

#ifndef HAVE_nbif_ebif_system_profile_2
#define HAVE_nbif_ebif_system_profile_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_system_profile_2))
ASYM(nbif_ebif_system_profile_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_system_profile_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_system_profile_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_system_profile_2))
#endif

#ifndef HAVE_nbif_system_profile_0
#define HAVE_nbif_system_profile_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_system_profile_0))
ASYM(nbif_system_profile_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(system_profile_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_system_profile_0))
	TYPE_FUNCTION(ASYM(nbif_system_profile_0))
#endif

#ifndef HAVE_nbif_ebif_system_profile_0
#define HAVE_nbif_ebif_system_profile_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_system_profile_0))
ASYM(nbif_ebif_system_profile_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(ebif_system_profile_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_ebif_system_profile_0))
	TYPE_FUNCTION(ASYM(nbif_ebif_system_profile_0))
#endif

#ifndef HAVE_nbif_ref_to_list_1
#define HAVE_nbif_ref_to_list_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ref_to_list_1))
ASYM(nbif_ref_to_list_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ref_to_list_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ref_to_list_1))
	TYPE_FUNCTION(ASYM(nbif_ref_to_list_1))
#endif

#ifndef HAVE_nbif_ebif_ref_to_string_1
#define HAVE_nbif_ebif_ref_to_string_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_ref_to_string_1))
ASYM(nbif_ebif_ref_to_string_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_ref_to_string_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_ref_to_string_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_ref_to_string_1))
#endif

#ifndef HAVE_nbif_port_to_list_1
#define HAVE_nbif_port_to_list_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_port_to_list_1))
ASYM(nbif_port_to_list_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(port_to_list_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_port_to_list_1))
	TYPE_FUNCTION(ASYM(nbif_port_to_list_1))
#endif

#ifndef HAVE_nbif_ebif_port_to_string_1
#define HAVE_nbif_ebif_port_to_string_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_port_to_string_1))
ASYM(nbif_ebif_port_to_string_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_port_to_string_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_port_to_string_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_port_to_string_1))
#endif

#ifndef HAVE_nbif_fun_to_list_1
#define HAVE_nbif_fun_to_list_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_fun_to_list_1))
ASYM(nbif_fun_to_list_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(fun_to_list_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_fun_to_list_1))
	TYPE_FUNCTION(ASYM(nbif_fun_to_list_1))
#endif

#ifndef HAVE_nbif_ebif_fun_to_string_1
#define HAVE_nbif_ebif_fun_to_string_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_fun_to_string_1))
ASYM(nbif_ebif_fun_to_string_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_fun_to_string_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_fun_to_string_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_fun_to_string_1))
#endif

#ifndef HAVE_nbif_monitor_2
#define HAVE_nbif_monitor_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_monitor_2))
ASYM(nbif_monitor_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(monitor_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_monitor_2))
	TYPE_FUNCTION(ASYM(nbif_monitor_2))
#endif

#ifndef HAVE_nbif_ebif_monitor_2
#define HAVE_nbif_ebif_monitor_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_monitor_2))
ASYM(nbif_ebif_monitor_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_monitor_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_monitor_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_monitor_2))
#endif

#ifndef HAVE_nbif_demonitor_1
#define HAVE_nbif_demonitor_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_demonitor_1))
ASYM(nbif_demonitor_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(demonitor_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_demonitor_1))
	TYPE_FUNCTION(ASYM(nbif_demonitor_1))
#endif

#ifndef HAVE_nbif_ebif_demonitor_1
#define HAVE_nbif_ebif_demonitor_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_demonitor_1))
ASYM(nbif_ebif_demonitor_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_demonitor_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_demonitor_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_demonitor_1))
#endif

#ifndef HAVE_nbif_demonitor_2
#define HAVE_nbif_demonitor_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_demonitor_2))
ASYM(nbif_demonitor_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(demonitor_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_demonitor_2))
	TYPE_FUNCTION(ASYM(nbif_demonitor_2))
#endif

#ifndef HAVE_nbif_ebif_demonitor_2
#define HAVE_nbif_ebif_demonitor_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_demonitor_2))
ASYM(nbif_ebif_demonitor_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_demonitor_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_demonitor_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_demonitor_2))
#endif

#ifndef HAVE_nbif_is_process_alive_1
#define HAVE_nbif_is_process_alive_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_is_process_alive_1))
ASYM(nbif_is_process_alive_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(is_process_alive_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_is_process_alive_1))
	TYPE_FUNCTION(ASYM(nbif_is_process_alive_1))
#endif

#ifndef HAVE_nbif_ebif_proc_is_alive_1
#define HAVE_nbif_ebif_proc_is_alive_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_proc_is_alive_1))
ASYM(nbif_ebif_proc_is_alive_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_proc_is_alive_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_proc_is_alive_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_proc_is_alive_1))
#endif

#ifndef HAVE_nbif_error_1
#define HAVE_nbif_error_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_error_1))
ASYM(nbif_error_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(error_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_error_1))
	TYPE_FUNCTION(ASYM(nbif_error_1))
#endif

#ifndef HAVE_nbif_ebif_error_1
#define HAVE_nbif_ebif_error_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_error_1))
ASYM(nbif_ebif_error_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_error_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_error_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_error_1))
#endif

#ifndef HAVE_nbif_error_2
#define HAVE_nbif_error_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_error_2))
ASYM(nbif_error_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(error_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_error_2))
	TYPE_FUNCTION(ASYM(nbif_error_2))
#endif

#ifndef HAVE_nbif_ebif_error_2
#define HAVE_nbif_ebif_error_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_error_2))
ASYM(nbif_ebif_error_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_error_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_error_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_error_2))
#endif

#ifndef HAVE_nbif_raise_3
#define HAVE_nbif_raise_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_raise_3))
ASYM(nbif_raise_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(raise_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_raise_3))
	TYPE_FUNCTION(ASYM(nbif_raise_3))
#endif

#ifndef HAVE_nbif_ebif_raise_3
#define HAVE_nbif_ebif_raise_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_raise_3))
ASYM(nbif_ebif_raise_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(ebif_raise_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_ebif_raise_3))
	TYPE_FUNCTION(ASYM(nbif_ebif_raise_3))
#endif

#ifndef HAVE_nbif_get_stacktrace_0
#define HAVE_nbif_get_stacktrace_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_get_stacktrace_0))
ASYM(nbif_get_stacktrace_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(get_stacktrace_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_get_stacktrace_0))
	TYPE_FUNCTION(ASYM(nbif_get_stacktrace_0))
#endif

#ifndef HAVE_nbif_ebif_get_stacktrace_0
#define HAVE_nbif_ebif_get_stacktrace_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_get_stacktrace_0))
ASYM(nbif_ebif_get_stacktrace_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(ebif_get_stacktrace_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_ebif_get_stacktrace_0))
	TYPE_FUNCTION(ASYM(nbif_ebif_get_stacktrace_0))
#endif

#ifndef HAVE_nbif_is_builtin_3
#define HAVE_nbif_is_builtin_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_is_builtin_3))
ASYM(nbif_is_builtin_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(is_builtin_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_is_builtin_3))
	TYPE_FUNCTION(ASYM(nbif_is_builtin_3))
#endif

#ifndef HAVE_nbif_ebif_is_builtin_3
#define HAVE_nbif_ebif_is_builtin_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_is_builtin_3))
ASYM(nbif_ebif_is_builtin_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(ebif_is_builtin_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_ebif_is_builtin_3))
	TYPE_FUNCTION(ASYM(nbif_ebif_is_builtin_3))
#endif

#ifndef HAVE_nbif_and_2
#define HAVE_nbif_and_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_and_2))
ASYM(nbif_and_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(and_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_and_2))
	TYPE_FUNCTION(ASYM(nbif_and_2))
#endif

#ifndef HAVE_nbif_ebif_and_2
#define HAVE_nbif_ebif_and_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_and_2))
ASYM(nbif_ebif_and_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_and_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_and_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_and_2))
#endif

#ifndef HAVE_nbif_or_2
#define HAVE_nbif_or_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_or_2))
ASYM(nbif_or_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(or_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_or_2))
	TYPE_FUNCTION(ASYM(nbif_or_2))
#endif

#ifndef HAVE_nbif_ebif_or_2
#define HAVE_nbif_ebif_or_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_or_2))
ASYM(nbif_ebif_or_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_or_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_or_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_or_2))
#endif

#ifndef HAVE_nbif_xor_2
#define HAVE_nbif_xor_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_xor_2))
ASYM(nbif_xor_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(xor_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_xor_2))
	TYPE_FUNCTION(ASYM(nbif_xor_2))
#endif

#ifndef HAVE_nbif_ebif_xor_2
#define HAVE_nbif_ebif_xor_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_xor_2))
ASYM(nbif_ebif_xor_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_xor_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_xor_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_xor_2))
#endif

#ifndef HAVE_nbif_not_1
#define HAVE_nbif_not_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_not_1))
ASYM(nbif_not_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(not_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_not_1))
	TYPE_FUNCTION(ASYM(nbif_not_1))
#endif

#ifndef HAVE_nbif_ebif_not_1
#define HAVE_nbif_ebif_not_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_not_1))
ASYM(nbif_ebif_not_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_not_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_not_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_not_1))
#endif

#ifndef HAVE_nbif_sgt_2
#define HAVE_nbif_sgt_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_sgt_2))
ASYM(nbif_sgt_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(sgt_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_sgt_2))
	TYPE_FUNCTION(ASYM(nbif_sgt_2))
#endif

#ifndef HAVE_nbif_ebif_gt_2
#define HAVE_nbif_ebif_gt_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_gt_2))
ASYM(nbif_ebif_gt_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_gt_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_gt_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_gt_2))
#endif

#ifndef HAVE_nbif_sge_2
#define HAVE_nbif_sge_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_sge_2))
ASYM(nbif_sge_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(sge_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_sge_2))
	TYPE_FUNCTION(ASYM(nbif_sge_2))
#endif

#ifndef HAVE_nbif_ebif_ge_2
#define HAVE_nbif_ebif_ge_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_ge_2))
ASYM(nbif_ebif_ge_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_ge_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_ge_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_ge_2))
#endif

#ifndef HAVE_nbif_slt_2
#define HAVE_nbif_slt_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_slt_2))
ASYM(nbif_slt_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(slt_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_slt_2))
	TYPE_FUNCTION(ASYM(nbif_slt_2))
#endif

#ifndef HAVE_nbif_ebif_lt_2
#define HAVE_nbif_ebif_lt_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_lt_2))
ASYM(nbif_ebif_lt_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_lt_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_lt_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_lt_2))
#endif

#ifndef HAVE_nbif_sle_2
#define HAVE_nbif_sle_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_sle_2))
ASYM(nbif_sle_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(sle_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_sle_2))
	TYPE_FUNCTION(ASYM(nbif_sle_2))
#endif

#ifndef HAVE_nbif_ebif_le_2
#define HAVE_nbif_ebif_le_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_le_2))
ASYM(nbif_ebif_le_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_le_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_le_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_le_2))
#endif

#ifndef HAVE_nbif_seq_2
#define HAVE_nbif_seq_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_seq_2))
ASYM(nbif_seq_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(seq_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_seq_2))
	TYPE_FUNCTION(ASYM(nbif_seq_2))
#endif

#ifndef HAVE_nbif_ebif_eq_2
#define HAVE_nbif_ebif_eq_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_eq_2))
ASYM(nbif_ebif_eq_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_eq_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_eq_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_eq_2))
#endif

#ifndef HAVE_nbif_seqeq_2
#define HAVE_nbif_seqeq_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_seqeq_2))
ASYM(nbif_seqeq_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(seqeq_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_seqeq_2))
	TYPE_FUNCTION(ASYM(nbif_seqeq_2))
#endif

#ifndef HAVE_nbif_ebif_areq_2
#define HAVE_nbif_ebif_areq_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_areq_2))
ASYM(nbif_ebif_areq_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_areq_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_areq_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_areq_2))
#endif

#ifndef HAVE_nbif_sneq_2
#define HAVE_nbif_sneq_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_sneq_2))
ASYM(nbif_sneq_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(sneq_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_sneq_2))
	TYPE_FUNCTION(ASYM(nbif_sneq_2))
#endif

#ifndef HAVE_nbif_ebif_neq_2
#define HAVE_nbif_ebif_neq_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_neq_2))
ASYM(nbif_ebif_neq_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_neq_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_neq_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_neq_2))
#endif

#ifndef HAVE_nbif_sneqeq_2
#define HAVE_nbif_sneqeq_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_sneqeq_2))
ASYM(nbif_sneqeq_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(sneqeq_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_sneqeq_2))
	TYPE_FUNCTION(ASYM(nbif_sneqeq_2))
#endif

#ifndef HAVE_nbif_ebif_nareq_2
#define HAVE_nbif_ebif_nareq_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_nareq_2))
ASYM(nbif_ebif_nareq_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_nareq_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_nareq_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_nareq_2))
#endif

#ifndef HAVE_nbif_splus_2
#define HAVE_nbif_splus_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_splus_2))
ASYM(nbif_splus_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(splus_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_splus_2))
	TYPE_FUNCTION(ASYM(nbif_splus_2))
#endif

#ifndef HAVE_nbif_ebif_plus_2
#define HAVE_nbif_ebif_plus_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_plus_2))
ASYM(nbif_ebif_plus_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_plus_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_plus_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_plus_2))
#endif

#ifndef HAVE_nbif_sminus_2
#define HAVE_nbif_sminus_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_sminus_2))
ASYM(nbif_sminus_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(sminus_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_sminus_2))
	TYPE_FUNCTION(ASYM(nbif_sminus_2))
#endif

#ifndef HAVE_nbif_ebif_minus_2
#define HAVE_nbif_ebif_minus_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_minus_2))
ASYM(nbif_ebif_minus_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_minus_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_minus_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_minus_2))
#endif

#ifndef HAVE_nbif_stimes_2
#define HAVE_nbif_stimes_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_stimes_2))
ASYM(nbif_stimes_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(stimes_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_stimes_2))
	TYPE_FUNCTION(ASYM(nbif_stimes_2))
#endif

#ifndef HAVE_nbif_ebif_multiply_2
#define HAVE_nbif_ebif_multiply_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_multiply_2))
ASYM(nbif_ebif_multiply_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_multiply_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_multiply_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_multiply_2))
#endif

#ifndef HAVE_nbif_div_2
#define HAVE_nbif_div_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_div_2))
ASYM(nbif_div_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(div_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_div_2))
	TYPE_FUNCTION(ASYM(nbif_div_2))
#endif

#ifndef HAVE_nbif_ebif_divide_2
#define HAVE_nbif_ebif_divide_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_divide_2))
ASYM(nbif_ebif_divide_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_divide_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_divide_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_divide_2))
#endif

#ifndef HAVE_nbif_intdiv_2
#define HAVE_nbif_intdiv_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_intdiv_2))
ASYM(nbif_intdiv_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(intdiv_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_intdiv_2))
	TYPE_FUNCTION(ASYM(nbif_intdiv_2))
#endif

#ifndef HAVE_nbif_ebif_intdiv_2
#define HAVE_nbif_ebif_intdiv_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_intdiv_2))
ASYM(nbif_ebif_intdiv_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_intdiv_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_intdiv_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_intdiv_2))
#endif

#ifndef HAVE_nbif_rem_2
#define HAVE_nbif_rem_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_rem_2))
ASYM(nbif_rem_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(rem_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_rem_2))
	TYPE_FUNCTION(ASYM(nbif_rem_2))
#endif

#ifndef HAVE_nbif_ebif_rem_2
#define HAVE_nbif_ebif_rem_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_rem_2))
ASYM(nbif_ebif_rem_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_rem_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_rem_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_rem_2))
#endif

#ifndef HAVE_nbif_bor_2
#define HAVE_nbif_bor_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_bor_2))
ASYM(nbif_bor_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(bor_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_bor_2))
	TYPE_FUNCTION(ASYM(nbif_bor_2))
#endif

#ifndef HAVE_nbif_ebif_bor_2
#define HAVE_nbif_ebif_bor_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_bor_2))
ASYM(nbif_ebif_bor_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_bor_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_bor_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_bor_2))
#endif

#ifndef HAVE_nbif_band_2
#define HAVE_nbif_band_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_band_2))
ASYM(nbif_band_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(band_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_band_2))
	TYPE_FUNCTION(ASYM(nbif_band_2))
#endif

#ifndef HAVE_nbif_ebif_band_2
#define HAVE_nbif_ebif_band_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_band_2))
ASYM(nbif_ebif_band_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_band_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_band_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_band_2))
#endif

#ifndef HAVE_nbif_bxor_2
#define HAVE_nbif_bxor_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_bxor_2))
ASYM(nbif_bxor_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(bxor_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_bxor_2))
	TYPE_FUNCTION(ASYM(nbif_bxor_2))
#endif

#ifndef HAVE_nbif_ebif_bxor_2
#define HAVE_nbif_ebif_bxor_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_bxor_2))
ASYM(nbif_ebif_bxor_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_bxor_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_bxor_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_bxor_2))
#endif

#ifndef HAVE_nbif_bsl_2
#define HAVE_nbif_bsl_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_bsl_2))
ASYM(nbif_bsl_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(bsl_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_bsl_2))
	TYPE_FUNCTION(ASYM(nbif_bsl_2))
#endif

#ifndef HAVE_nbif_ebif_bsl_2
#define HAVE_nbif_ebif_bsl_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_bsl_2))
ASYM(nbif_ebif_bsl_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_bsl_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_bsl_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_bsl_2))
#endif

#ifndef HAVE_nbif_bsr_2
#define HAVE_nbif_bsr_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_bsr_2))
ASYM(nbif_bsr_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(bsr_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_bsr_2))
	TYPE_FUNCTION(ASYM(nbif_bsr_2))
#endif

#ifndef HAVE_nbif_ebif_bsr_2
#define HAVE_nbif_ebif_bsr_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_bsr_2))
ASYM(nbif_ebif_bsr_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_bsr_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_bsr_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_bsr_2))
#endif

#ifndef HAVE_nbif_bnot_1
#define HAVE_nbif_bnot_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_bnot_1))
ASYM(nbif_bnot_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(bnot_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_bnot_1))
	TYPE_FUNCTION(ASYM(nbif_bnot_1))
#endif

#ifndef HAVE_nbif_ebif_bnot_1
#define HAVE_nbif_ebif_bnot_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_bnot_1))
ASYM(nbif_ebif_bnot_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_bnot_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_bnot_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_bnot_1))
#endif

#ifndef HAVE_nbif_sminus_1
#define HAVE_nbif_sminus_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_sminus_1))
ASYM(nbif_sminus_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(sminus_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_sminus_1))
	TYPE_FUNCTION(ASYM(nbif_sminus_1))
#endif

#ifndef HAVE_nbif_ebif_minus_1
#define HAVE_nbif_ebif_minus_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_minus_1))
ASYM(nbif_ebif_minus_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_minus_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_minus_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_minus_1))
#endif

#ifndef HAVE_nbif_splus_1
#define HAVE_nbif_splus_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_splus_1))
ASYM(nbif_splus_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(splus_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_splus_1))
	TYPE_FUNCTION(ASYM(nbif_splus_1))
#endif

#ifndef HAVE_nbif_ebif_plus_1
#define HAVE_nbif_ebif_plus_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_plus_1))
ASYM(nbif_ebif_plus_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_plus_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_plus_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_plus_1))
#endif

#ifndef HAVE_nbif_ebif_bang_2
#define HAVE_nbif_ebif_bang_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_bang_2))
ASYM(nbif_ebif_bang_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_bang_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_bang_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_bang_2))
#endif

#ifndef HAVE_nbif_ebif_send_2
#define HAVE_nbif_ebif_send_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_send_2))
ASYM(nbif_ebif_send_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_send_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_send_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_send_2))
#endif

#ifndef HAVE_nbif_send_2
#define HAVE_nbif_send_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_send_2))
ASYM(nbif_send_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(send_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_send_2))
	TYPE_FUNCTION(ASYM(nbif_send_2))
#endif

#ifndef HAVE_nbif_ebif_send_3
#define HAVE_nbif_ebif_send_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_send_3))
ASYM(nbif_ebif_send_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(ebif_send_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_ebif_send_3))
	TYPE_FUNCTION(ASYM(nbif_ebif_send_3))
#endif

#ifndef HAVE_nbif_send_3
#define HAVE_nbif_send_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_send_3))
ASYM(nbif_send_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(send_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_send_3))
	TYPE_FUNCTION(ASYM(nbif_send_3))
#endif

#ifndef HAVE_nbif_ebif_plusplus_2
#define HAVE_nbif_ebif_plusplus_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_plusplus_2))
ASYM(nbif_ebif_plusplus_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_plusplus_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_plusplus_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_plusplus_2))
#endif

#ifndef HAVE_nbif_ebif_append_2
#define HAVE_nbif_ebif_append_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_append_2))
ASYM(nbif_ebif_append_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_append_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_append_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_append_2))
#endif

#ifndef HAVE_nbif_append_2
#define HAVE_nbif_append_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_append_2))
ASYM(nbif_append_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(append_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_append_2))
	TYPE_FUNCTION(ASYM(nbif_append_2))
#endif

#ifndef HAVE_nbif_ebif_minusminus_2
#define HAVE_nbif_ebif_minusminus_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_minusminus_2))
ASYM(nbif_ebif_minusminus_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_minusminus_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_minusminus_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_minusminus_2))
#endif

#ifndef HAVE_nbif_ebif_list_subtract_2
#define HAVE_nbif_ebif_list_subtract_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_list_subtract_2))
ASYM(nbif_ebif_list_subtract_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_list_subtract_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_list_subtract_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_list_subtract_2))
#endif

#ifndef HAVE_nbif_subtract_2
#define HAVE_nbif_subtract_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_subtract_2))
ASYM(nbif_subtract_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(subtract_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_subtract_2))
	TYPE_FUNCTION(ASYM(nbif_subtract_2))
#endif

#ifndef HAVE_nbif_is_atom_1
#define HAVE_nbif_is_atom_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_is_atom_1))
ASYM(nbif_is_atom_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(is_atom_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_is_atom_1))
	TYPE_FUNCTION(ASYM(nbif_is_atom_1))
#endif

#ifndef HAVE_nbif_ebif_is_atom_1
#define HAVE_nbif_ebif_is_atom_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_is_atom_1))
ASYM(nbif_ebif_is_atom_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_is_atom_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_is_atom_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_is_atom_1))
#endif

#ifndef HAVE_nbif_is_list_1
#define HAVE_nbif_is_list_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_is_list_1))
ASYM(nbif_is_list_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(is_list_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_is_list_1))
	TYPE_FUNCTION(ASYM(nbif_is_list_1))
#endif

#ifndef HAVE_nbif_ebif_is_list_1
#define HAVE_nbif_ebif_is_list_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_is_list_1))
ASYM(nbif_ebif_is_list_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_is_list_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_is_list_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_is_list_1))
#endif

#ifndef HAVE_nbif_is_tuple_1
#define HAVE_nbif_is_tuple_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_is_tuple_1))
ASYM(nbif_is_tuple_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(is_tuple_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_is_tuple_1))
	TYPE_FUNCTION(ASYM(nbif_is_tuple_1))
#endif

#ifndef HAVE_nbif_ebif_is_tuple_1
#define HAVE_nbif_ebif_is_tuple_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_is_tuple_1))
ASYM(nbif_ebif_is_tuple_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_is_tuple_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_is_tuple_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_is_tuple_1))
#endif

#ifndef HAVE_nbif_is_float_1
#define HAVE_nbif_is_float_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_is_float_1))
ASYM(nbif_is_float_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(is_float_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_is_float_1))
	TYPE_FUNCTION(ASYM(nbif_is_float_1))
#endif

#ifndef HAVE_nbif_ebif_is_float_1
#define HAVE_nbif_ebif_is_float_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_is_float_1))
ASYM(nbif_ebif_is_float_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_is_float_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_is_float_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_is_float_1))
#endif

#ifndef HAVE_nbif_is_integer_1
#define HAVE_nbif_is_integer_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_is_integer_1))
ASYM(nbif_is_integer_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(is_integer_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_is_integer_1))
	TYPE_FUNCTION(ASYM(nbif_is_integer_1))
#endif

#ifndef HAVE_nbif_ebif_is_integer_1
#define HAVE_nbif_ebif_is_integer_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_is_integer_1))
ASYM(nbif_ebif_is_integer_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_is_integer_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_is_integer_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_is_integer_1))
#endif

#ifndef HAVE_nbif_is_number_1
#define HAVE_nbif_is_number_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_is_number_1))
ASYM(nbif_is_number_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(is_number_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_is_number_1))
	TYPE_FUNCTION(ASYM(nbif_is_number_1))
#endif

#ifndef HAVE_nbif_ebif_is_number_1
#define HAVE_nbif_ebif_is_number_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_is_number_1))
ASYM(nbif_ebif_is_number_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_is_number_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_is_number_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_is_number_1))
#endif

#ifndef HAVE_nbif_is_pid_1
#define HAVE_nbif_is_pid_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_is_pid_1))
ASYM(nbif_is_pid_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(is_pid_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_is_pid_1))
	TYPE_FUNCTION(ASYM(nbif_is_pid_1))
#endif

#ifndef HAVE_nbif_ebif_is_pid_1
#define HAVE_nbif_ebif_is_pid_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_is_pid_1))
ASYM(nbif_ebif_is_pid_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_is_pid_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_is_pid_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_is_pid_1))
#endif

#ifndef HAVE_nbif_is_port_1
#define HAVE_nbif_is_port_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_is_port_1))
ASYM(nbif_is_port_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(is_port_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_is_port_1))
	TYPE_FUNCTION(ASYM(nbif_is_port_1))
#endif

#ifndef HAVE_nbif_ebif_is_port_1
#define HAVE_nbif_ebif_is_port_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_is_port_1))
ASYM(nbif_ebif_is_port_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_is_port_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_is_port_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_is_port_1))
#endif

#ifndef HAVE_nbif_is_reference_1
#define HAVE_nbif_is_reference_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_is_reference_1))
ASYM(nbif_is_reference_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(is_reference_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_is_reference_1))
	TYPE_FUNCTION(ASYM(nbif_is_reference_1))
#endif

#ifndef HAVE_nbif_ebif_is_reference_1
#define HAVE_nbif_ebif_is_reference_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_is_reference_1))
ASYM(nbif_ebif_is_reference_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_is_reference_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_is_reference_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_is_reference_1))
#endif

#ifndef HAVE_nbif_is_binary_1
#define HAVE_nbif_is_binary_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_is_binary_1))
ASYM(nbif_is_binary_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(is_binary_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_is_binary_1))
	TYPE_FUNCTION(ASYM(nbif_is_binary_1))
#endif

#ifndef HAVE_nbif_ebif_is_binary_1
#define HAVE_nbif_ebif_is_binary_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_is_binary_1))
ASYM(nbif_ebif_is_binary_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_is_binary_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_is_binary_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_is_binary_1))
#endif

#ifndef HAVE_nbif_is_function_1
#define HAVE_nbif_is_function_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_is_function_1))
ASYM(nbif_is_function_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(is_function_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_is_function_1))
	TYPE_FUNCTION(ASYM(nbif_is_function_1))
#endif

#ifndef HAVE_nbif_ebif_is_function_1
#define HAVE_nbif_ebif_is_function_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_is_function_1))
ASYM(nbif_ebif_is_function_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_is_function_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_is_function_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_is_function_1))
#endif

#ifndef HAVE_nbif_is_function_2
#define HAVE_nbif_is_function_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_is_function_2))
ASYM(nbif_is_function_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(is_function_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_is_function_2))
	TYPE_FUNCTION(ASYM(nbif_is_function_2))
#endif

#ifndef HAVE_nbif_ebif_is_function_2
#define HAVE_nbif_ebif_is_function_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_is_function_2))
ASYM(nbif_ebif_is_function_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_is_function_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_is_function_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_is_function_2))
#endif

#ifndef HAVE_nbif_is_record_2
#define HAVE_nbif_is_record_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_is_record_2))
ASYM(nbif_is_record_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(is_record_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_is_record_2))
	TYPE_FUNCTION(ASYM(nbif_is_record_2))
#endif

#ifndef HAVE_nbif_ebif_is_record_2
#define HAVE_nbif_ebif_is_record_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_is_record_2))
ASYM(nbif_ebif_is_record_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_is_record_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_is_record_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_is_record_2))
#endif

#ifndef HAVE_nbif_is_record_3
#define HAVE_nbif_is_record_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_is_record_3))
ASYM(nbif_is_record_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(is_record_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_is_record_3))
	TYPE_FUNCTION(ASYM(nbif_is_record_3))
#endif

#ifndef HAVE_nbif_ebif_is_record_3
#define HAVE_nbif_ebif_is_record_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_is_record_3))
ASYM(nbif_ebif_is_record_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(ebif_is_record_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_ebif_is_record_3))
	TYPE_FUNCTION(ASYM(nbif_ebif_is_record_3))
#endif

#ifndef HAVE_nbif_match_spec_test_3
#define HAVE_nbif_match_spec_test_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_match_spec_test_3))
ASYM(nbif_match_spec_test_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(match_spec_test_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_match_spec_test_3))
	TYPE_FUNCTION(ASYM(nbif_match_spec_test_3))
#endif

#ifndef HAVE_nbif_ets_all_0
#define HAVE_nbif_ets_all_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ets_all_0))
ASYM(nbif_ets_all_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(ets_all_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_ets_all_0))
	TYPE_FUNCTION(ASYM(nbif_ets_all_0))
#endif

#ifndef HAVE_nbif_ebif_ets_all_0
#define HAVE_nbif_ebif_ets_all_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_ets_all_0))
ASYM(nbif_ebif_ets_all_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(ebif_ets_all_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_ebif_ets_all_0))
	TYPE_FUNCTION(ASYM(nbif_ebif_ets_all_0))
#endif

#ifndef HAVE_nbif_ets_new_2
#define HAVE_nbif_ets_new_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ets_new_2))
ASYM(nbif_ets_new_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ets_new_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ets_new_2))
	TYPE_FUNCTION(ASYM(nbif_ets_new_2))
#endif

#ifndef HAVE_nbif_ebif_ets_new_2
#define HAVE_nbif_ebif_ets_new_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_ets_new_2))
ASYM(nbif_ebif_ets_new_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_ets_new_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_ets_new_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_ets_new_2))
#endif

#ifndef HAVE_nbif_ets_delete_1
#define HAVE_nbif_ets_delete_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ets_delete_1))
ASYM(nbif_ets_delete_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ets_delete_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ets_delete_1))
	TYPE_FUNCTION(ASYM(nbif_ets_delete_1))
#endif

#ifndef HAVE_nbif_ebif_ets_delete_1
#define HAVE_nbif_ebif_ets_delete_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_ets_delete_1))
ASYM(nbif_ebif_ets_delete_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_ets_delete_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_ets_delete_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_ets_delete_1))
#endif

#ifndef HAVE_nbif_ets_delete_2
#define HAVE_nbif_ets_delete_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ets_delete_2))
ASYM(nbif_ets_delete_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ets_delete_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ets_delete_2))
	TYPE_FUNCTION(ASYM(nbif_ets_delete_2))
#endif

#ifndef HAVE_nbif_ebif_ets_delete_2
#define HAVE_nbif_ebif_ets_delete_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_ets_delete_2))
ASYM(nbif_ebif_ets_delete_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_ets_delete_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_ets_delete_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_ets_delete_2))
#endif

#ifndef HAVE_nbif_ets_delete_all_objects_1
#define HAVE_nbif_ets_delete_all_objects_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ets_delete_all_objects_1))
ASYM(nbif_ets_delete_all_objects_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ets_delete_all_objects_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ets_delete_all_objects_1))
	TYPE_FUNCTION(ASYM(nbif_ets_delete_all_objects_1))
#endif

#ifndef HAVE_nbif_ebif_ets_delete_all_objects_1
#define HAVE_nbif_ebif_ets_delete_all_objects_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_ets_delete_all_objects_1))
ASYM(nbif_ebif_ets_delete_all_objects_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_ets_delete_all_objects_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_ets_delete_all_objects_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_ets_delete_all_objects_1))
#endif

#ifndef HAVE_nbif_ets_delete_object_2
#define HAVE_nbif_ets_delete_object_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ets_delete_object_2))
ASYM(nbif_ets_delete_object_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ets_delete_object_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ets_delete_object_2))
	TYPE_FUNCTION(ASYM(nbif_ets_delete_object_2))
#endif

#ifndef HAVE_nbif_ebif_ets_delete_object_2
#define HAVE_nbif_ebif_ets_delete_object_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_ets_delete_object_2))
ASYM(nbif_ebif_ets_delete_object_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_ets_delete_object_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_ets_delete_object_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_ets_delete_object_2))
#endif

#ifndef HAVE_nbif_ets_first_1
#define HAVE_nbif_ets_first_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ets_first_1))
ASYM(nbif_ets_first_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ets_first_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ets_first_1))
	TYPE_FUNCTION(ASYM(nbif_ets_first_1))
#endif

#ifndef HAVE_nbif_ebif_ets_first_1
#define HAVE_nbif_ebif_ets_first_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_ets_first_1))
ASYM(nbif_ebif_ets_first_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_ets_first_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_ets_first_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_ets_first_1))
#endif

#ifndef HAVE_nbif_ets_is_compiled_ms_1
#define HAVE_nbif_ets_is_compiled_ms_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ets_is_compiled_ms_1))
ASYM(nbif_ets_is_compiled_ms_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ets_is_compiled_ms_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ets_is_compiled_ms_1))
	TYPE_FUNCTION(ASYM(nbif_ets_is_compiled_ms_1))
#endif

#ifndef HAVE_nbif_ebif_ets_is_compiled_ms_1
#define HAVE_nbif_ebif_ets_is_compiled_ms_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_ets_is_compiled_ms_1))
ASYM(nbif_ebif_ets_is_compiled_ms_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_ets_is_compiled_ms_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_ets_is_compiled_ms_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_ets_is_compiled_ms_1))
#endif

#ifndef HAVE_nbif_ets_lookup_2
#define HAVE_nbif_ets_lookup_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ets_lookup_2))
ASYM(nbif_ets_lookup_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ets_lookup_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ets_lookup_2))
	TYPE_FUNCTION(ASYM(nbif_ets_lookup_2))
#endif

#ifndef HAVE_nbif_ebif_ets_lookup_2
#define HAVE_nbif_ebif_ets_lookup_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_ets_lookup_2))
ASYM(nbif_ebif_ets_lookup_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_ets_lookup_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_ets_lookup_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_ets_lookup_2))
#endif

#ifndef HAVE_nbif_ets_lookup_element_3
#define HAVE_nbif_ets_lookup_element_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ets_lookup_element_3))
ASYM(nbif_ets_lookup_element_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(ets_lookup_element_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_ets_lookup_element_3))
	TYPE_FUNCTION(ASYM(nbif_ets_lookup_element_3))
#endif

#ifndef HAVE_nbif_ebif_ets_lookup_element_3
#define HAVE_nbif_ebif_ets_lookup_element_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_ets_lookup_element_3))
ASYM(nbif_ebif_ets_lookup_element_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(ebif_ets_lookup_element_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_ebif_ets_lookup_element_3))
	TYPE_FUNCTION(ASYM(nbif_ebif_ets_lookup_element_3))
#endif

#ifndef HAVE_nbif_ets_info_1
#define HAVE_nbif_ets_info_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ets_info_1))
ASYM(nbif_ets_info_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ets_info_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ets_info_1))
	TYPE_FUNCTION(ASYM(nbif_ets_info_1))
#endif

#ifndef HAVE_nbif_ebif_ets_info_1
#define HAVE_nbif_ebif_ets_info_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_ets_info_1))
ASYM(nbif_ebif_ets_info_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_ets_info_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_ets_info_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_ets_info_1))
#endif

#ifndef HAVE_nbif_ets_info_2
#define HAVE_nbif_ets_info_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ets_info_2))
ASYM(nbif_ets_info_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ets_info_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ets_info_2))
	TYPE_FUNCTION(ASYM(nbif_ets_info_2))
#endif

#ifndef HAVE_nbif_ebif_ets_info_2
#define HAVE_nbif_ebif_ets_info_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_ets_info_2))
ASYM(nbif_ebif_ets_info_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_ets_info_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_ets_info_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_ets_info_2))
#endif

#ifndef HAVE_nbif_ets_last_1
#define HAVE_nbif_ets_last_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ets_last_1))
ASYM(nbif_ets_last_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ets_last_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ets_last_1))
	TYPE_FUNCTION(ASYM(nbif_ets_last_1))
#endif

#ifndef HAVE_nbif_ebif_ets_last_1
#define HAVE_nbif_ebif_ets_last_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_ets_last_1))
ASYM(nbif_ebif_ets_last_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_ets_last_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_ets_last_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_ets_last_1))
#endif

#ifndef HAVE_nbif_ets_match_1
#define HAVE_nbif_ets_match_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ets_match_1))
ASYM(nbif_ets_match_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ets_match_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ets_match_1))
	TYPE_FUNCTION(ASYM(nbif_ets_match_1))
#endif

#ifndef HAVE_nbif_ebif_ets_match_1
#define HAVE_nbif_ebif_ets_match_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_ets_match_1))
ASYM(nbif_ebif_ets_match_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_ets_match_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_ets_match_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_ets_match_1))
#endif

#ifndef HAVE_nbif_ets_match_2
#define HAVE_nbif_ets_match_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ets_match_2))
ASYM(nbif_ets_match_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ets_match_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ets_match_2))
	TYPE_FUNCTION(ASYM(nbif_ets_match_2))
#endif

#ifndef HAVE_nbif_ebif_ets_match_2
#define HAVE_nbif_ebif_ets_match_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_ets_match_2))
ASYM(nbif_ebif_ets_match_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_ets_match_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_ets_match_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_ets_match_2))
#endif

#ifndef HAVE_nbif_ets_match_3
#define HAVE_nbif_ets_match_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ets_match_3))
ASYM(nbif_ets_match_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(ets_match_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_ets_match_3))
	TYPE_FUNCTION(ASYM(nbif_ets_match_3))
#endif

#ifndef HAVE_nbif_ebif_ets_match_3
#define HAVE_nbif_ebif_ets_match_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_ets_match_3))
ASYM(nbif_ebif_ets_match_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(ebif_ets_match_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_ebif_ets_match_3))
	TYPE_FUNCTION(ASYM(nbif_ebif_ets_match_3))
#endif

#ifndef HAVE_nbif_ets_match_object_1
#define HAVE_nbif_ets_match_object_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ets_match_object_1))
ASYM(nbif_ets_match_object_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ets_match_object_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ets_match_object_1))
	TYPE_FUNCTION(ASYM(nbif_ets_match_object_1))
#endif

#ifndef HAVE_nbif_ebif_ets_match_object_1
#define HAVE_nbif_ebif_ets_match_object_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_ets_match_object_1))
ASYM(nbif_ebif_ets_match_object_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_ets_match_object_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_ets_match_object_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_ets_match_object_1))
#endif

#ifndef HAVE_nbif_ets_match_object_2
#define HAVE_nbif_ets_match_object_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ets_match_object_2))
ASYM(nbif_ets_match_object_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ets_match_object_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ets_match_object_2))
	TYPE_FUNCTION(ASYM(nbif_ets_match_object_2))
#endif

#ifndef HAVE_nbif_ebif_ets_match_object_2
#define HAVE_nbif_ebif_ets_match_object_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_ets_match_object_2))
ASYM(nbif_ebif_ets_match_object_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_ets_match_object_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_ets_match_object_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_ets_match_object_2))
#endif

#ifndef HAVE_nbif_ets_match_object_3
#define HAVE_nbif_ets_match_object_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ets_match_object_3))
ASYM(nbif_ets_match_object_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(ets_match_object_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_ets_match_object_3))
	TYPE_FUNCTION(ASYM(nbif_ets_match_object_3))
#endif

#ifndef HAVE_nbif_ebif_ets_match_object_3
#define HAVE_nbif_ebif_ets_match_object_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_ets_match_object_3))
ASYM(nbif_ebif_ets_match_object_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(ebif_ets_match_object_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_ebif_ets_match_object_3))
	TYPE_FUNCTION(ASYM(nbif_ebif_ets_match_object_3))
#endif

#ifndef HAVE_nbif_ets_member_2
#define HAVE_nbif_ets_member_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ets_member_2))
ASYM(nbif_ets_member_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ets_member_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ets_member_2))
	TYPE_FUNCTION(ASYM(nbif_ets_member_2))
#endif

#ifndef HAVE_nbif_ebif_ets_member_2
#define HAVE_nbif_ebif_ets_member_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_ets_member_2))
ASYM(nbif_ebif_ets_member_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_ets_member_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_ets_member_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_ets_member_2))
#endif

#ifndef HAVE_nbif_ets_next_2
#define HAVE_nbif_ets_next_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ets_next_2))
ASYM(nbif_ets_next_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ets_next_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ets_next_2))
	TYPE_FUNCTION(ASYM(nbif_ets_next_2))
#endif

#ifndef HAVE_nbif_ebif_ets_next_2
#define HAVE_nbif_ebif_ets_next_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_ets_next_2))
ASYM(nbif_ebif_ets_next_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_ets_next_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_ets_next_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_ets_next_2))
#endif

#ifndef HAVE_nbif_ets_prev_2
#define HAVE_nbif_ets_prev_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ets_prev_2))
ASYM(nbif_ets_prev_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ets_prev_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ets_prev_2))
	TYPE_FUNCTION(ASYM(nbif_ets_prev_2))
#endif

#ifndef HAVE_nbif_ebif_ets_prev_2
#define HAVE_nbif_ebif_ets_prev_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_ets_prev_2))
ASYM(nbif_ebif_ets_prev_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_ets_prev_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_ets_prev_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_ets_prev_2))
#endif

#ifndef HAVE_nbif_ets_insert_2
#define HAVE_nbif_ets_insert_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ets_insert_2))
ASYM(nbif_ets_insert_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ets_insert_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ets_insert_2))
	TYPE_FUNCTION(ASYM(nbif_ets_insert_2))
#endif

#ifndef HAVE_nbif_ebif_ets_insert_2
#define HAVE_nbif_ebif_ets_insert_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_ets_insert_2))
ASYM(nbif_ebif_ets_insert_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_ets_insert_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_ets_insert_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_ets_insert_2))
#endif

#ifndef HAVE_nbif_ets_insert_new_2
#define HAVE_nbif_ets_insert_new_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ets_insert_new_2))
ASYM(nbif_ets_insert_new_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ets_insert_new_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ets_insert_new_2))
	TYPE_FUNCTION(ASYM(nbif_ets_insert_new_2))
#endif

#ifndef HAVE_nbif_ebif_ets_insert_new_2
#define HAVE_nbif_ebif_ets_insert_new_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_ets_insert_new_2))
ASYM(nbif_ebif_ets_insert_new_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_ets_insert_new_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_ets_insert_new_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_ets_insert_new_2))
#endif

#ifndef HAVE_nbif_ets_rename_2
#define HAVE_nbif_ets_rename_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ets_rename_2))
ASYM(nbif_ets_rename_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ets_rename_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ets_rename_2))
	TYPE_FUNCTION(ASYM(nbif_ets_rename_2))
#endif

#ifndef HAVE_nbif_ebif_ets_rename_2
#define HAVE_nbif_ebif_ets_rename_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_ets_rename_2))
ASYM(nbif_ebif_ets_rename_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_ets_rename_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_ets_rename_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_ets_rename_2))
#endif

#ifndef HAVE_nbif_ets_safe_fixtable_2
#define HAVE_nbif_ets_safe_fixtable_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ets_safe_fixtable_2))
ASYM(nbif_ets_safe_fixtable_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ets_safe_fixtable_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ets_safe_fixtable_2))
	TYPE_FUNCTION(ASYM(nbif_ets_safe_fixtable_2))
#endif

#ifndef HAVE_nbif_ebif_ets_safe_fixtable_2
#define HAVE_nbif_ebif_ets_safe_fixtable_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_ets_safe_fixtable_2))
ASYM(nbif_ebif_ets_safe_fixtable_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_ets_safe_fixtable_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_ets_safe_fixtable_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_ets_safe_fixtable_2))
#endif

#ifndef HAVE_nbif_ets_slot_2
#define HAVE_nbif_ets_slot_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ets_slot_2))
ASYM(nbif_ets_slot_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ets_slot_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ets_slot_2))
	TYPE_FUNCTION(ASYM(nbif_ets_slot_2))
#endif

#ifndef HAVE_nbif_ebif_ets_slot_2
#define HAVE_nbif_ebif_ets_slot_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_ets_slot_2))
ASYM(nbif_ebif_ets_slot_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_ets_slot_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_ets_slot_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_ets_slot_2))
#endif

#ifndef HAVE_nbif_ets_update_counter_3
#define HAVE_nbif_ets_update_counter_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ets_update_counter_3))
ASYM(nbif_ets_update_counter_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(ets_update_counter_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_ets_update_counter_3))
	TYPE_FUNCTION(ASYM(nbif_ets_update_counter_3))
#endif

#ifndef HAVE_nbif_ebif_ets_update_counter_3
#define HAVE_nbif_ebif_ets_update_counter_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_ets_update_counter_3))
ASYM(nbif_ebif_ets_update_counter_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(ebif_ets_update_counter_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_ebif_ets_update_counter_3))
	TYPE_FUNCTION(ASYM(nbif_ebif_ets_update_counter_3))
#endif

#ifndef HAVE_nbif_ets_select_1
#define HAVE_nbif_ets_select_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ets_select_1))
ASYM(nbif_ets_select_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ets_select_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ets_select_1))
	TYPE_FUNCTION(ASYM(nbif_ets_select_1))
#endif

#ifndef HAVE_nbif_ebif_ets_select_1
#define HAVE_nbif_ebif_ets_select_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_ets_select_1))
ASYM(nbif_ebif_ets_select_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_ets_select_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_ets_select_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_ets_select_1))
#endif

#ifndef HAVE_nbif_ets_select_2
#define HAVE_nbif_ets_select_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ets_select_2))
ASYM(nbif_ets_select_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ets_select_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ets_select_2))
	TYPE_FUNCTION(ASYM(nbif_ets_select_2))
#endif

#ifndef HAVE_nbif_ebif_ets_select_2
#define HAVE_nbif_ebif_ets_select_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_ets_select_2))
ASYM(nbif_ebif_ets_select_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_ets_select_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_ets_select_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_ets_select_2))
#endif

#ifndef HAVE_nbif_ets_select_3
#define HAVE_nbif_ets_select_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ets_select_3))
ASYM(nbif_ets_select_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(ets_select_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_ets_select_3))
	TYPE_FUNCTION(ASYM(nbif_ets_select_3))
#endif

#ifndef HAVE_nbif_ebif_ets_select_3
#define HAVE_nbif_ebif_ets_select_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_ets_select_3))
ASYM(nbif_ebif_ets_select_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(ebif_ets_select_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_ebif_ets_select_3))
	TYPE_FUNCTION(ASYM(nbif_ebif_ets_select_3))
#endif

#ifndef HAVE_nbif_ets_select_count_2
#define HAVE_nbif_ets_select_count_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ets_select_count_2))
ASYM(nbif_ets_select_count_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ets_select_count_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ets_select_count_2))
	TYPE_FUNCTION(ASYM(nbif_ets_select_count_2))
#endif

#ifndef HAVE_nbif_ebif_ets_select_count_2
#define HAVE_nbif_ebif_ets_select_count_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_ets_select_count_2))
ASYM(nbif_ebif_ets_select_count_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_ets_select_count_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_ets_select_count_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_ets_select_count_2))
#endif

#ifndef HAVE_nbif_ets_select_reverse_1
#define HAVE_nbif_ets_select_reverse_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ets_select_reverse_1))
ASYM(nbif_ets_select_reverse_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ets_select_reverse_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ets_select_reverse_1))
	TYPE_FUNCTION(ASYM(nbif_ets_select_reverse_1))
#endif

#ifndef HAVE_nbif_ebif_ets_select_reverse_1
#define HAVE_nbif_ebif_ets_select_reverse_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_ets_select_reverse_1))
ASYM(nbif_ebif_ets_select_reverse_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_ets_select_reverse_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_ets_select_reverse_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_ets_select_reverse_1))
#endif

#ifndef HAVE_nbif_ets_select_reverse_2
#define HAVE_nbif_ets_select_reverse_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ets_select_reverse_2))
ASYM(nbif_ets_select_reverse_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ets_select_reverse_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ets_select_reverse_2))
	TYPE_FUNCTION(ASYM(nbif_ets_select_reverse_2))
#endif

#ifndef HAVE_nbif_ebif_ets_select_reverse_2
#define HAVE_nbif_ebif_ets_select_reverse_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_ets_select_reverse_2))
ASYM(nbif_ebif_ets_select_reverse_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_ets_select_reverse_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_ets_select_reverse_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_ets_select_reverse_2))
#endif

#ifndef HAVE_nbif_ets_select_reverse_3
#define HAVE_nbif_ets_select_reverse_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ets_select_reverse_3))
ASYM(nbif_ets_select_reverse_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(ets_select_reverse_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_ets_select_reverse_3))
	TYPE_FUNCTION(ASYM(nbif_ets_select_reverse_3))
#endif

#ifndef HAVE_nbif_ebif_ets_select_reverse_3
#define HAVE_nbif_ebif_ets_select_reverse_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_ets_select_reverse_3))
ASYM(nbif_ebif_ets_select_reverse_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(ebif_ets_select_reverse_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_ebif_ets_select_reverse_3))
	TYPE_FUNCTION(ASYM(nbif_ebif_ets_select_reverse_3))
#endif

#ifndef HAVE_nbif_ets_select_delete_2
#define HAVE_nbif_ets_select_delete_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ets_select_delete_2))
ASYM(nbif_ets_select_delete_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ets_select_delete_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ets_select_delete_2))
	TYPE_FUNCTION(ASYM(nbif_ets_select_delete_2))
#endif

#ifndef HAVE_nbif_ebif_ets_select_delete_2
#define HAVE_nbif_ebif_ets_select_delete_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_ets_select_delete_2))
ASYM(nbif_ebif_ets_select_delete_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_ets_select_delete_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_ets_select_delete_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_ets_select_delete_2))
#endif

#ifndef HAVE_nbif_ets_match_spec_compile_1
#define HAVE_nbif_ets_match_spec_compile_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ets_match_spec_compile_1))
ASYM(nbif_ets_match_spec_compile_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ets_match_spec_compile_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ets_match_spec_compile_1))
	TYPE_FUNCTION(ASYM(nbif_ets_match_spec_compile_1))
#endif

#ifndef HAVE_nbif_ebif_ets_match_spec_compile_1
#define HAVE_nbif_ebif_ets_match_spec_compile_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_ets_match_spec_compile_1))
ASYM(nbif_ebif_ets_match_spec_compile_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_ets_match_spec_compile_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_ets_match_spec_compile_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_ets_match_spec_compile_1))
#endif

#ifndef HAVE_nbif_ets_match_spec_run_r_3
#define HAVE_nbif_ets_match_spec_run_r_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ets_match_spec_run_r_3))
ASYM(nbif_ets_match_spec_run_r_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(ets_match_spec_run_r_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_ets_match_spec_run_r_3))
	TYPE_FUNCTION(ASYM(nbif_ets_match_spec_run_r_3))
#endif

#ifndef HAVE_nbif_ebif_ets_match_spec_run_r_3
#define HAVE_nbif_ebif_ets_match_spec_run_r_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_ets_match_spec_run_r_3))
ASYM(nbif_ebif_ets_match_spec_run_r_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(ebif_ets_match_spec_run_r_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_ebif_ets_match_spec_run_r_3))
	TYPE_FUNCTION(ASYM(nbif_ebif_ets_match_spec_run_r_3))
#endif

#ifndef HAVE_nbif_os_putenv_2
#define HAVE_nbif_os_putenv_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_os_putenv_2))
ASYM(nbif_os_putenv_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(os_putenv_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_os_putenv_2))
	TYPE_FUNCTION(ASYM(nbif_os_putenv_2))
#endif

#ifndef HAVE_nbif_ebif_os_setenv_2
#define HAVE_nbif_ebif_os_setenv_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_os_setenv_2))
ASYM(nbif_ebif_os_setenv_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_os_setenv_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_os_setenv_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_os_setenv_2))
#endif

#ifndef HAVE_nbif_os_getenv_0
#define HAVE_nbif_os_getenv_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_os_getenv_0))
ASYM(nbif_os_getenv_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(os_getenv_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_os_getenv_0))
	TYPE_FUNCTION(ASYM(nbif_os_getenv_0))
#endif

#ifndef HAVE_nbif_ebif_os_getenv_0
#define HAVE_nbif_ebif_os_getenv_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_os_getenv_0))
ASYM(nbif_ebif_os_getenv_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(ebif_os_getenv_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_ebif_os_getenv_0))
	TYPE_FUNCTION(ASYM(nbif_ebif_os_getenv_0))
#endif

#ifndef HAVE_nbif_os_getenv_1
#define HAVE_nbif_os_getenv_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_os_getenv_1))
ASYM(nbif_os_getenv_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(os_getenv_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_os_getenv_1))
	TYPE_FUNCTION(ASYM(nbif_os_getenv_1))
#endif

#ifndef HAVE_nbif_ebif_os_getenv_1
#define HAVE_nbif_ebif_os_getenv_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_os_getenv_1))
ASYM(nbif_ebif_os_getenv_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_os_getenv_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_os_getenv_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_os_getenv_1))
#endif

#ifndef HAVE_nbif_os_getpid_0
#define HAVE_nbif_os_getpid_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_os_getpid_0))
ASYM(nbif_os_getpid_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(os_getpid_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_os_getpid_0))
	TYPE_FUNCTION(ASYM(nbif_os_getpid_0))
#endif

#ifndef HAVE_nbif_ebif_os_pid_0
#define HAVE_nbif_ebif_os_pid_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_os_pid_0))
ASYM(nbif_ebif_os_pid_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(ebif_os_pid_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_ebif_os_pid_0))
	TYPE_FUNCTION(ASYM(nbif_ebif_os_pid_0))
#endif

#ifndef HAVE_nbif_os_timestamp_0
#define HAVE_nbif_os_timestamp_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_os_timestamp_0))
ASYM(nbif_os_timestamp_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(os_timestamp_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_os_timestamp_0))
	TYPE_FUNCTION(ASYM(nbif_os_timestamp_0))
#endif

#ifndef HAVE_nbif_ebif_os_timestamp_0
#define HAVE_nbif_ebif_os_timestamp_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_os_timestamp_0))
ASYM(nbif_ebif_os_timestamp_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(ebif_os_timestamp_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_ebif_os_timestamp_0))
	TYPE_FUNCTION(ASYM(nbif_ebif_os_timestamp_0))
#endif

#ifndef HAVE_nbif_erl_ddll_try_load_3
#define HAVE_nbif_erl_ddll_try_load_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_erl_ddll_try_load_3))
ASYM(nbif_erl_ddll_try_load_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(erl_ddll_try_load_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_erl_ddll_try_load_3))
	TYPE_FUNCTION(ASYM(nbif_erl_ddll_try_load_3))
#endif

#ifndef HAVE_nbif_erl_ddll_try_unload_2
#define HAVE_nbif_erl_ddll_try_unload_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_erl_ddll_try_unload_2))
ASYM(nbif_erl_ddll_try_unload_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(erl_ddll_try_unload_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_erl_ddll_try_unload_2))
	TYPE_FUNCTION(ASYM(nbif_erl_ddll_try_unload_2))
#endif

#ifndef HAVE_nbif_erl_ddll_loaded_drivers_0
#define HAVE_nbif_erl_ddll_loaded_drivers_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_erl_ddll_loaded_drivers_0))
ASYM(nbif_erl_ddll_loaded_drivers_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(erl_ddll_loaded_drivers_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_erl_ddll_loaded_drivers_0))
	TYPE_FUNCTION(ASYM(nbif_erl_ddll_loaded_drivers_0))
#endif

#ifndef HAVE_nbif_erl_ddll_info_2
#define HAVE_nbif_erl_ddll_info_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_erl_ddll_info_2))
ASYM(nbif_erl_ddll_info_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(erl_ddll_info_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_erl_ddll_info_2))
	TYPE_FUNCTION(ASYM(nbif_erl_ddll_info_2))
#endif

#ifndef HAVE_nbif_erl_ddll_format_error_int_1
#define HAVE_nbif_erl_ddll_format_error_int_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_erl_ddll_format_error_int_1))
ASYM(nbif_erl_ddll_format_error_int_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(erl_ddll_format_error_int_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_erl_ddll_format_error_int_1))
	TYPE_FUNCTION(ASYM(nbif_erl_ddll_format_error_int_1))
#endif

#ifndef HAVE_nbif_erl_ddll_monitor_2
#define HAVE_nbif_erl_ddll_monitor_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_erl_ddll_monitor_2))
ASYM(nbif_erl_ddll_monitor_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(erl_ddll_monitor_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_erl_ddll_monitor_2))
	TYPE_FUNCTION(ASYM(nbif_erl_ddll_monitor_2))
#endif

#ifndef HAVE_nbif_erl_ddll_demonitor_1
#define HAVE_nbif_erl_ddll_demonitor_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_erl_ddll_demonitor_1))
ASYM(nbif_erl_ddll_demonitor_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(erl_ddll_demonitor_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_erl_ddll_demonitor_1))
	TYPE_FUNCTION(ASYM(nbif_erl_ddll_demonitor_1))
#endif

#ifndef HAVE_nbif_re_compile_1
#define HAVE_nbif_re_compile_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_re_compile_1))
ASYM(nbif_re_compile_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(re_compile_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_re_compile_1))
	TYPE_FUNCTION(ASYM(nbif_re_compile_1))
#endif

#ifndef HAVE_nbif_re_compile_2
#define HAVE_nbif_re_compile_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_re_compile_2))
ASYM(nbif_re_compile_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(re_compile_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_re_compile_2))
	TYPE_FUNCTION(ASYM(nbif_re_compile_2))
#endif

#ifndef HAVE_nbif_re_run_2
#define HAVE_nbif_re_run_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_re_run_2))
ASYM(nbif_re_run_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(re_run_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_re_run_2))
	TYPE_FUNCTION(ASYM(nbif_re_run_2))
#endif

#ifndef HAVE_nbif_re_run_3
#define HAVE_nbif_re_run_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_re_run_3))
ASYM(nbif_re_run_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(re_run_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_re_run_3))
	TYPE_FUNCTION(ASYM(nbif_re_run_3))
#endif

#ifndef HAVE_nbif_lists_member_2
#define HAVE_nbif_lists_member_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_lists_member_2))
ASYM(nbif_lists_member_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(lists_member_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_lists_member_2))
	TYPE_FUNCTION(ASYM(nbif_lists_member_2))
#endif

#ifndef HAVE_nbif_ebif_list_is_element_2
#define HAVE_nbif_ebif_list_is_element_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_list_is_element_2))
ASYM(nbif_ebif_list_is_element_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_list_is_element_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_list_is_element_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_list_is_element_2))
#endif

#ifndef HAVE_nbif_lists_reverse_2
#define HAVE_nbif_lists_reverse_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_lists_reverse_2))
ASYM(nbif_lists_reverse_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(lists_reverse_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_lists_reverse_2))
	TYPE_FUNCTION(ASYM(nbif_lists_reverse_2))
#endif

#ifndef HAVE_nbif_ebif_list_reverse_2
#define HAVE_nbif_ebif_list_reverse_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_list_reverse_2))
ASYM(nbif_ebif_list_reverse_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_list_reverse_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_list_reverse_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_list_reverse_2))
#endif

#ifndef HAVE_nbif_lists_keymember_3
#define HAVE_nbif_lists_keymember_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_lists_keymember_3))
ASYM(nbif_lists_keymember_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(lists_keymember_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_lists_keymember_3))
	TYPE_FUNCTION(ASYM(nbif_lists_keymember_3))
#endif

#ifndef HAVE_nbif_ebif_keylist_is_element_3
#define HAVE_nbif_ebif_keylist_is_element_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_keylist_is_element_3))
ASYM(nbif_ebif_keylist_is_element_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(ebif_keylist_is_element_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_ebif_keylist_is_element_3))
	TYPE_FUNCTION(ASYM(nbif_ebif_keylist_is_element_3))
#endif

#ifndef HAVE_nbif_lists_keysearch_3
#define HAVE_nbif_lists_keysearch_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_lists_keysearch_3))
ASYM(nbif_lists_keysearch_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(lists_keysearch_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_lists_keysearch_3))
	TYPE_FUNCTION(ASYM(nbif_lists_keysearch_3))
#endif

#ifndef HAVE_nbif_ebif_keylist_search_3
#define HAVE_nbif_ebif_keylist_search_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_keylist_search_3))
ASYM(nbif_ebif_keylist_search_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(ebif_keylist_search_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_ebif_keylist_search_3))
	TYPE_FUNCTION(ASYM(nbif_ebif_keylist_search_3))
#endif

#ifndef HAVE_nbif_lists_keyfind_3
#define HAVE_nbif_lists_keyfind_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_lists_keyfind_3))
ASYM(nbif_lists_keyfind_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(lists_keyfind_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_lists_keyfind_3))
	TYPE_FUNCTION(ASYM(nbif_lists_keyfind_3))
#endif

#ifndef HAVE_nbif_erts_debug_disassemble_1
#define HAVE_nbif_erts_debug_disassemble_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_erts_debug_disassemble_1))
ASYM(nbif_erts_debug_disassemble_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(erts_debug_disassemble_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_erts_debug_disassemble_1))
	TYPE_FUNCTION(ASYM(nbif_erts_debug_disassemble_1))
#endif

#ifndef HAVE_nbif_ebif_erts_debug_disassemble_1
#define HAVE_nbif_ebif_erts_debug_disassemble_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_erts_debug_disassemble_1))
ASYM(nbif_ebif_erts_debug_disassemble_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_erts_debug_disassemble_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_erts_debug_disassemble_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_erts_debug_disassemble_1))
#endif

#ifndef HAVE_nbif_erts_debug_breakpoint_2
#define HAVE_nbif_erts_debug_breakpoint_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_erts_debug_breakpoint_2))
ASYM(nbif_erts_debug_breakpoint_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(erts_debug_breakpoint_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_erts_debug_breakpoint_2))
	TYPE_FUNCTION(ASYM(nbif_erts_debug_breakpoint_2))
#endif

#ifndef HAVE_nbif_ebif_erts_debug_breakpoint_2
#define HAVE_nbif_ebif_erts_debug_breakpoint_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_erts_debug_breakpoint_2))
ASYM(nbif_ebif_erts_debug_breakpoint_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_erts_debug_breakpoint_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_erts_debug_breakpoint_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_erts_debug_breakpoint_2))
#endif

#ifndef HAVE_nbif_erts_debug_same_2
#define HAVE_nbif_erts_debug_same_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_erts_debug_same_2))
ASYM(nbif_erts_debug_same_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(erts_debug_same_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_erts_debug_same_2))
	TYPE_FUNCTION(ASYM(nbif_erts_debug_same_2))
#endif

#ifndef HAVE_nbif_ebif_erts_debug_same_2
#define HAVE_nbif_ebif_erts_debug_same_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_erts_debug_same_2))
ASYM(nbif_ebif_erts_debug_same_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_erts_debug_same_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_erts_debug_same_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_erts_debug_same_2))
#endif

#ifndef HAVE_nbif_erts_debug_flat_size_1
#define HAVE_nbif_erts_debug_flat_size_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_erts_debug_flat_size_1))
ASYM(nbif_erts_debug_flat_size_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(erts_debug_flat_size_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_erts_debug_flat_size_1))
	TYPE_FUNCTION(ASYM(nbif_erts_debug_flat_size_1))
#endif

#ifndef HAVE_nbif_ebif_erts_debug_flat_size_1
#define HAVE_nbif_ebif_erts_debug_flat_size_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_erts_debug_flat_size_1))
ASYM(nbif_ebif_erts_debug_flat_size_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_erts_debug_flat_size_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_erts_debug_flat_size_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_erts_debug_flat_size_1))
#endif

#ifndef HAVE_nbif_erts_debug_get_internal_state_1
#define HAVE_nbif_erts_debug_get_internal_state_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_erts_debug_get_internal_state_1))
ASYM(nbif_erts_debug_get_internal_state_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(erts_debug_get_internal_state_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_erts_debug_get_internal_state_1))
	TYPE_FUNCTION(ASYM(nbif_erts_debug_get_internal_state_1))
#endif

#ifndef HAVE_nbif_ebif_erts_debug_get_internal_state_1
#define HAVE_nbif_ebif_erts_debug_get_internal_state_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_erts_debug_get_internal_state_1))
ASYM(nbif_ebif_erts_debug_get_internal_state_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_erts_debug_get_internal_state_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_erts_debug_get_internal_state_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_erts_debug_get_internal_state_1))
#endif

#ifndef HAVE_nbif_erts_debug_set_internal_state_2
#define HAVE_nbif_erts_debug_set_internal_state_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_erts_debug_set_internal_state_2))
ASYM(nbif_erts_debug_set_internal_state_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(erts_debug_set_internal_state_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_erts_debug_set_internal_state_2))
	TYPE_FUNCTION(ASYM(nbif_erts_debug_set_internal_state_2))
#endif

#ifndef HAVE_nbif_ebif_erts_debug_set_internal_state_2
#define HAVE_nbif_ebif_erts_debug_set_internal_state_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_erts_debug_set_internal_state_2))
ASYM(nbif_ebif_erts_debug_set_internal_state_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_erts_debug_set_internal_state_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_erts_debug_set_internal_state_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_erts_debug_set_internal_state_2))
#endif

#ifndef HAVE_nbif_erts_debug_display_1
#define HAVE_nbif_erts_debug_display_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_erts_debug_display_1))
ASYM(nbif_erts_debug_display_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(erts_debug_display_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_erts_debug_display_1))
	TYPE_FUNCTION(ASYM(nbif_erts_debug_display_1))
#endif

#ifndef HAVE_nbif_ebif_erts_debug_display_1
#define HAVE_nbif_ebif_erts_debug_display_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_erts_debug_display_1))
ASYM(nbif_ebif_erts_debug_display_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_erts_debug_display_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_erts_debug_display_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_erts_debug_display_1))
#endif

#ifndef HAVE_nbif_erts_debug_dist_ext_to_term_2
#define HAVE_nbif_erts_debug_dist_ext_to_term_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_erts_debug_dist_ext_to_term_2))
ASYM(nbif_erts_debug_dist_ext_to_term_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(erts_debug_dist_ext_to_term_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_erts_debug_dist_ext_to_term_2))
	TYPE_FUNCTION(ASYM(nbif_erts_debug_dist_ext_to_term_2))
#endif

#ifndef HAVE_nbif_ebif_erts_debug_dist_ext_to_term_2
#define HAVE_nbif_ebif_erts_debug_dist_ext_to_term_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_erts_debug_dist_ext_to_term_2))
ASYM(nbif_ebif_erts_debug_dist_ext_to_term_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_erts_debug_dist_ext_to_term_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_erts_debug_dist_ext_to_term_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_erts_debug_dist_ext_to_term_2))
#endif

#ifndef HAVE_nbif_erts_debug_instructions_0
#define HAVE_nbif_erts_debug_instructions_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_erts_debug_instructions_0))
ASYM(nbif_erts_debug_instructions_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(erts_debug_instructions_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_erts_debug_instructions_0))
	TYPE_FUNCTION(ASYM(nbif_erts_debug_instructions_0))
#endif

#ifndef HAVE_nbif_erts_debug_dump_monitors_1
#define HAVE_nbif_erts_debug_dump_monitors_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_erts_debug_dump_monitors_1))
ASYM(nbif_erts_debug_dump_monitors_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(erts_debug_dump_monitors_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_erts_debug_dump_monitors_1))
	TYPE_FUNCTION(ASYM(nbif_erts_debug_dump_monitors_1))
#endif

#ifndef HAVE_nbif_erts_debug_dump_links_1
#define HAVE_nbif_erts_debug_dump_links_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_erts_debug_dump_links_1))
ASYM(nbif_erts_debug_dump_links_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(erts_debug_dump_links_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_erts_debug_dump_links_1))
	TYPE_FUNCTION(ASYM(nbif_erts_debug_dump_links_1))
#endif

#ifndef HAVE_nbif_erts_debug_lock_counters_1
#define HAVE_nbif_erts_debug_lock_counters_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_erts_debug_lock_counters_1))
ASYM(nbif_erts_debug_lock_counters_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(erts_debug_lock_counters_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_erts_debug_lock_counters_1))
	TYPE_FUNCTION(ASYM(nbif_erts_debug_lock_counters_1))
#endif

#ifndef HAVE_nbif_code_get_chunk_2
#define HAVE_nbif_code_get_chunk_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_code_get_chunk_2))
ASYM(nbif_code_get_chunk_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(code_get_chunk_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_code_get_chunk_2))
	TYPE_FUNCTION(ASYM(nbif_code_get_chunk_2))
#endif

#ifndef HAVE_nbif_ebif_code_get_chunk_2
#define HAVE_nbif_ebif_code_get_chunk_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_code_get_chunk_2))
ASYM(nbif_ebif_code_get_chunk_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ebif_code_get_chunk_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ebif_code_get_chunk_2))
	TYPE_FUNCTION(ASYM(nbif_ebif_code_get_chunk_2))
#endif

#ifndef HAVE_nbif_code_module_md5_1
#define HAVE_nbif_code_module_md5_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_code_module_md5_1))
ASYM(nbif_code_module_md5_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(code_module_md5_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_code_module_md5_1))
	TYPE_FUNCTION(ASYM(nbif_code_module_md5_1))
#endif

#ifndef HAVE_nbif_ebif_code_module_md5_1
#define HAVE_nbif_ebif_code_module_md5_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_code_module_md5_1))
ASYM(nbif_ebif_code_module_md5_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_code_module_md5_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_code_module_md5_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_code_module_md5_1))
#endif

#ifndef HAVE_nbif_code_make_stub_module_3
#define HAVE_nbif_code_make_stub_module_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_code_make_stub_module_3))
ASYM(nbif_code_make_stub_module_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(code_make_stub_module_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_code_make_stub_module_3))
	TYPE_FUNCTION(ASYM(nbif_code_make_stub_module_3))
#endif

#ifndef HAVE_nbif_ebif_code_make_stub_module_3
#define HAVE_nbif_ebif_code_make_stub_module_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_code_make_stub_module_3))
ASYM(nbif_ebif_code_make_stub_module_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(ebif_code_make_stub_module_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_ebif_code_make_stub_module_3))
	TYPE_FUNCTION(ASYM(nbif_ebif_code_make_stub_module_3))
#endif

#ifndef HAVE_nbif_code_is_module_native_1
#define HAVE_nbif_code_is_module_native_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_code_is_module_native_1))
ASYM(nbif_code_is_module_native_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(code_is_module_native_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_code_is_module_native_1))
	TYPE_FUNCTION(ASYM(nbif_code_is_module_native_1))
#endif

#ifndef HAVE_nbif_ebif_code_is_native_1
#define HAVE_nbif_ebif_code_is_native_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ebif_code_is_native_1))
ASYM(nbif_ebif_code_is_native_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(ebif_code_is_native_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_ebif_code_is_native_1))
	TYPE_FUNCTION(ASYM(nbif_ebif_code_is_native_1))
#endif

#ifndef HAVE_nbif_hibernate_3
#define HAVE_nbif_hibernate_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hibernate_3))
ASYM(nbif_hibernate_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(hibernate_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_hibernate_3))
	TYPE_FUNCTION(ASYM(nbif_hibernate_3))
#endif

#ifndef HAVE_nbif_error_logger_warning_map_0
#define HAVE_nbif_error_logger_warning_map_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_error_logger_warning_map_0))
ASYM(nbif_error_logger_warning_map_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(error_logger_warning_map_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_error_logger_warning_map_0))
	TYPE_FUNCTION(ASYM(nbif_error_logger_warning_map_0))
#endif

#ifndef HAVE_nbif_get_module_info_1
#define HAVE_nbif_get_module_info_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_get_module_info_1))
ASYM(nbif_get_module_info_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(get_module_info_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_get_module_info_1))
	TYPE_FUNCTION(ASYM(nbif_get_module_info_1))
#endif

#ifndef HAVE_nbif_get_module_info_2
#define HAVE_nbif_get_module_info_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_get_module_info_2))
ASYM(nbif_get_module_info_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(get_module_info_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_get_module_info_2))
	TYPE_FUNCTION(ASYM(nbif_get_module_info_2))
#endif

#ifndef HAVE_nbif_is_boolean_1
#define HAVE_nbif_is_boolean_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_is_boolean_1))
ASYM(nbif_is_boolean_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(is_boolean_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_is_boolean_1))
	TYPE_FUNCTION(ASYM(nbif_is_boolean_1))
#endif

#ifndef HAVE_nbif_string_to_integer_1
#define HAVE_nbif_string_to_integer_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_string_to_integer_1))
ASYM(nbif_string_to_integer_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(string_to_integer_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_string_to_integer_1))
	TYPE_FUNCTION(ASYM(nbif_string_to_integer_1))
#endif

#ifndef HAVE_nbif_string_to_float_1
#define HAVE_nbif_string_to_float_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_string_to_float_1))
ASYM(nbif_string_to_float_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(string_to_float_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_string_to_float_1))
	TYPE_FUNCTION(ASYM(nbif_string_to_float_1))
#endif

#ifndef HAVE_nbif_make_fun_3
#define HAVE_nbif_make_fun_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_make_fun_3))
ASYM(nbif_make_fun_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(make_fun_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_make_fun_3))
	TYPE_FUNCTION(ASYM(nbif_make_fun_3))
#endif

#ifndef HAVE_nbif_iolist_size_1
#define HAVE_nbif_iolist_size_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_iolist_size_1))
ASYM(nbif_iolist_size_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(iolist_size_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_iolist_size_1))
	TYPE_FUNCTION(ASYM(nbif_iolist_size_1))
#endif

#ifndef HAVE_nbif_iolist_to_binary_1
#define HAVE_nbif_iolist_to_binary_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_iolist_to_binary_1))
ASYM(nbif_iolist_to_binary_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(iolist_to_binary_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_iolist_to_binary_1))
	TYPE_FUNCTION(ASYM(nbif_iolist_to_binary_1))
#endif

#ifndef HAVE_nbif_list_to_existing_atom_1
#define HAVE_nbif_list_to_existing_atom_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_list_to_existing_atom_1))
ASYM(nbif_list_to_existing_atom_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(list_to_existing_atom_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_list_to_existing_atom_1))
	TYPE_FUNCTION(ASYM(nbif_list_to_existing_atom_1))
#endif

#ifndef HAVE_nbif_is_bitstring_1
#define HAVE_nbif_is_bitstring_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_is_bitstring_1))
ASYM(nbif_is_bitstring_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(is_bitstring_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_is_bitstring_1))
	TYPE_FUNCTION(ASYM(nbif_is_bitstring_1))
#endif

#ifndef HAVE_nbif_tuple_size_1
#define HAVE_nbif_tuple_size_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_tuple_size_1))
ASYM(nbif_tuple_size_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(tuple_size_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_tuple_size_1))
	TYPE_FUNCTION(ASYM(nbif_tuple_size_1))
#endif

#ifndef HAVE_nbif_byte_size_1
#define HAVE_nbif_byte_size_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_byte_size_1))
ASYM(nbif_byte_size_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(byte_size_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_byte_size_1))
	TYPE_FUNCTION(ASYM(nbif_byte_size_1))
#endif

#ifndef HAVE_nbif_bit_size_1
#define HAVE_nbif_bit_size_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_bit_size_1))
ASYM(nbif_bit_size_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(bit_size_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_bit_size_1))
	TYPE_FUNCTION(ASYM(nbif_bit_size_1))
#endif

#ifndef HAVE_nbif_list_to_bitstring_1
#define HAVE_nbif_list_to_bitstring_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_list_to_bitstring_1))
ASYM(nbif_list_to_bitstring_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(list_to_bitstring_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_list_to_bitstring_1))
	TYPE_FUNCTION(ASYM(nbif_list_to_bitstring_1))
#endif

#ifndef HAVE_nbif_bitstring_to_list_1
#define HAVE_nbif_bitstring_to_list_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_bitstring_to_list_1))
ASYM(nbif_bitstring_to_list_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(bitstring_to_list_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_bitstring_to_list_1))
	TYPE_FUNCTION(ASYM(nbif_bitstring_to_list_1))
#endif

#ifndef HAVE_nbif_ets_update_element_3
#define HAVE_nbif_ets_update_element_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ets_update_element_3))
ASYM(nbif_ets_update_element_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(ets_update_element_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_ets_update_element_3))
	TYPE_FUNCTION(ASYM(nbif_ets_update_element_3))
#endif

#ifndef HAVE_nbif_decode_packet_3
#define HAVE_nbif_decode_packet_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_decode_packet_3))
ASYM(nbif_decode_packet_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(decode_packet_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_decode_packet_3))
	TYPE_FUNCTION(ASYM(nbif_decode_packet_3))
#endif

#ifndef HAVE_nbif_unicode_characters_to_binary_2
#define HAVE_nbif_unicode_characters_to_binary_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_unicode_characters_to_binary_2))
ASYM(nbif_unicode_characters_to_binary_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(unicode_characters_to_binary_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_unicode_characters_to_binary_2))
	TYPE_FUNCTION(ASYM(nbif_unicode_characters_to_binary_2))
#endif

#ifndef HAVE_nbif_unicode_characters_to_list_2
#define HAVE_nbif_unicode_characters_to_list_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_unicode_characters_to_list_2))
ASYM(nbif_unicode_characters_to_list_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(unicode_characters_to_list_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_unicode_characters_to_list_2))
	TYPE_FUNCTION(ASYM(nbif_unicode_characters_to_list_2))
#endif

#ifndef HAVE_nbif_unicode_bin_is_7bit_1
#define HAVE_nbif_unicode_bin_is_7bit_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_unicode_bin_is_7bit_1))
ASYM(nbif_unicode_bin_is_7bit_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(unicode_bin_is_7bit_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_unicode_bin_is_7bit_1))
	TYPE_FUNCTION(ASYM(nbif_unicode_bin_is_7bit_1))
#endif

#ifndef HAVE_nbif_atom_to_binary_2
#define HAVE_nbif_atom_to_binary_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_atom_to_binary_2))
ASYM(nbif_atom_to_binary_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(atom_to_binary_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_atom_to_binary_2))
	TYPE_FUNCTION(ASYM(nbif_atom_to_binary_2))
#endif

#ifndef HAVE_nbif_binary_to_atom_2
#define HAVE_nbif_binary_to_atom_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_binary_to_atom_2))
ASYM(nbif_binary_to_atom_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(binary_to_atom_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_binary_to_atom_2))
	TYPE_FUNCTION(ASYM(nbif_binary_to_atom_2))
#endif

#ifndef HAVE_nbif_binary_to_existing_atom_2
#define HAVE_nbif_binary_to_existing_atom_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_binary_to_existing_atom_2))
ASYM(nbif_binary_to_existing_atom_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(binary_to_existing_atom_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_binary_to_existing_atom_2))
	TYPE_FUNCTION(ASYM(nbif_binary_to_existing_atom_2))
#endif

#ifndef HAVE_nbif_net_kernel_dflag_unicode_io_1
#define HAVE_nbif_net_kernel_dflag_unicode_io_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_net_kernel_dflag_unicode_io_1))
ASYM(nbif_net_kernel_dflag_unicode_io_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(net_kernel_dflag_unicode_io_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_net_kernel_dflag_unicode_io_1))
	TYPE_FUNCTION(ASYM(nbif_net_kernel_dflag_unicode_io_1))
#endif

#ifndef HAVE_nbif_ets_give_away_3
#define HAVE_nbif_ets_give_away_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ets_give_away_3))
ASYM(nbif_ets_give_away_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(ets_give_away_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_ets_give_away_3))
	TYPE_FUNCTION(ASYM(nbif_ets_give_away_3))
#endif

#ifndef HAVE_nbif_ets_setopts_2
#define HAVE_nbif_ets_setopts_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_ets_setopts_2))
ASYM(nbif_ets_setopts_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(ets_setopts_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_ets_setopts_2))
	TYPE_FUNCTION(ASYM(nbif_ets_setopts_2))
#endif

#ifndef HAVE_nbif_load_nif_2
#define HAVE_nbif_load_nif_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_load_nif_2))
ASYM(nbif_load_nif_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(load_nif_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_load_nif_2))
	TYPE_FUNCTION(ASYM(nbif_load_nif_2))
#endif

#ifndef HAVE_nbif_call_on_load_function_1
#define HAVE_nbif_call_on_load_function_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_call_on_load_function_1))
ASYM(nbif_call_on_load_function_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(call_on_load_function_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_call_on_load_function_1))
	TYPE_FUNCTION(ASYM(nbif_call_on_load_function_1))
#endif

#ifndef HAVE_nbif_finish_after_on_load_2
#define HAVE_nbif_finish_after_on_load_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_finish_after_on_load_2))
ASYM(nbif_finish_after_on_load_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(finish_after_on_load_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_finish_after_on_load_2))
	TYPE_FUNCTION(ASYM(nbif_finish_after_on_load_2))
#endif

#ifndef HAVE_nbif_binary_to_term_2
#define HAVE_nbif_binary_to_term_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_binary_to_term_2))
ASYM(nbif_binary_to_term_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(binary_to_term_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_binary_to_term_2))
	TYPE_FUNCTION(ASYM(nbif_binary_to_term_2))
#endif

#ifndef HAVE_nbif_binary_part_2
#define HAVE_nbif_binary_part_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_binary_part_2))
ASYM(nbif_binary_part_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(binary_part_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_binary_part_2))
	TYPE_FUNCTION(ASYM(nbif_binary_part_2))
#endif

#ifndef HAVE_nbif_binary_part_3
#define HAVE_nbif_binary_part_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_binary_part_3))
ASYM(nbif_binary_part_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(binary_part_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_binary_part_3))
	TYPE_FUNCTION(ASYM(nbif_binary_part_3))
#endif

#ifndef HAVE_nbif_binary_compile_pattern_1
#define HAVE_nbif_binary_compile_pattern_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_binary_compile_pattern_1))
ASYM(nbif_binary_compile_pattern_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(binary_compile_pattern_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_binary_compile_pattern_1))
	TYPE_FUNCTION(ASYM(nbif_binary_compile_pattern_1))
#endif

#ifndef HAVE_nbif_binary_match_2
#define HAVE_nbif_binary_match_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_binary_match_2))
ASYM(nbif_binary_match_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(binary_match_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_binary_match_2))
	TYPE_FUNCTION(ASYM(nbif_binary_match_2))
#endif

#ifndef HAVE_nbif_binary_match_3
#define HAVE_nbif_binary_match_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_binary_match_3))
ASYM(nbif_binary_match_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(binary_match_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_binary_match_3))
	TYPE_FUNCTION(ASYM(nbif_binary_match_3))
#endif

#ifndef HAVE_nbif_binary_matches_2
#define HAVE_nbif_binary_matches_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_binary_matches_2))
ASYM(nbif_binary_matches_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(binary_matches_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_binary_matches_2))
	TYPE_FUNCTION(ASYM(nbif_binary_matches_2))
#endif

#ifndef HAVE_nbif_binary_matches_3
#define HAVE_nbif_binary_matches_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_binary_matches_3))
ASYM(nbif_binary_matches_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(binary_matches_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_binary_matches_3))
	TYPE_FUNCTION(ASYM(nbif_binary_matches_3))
#endif

#ifndef HAVE_nbif_binary_longest_common_prefix_1
#define HAVE_nbif_binary_longest_common_prefix_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_binary_longest_common_prefix_1))
ASYM(nbif_binary_longest_common_prefix_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(binary_longest_common_prefix_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_binary_longest_common_prefix_1))
	TYPE_FUNCTION(ASYM(nbif_binary_longest_common_prefix_1))
#endif

#ifndef HAVE_nbif_binary_longest_common_suffix_1
#define HAVE_nbif_binary_longest_common_suffix_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_binary_longest_common_suffix_1))
ASYM(nbif_binary_longest_common_suffix_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(binary_longest_common_suffix_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_binary_longest_common_suffix_1))
	TYPE_FUNCTION(ASYM(nbif_binary_longest_common_suffix_1))
#endif

#ifndef HAVE_nbif_binary_first_1
#define HAVE_nbif_binary_first_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_binary_first_1))
ASYM(nbif_binary_first_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(binary_first_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_binary_first_1))
	TYPE_FUNCTION(ASYM(nbif_binary_first_1))
#endif

#ifndef HAVE_nbif_binary_last_1
#define HAVE_nbif_binary_last_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_binary_last_1))
ASYM(nbif_binary_last_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(binary_last_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_binary_last_1))
	TYPE_FUNCTION(ASYM(nbif_binary_last_1))
#endif

#ifndef HAVE_nbif_binary_at_2
#define HAVE_nbif_binary_at_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_binary_at_2))
ASYM(nbif_binary_at_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(binary_at_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_binary_at_2))
	TYPE_FUNCTION(ASYM(nbif_binary_at_2))
#endif

#ifndef HAVE_nbif_binary_binary_part_2
#define HAVE_nbif_binary_binary_part_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_binary_binary_part_2))
ASYM(nbif_binary_binary_part_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(binary_binary_part_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_binary_binary_part_2))
	TYPE_FUNCTION(ASYM(nbif_binary_binary_part_2))
#endif

#ifndef HAVE_nbif_binary_binary_part_3
#define HAVE_nbif_binary_binary_part_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_binary_binary_part_3))
ASYM(nbif_binary_binary_part_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(binary_binary_part_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_binary_binary_part_3))
	TYPE_FUNCTION(ASYM(nbif_binary_binary_part_3))
#endif

#ifndef HAVE_nbif_binary_bin_to_list_1
#define HAVE_nbif_binary_bin_to_list_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_binary_bin_to_list_1))
ASYM(nbif_binary_bin_to_list_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(binary_bin_to_list_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_binary_bin_to_list_1))
	TYPE_FUNCTION(ASYM(nbif_binary_bin_to_list_1))
#endif

#ifndef HAVE_nbif_binary_bin_to_list_2
#define HAVE_nbif_binary_bin_to_list_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_binary_bin_to_list_2))
ASYM(nbif_binary_bin_to_list_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(binary_bin_to_list_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_binary_bin_to_list_2))
	TYPE_FUNCTION(ASYM(nbif_binary_bin_to_list_2))
#endif

#ifndef HAVE_nbif_binary_bin_to_list_3
#define HAVE_nbif_binary_bin_to_list_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_binary_bin_to_list_3))
ASYM(nbif_binary_bin_to_list_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(binary_bin_to_list_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_binary_bin_to_list_3))
	TYPE_FUNCTION(ASYM(nbif_binary_bin_to_list_3))
#endif

#ifndef HAVE_nbif_binary_list_to_bin_1
#define HAVE_nbif_binary_list_to_bin_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_binary_list_to_bin_1))
ASYM(nbif_binary_list_to_bin_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(binary_list_to_bin_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_binary_list_to_bin_1))
	TYPE_FUNCTION(ASYM(nbif_binary_list_to_bin_1))
#endif

#ifndef HAVE_nbif_binary_copy_1
#define HAVE_nbif_binary_copy_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_binary_copy_1))
ASYM(nbif_binary_copy_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(binary_copy_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_binary_copy_1))
	TYPE_FUNCTION(ASYM(nbif_binary_copy_1))
#endif

#ifndef HAVE_nbif_binary_copy_2
#define HAVE_nbif_binary_copy_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_binary_copy_2))
ASYM(nbif_binary_copy_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(binary_copy_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_binary_copy_2))
	TYPE_FUNCTION(ASYM(nbif_binary_copy_2))
#endif

#ifndef HAVE_nbif_binary_referenced_byte_size_1
#define HAVE_nbif_binary_referenced_byte_size_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_binary_referenced_byte_size_1))
ASYM(nbif_binary_referenced_byte_size_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(binary_referenced_byte_size_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_binary_referenced_byte_size_1))
	TYPE_FUNCTION(ASYM(nbif_binary_referenced_byte_size_1))
#endif

#ifndef HAVE_nbif_binary_encode_unsigned_1
#define HAVE_nbif_binary_encode_unsigned_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_binary_encode_unsigned_1))
ASYM(nbif_binary_encode_unsigned_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(binary_encode_unsigned_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_binary_encode_unsigned_1))
	TYPE_FUNCTION(ASYM(nbif_binary_encode_unsigned_1))
#endif

#ifndef HAVE_nbif_binary_encode_unsigned_2
#define HAVE_nbif_binary_encode_unsigned_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_binary_encode_unsigned_2))
ASYM(nbif_binary_encode_unsigned_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(binary_encode_unsigned_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_binary_encode_unsigned_2))
	TYPE_FUNCTION(ASYM(nbif_binary_encode_unsigned_2))
#endif

#ifndef HAVE_nbif_binary_decode_unsigned_1
#define HAVE_nbif_binary_decode_unsigned_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_binary_decode_unsigned_1))
ASYM(nbif_binary_decode_unsigned_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(binary_decode_unsigned_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_binary_decode_unsigned_1))
	TYPE_FUNCTION(ASYM(nbif_binary_decode_unsigned_1))
#endif

#ifndef HAVE_nbif_binary_decode_unsigned_2
#define HAVE_nbif_binary_decode_unsigned_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_binary_decode_unsigned_2))
ASYM(nbif_binary_decode_unsigned_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(binary_decode_unsigned_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_binary_decode_unsigned_2))
	TYPE_FUNCTION(ASYM(nbif_binary_decode_unsigned_2))
#endif

#ifndef HAVE_nbif_nif_error_1
#define HAVE_nbif_nif_error_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_nif_error_1))
ASYM(nbif_nif_error_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(nif_error_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_nif_error_1))
	TYPE_FUNCTION(ASYM(nbif_nif_error_1))
#endif

#ifndef HAVE_nbif_nif_error_2
#define HAVE_nbif_nif_error_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_nif_error_2))
ASYM(nbif_nif_error_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(nif_error_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_nif_error_2))
	TYPE_FUNCTION(ASYM(nbif_nif_error_2))
#endif

#ifndef HAVE_nbif_prim_file_internal_name2native_1
#define HAVE_nbif_prim_file_internal_name2native_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_prim_file_internal_name2native_1))
ASYM(nbif_prim_file_internal_name2native_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(prim_file_internal_name2native_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_prim_file_internal_name2native_1))
	TYPE_FUNCTION(ASYM(nbif_prim_file_internal_name2native_1))
#endif

#ifndef HAVE_nbif_prim_file_internal_native2name_1
#define HAVE_nbif_prim_file_internal_native2name_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_prim_file_internal_native2name_1))
ASYM(nbif_prim_file_internal_native2name_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(prim_file_internal_native2name_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_prim_file_internal_native2name_1))
	TYPE_FUNCTION(ASYM(nbif_prim_file_internal_native2name_1))
#endif

#ifndef HAVE_nbif_prim_file_internal_normalize_utf8_1
#define HAVE_nbif_prim_file_internal_normalize_utf8_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_prim_file_internal_normalize_utf8_1))
ASYM(nbif_prim_file_internal_normalize_utf8_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(prim_file_internal_normalize_utf8_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_prim_file_internal_normalize_utf8_1))
	TYPE_FUNCTION(ASYM(nbif_prim_file_internal_normalize_utf8_1))
#endif

#ifndef HAVE_nbif_file_native_name_encoding_0
#define HAVE_nbif_file_native_name_encoding_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_file_native_name_encoding_0))
ASYM(nbif_file_native_name_encoding_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(file_native_name_encoding_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_file_native_name_encoding_0))
	TYPE_FUNCTION(ASYM(nbif_file_native_name_encoding_0))
#endif

#ifndef HAVE_nbif_hash_2
#define HAVE_nbif_hash_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hash_2))
ASYM(nbif_hash_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(hash_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_hash_2))
	TYPE_FUNCTION(ASYM(nbif_hash_2))
#endif

#ifndef HAVE_nbif_hipe_bifs_write_u8_2
#define HAVE_nbif_hipe_bifs_write_u8_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_write_u8_2))
ASYM(nbif_hipe_bifs_write_u8_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(hipe_bifs_write_u8_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_hipe_bifs_write_u8_2))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_write_u8_2))
#endif

#ifndef HAVE_nbif_hipe_bifs_write_u32_2
#define HAVE_nbif_hipe_bifs_write_u32_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_write_u32_2))
ASYM(nbif_hipe_bifs_write_u32_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(hipe_bifs_write_u32_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_hipe_bifs_write_u32_2))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_write_u32_2))
#endif

#ifndef HAVE_nbif_hipe_bifs_bytearray_2
#define HAVE_nbif_hipe_bifs_bytearray_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_bytearray_2))
ASYM(nbif_hipe_bifs_bytearray_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(hipe_bifs_bytearray_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_hipe_bifs_bytearray_2))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_bytearray_2))
#endif

#ifndef HAVE_nbif_hipe_bifs_bytearray_sub_2
#define HAVE_nbif_hipe_bifs_bytearray_sub_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_bytearray_sub_2))
ASYM(nbif_hipe_bifs_bytearray_sub_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(hipe_bifs_bytearray_sub_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_hipe_bifs_bytearray_sub_2))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_bytearray_sub_2))
#endif

#ifndef HAVE_nbif_hipe_bifs_bytearray_update_3
#define HAVE_nbif_hipe_bifs_bytearray_update_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_bytearray_update_3))
ASYM(nbif_hipe_bifs_bytearray_update_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(hipe_bifs_bytearray_update_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_hipe_bifs_bytearray_update_3))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_bytearray_update_3))
#endif

#ifndef HAVE_nbif_hipe_bifs_bitarray_2
#define HAVE_nbif_hipe_bifs_bitarray_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_bitarray_2))
ASYM(nbif_hipe_bifs_bitarray_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(hipe_bifs_bitarray_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_hipe_bifs_bitarray_2))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_bitarray_2))
#endif

#ifndef HAVE_nbif_hipe_bifs_bitarray_sub_2
#define HAVE_nbif_hipe_bifs_bitarray_sub_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_bitarray_sub_2))
ASYM(nbif_hipe_bifs_bitarray_sub_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(hipe_bifs_bitarray_sub_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_hipe_bifs_bitarray_sub_2))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_bitarray_sub_2))
#endif

#ifndef HAVE_nbif_hipe_bifs_bitarray_update_3
#define HAVE_nbif_hipe_bifs_bitarray_update_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_bitarray_update_3))
ASYM(nbif_hipe_bifs_bitarray_update_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(hipe_bifs_bitarray_update_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_hipe_bifs_bitarray_update_3))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_bitarray_update_3))
#endif

#ifndef HAVE_nbif_hipe_bifs_array_2
#define HAVE_nbif_hipe_bifs_array_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_array_2))
ASYM(nbif_hipe_bifs_array_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(hipe_bifs_array_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_hipe_bifs_array_2))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_array_2))
#endif

#ifndef HAVE_nbif_hipe_bifs_array_length_1
#define HAVE_nbif_hipe_bifs_array_length_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_array_length_1))
ASYM(nbif_hipe_bifs_array_length_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(hipe_bifs_array_length_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_hipe_bifs_array_length_1))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_array_length_1))
#endif

#ifndef HAVE_nbif_hipe_bifs_array_sub_2
#define HAVE_nbif_hipe_bifs_array_sub_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_array_sub_2))
ASYM(nbif_hipe_bifs_array_sub_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(hipe_bifs_array_sub_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_hipe_bifs_array_sub_2))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_array_sub_2))
#endif

#ifndef HAVE_nbif_hipe_bifs_array_update_3
#define HAVE_nbif_hipe_bifs_array_update_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_array_update_3))
ASYM(nbif_hipe_bifs_array_update_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(hipe_bifs_array_update_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_hipe_bifs_array_update_3))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_array_update_3))
#endif

#ifndef HAVE_nbif_hipe_bifs_ref_1
#define HAVE_nbif_hipe_bifs_ref_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_ref_1))
ASYM(nbif_hipe_bifs_ref_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(hipe_bifs_ref_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_hipe_bifs_ref_1))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_ref_1))
#endif

#ifndef HAVE_nbif_hipe_bifs_ref_get_1
#define HAVE_nbif_hipe_bifs_ref_get_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_ref_get_1))
ASYM(nbif_hipe_bifs_ref_get_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(hipe_bifs_ref_get_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_hipe_bifs_ref_get_1))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_ref_get_1))
#endif

#ifndef HAVE_nbif_hipe_bifs_ref_set_2
#define HAVE_nbif_hipe_bifs_ref_set_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_ref_set_2))
ASYM(nbif_hipe_bifs_ref_set_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(hipe_bifs_ref_set_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_hipe_bifs_ref_set_2))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_ref_set_2))
#endif

#ifndef HAVE_nbif_hipe_bifs_enter_code_2
#define HAVE_nbif_hipe_bifs_enter_code_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_enter_code_2))
ASYM(nbif_hipe_bifs_enter_code_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(hipe_bifs_enter_code_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_hipe_bifs_enter_code_2))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_enter_code_2))
#endif

#ifndef HAVE_nbif_hipe_bifs_alloc_data_2
#define HAVE_nbif_hipe_bifs_alloc_data_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_alloc_data_2))
ASYM(nbif_hipe_bifs_alloc_data_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(hipe_bifs_alloc_data_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_hipe_bifs_alloc_data_2))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_alloc_data_2))
#endif

#ifndef HAVE_nbif_hipe_bifs_constants_size_0
#define HAVE_nbif_hipe_bifs_constants_size_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_constants_size_0))
ASYM(nbif_hipe_bifs_constants_size_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(hipe_bifs_constants_size_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_hipe_bifs_constants_size_0))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_constants_size_0))
#endif

#ifndef HAVE_nbif_hipe_bifs_merge_term_1
#define HAVE_nbif_hipe_bifs_merge_term_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_merge_term_1))
ASYM(nbif_hipe_bifs_merge_term_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(hipe_bifs_merge_term_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_hipe_bifs_merge_term_1))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_merge_term_1))
#endif

#ifndef HAVE_nbif_hipe_bifs_fun_to_address_1
#define HAVE_nbif_hipe_bifs_fun_to_address_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_fun_to_address_1))
ASYM(nbif_hipe_bifs_fun_to_address_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(hipe_bifs_fun_to_address_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_hipe_bifs_fun_to_address_1))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_fun_to_address_1))
#endif

#ifndef HAVE_nbif_hipe_bifs_set_native_address_3
#define HAVE_nbif_hipe_bifs_set_native_address_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_set_native_address_3))
ASYM(nbif_hipe_bifs_set_native_address_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(hipe_bifs_set_native_address_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_hipe_bifs_set_native_address_3))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_set_native_address_3))
#endif

#ifndef HAVE_nbif_hipe_bifs_set_funinfo_native_address_3
#define HAVE_nbif_hipe_bifs_set_funinfo_native_address_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_set_funinfo_native_address_3))
ASYM(nbif_hipe_bifs_set_funinfo_native_address_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(hipe_bifs_set_funinfo_native_address_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_hipe_bifs_set_funinfo_native_address_3))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_set_funinfo_native_address_3))
#endif

#ifndef HAVE_nbif_hipe_bifs_invalidate_funinfo_native_addresses_1
#define HAVE_nbif_hipe_bifs_invalidate_funinfo_native_addresses_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_invalidate_funinfo_native_addresses_1))
ASYM(nbif_hipe_bifs_invalidate_funinfo_native_addresses_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(hipe_bifs_invalidate_funinfo_native_addresses_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_hipe_bifs_invalidate_funinfo_native_addresses_1))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_invalidate_funinfo_native_addresses_1))
#endif

#ifndef HAVE_nbif_hipe_bifs_update_code_size_3
#define HAVE_nbif_hipe_bifs_update_code_size_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_update_code_size_3))
ASYM(nbif_hipe_bifs_update_code_size_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(hipe_bifs_update_code_size_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_hipe_bifs_update_code_size_3))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_update_code_size_3))
#endif

#ifndef HAVE_nbif_hipe_bifs_code_size_1
#define HAVE_nbif_hipe_bifs_code_size_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_code_size_1))
ASYM(nbif_hipe_bifs_code_size_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(hipe_bifs_code_size_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_hipe_bifs_code_size_1))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_code_size_1))
#endif

#ifndef HAVE_nbif_hipe_bifs_enter_sdesc_1
#define HAVE_nbif_hipe_bifs_enter_sdesc_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_enter_sdesc_1))
ASYM(nbif_hipe_bifs_enter_sdesc_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(hipe_bifs_enter_sdesc_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_hipe_bifs_enter_sdesc_1))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_enter_sdesc_1))
#endif

#ifndef HAVE_nbif_hipe_bifs_bif_address_3
#define HAVE_nbif_hipe_bifs_bif_address_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_bif_address_3))
ASYM(nbif_hipe_bifs_bif_address_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(hipe_bifs_bif_address_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_hipe_bifs_bif_address_3))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_bif_address_3))
#endif

#ifndef HAVE_nbif_hipe_bifs_primop_address_1
#define HAVE_nbif_hipe_bifs_primop_address_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_primop_address_1))
ASYM(nbif_hipe_bifs_primop_address_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(hipe_bifs_primop_address_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_hipe_bifs_primop_address_1))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_primop_address_1))
#endif

#ifndef HAVE_nbif_hipe_bifs_atom_to_word_1
#define HAVE_nbif_hipe_bifs_atom_to_word_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_atom_to_word_1))
ASYM(nbif_hipe_bifs_atom_to_word_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(hipe_bifs_atom_to_word_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_hipe_bifs_atom_to_word_1))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_atom_to_word_1))
#endif

#ifndef HAVE_nbif_hipe_bifs_term_to_word_1
#define HAVE_nbif_hipe_bifs_term_to_word_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_term_to_word_1))
ASYM(nbif_hipe_bifs_term_to_word_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(hipe_bifs_term_to_word_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_hipe_bifs_term_to_word_1))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_term_to_word_1))
#endif

#ifndef HAVE_nbif_hipe_bifs_make_fe_3
#define HAVE_nbif_hipe_bifs_make_fe_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_make_fe_3))
ASYM(nbif_hipe_bifs_make_fe_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(hipe_bifs_make_fe_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_hipe_bifs_make_fe_3))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_make_fe_3))
#endif

#ifndef HAVE_nbif_hipe_bifs_find_na_or_make_stub_2
#define HAVE_nbif_hipe_bifs_find_na_or_make_stub_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_find_na_or_make_stub_2))
ASYM(nbif_hipe_bifs_find_na_or_make_stub_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(hipe_bifs_find_na_or_make_stub_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_hipe_bifs_find_na_or_make_stub_2))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_find_na_or_make_stub_2))
#endif

#ifndef HAVE_nbif_hipe_bifs_check_crc_1
#define HAVE_nbif_hipe_bifs_check_crc_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_check_crc_1))
ASYM(nbif_hipe_bifs_check_crc_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(hipe_bifs_check_crc_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_hipe_bifs_check_crc_1))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_check_crc_1))
#endif

#ifndef HAVE_nbif_hipe_bifs_system_crc_1
#define HAVE_nbif_hipe_bifs_system_crc_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_system_crc_1))
ASYM(nbif_hipe_bifs_system_crc_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(hipe_bifs_system_crc_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_hipe_bifs_system_crc_1))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_system_crc_1))
#endif

#ifndef HAVE_nbif_hipe_bifs_get_rts_param_1
#define HAVE_nbif_hipe_bifs_get_rts_param_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_get_rts_param_1))
ASYM(nbif_hipe_bifs_get_rts_param_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(hipe_bifs_get_rts_param_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_hipe_bifs_get_rts_param_1))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_get_rts_param_1))
#endif

#ifndef HAVE_nbif_hipe_bifs_patch_insn_3
#define HAVE_nbif_hipe_bifs_patch_insn_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_patch_insn_3))
ASYM(nbif_hipe_bifs_patch_insn_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(hipe_bifs_patch_insn_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_hipe_bifs_patch_insn_3))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_patch_insn_3))
#endif

#ifndef HAVE_nbif_hipe_bifs_patch_call_3
#define HAVE_nbif_hipe_bifs_patch_call_3
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_patch_call_3))
ASYM(nbif_hipe_bifs_patch_call_3):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	movl ARG2,12(%esp)
	call	CSYM(hipe_bifs_patch_call_3)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(3)
	SET_SIZE(ASYM(nbif_hipe_bifs_patch_call_3))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_patch_call_3))
#endif

#ifndef HAVE_nbif_hipe_bifs_add_ref_2
#define HAVE_nbif_hipe_bifs_add_ref_2
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_add_ref_2))
ASYM(nbif_hipe_bifs_add_ref_2):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	movl ARG1,8(%esp)
	call	CSYM(hipe_bifs_add_ref_2)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(2)
	SET_SIZE(ASYM(nbif_hipe_bifs_add_ref_2))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_add_ref_2))
#endif

#ifndef HAVE_nbif_hipe_bifs_mark_referred_from_1
#define HAVE_nbif_hipe_bifs_mark_referred_from_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_mark_referred_from_1))
ASYM(nbif_hipe_bifs_mark_referred_from_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(hipe_bifs_mark_referred_from_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_hipe_bifs_mark_referred_from_1))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_mark_referred_from_1))
#endif

#ifndef HAVE_nbif_hipe_bifs_remove_refs_from_1
#define HAVE_nbif_hipe_bifs_remove_refs_from_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_remove_refs_from_1))
ASYM(nbif_hipe_bifs_remove_refs_from_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(hipe_bifs_remove_refs_from_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_hipe_bifs_remove_refs_from_1))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_remove_refs_from_1))
#endif

#ifndef HAVE_nbif_hipe_bifs_redirect_referred_from_1
#define HAVE_nbif_hipe_bifs_redirect_referred_from_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_redirect_referred_from_1))
ASYM(nbif_hipe_bifs_redirect_referred_from_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(hipe_bifs_redirect_referred_from_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_hipe_bifs_redirect_referred_from_1))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_redirect_referred_from_1))
#endif

#ifndef HAVE_nbif_hipe_bifs_call_count_on_1
#define HAVE_nbif_hipe_bifs_call_count_on_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_call_count_on_1))
ASYM(nbif_hipe_bifs_call_count_on_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(hipe_bifs_call_count_on_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_hipe_bifs_call_count_on_1))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_call_count_on_1))
#endif

#ifndef HAVE_nbif_hipe_bifs_call_count_off_1
#define HAVE_nbif_hipe_bifs_call_count_off_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_call_count_off_1))
ASYM(nbif_hipe_bifs_call_count_off_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(hipe_bifs_call_count_off_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_hipe_bifs_call_count_off_1))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_call_count_off_1))
#endif

#ifndef HAVE_nbif_hipe_bifs_call_count_get_1
#define HAVE_nbif_hipe_bifs_call_count_get_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_call_count_get_1))
ASYM(nbif_hipe_bifs_call_count_get_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(hipe_bifs_call_count_get_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_hipe_bifs_call_count_get_1))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_call_count_get_1))
#endif

#ifndef HAVE_nbif_hipe_bifs_call_count_clear_1
#define HAVE_nbif_hipe_bifs_call_count_clear_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_call_count_clear_1))
ASYM(nbif_hipe_bifs_call_count_clear_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(hipe_bifs_call_count_clear_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_hipe_bifs_call_count_clear_1))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_call_count_clear_1))
#endif

#ifndef HAVE_nbif_hipe_bifs_trap_count_get_0
#define HAVE_nbif_hipe_bifs_trap_count_get_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_trap_count_get_0))
ASYM(nbif_hipe_bifs_trap_count_get_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(hipe_bifs_trap_count_get_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_hipe_bifs_trap_count_get_0))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_trap_count_get_0))
#endif

#ifndef HAVE_nbif_hipe_bifs_trap_count_clear_0
#define HAVE_nbif_hipe_bifs_trap_count_clear_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_trap_count_clear_0))
ASYM(nbif_hipe_bifs_trap_count_clear_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(hipe_bifs_trap_count_clear_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_hipe_bifs_trap_count_clear_0))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_trap_count_clear_0))
#endif

#ifndef HAVE_nbif_hipe_bifs_process_info_0
#define HAVE_nbif_hipe_bifs_process_info_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_process_info_0))
ASYM(nbif_hipe_bifs_process_info_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(hipe_bifs_process_info_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_hipe_bifs_process_info_0))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_process_info_0))
#endif

#ifndef HAVE_nbif_hipe_bifs_process_info_clear_0
#define HAVE_nbif_hipe_bifs_process_info_clear_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_process_info_clear_0))
ASYM(nbif_hipe_bifs_process_info_clear_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(hipe_bifs_process_info_clear_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_hipe_bifs_process_info_clear_0))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_process_info_clear_0))
#endif

#ifndef HAVE_nbif_hipe_bifs_message_info_0
#define HAVE_nbif_hipe_bifs_message_info_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_message_info_0))
ASYM(nbif_hipe_bifs_message_info_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(hipe_bifs_message_info_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_hipe_bifs_message_info_0))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_message_info_0))
#endif

#ifndef HAVE_nbif_hipe_bifs_message_info_clear_0
#define HAVE_nbif_hipe_bifs_message_info_clear_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_message_info_clear_0))
ASYM(nbif_hipe_bifs_message_info_clear_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(hipe_bifs_message_info_clear_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_hipe_bifs_message_info_clear_0))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_message_info_clear_0))
#endif

#ifndef HAVE_nbif_hipe_bifs_message_sizes_0
#define HAVE_nbif_hipe_bifs_message_sizes_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_message_sizes_0))
ASYM(nbif_hipe_bifs_message_sizes_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(hipe_bifs_message_sizes_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_hipe_bifs_message_sizes_0))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_message_sizes_0))
#endif

#ifndef HAVE_nbif_hipe_bifs_gc_info_0
#define HAVE_nbif_hipe_bifs_gc_info_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_gc_info_0))
ASYM(nbif_hipe_bifs_gc_info_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(hipe_bifs_gc_info_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_hipe_bifs_gc_info_0))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_gc_info_0))
#endif

#ifndef HAVE_nbif_hipe_bifs_shared_gc_info_0
#define HAVE_nbif_hipe_bifs_shared_gc_info_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_shared_gc_info_0))
ASYM(nbif_hipe_bifs_shared_gc_info_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(hipe_bifs_shared_gc_info_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_hipe_bifs_shared_gc_info_0))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_shared_gc_info_0))
#endif

#ifndef HAVE_nbif_hipe_bifs_incremental_gc_info_0
#define HAVE_nbif_hipe_bifs_incremental_gc_info_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_incremental_gc_info_0))
ASYM(nbif_hipe_bifs_incremental_gc_info_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(hipe_bifs_incremental_gc_info_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_hipe_bifs_incremental_gc_info_0))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_incremental_gc_info_0))
#endif

#ifndef HAVE_nbif_hipe_bifs_gc_info_clear_0
#define HAVE_nbif_hipe_bifs_gc_info_clear_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_gc_info_clear_0))
ASYM(nbif_hipe_bifs_gc_info_clear_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(hipe_bifs_gc_info_clear_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_hipe_bifs_gc_info_clear_0))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_gc_info_clear_0))
#endif

#ifndef HAVE_nbif_hipe_bifs_pause_times_0
#define HAVE_nbif_hipe_bifs_pause_times_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_pause_times_0))
ASYM(nbif_hipe_bifs_pause_times_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(hipe_bifs_pause_times_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_hipe_bifs_pause_times_0))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_pause_times_0))
#endif

#ifndef HAVE_nbif_hipe_bifs_system_timer_0
#define HAVE_nbif_hipe_bifs_system_timer_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_system_timer_0))
ASYM(nbif_hipe_bifs_system_timer_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(hipe_bifs_system_timer_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_hipe_bifs_system_timer_0))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_system_timer_0))
#endif

#ifndef HAVE_nbif_hipe_bifs_system_timer_clear_0
#define HAVE_nbif_hipe_bifs_system_timer_clear_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_system_timer_clear_0))
ASYM(nbif_hipe_bifs_system_timer_clear_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(hipe_bifs_system_timer_clear_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_hipe_bifs_system_timer_clear_0))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_system_timer_clear_0))
#endif

#ifndef HAVE_nbif_hipe_bifs_send_timer_0
#define HAVE_nbif_hipe_bifs_send_timer_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_send_timer_0))
ASYM(nbif_hipe_bifs_send_timer_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(hipe_bifs_send_timer_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_hipe_bifs_send_timer_0))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_send_timer_0))
#endif

#ifndef HAVE_nbif_hipe_bifs_send_timer_clear_0
#define HAVE_nbif_hipe_bifs_send_timer_clear_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_send_timer_clear_0))
ASYM(nbif_hipe_bifs_send_timer_clear_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(hipe_bifs_send_timer_clear_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_hipe_bifs_send_timer_clear_0))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_send_timer_clear_0))
#endif

#ifndef HAVE_nbif_hipe_bifs_gc_timer_0
#define HAVE_nbif_hipe_bifs_gc_timer_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_gc_timer_0))
ASYM(nbif_hipe_bifs_gc_timer_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(hipe_bifs_gc_timer_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_hipe_bifs_gc_timer_0))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_gc_timer_0))
#endif

#ifndef HAVE_nbif_hipe_bifs_shared_gc_timer_0
#define HAVE_nbif_hipe_bifs_shared_gc_timer_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_shared_gc_timer_0))
ASYM(nbif_hipe_bifs_shared_gc_timer_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(hipe_bifs_shared_gc_timer_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_hipe_bifs_shared_gc_timer_0))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_shared_gc_timer_0))
#endif

#ifndef HAVE_nbif_hipe_bifs_gc_timer_clear_0
#define HAVE_nbif_hipe_bifs_gc_timer_clear_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_gc_timer_clear_0))
ASYM(nbif_hipe_bifs_gc_timer_clear_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(hipe_bifs_gc_timer_clear_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_hipe_bifs_gc_timer_clear_0))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_gc_timer_clear_0))
#endif

#ifndef HAVE_nbif_hipe_bifs_misc_timer_0
#define HAVE_nbif_hipe_bifs_misc_timer_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_misc_timer_0))
ASYM(nbif_hipe_bifs_misc_timer_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(hipe_bifs_misc_timer_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_hipe_bifs_misc_timer_0))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_misc_timer_0))
#endif

#ifndef HAVE_nbif_hipe_bifs_misc_timer_clear_0
#define HAVE_nbif_hipe_bifs_misc_timer_clear_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_misc_timer_clear_0))
ASYM(nbif_hipe_bifs_misc_timer_clear_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(hipe_bifs_misc_timer_clear_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_hipe_bifs_misc_timer_clear_0))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_misc_timer_clear_0))
#endif

#ifndef HAVE_nbif_hipe_bifs_get_hrvtime_0
#define HAVE_nbif_hipe_bifs_get_hrvtime_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_get_hrvtime_0))
ASYM(nbif_hipe_bifs_get_hrvtime_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(hipe_bifs_get_hrvtime_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_hipe_bifs_get_hrvtime_0))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_get_hrvtime_0))
#endif

#ifndef HAVE_nbif_hipe_bifs_stop_hrvtime_0
#define HAVE_nbif_hipe_bifs_stop_hrvtime_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_stop_hrvtime_0))
ASYM(nbif_hipe_bifs_stop_hrvtime_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(hipe_bifs_stop_hrvtime_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_hipe_bifs_stop_hrvtime_0))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_stop_hrvtime_0))
#endif

#ifndef HAVE_nbif_hipe_bifs_show_estack_1
#define HAVE_nbif_hipe_bifs_show_estack_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_show_estack_1))
ASYM(nbif_hipe_bifs_show_estack_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(hipe_bifs_show_estack_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_hipe_bifs_show_estack_1))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_show_estack_1))
#endif

#ifndef HAVE_nbif_hipe_bifs_show_heap_1
#define HAVE_nbif_hipe_bifs_show_heap_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_show_heap_1))
ASYM(nbif_hipe_bifs_show_heap_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(hipe_bifs_show_heap_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_hipe_bifs_show_heap_1))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_show_heap_1))
#endif

#ifndef HAVE_nbif_hipe_bifs_show_nstack_1
#define HAVE_nbif_hipe_bifs_show_nstack_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_show_nstack_1))
ASYM(nbif_hipe_bifs_show_nstack_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(hipe_bifs_show_nstack_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_hipe_bifs_show_nstack_1))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_show_nstack_1))
#endif

#ifndef HAVE_nbif_hipe_bifs_nstack_used_size_0
#define HAVE_nbif_hipe_bifs_nstack_used_size_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_nstack_used_size_0))
ASYM(nbif_hipe_bifs_nstack_used_size_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(hipe_bifs_nstack_used_size_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_hipe_bifs_nstack_used_size_0))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_nstack_used_size_0))
#endif

#ifndef HAVE_nbif_hipe_bifs_show_pcb_1
#define HAVE_nbif_hipe_bifs_show_pcb_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_show_pcb_1))
ASYM(nbif_hipe_bifs_show_pcb_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(hipe_bifs_show_pcb_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_hipe_bifs_show_pcb_1))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_show_pcb_1))
#endif

#ifndef HAVE_nbif_hipe_bifs_show_term_1
#define HAVE_nbif_hipe_bifs_show_term_1
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_show_term_1))
ASYM(nbif_hipe_bifs_show_term_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(hipe_bifs_show_term_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(nbif_hipe_bifs_show_term_1))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_show_term_1))
#endif

#ifndef HAVE_nbif_hipe_bifs_in_native_0
#define HAVE_nbif_hipe_bifs_in_native_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_in_native_0))
ASYM(nbif_hipe_bifs_in_native_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(hipe_bifs_in_native_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_hipe_bifs_in_native_0))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_in_native_0))
#endif

#ifndef HAVE_nbif_hipe_bifs_modeswitch_debug_on_0
#define HAVE_nbif_hipe_bifs_modeswitch_debug_on_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_modeswitch_debug_on_0))
ASYM(nbif_hipe_bifs_modeswitch_debug_on_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(hipe_bifs_modeswitch_debug_on_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_hipe_bifs_modeswitch_debug_on_0))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_modeswitch_debug_on_0))
#endif

#ifndef HAVE_nbif_hipe_bifs_modeswitch_debug_off_0
#define HAVE_nbif_hipe_bifs_modeswitch_debug_off_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_modeswitch_debug_off_0))
ASYM(nbif_hipe_bifs_modeswitch_debug_off_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(hipe_bifs_modeswitch_debug_off_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_hipe_bifs_modeswitch_debug_off_0))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_modeswitch_debug_off_0))
#endif

#ifndef HAVE_nbif_hipe_bifs_show_message_area_0
#define HAVE_nbif_hipe_bifs_show_message_area_0
	TEXT
	.align	4
	GLOBAL(ASYM(nbif_hipe_bifs_show_message_area_0))
ASYM(nbif_hipe_bifs_show_message_area_0):
	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	call	CSYM(hipe_bifs_show_message_area_0)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(0)
	SET_SIZE(ASYM(nbif_hipe_bifs_show_message_area_0))
	TYPE_FUNCTION(ASYM(nbif_hipe_bifs_show_message_area_0))
#endif


/*
 * Guard BIFs.
 * GBIF_LIST(FunctionAtom,Arity,CFun)
 */

/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2001-2011. All Rights Reserved.
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
/*
 * GBIF_LIST(FunctionAtom,Arity,CFun)
 * manually maintained for now -- expand when necessary
 */

#ifndef HAVE_gbif_node_1
#define HAVE_gbif_node_1
	TEXT
	.align	4
	GLOBAL(ASYM(gbif_node_1))
ASYM(gbif_node_1):
	/* copy native stack pointer */
	

	/* switch to C stack */
	SWITCH_ERLANG_TO_C

	/* make the call on the C stack */
	movl P,(%esp)
	movl ARG0,4(%esp)
	call	CSYM(node_1)
	TEST_GOT_MBUF

	/* switch to native stack */
	SWITCH_C_TO_ERLANG

	/* return */
	NSP_RET0
	HANDLE_GOT_MBUF(1)
	SET_SIZE(ASYM(gbif_node_1))
	TYPE_FUNCTION(ASYM(gbif_node_1))
#endif



#if defined(__linux__) && defined(__ELF__)
.section .note.GNU-stack,"",%progbits
#endif
