/* BEGIN LICENSE BLOCK
 * Version: CMPL 1.1
 *
 * The contents of this file are subject to the Cisco-style Mozilla Public
 * License Version 1.1 (the "License"); you may not use this file except
 * in compliance with the License.  You may obtain a copy of the License
 * at www.eclipse-clp.org/license.
 * 
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See
 * the License for the specific language governing rights and limitations
 * under the License. 
 * 
 * The Original Code is  The ECLiPSe Constraint Logic Programming System. 
 * The Initial Developer of the Original Code is  Cisco Systems, Inc. 
 * Portions created by the Initial Developer are
 * Copyright (C) 1989-2006 Cisco Systems, Inc.  All Rights Reserved.
 * 
 * Contributor(s): 
 * 
 * END LICENSE BLOCK */

/*
 * SEPIA INCLUDE FILE
 *
 * VERSION	$Id: error.h,v 1.9 2017/09/01 03:05:09 jschimpf Exp $
 *
 * IDENTIFICATION		error.h
 *
 * DESCRIPTION:                 defines the SEPIA errors as their associated	
 *				error numbers.	
 */

/*
 * DEFINES:
 */

/* Macro that expands into "<file>:<line>" */
#define __FILE_LINE__ __FILE__ ":" stringify_step1(__LINE__)
#define stringify_step1(s) stringify_step2(s)
#define stringify_step2(s) #s


#define MAX_ERRORS		370	/* size of the message array */
#define USER_ERROR		340	/* first user-reserved entry */

/*
 * Non-error status usually returned by the builtins
 */

/* 0..7 are PSUCCEED,PFAIL,PTHROW,PEXITED,PYIELD,PRUNNING,PWAITIO,PFLUSHIO	*/
#define PKEEP			0x08	/* keep-flag, not used by builtins */

/* 16 and above are used for delaying on builtin arguments	*/
/* the lower 3 bits specify on which args to delay	*/
/* if they are zero, the SV list is used to delay on 	*/
#define PDELAY			0x10	/* delay on argument or SV list	*/
#define PDELAY_BOUND		0x08	/* put on the bound list, not inst */
#define PDELAY_MASK		0x07	/* mask for the arity		*/
#define PDELAY_1		0x11	/* delay on argument 1		*/
#define PDELAY_2		0x12	/* delay on argument 2		*/
#define PDELAY_1_2		0x13	/* delay on argument 1 and 2	*/
#define PDELAY_3		0x14	/* delay on argument 3		*/
#define PDELAY_1_3		0x15	/* delay on argument 1 and 3	*/
#define PDELAY_2_3		0x16	/* delay on argument 2 and 3	*/
#define PDELAY_1_2_3		0x17	/* delay on argument 1, 2 and 3	*/

/*
 * Error codes are negative numbers in C code.
 * Note that in Prolog the positive counterparts are used!
 */

#define PERROR			-1	/* general error */
#define UNIFY_OVNI		-2	/* unknown object type */
#if 0	/* these have been moved to ec_public.h */
#define EC_EXTERNAL_ERROR	-3	/* error in a user-defined external */
#define INSTANTIATION_FAULT	-4	/* variable instead of constant */
#define TYPE_ERROR		-5	/* wrong type */
#define RANGE_ERROR		-6	/* out of range */
#endif
#define BAD_FORMAT_STRING	-7	/* in printf */
#define BAD_ARGUMENT_LIST	-8	/* in printf */

#define META_TERM_UNIFY		-11	/* metaterm unification */
#define	FAIL_TO_PAR_CHP		(-16)
#define	CREATING_PAR_CHP	(-15)
#define RECOMP_FAILED		-17	/* failure during oracle following */
#define ARITH_EXCEPTION		-20	/* arithmetic exception(0 division) */
#define COMPARE_TRAP		-23	/* non-number in comparison	*/
#define ARITH_TYPE_ERROR	-24	/* non-number arithmetic operand */
#define INTEGER_OVERFLOW	-25
#define ARITY_LIMIT		-31	/* arity limit exceeded */
#define EVENT_IGNORED		-32
#define EVENT_QUEUE_FULL	-33
#define STALE_HANDLE		-40	/* stale external object handle */
#define NOGLOBAL		-41	/* global var. or array does not
					 * exist */
#define ARRAY_EXISTS		-42
#define ILLEGAL_OP_DEF          -43     /* restriction on operator definition */
#define LOCAL_REC		-44	/* local record exists		*/
#define	NO_LOCAL_REC		-45	/* no local record		*/

#define GROUND_CONST_MODIFY	-50

#define NOENTRY			-60	/* entry does not exist */
#define TOOL_REDEF		-61
#define INCONSISTENCY		-62
#define	NOT_DYNAMIC		-63	/* should be dynamic */
#define ALREADY_DYNAMIC		-64
#define ALREADY_DEFINED		-65	/* probably not necessary */
#define REDEF_SYS		-66
#define NOT_LOADED		-67
#define CALLING_UNDEFINED	-68
#define CALLING_AUTOLOAD	-69
#define ACCESSING_UNDEF_DYN_PROC -70	/* no defined dynamic at all */
#define ALREADY_PARALLEL	-71
#define UNDEF_OPERATOR		-72
#define REDEF_OPERATOR		-73
#define HIDING_OPERATOR		-74
#define CALLING_DYNAMIC		-79

#define MODULENAME		-80
#define DIRECTIVE		-81
#define LOCKED			-82
#define NO_LOOKUP_MODULE	-86
#define REEXPORT_EXISTS		-90
#define NO_TOOL			-91
#define LOCAL_EXISTS		-92
#define EXPORT_EXISTS		-93
#define IMPORT_EXISTS		-94
#define IMPORT_PENDING		-95
#define IMPORT_CLASH_RESOLVE	-96
#define MODULE_EXISTS		-97
#define WRONG_UNLOCK_STRING	-98
#define IMPORT_CLASH		-99
#define	ACCESSING_NON_LOCAL	-100
#define SELF_DESTRUCTION	-101

#define	ERR_PARSE		-110

#define PUNCTUATION             -111
#define ILL_QUOTED		-112
#define UNEXCOMMA 	        -113
#define UNEXPECTED              -114
#define ENDOFFILE               -115
#define	BAD_NUMERIC_CONSTANT	-116
#define BRACKET                 -117
#define	ENDOFCLAUSE		-118
#define	POSTINF			-119
#define	SOLOCH			-120
#define BLANK			-121
#define MULTI_META		-122

#define	PREFINF			-124
#define	UNCLOSBR 		-125
#define GRAMMAR_HEAD		-126
#define GRAMMAR_BODY		-127
#define TRANS_ERROR		-128
#define	QUERY_FLOUNDERED	-129
#define	ILLEGAL_HEAD		-130
#define	ILLEGAL_GOAL		-131
#define VARIABLE_GOAL		ILLEGAL_GOAL
#define WRONG_TYPE              -132
#define	LIBRARY			-133
#define	CONSECUTIVE		-134
#define	PROTECT_REDEF		-135
#define	BUILT_IN_REDEF		-136
#define	INCONSISTENT_REDEF	-137
#define COMPILED_FILE		-139
#define UNDEFINED		-140	/* unknown instruction */
#define UNIMPLEMENTED		-141
#define NOT_AVAILABLE		-142
#define QUERY_FAILED		-143
#define MULTIFILE		-145
#define BAD_PRAGMA		-148
#define CODE_UNIT_LOADED	-149
#define GLOBAL_TR_EXISTS	-160
#define TR_IN_MOD		-161
#define NO_TR			-162
#define	ONE_SQ_AQ		-163

#define SYS_ERROR		-170	/* operating system error: error code
					 * is in ec_eng->last_os_err{no,grp}
					 */
#define NO_FILE			-171
#define MPS_ERROR		-176
#define NO_SHARED_LIB		-177
/*
 * More specific forms of SYS_ERROR that indicate where the
 * actual system error code is to be found:
 *	SYS_ERROR_ERRNO - errno (used on Unix and Windows)
 *	SYS_ERROR_WIN   - GetLastError() (used on Windows only)
 *	SYS_ERROR_OS    - one of the above, depending on the OS
 * They should ideally not be passed to the Prolog level (use SYS_ERROR)!
 */
#define SYS_ERROR_ERRNO		-178
#define SYS_ERROR_WIN		-179
#ifdef _WIN32
#define SYS_ERROR_OS		SYS_ERROR_WIN
#else
#define SYS_ERROR_OS		SYS_ERROR_ERRNO
#endif

#define ENGINE_BUSY		-180
#define ENGINE_NOT_ASYNC	-181
#define ENGINE_DEAD		-182
#define ENGINE_NOT_UP		-183
#define ENGINE_NOT_OWNED	-184
#define PEOF			-190	/* end of file in an IO predicate */
/*#define OUT_ERROR		-191	   like SYS_ERROR for output functions*/
#define STREAM_MODE		-192 	/* wrong stream mode */
#define STREAM_SPEC		-193
#define TOO_MANY_NAMES		-194
#define YIELD_ON_FLUSH_REQ	-195
#define SYSTEM_STREAM		-196
#define INCORRECT_USER		-197
#define READ_PAST_EOF		-198	/* reading after PEOF was reached */
#define	REMEMBER		-210
#define NOCODE			-211	/* no code in the descriptor	*/
#define ILLEGAL_RETURN		-212	/* bad return code from external */
#define EC_LICENSE_ERROR	-214

#define IC_EXCLUSION_ERROR	-220    /* A value was excluded from
                                           an IC variable which should
                                           have been representable in
                                           the domain but was not
                                           because of size limitations
                                           (eg. size of a bitmap) */

#define	DEBUG_INIT_EVENT	-250	/* debugger interface events */
#define	DEBUG_PORT_EVENT	-252
#define	DEBUG_BIPFAIL_EVENT	-251	/* debug notification events */
#define	DEBUG_CALL_EVENT	-253
#define	DEBUG_EXIT_EVENT	-254
#define	DEBUG_REDO_EVENT	-255
#define	DEBUG_DELAY_EVENT	-256
#define	DEBUG_WAKE_EVENT	-257
#define	DEBUG_BIPCALL_EVENT	-258
#define	DEBUG_BIPEXIT_EVENT	-259
#define	DEBUG_SUSP_EVENT	-249

#define UNEXPECTED_EOF		-260
#define INVALID_SS		-261
#define CANT_ALLOCATE		-262
#define WRONG_LEVEL		-263
#define NOT_DUMP_FILE		-264
#define BAD_DUMP_VERSION	-265
#define NOT_IMPLEMENTED		-267
#define NOT_IN_PARALLEL		-268

#define	UNDEF_ATTR	        -270
#define	ATTR_FORMAT	        -271
#define	BAD_DELAY_CONDITION     -272


/* Panic messages */
#define	MEMORY_P		"Out of memory - no more swap space"
#define MEMORY_CORRUPTED_P	"Internal error - memory corrupted"

#define Not_Available_Built_In(Name)		\
		static int Name(){Bip_Error(NOT_AVAILABLE);}
