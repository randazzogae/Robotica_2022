% ----------------------------------------------------------------------
% BEGIN LICENSE BLOCK
% Version: CMPL 1.1
%
% The contents of this file are subject to the Cisco-style Mozilla Public
% License Version 1.1 (the "License"); you may not use this file except
% in compliance with the License.  You may obtain a copy of the License
% at www.eclipse-clp.org/license.
% 
% Software distributed under the License is distributed on an "AS IS"
% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See
% the License for the specific language governing rights and limitations
% under the License. 
% 
% The Original Code is  The ECLiPSe Library iso_strict.
% The Initial Developer of the Original Code is  Coninfer Ltd.
% Portions created by the Initial Developer are
% Copyright (C) 2010-2013 Coninfer Ltd
% 
% END LICENSE BLOCK
%
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: iso_strict.ecl,v 1.16 2017/08/19 20:00:11 jschimpf Exp $
% ----------------------------------------------------------------------

%
% ECLiPSe PROLOG LIBRARY MODULE
%
% IDENTIFICATION:	iso_strict.pl
%
% AUTHOR:		Joachim Schimpf
%
% CONTENTS:		see export directive
%
% DESCRIPTION:		Strict ISO 13211-1 standard language module
%

:- module(iso_strict).

:- pragma(system).

% Workaround: make tools (which don't obey the system directive) into built-ins
:- local initialization((
    current_module_predicate(exported,P), set_flag(P, type, built_in), fail ; true
)).

% Tell the compiler that we are strict
:- export initialization(eclipse_language:error(148,pragma(iso(strict)))).

:- export
	chtab(0'`, string_quote),
	chtab(0'", list_quote).

:- comment(categories, [`Compatibility`]).
:- comment(summary, `Strict ISO Prolog compatibility library`).
:- comment(author, `Joachim Schimpf, Coninfer Ltd`).
:- comment(copyright, 'Joachim Schimpf, Coninfer Ltd').
:- comment(date, `$Date: 2017/08/19 20:00:11 $`).
:- comment(see_also, [library(multifile),library(iso),library(iso_light)]).
:- comment(desc, html(`
<h3>Overview</h3>
    This library provides an implementation of Standard Prolog as
    defined in ISO/IEC 13211-1 (Information Technology, Programming
    Languages, Prolog, Part 1, General Core, 1995) and the technical
    corrigenda ISO/IEC 13211-1 TC1 (2007), TC2 (2012) and TC3 (2017).
    <P>
    It represents the "strict mode" required by ISO 13211-1, paragraph 5.1.e.
    For a non-strict version, see library(iso).
    <P>
    This is one of three libraries providing different degrees of ISO
    compatibility:
    <DL>
    <DT><STRONG>iso_strict</STRONG></DT>
        <DD>Aims to be fully ISO compliant, and represents the
        'strict mode' required by the standard.</DD>
    <DT><STRONG>iso</STRONG></DT>
        <DD>Aims to be fully ISO compliant, but also includes
        ECLiPSe functionality that does not conflict with the standard.
    <DT><STRONG>iso_light</STRONG></DT>
        <DD>Provides the essence of ISO features without aiming for full
        conformance (in particular with respect to error handling), and
        may include ECLiPSe extensions that go beyond what the letter of
        the standard allows.  Also useful for importing individual ISO
        features into a program that is otherwise written in ECLiPSe dialect.
    </DL>
    <P>
<h3>Usage</h3>
    The effect of this compatibility library is (with minor exceptions)
    local to the module where it is loaded.  An ISO-program should always
    be contained in a separate module, starting with a directive like
    <PRE>
    :- module(myisomodule, [], iso_strict).
    </PRE>
    Here, the last argument of the module/3 directive indicates the language.
    It is not advisable to use ":-lib(iso_strict)" or
    ":-ensure_loaded(library(iso_strict))" within an eclipse_language module,
    because this would lead to import conflicts between the different
    versions of built-in predicates.
    <P>
    Alternatively, in order to use ISO-Prolog without having different
    modules, one can invoke eclipse with a "-L iso_strict" command line option,
    or set the ECLIPSEDEFFAULTLANGUAGE environment variable to 'iso_strict'.
    This will launch eclipse with a default module accepting 'iso_strict'
    language instead of the usual 'eclipse_language'.
    <P>
<h3>Specification of implementation defined features</h3>
    <DL>
    <DT>6.5 Processor characted set</DT>
	<DD>The PCS is the ISO 8859-1 character set.  Classification of
	extended characters: 7f-a0 layout; a1-bf, d7, f7 graphic;
	c0-d6, d8-f6, f8-ff alphanumeric.</DD>
    <DT>6.6 Collating sequence</DT>
	<DD>The collating sequence is that of the ISO 8859-1 character set</DD>
    <DT>7.1.2.2 Character codes</DT>
	<DD>Each character maps to a corresponding byte</DD>
    <DT>7.1.4.1 Characters</DT>
	<DD>As in the ISO 8859-1 character set</DD>
    <DT>7.2.1 Variable term order</DT>
	<DD>Older variables precede newer variables</DD>
    <DT>7.4.2.4,5 op/3 and char_conversion/2</DT>
	<DD>An operator or character-conversion defined in a directive is
	effective at runtime, and only in the module in which it occurs</DD>
    <DT>7.4.2.6 initialization/1</DT>
	<DD>Initialization goals are executed in the order in which they
	occur in the Prolog text</DD>
    <DT>7.4.2.7 include/1</DT>
	<DD>The argument is a file name atom according to ECLiPSe\'s canonical
	file name syntax, or a term of the form library(atom)</DD>
    <DT>7.4.2.8 ensure_loaded/1</DT>
	<DD>The argument is a file name atom according to ECLiPSe\'s canonical
	file name syntax, or a term of the form library(atom).  A file will
	be loaded on the first occurrence of ensure_loaded/1 in a prolog text,
	and if the file has been modified since the time it was first loaded.
	</DD>
    <DT>7.4.2.7 set_prolog_flag/2</DT>
	<DD>Flag setting are effective at runtime and globally (except for
	a few module-local flags in non-strict mode, see set_flag/2)</DD>
    <DT>7.5.1 Preparing for execution</DT>
	<DD>See the eclipse_language built-ins, menu items and command line
	options for compiling, loading and module handling, and also the
	instructions for using library(iso) or library(iso_strict)</DD>
    <DT>7.7.1,3 Execution and Initialization</DT>
	<DD>See the general ECLiPSe facilities, i.e. toplevel, graphical
	user interface and command line options</DD>
    <DT>7.10.1 Sources and sinks</DT>
	<DD>See open/3,4</DD>
    <DT>7.10.2.6 Text streams</DT>
	<DD>Text streams are very similar to binary streams, no characters
	are implicitly inserted or removed.  The nl/0,1 predicates emit
	an operating system and device dependent newline sequence.</DD>
    <DT>7.10.2.8,11 Stream positions</DT>
	<DD>File, string, and null streams can be (re)positioned</DD>
    <DT>7.10.2.9 End position of a stream</DT>
	<DD>The end position of a stream is the same as the position that
	a character appended to the stream would have</DD>
    <DT>7.10.2.11 Stream options</DT>
	<DD>The default eof_action is error</DD>
    <DT>7.10.2.13 Stream properties</DT>
	<DD>File names are atoms according to ECLiPSe\'s canonical file name
	syntax.</DD>
    <DT>7.11 Flags</DT>
	<DD>Fixed values: bounded=false, min_integer and max_integer fail,
	integer_rounding_function=toward_zero, max_arity=unbounded,
	char_conversion=off.
	Default values: double_quotes=chars, debug=off.
	If debug=on, the ECLiPSe tracer is active.  In non-strict mode,
	there is an additional flag max_predicate_arity, which indicates
	the limit on predicate arity (there is no limit on term arity).
	</DD>
    <DT>7.12.1 Effect of an error</DT>
	<DD>The implementation defined error term argument is normally the
	predicate indicator of the culprit goal.  For syntax errors, it is
	a term describing the error location.</DD>
    <DT>8.15.4 call/N</DT>
	<DD>The maximum N is given by the flag max_predicate_arity (255)</DD>
    <DT>8.17.1 set_prolog_flag/2</DT>
	<DD>The admissible flag values are the ones defined by ISO-Prolog</DD>
    <DT>8.17.3,4 halt/0,1</DT>
	<DD>Exits the OS process with the given return code (or 0)</DD>
    <DT>9 Evaluable functors</DT>
        <DD>The 'exceptional values' are realized as follows: 'float_overflow'
	leads to a floating point infinity result; 'underflow' leads to
	a floating point denormalized value result; 'zero_divisor' leads
	to a floating point infinity result in the case of floats,
	or an evaluation_error(zero_divisor) in the case of integers;
	'int_overflow' does not occur and might lead to running out of
	memory instead.</DD>
    <DT>9.3 Other arithmetic operations</DT>
	<DD>When min/2 or max/2 are used with mixed integer and float
	arguments, the integer is coerced to float, and the result
	computed by comparing two floats</DD>
    <DT>9.4 Bitwise arithmetic operations</DT>
	<DD>The bitwise arithmetic operations behave as if operating on
	an unlimited length two\'s complement representation</DD>
    </DL>

<h3>Implementation specific features</h3>
    None.

<h3>Remaining deviations from Standard</h3>
    <OL>
    <LI>The char_conversion flag is always off, meaning that character
    conversion is not applied to prolog texts or on term input.  However,
    char_conversion/2 and current_char_conversion/2 predicates are operational.
    </OL>
    `)).

:- reexport
	true/0,				% built-ins
	fail/0,
	!/0,
	(',')/2,
	(;)/2,
	(->)/2,
	catch/3,
	(=)/2,				% 8.2
	(\=)/2,
	var/1,				% 8.3
	atom/1,
	integer/1,
	float/1,
	atomic/1,
	compound/1,
	nonvar/1,
	number/1,
	callable/1,
	ground/1,
	acyclic_term/1,
	(@=<)/2,			% 8.4
	(==)/2,
	(\==)/2,
	(@<)/2,
	(@>)/2,
	(@>=)/2,
	functor/3,			% 8.5
	arg/3,
	(=..)/2,
	copy_term/2,
	current_predicate/1,
	retractall/1,			% 8.9
	close/1,			% 8.11
	close/2,
	nl/0,				% 8.12
	nl/1,
	throw/1,
	write/1,
	write/2,
	writeq/1,
	writeq/2,
	write_canonical/1,
	write_canonical/2,
	(\+)/1,				% 8.15
	once/1,
	false/0,
	repeat/0,
	char_code/2,
	halt/0,				% 8.17

	(:)/2				% for modules

   from eclipse_language.


:- export
	syntax_option(dense_output),
	syntax_option(not(nl_in_quotes)),
	syntax_option(iso_escapes),
	syntax_option(iso_base_prefix),
	syntax_option(iso_restrictions),
	syntax_option(no_string_concatenation),
	syntax_option(eof_is_no_fullstop),
	syntax_option(not(syntax_errors_fail)),
	syntax_option(plus_is_no_sign),
	syntax_option(doubled_quote_is_quote),
	syntax_option(no_array_subscripts),
	syntax_option(bar_is_no_atom),
	syntax_option(no_attributes),
	syntax_option(no_curly_arguments),
	syntax_option(blanks_after_sign),
	syntax_option(float_needs_point),
	syntax_option(limit_arg_precedence).


:- reexport
	(abolish)/1,
	asserta/1,			% 8.9
	assertz/1,
	at_end_of_stream/0,
	at_end_of_stream/1,
	atom_concat/3,
	atom_codes/2,
	atom_chars/2,
	atom_length/2,			% 8.16
	char_conversion/2,
	clause/2,			% 8.8
	current_char_conversion/2,
	current_input/1,
	current_op/3,
	current_output/1,
	flush_output/0,
	flush_output/1,
	get_byte/1,
	get_byte/2,
	get_char/1,
	get_char/2,
	get_code/1,
	get_code/2,
	halt/1,
	number_chars/2,
	number_codes/2,
	op/3,
	peek_byte/1,
	peek_byte/2,
	peek_char/1,
	peek_char/2,
	peek_code/1,
	peek_code/2,
	put_byte/1,
	put_byte/2,
	put_code/1,
	put_code/2,
	put_char/1,
	put_char/2,
	retract/1,
	set_input/1,
	set_output/1,
	set_stream_position/2,
	sub_atom/5,
	subsumes_term/2,
	unify_with_occurs_check/2
    from iso_light.

:- reexport
	call/1,					% 7.8
	call/2,
	call/3,
	compare/3,				% 8.4
	sort/2,
	keysort/2,
	term_variables/2,			% 8.5.5
	findall/3,				% 8.10
	setof/3,
	bagof/3,
	read/1,
	read/2
    from iso_heavy.

:- ensure_loaded(library(multifile)).	% for directive
:- ensure_loaded(library(iso_error)).
:- use_module(library(iso_aux)).

t_syntax(_IllegalType, _) :-
	throw(114).	% UNEXPECTED token

?- export initialization((
					% hide (global,non-portray) macros
	eclipse_language:current_macro(F,_,Opt,sepia_kernel),
	eclipse_language:nonmember(write, Opt),
	eclipse_language:delete(global, Opt, Opt1),
	eclipse_language:local(macro(F, (=)/2, Opt1)),
	fail
    ;
					% disallow ECLiPSe types
	eclipse_language:local(macro(type(rational), t_syntax/2, [])),
	eclipse_language:local(macro(type(breal), t_syntax/2, [])),
	eclipse_language:local(macro(type(string), t_syntax/2, [])),
	fail
    ;
	eclipse_language:current_op(P, A, Op),		% hide all (global) operators
	Op \== (','),
	op(0, A, Op),
	fail
    ;
	op(1200,  fx, :-),		% define ISO ones only
	op(1200, xfx, -->),
	op(1200,  fx, ?-),
	op(1200, xfx, :-),
	op(1100, xfy, ;),
	op(1050, xfy, ->),
	%op(1000, xfy, (',')),
	op(900,  fy, \+),
	op(700, xfx, =..),
	op(700, xfx, =),
	op(700, xfx, \=),
	op(700, xfx, ==),
	op(700, xfx, \==),
	op(700, xfx, @<),
	op(700, xfx, @=<),
	op(700, xfx, @>),
	op(700, xfx, @>=),
	op(700, xfx, is),
	op(700, xfx, =:=),
	op(700, xfx, =\=),
	op(700, xfx, <),
	op(700, xfx, =<),
	op(700, xfx, >),
	op(700, xfx, >=),
	op(600, xfy, :),
	op(500, yfx, +),
	op(500, yfx, -),
	op(500, yfx, /\),
	op(500, yfx, \/),
	op(400, yfx, /),
	op(400, yfx, *),
	op(400, yfx, //),
	op(400, yfx, >>),
	op(400, yfx, <<),
	op(400, yfx, rem),
	op(400, yfx, div),
	op(400, yfx, mod),
	op(200, xfx, **),
	op(200, xfy, ^),
	op(200,  fy, +),
	op(200,  fy, -),
	op(200,  fy, \)
    )).


%-----------------------------------------------------------------------
% 8.11 Stream selection and control
% open/3,4:	Restrictions on mode and options
%-----------------------------------------------------------------------

:- export open/3, open/4.
open(SourceSink, Mode, Stream) :-
	iso_heavy:open(SourceSink, Mode, Stream, [], true).

open(SourceSink, Mode, Stream, Options) :-
	iso_heavy:open(SourceSink, Mode, Stream, Options, true).


:- export stream_property/2.
stream_property(Stream, Property) :-
	iso_heavy:stream_property(Stream, Property, true).


%-----------------------------------------------------------------------
% 8.14 Term input/output
% read_term:	binary restriction and option error handling
% write_term:	binary restriction, options, option error handling
%-----------------------------------------------------------------------

:- export read_term/2.
:- tool(read_term/2, read_term_/3).
read_term_(Term, Options, Module) :-
	iso_heavy:read_term(input, Term, Options, Module, true).

:- export read_term/3.
:- tool(read_term/3, read_term_/4).
read_term_(Stream, Term, Options, Module) :-		% 8.14.1
	iso_heavy:read_term(Stream, Term, Options, Module, true).


:- export write_term/2.
:- tool(write_term/2, write_term_/3).
write_term_(Term, Options, Module) :-
	iso_heavy:write_term(output, Term, Options, Module, true).

:- export write_term/3.
:- tool(write_term/3, write_term_/4).
write_term_(Stream, Term, Options, Module) :-		% 8.14.2
	iso_heavy:write_term(Stream, Term, Options, Module, true).


%-----------------------------------------------------------------------
% 8.17 Implementation defined hooks
%-----------------------------------------------------------------------

:- export set_prolog_flag/2.
:- tool(set_prolog_flag/2, set_prolog_flag_/3).
set_prolog_flag_(Flag, Value, M) :-
	iso_aux:set_prolog_flag(Flag, Value, M, true).

:- export current_prolog_flag/2.
:- tool(current_prolog_flag/2, current_prolog_flag_/3).
current_prolog_flag_(Flag, Value, M) :-
	iso_aux:current_prolog_flag(Flag, Value, M, true).


%-----------------------------------------------------------------------
% 9. Evaluable functors
% - restricted set of operations
% - no user-defined operations
% - ISO errors
% The bulk of this is in iso_aux!
%-----------------------------------------------------------------------

:- export (is)/2.
:- tool((is)/2, is_/3).
is_(R, X, M) :-
	is_(R, X, M, strict).

trans_is(Goal, Expanded) :-
	trans_is(Goal, Expanded, strict).

:- inline((is)/2, trans_is/2).


:- export (>=)/2, (>)/2, (=<)/2, (<)/2, (=:=)/2, (=\=)/2.
:- tool((<)/2, (<)/3),
   tool((>)/2, (>)/3),
   tool((=<)/2, (=<)/3),
   tool((>=)/2, (>=)/3),
   tool((=:=)/2, (=:=)/3),
   tool((=\=)/2, (=\=)/3).

% redefine the comparisons, using the visible is/2
<(X,Y,M)   :- (X1 is X)@M, (Y1 is Y)@M, eclipse_language:(X1<Y1)@M.
>(X,Y,M)   :- (X1 is X)@M, (Y1 is Y)@M, eclipse_language:(X1>Y1)@M.
=<(X,Y,M)  :- (X1 is X)@M, (Y1 is Y)@M, eclipse_language:(X1=<Y1)@M.
>=(X,Y,M)  :- (X1 is X)@M, (Y1 is Y)@M, eclipse_language:(X1>=Y1)@M.
=:=(X,Y,M) :- (X1 is X)@M, (Y1 is Y)@M, eclipse_language:(X1=:=Y1)@M.
=\=(X,Y,M) :- (X1 is X)@M, (Y1 is Y)@M, eclipse_language:(X1=\=Y1)@M.

trans_compare(Goal, Expanded) :-
	trans_compare(Goal, Expanded, strict).

:- inline((>=)/2, trans_compare/2).
:- inline((>)/2, trans_compare/2).
:- inline((=<)/2, trans_compare/2).
:- inline((<)/2, trans_compare/2).
:- inline((=:=)/2, trans_compare/2).
:- inline((=\=)/2, trans_compare/2).

